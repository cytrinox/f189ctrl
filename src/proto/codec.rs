use super::{command::ClearMemory, response::DeviceSettings};
use crate::{
    proto::command::Command,
    rawmea::{RawRecordingSession, RAW_INFO_LEN},
};
use crate::{
    proto::response::{Ident, Response, ResponsePayload},
    rawmea::RawMeasurement,
};
use bytes::BytesMut;
use std::{
    fmt::{self, Write},
    io::{self},
};
use tokio_util::codec::{Decoder, Encoder};

const STATUS_LEN: usize = 2;
const EOL_LEN: usize = 1; // one byte for '\r'

#[derive(Default)]
pub struct ProtocolCodec {
    last_cmd: Option<Command>,
}

impl ProtocolCodec {
    pub(crate) fn get_payload(src: &BytesMut) -> Option<Vec<u8>> {
        let offset = src.as_ref().iter().skip(2).position(|b| *b == b'\r');
        offset.map(|n| Vec::from(&src[2..n + 2]))
    }
}

impl Decoder for ProtocolCodec {
    type Item = Response;
    // We use io::Error here instead of our own Error type beacause
    // for the low level protocol, receiving an ExecutionError or the like
    // is totally fine, as the decoding is successful. Deciding if this should
    // be returned as an error is up to a higher level.
    type Error = io::Error;

    fn decode(&mut self, src: &mut BytesMut) -> Result<Option<Self::Item>, Self::Error> {
        if src.len() >= 2 {
            if (src[1] as char) != '\r' {
                if src[0..=1] == [b'X', b'E'] {
                    // While device is in logging mode + SAVE is pressed and QD command are executed,
                    // the device responses with a XE block, followed by the ACK and QD
                    // response. The XE block is a raw measurement block holding the saved
                    // measurement.
                    if src.len() >= 2 + 44 {
                        // Take out the XE block and continue.
                        let _saved_mea = RawMeasurement::try_from(&src[4..44 + 2])?; // 'XE, '
                        let _ = src.split_to(2 + 44);
                        return Ok(None);
                    } else {
                        return Ok(None);
                    }
                } else {
                    return Err(io::Error::new(
                        io::ErrorKind::Other,
                        "Device response code expected",
                    ));
                }
            }
            match src[0] as char {
                '0' => {
                    // Success

                    match self.last_cmd {
                        Some(Command::SetSettings(_))
                        | Some(Command::Clear(_))
                        | Some(Command::StartLogging)
                        | Some(Command::StopLogging)
                        | Some(Command::ResetDevice) => {
                            let _ = src.split_to(STATUS_LEN);
                            Ok(Some(Response::Success(None)))
                        }
                        Some(Command::Id) => {
                            if let Some(payload) = Self::get_payload(src) {
                                let _ = src.split_to(STATUS_LEN + payload.len() + EOL_LEN);
                                Ident::try_from(payload.as_slice()).map(|id| {
                                    Some(Response::Success(Some(ResponsePayload::Id(id))))
                                })
                            } else {
                                Ok(None)
                            }
                        }

                        Some(Command::QuerySettings) => {
                            let payload = &src[STATUS_LEN..];
                            if payload.len() >= 32 {
                                let settings = DeviceSettings::try_from(payload)?;
                                let _ = src.split_to(STATUS_LEN + 32);
                                Ok(Some(Response::Success(Some(ResponsePayload::Settings(
                                    settings,
                                )))))
                            } else {
                                Ok(None)
                            }
                        }

                        Some(Command::GetMeasurementBinary) => {
                            if src.len() >= 5 {
                                let len = src[4];
                                if src.len() >= len as usize + 3 {
                                    let total = len as usize + 3;

                                    assert_eq!(src[2..5], [b'Q', b'D', b',']);

                                    let m = RawMeasurement::try_from(&src[5..total])?; // Skip ACK and 'QD,'
                                    let _ = src.split_to(total);
                                    return Ok(Some(Response::Success(Some(
                                        ResponsePayload::MeasurementBinary(m),
                                    ))));
                                }
                            }
                            Ok(None) // Not enough bytes yet
                        }

                        Some(Command::QuerySavedMeasurements) => {
                            const DATA_SET_LEN: usize = 42;
                            let req_bytes = STATUS_LEN + 3 + 1; // ACK_CR + 'QD,' + DATA_COUNT
                            let data_count = *src.get(STATUS_LEN + 3).unwrap_or(&0) as usize;
                            if src.len() >= req_bytes + data_count * DATA_SET_LEN {
                                let header = src.split_to(req_bytes);
                                assert_eq!(header[2..5], [b'Q', b'D', b',']);
                                let mut dataset = Vec::new();
                                for _ in 0..data_count {
                                    let payload = src.split_to(DATA_SET_LEN);
                                    dataset.push(RawMeasurement::try_from(&payload[..])?);
                                    //println!("sec2: {:b}", dataset.last().unwrap().sec2_value);
                                }
                                Ok(Some(Response::Success(Some(
                                    ResponsePayload::SavedMeasurements(dataset),
                                ))))
                            } else {
                                Ok(None) // Not enough bytes yet
                            }
                        }

                        Some(Command::QueryRecordingSession) => {
                            const DATA_SET_LEN: usize = 32;
                            let req_bytes = STATUS_LEN + 3 + 2; // ACK_CR + 'QD,' + DATA_COUNT
                            if src.len() >= req_bytes {
                                let data_count = u16::from_le_bytes([src[5], src[6]]) as usize;
                                if src.len()
                                    >= req_bytes
                                        + 4
                                        + 1
                                        + 1
                                        + RAW_INFO_LEN
                                        + (data_count * DATA_SET_LEN)
                                {
                                    let header = src.split_to(STATUS_LEN + 3);
                                    assert_eq!(header[2..5], [b'Q', b'D', b',']);
                                    let payload = src.split_to(
                                        2 + 4 + 1 + 1 + RAW_INFO_LEN + data_count * DATA_SET_LEN,
                                    );
                                    let session: RawRecordingSession =
                                        RawRecordingSession::try_from(&payload[..])?;
                                    return Ok(Some(Response::Success(Some(
                                        ResponsePayload::RecordingSession(session),
                                    ))));
                                }
                            }
                            Ok(None) // Not enough bytes yet
                        }
                        None => panic!("No command called"),
                    }
                }
                '1' => {
                    // Error
                    let _ = src.split_to(STATUS_LEN);
                    Ok(Some(Response::SyntaxError))
                }
                '2' => {
                    // Device locked
                    let _ = src.split_to(STATUS_LEN);
                    Ok(Some(Response::ExecutionError))
                }
                '5' => {
                    // No data
                    let _ = src.split_to(STATUS_LEN);
                    Ok(Some(Response::NoData))
                }
                code => Err(io::Error::new(
                    io::ErrorKind::Other,
                    format!("Unknown device response code: {:?}", code),
                )),
            }
        } else {
            Ok(None)
        }
    }
}

fn write_fmt_guarded(dst: &mut BytesMut, args: fmt::Arguments<'_>) -> Result<(), io::Error> {
    dst.write_fmt(args)
        .map_err(|e| io::Error::new(io::ErrorKind::Other, e))
}

impl Encoder<Command> for ProtocolCodec {
    type Error = io::Error;

    fn encode(&mut self, item: Command, dst: &mut BytesMut) -> Result<(), Self::Error> {
        match &item {
            Command::Id => write_fmt_guarded(dst, format_args!("ID"))?,
            Command::SetSettings(settings) => {
                write_fmt_guarded(dst, format_args!("PS "))?;
                dst.extend_from_slice(settings.as_bytes()?.as_ref());
            }
            Command::GetMeasurementBinary => write_fmt_guarded(dst, format_args!("QD 0"))?,
            Command::QuerySavedMeasurements => write_fmt_guarded(dst, format_args!("QD 4"))?,
            Command::QueryRecordingSession => write_fmt_guarded(dst, format_args!("QD 2"))?,
            Command::Clear(mem) => {
                let s = match mem {
                    ClearMemory::Measurements => "0",
                    ClearMemory::Recordings => "1",
                };
                write_fmt_guarded(dst, format_args!("CD {}", s))?;
            }
            Command::QuerySettings => write_fmt_guarded(dst, format_args!("QS"))?,
            Command::ResetDevice => write_fmt_guarded(dst, format_args!("RI"))?,
            // XE 1 and XE 0 controls logging, but this logs not into device memory.
            // Instead, the current state has to be polled with QD 6, which returns
            // the readings. This is beyond the scope of this library.
            Command::StartLogging => write_fmt_guarded(dst, format_args!("XE 1"))?,
            Command::StopLogging => write_fmt_guarded(dst, format_args!("XE 0"))?,
        }
        dst.write_str("\r")
            .map_err(|e| io::Error::new(io::ErrorKind::Other, e))?;
        self.last_cmd = Some(item);
        Ok(())
    }
}
