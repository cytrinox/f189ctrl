use byteorder::{LittleEndian, ReadBytesExt, WriteBytesExt};
use bytes::Bytes;
use chrono::{NaiveTime, Timelike};
use std::{
    io::{self, Cursor},
    str,
    time::Duration,
};

use super::command::{AcHertz, DbMode, DigitCount, TempUnit};
use crate::rawmea::{RawMeasurement, RawRecordingSession};

/// Device response is build by an ASCII status code
/// and a CARRIGDE RETURN (0x13).
/// For commands returning a data line, the line
/// follows the status response line.
#[derive(Debug, Clone)]
pub enum Response {
    Success(Option<ResponsePayload>), // 0
    SyntaxError,                      // 1
    ExecutionError,                   // 2
    NoData,                           // 5
}

impl PartialEq for Response {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Success(_), Self::Success(_)) => true,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ResponsePayload {
    Id(Ident),
    Settings(DeviceSettings),
    MeasurementBinary(RawMeasurement),
    SavedMeasurements(Vec<RawMeasurement>),
    RecordingSession(RawRecordingSession),
}

#[derive(Debug, Clone)]
pub struct Ident {
    pub model: String,
    pub firmware: String,
    pub serial: String,
}

impl TryFrom<&[u8]> for Ident {
    type Error = io::Error;

    fn try_from(value: &[u8]) -> Result<Self, Self::Error> {
        let value = str::from_utf8(value)
            .map_err(|e| io::Error::new(io::ErrorKind::Other, e))?
            .to_string();
        let values: Vec<&str> = value.split(',').collect();
        if values.len() == 3 {
            Ok(Self {
                model: String::from(values[0]),
                firmware: String::from(values[1]),
                serial: String::from(values[2]),
            })
        } else {
            Err(io::Error::new(
                io::ErrorKind::Other,
                format!("Invalid data for ID response: {}", value),
            ))
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct DeviceSettings {
    // Seconds * 10, range [00:00 - 99:59]
    pub interval: u16,
    // dBm=0 or dBV=1
    pub db_mode: u16,
    // dBm reference in Ohm, [range 1 - 1999]
    pub db_ref: u16,
    /// Offset * 10, but seems to be different for C/F unit?! [range -999.9 - 999.9]
    /// Conversion is wrong in device, the value is always in Fahrenheit, but
    /// the formula used is F=C*9/5, not F=C*9/5+32.
    pub temp_offset: i16,
    // Temp. unit, F=0, C=1
    pub temp_unit: u16,
    // Backlight off, Seconds * 10 [range 00:00 - 99:59]
    pub bl_off: u16,
    // Internal clock. Starts at 0 für 00:00:00. Inrements each day by (24*60*60*10).
    pub clock: u32,
    // Power off, minutes [range 00:00 - 23:59]
    pub po_off: u16,
    // AC Hertz: 50=0, 60=1
    pub hz: u16,
    // Digits (4/5), 4=1, 5=0
    pub digits: u16,
    // Beeper, on=1, off=0
    pub beep: u16,
    // Recoring thd.% * 100, range 0-100
    pub rec_tolerance: u16,
    // Unknown value
    pub unknown1: u8,
    // Always 00 00
    pub unknown2: u16,
}

impl DeviceSettings {
    pub fn beeper(&self) -> bool {
        self.beep == 1
    }
    pub fn set_beeper(&mut self, beep: bool) {
        self.beep = if beep { 1 } else { 0 };
    }

    pub fn digit_count(&self) -> DigitCount {
        match self.digits {
            1 => DigitCount::Digit4,
            0 => DigitCount::Digit5,
            _ => panic!("Unknown digit count: {}", self.digits),
        }
    }

    pub fn set_digit_count(&mut self, digits: DigitCount) {
        match digits {
            DigitCount::Digit4 => self.digits = 1,
            DigitCount::Digit5 => self.digits = 0,
        }
    }

    pub fn ac_hertz(&self) -> AcHertz {
        match self.hz {
            0 => AcHertz::Hertz50,
            1 => AcHertz::Hertz60,
            _ => panic!("Unknown ac hertz: {}", self.hz),
        }
    }

    pub fn set_ac_hertz(&mut self, hertz: AcHertz) {
        match hertz {
            AcHertz::Hertz50 => self.hz = 0,
            AcHertz::Hertz60 => self.hz = 1,
        }
    }

    pub fn backlight_off(&self) -> u16 {
        self.bl_off / 10
    }

    pub fn set_backlight_off(&mut self, seconds: u16) {
        self.bl_off = seconds * 10;
    }

    pub fn autopower_off(&self) -> u16 {
        self.po_off
    }

    pub fn set_autopower_off(&mut self, minutes: u16) {
        self.po_off = minutes;
    }

    pub fn temp_unit(&self) -> TempUnit {
        match self.temp_unit {
            0 => TempUnit::F,
            1 => TempUnit::C,
            _ => panic!("Invalid temp unit"),
        }
    }

    pub fn set_temp_unit(&mut self, unit: TempUnit) {
        self.temp_unit = match unit {
            TempUnit::F => 0,
            TempUnit::C => 1,
        };
    }

    pub fn db_mode(&self) -> DbMode {
        match self.db_mode {
            0 => DbMode::Millivolt,
            1 => DbMode::Volt,
            _ => panic!("Invalid db mode"),
        }
    }

    pub fn set_db_mode(&mut self, mode: DbMode) {
        self.db_mode = match mode {
            DbMode::Millivolt => 0,
            DbMode::Volt => 1,
        };
    }

    pub fn recording_thd(&self) -> u8 {
        (self.rec_tolerance / 100) as u8
    }

    pub fn set_recording_thd(&mut self, thd: u8) {
        self.rec_tolerance = thd as u16 * 100;
    }

    pub fn recording_interval(&self) -> u16 {
        self.interval / 10
    }

    pub fn set_recording_interval(&mut self, seconds: u16) {
        self.interval = seconds * 10;
    }

    pub fn d_bm_ref(&self) -> u16 {
        self.db_ref
    }

    pub fn set_d_bm_ref(&mut self, ohm: u16) {
        self.db_ref = ohm & 0xFFF
    }

    pub fn temp_offset(&self) -> i16 {
        match self.temp_unit {
            0 => self.temp_offset,
            1 => (self.temp_offset as f32 / (9.0 / 5.0)) as i16,
            _ => panic!("Unknown temp_unit"),
        }
    }

    pub fn set_temp_offset(&mut self, offset: i16) {
        if (-1999..=1999).contains(&offset) {
            match self.temp_unit {
                // Fahrenheit
                0 => self.temp_offset = offset,
                // Celsius
                1 => self.temp_offset = ((offset as f32) * (9.0 / 5.0)) as i16,
                _ => panic!("Unknown temp_unit"),
            }
        } else {
            panic!("Temp. offset max allowed = +/-1999, value is: {}", offset);
        }
    }

    pub fn clock(&self) -> NaiveTime {
        clock_to_naive_time(self.clock)
    }

    pub fn set_clock(&mut self, time: NaiveTime) {
        self.clock = time.num_seconds_from_midnight() * 10;
    }

    pub fn as_bytes(&self) -> Result<Bytes, std::io::Error> {
        let mut cur = Cursor::new(Vec::new());
        cur.write_u16::<LittleEndian>(self.interval)?;
        cur.write_u16::<LittleEndian>(self.db_mode)?;
        cur.write_u16::<LittleEndian>(self.db_ref)?;
        cur.write_i16::<LittleEndian>(self.temp_offset)?;
        cur.write_u16::<LittleEndian>(self.temp_unit)?;
        cur.write_u16::<LittleEndian>(self.bl_off)?;
        cur.write_u32::<LittleEndian>(self.clock)?;
        cur.write_u16::<LittleEndian>(self.po_off)?;
        cur.write_u16::<LittleEndian>(self.hz)?;
        cur.write_u16::<LittleEndian>(self.digits)?;
        cur.write_u16::<LittleEndian>(self.beep)?;
        cur.write_u16::<LittleEndian>(self.rec_tolerance)?;
        cur.write_u8(self.unknown1)?;
        cur.write_u16::<LittleEndian>(self.unknown2)?;
        Ok(cur.into_inner().into())
    }
}

impl TryFrom<&[u8]> for DeviceSettings {
    type Error = std::io::Error;

    fn try_from(value: &[u8]) -> std::result::Result<Self, Self::Error> {
        if value[0..3].ne(&[b'Q', b'S', b',']) {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "Failed to read QS header",
            ));
        }
        let value = &value[3..];
        let mut cur = Cursor::new(value);
        let interval = cur.read_u16::<LittleEndian>()?;
        let db_mode = cur.read_u16::<LittleEndian>()?;
        let db_ref = cur.read_u16::<LittleEndian>()?;
        let temp_offset = cur.read_i16::<LittleEndian>()?;
        let temp_unit = cur.read_u16::<LittleEndian>()?;
        let bl_off = cur.read_u16::<LittleEndian>()?;
        let clock = cur.read_u32::<LittleEndian>()?;
        let po_off = cur.read_u16::<LittleEndian>()?;
        let hz = cur.read_u16::<LittleEndian>()?;
        let digits = cur.read_u16::<LittleEndian>()?;
        let beep = cur.read_u16::<LittleEndian>()?;
        let rec_tolerance = cur.read_u16::<LittleEndian>()?;
        let unknown1 = cur.read_u8()?;
        let unknown2 = cur.read_u16::<LittleEndian>()?;
        Ok(Self {
            interval,
            db_mode,
            db_ref,
            temp_offset,
            temp_unit,
            bl_off,
            clock,
            po_off,
            hz,
            digits,
            beep,
            rec_tolerance,
            unknown1,
            unknown2,
        })
    }
}

pub fn clock_to_naive_time(clock: u32) -> NaiveTime {
    const DAY_DURATION: f32 = (24 * 60 * 60) as f32;
    let internal_clock = clock as f32 / 10.0;
    let since_midnight = Duration::from_secs_f32(internal_clock % DAY_DURATION);
    NaiveTime::default() + since_midnight
}
