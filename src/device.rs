use chrono::{DateTime, Local, NaiveTime};
use futures::{SinkExt, StreamExt};
use std::{pin::Pin, time::Duration};
use tokio_serial::SerialPortBuilderExt;
use tokio_util::codec::Decoder;

use super::proto::{
    codec::ProtocolCodec,
    command::Command,
    response::{Ident, Response, ResponsePayload},
    ProtoError,
};
use super::rawmea::RawMeasurement;
use crate::proto::command::{AcHertz, ClearMemory, DbMode, DigitCount, TempUnit};
use crate::proto::response::DeviceSettings;
use crate::proto::Result;
use crate::rawmea::RawRecordingSession;

trait AsyncReadWrite<S>: futures::Sink<S> + futures::Stream {}

impl<T, S> AsyncReadWrite<S> for T where T: futures::Sink<S> + futures::Stream {}

#[allow(clippy::type_complexity)]
pub struct Device {
    stream: Pin<
        Box<
            dyn AsyncReadWrite<
                Command,
                Error = std::io::Error,
                Item = std::result::Result<Response, std::io::Error>,
            >,
        >,
    >,
}

impl Device {
    pub fn new(com: impl AsRef<str>, baudrate: u32) -> Result<Self> {
        let mut port = tokio_serial::new(com.as_ref(), baudrate).open_native_async()?;

        #[cfg(unix)]
        port.set_exclusive(false)
            .expect("Unable to set serial port exclusive to false");

        let stream = ProtocolCodec::default().framed(port);

        Ok(Self {
            stream: Box::pin(stream),
        })
    }

    #[cfg(test)]
    pub fn new_faked(response_buf: Vec<char>) -> Self {
        let converted = response_buf.iter().map(|x| *x as u8).collect();
        let stream =
            ProtocolCodec::default().framed(super::proto::fake::FakeBuffer::new(converted));

        Self {
            stream: Box::pin(stream),
        }
    }

    /// Query device identification
    pub async fn ident(&mut self) -> Result<Ident> {
        self.stream.send(Command::Id).await?;
        match self.stream.next().await {
            Some(Ok(Response::Success(Some(ResponsePayload::Id(id))))) => Ok(id),
            Some(Ok(response)) => Err(response.into()),
            Some(Err(ioerr)) => Err(ioerr.into()),
            None => Err(ProtoError::Abort),
        }
    }

    /// Query backlight timeout
    pub async fn backlight_off(&mut self) -> Result<Duration> {
        let settings = self.query_settings().await?;
        Ok(Duration::from_secs(settings.backlight_off() as u64))
    }

    /// Set backlight timeout
    pub async fn set_backlight_off(&mut self, value: Duration) -> Result<()> {
        let mut settings = self.query_settings().await?;
        settings.set_backlight_off(value.as_secs() as u16);
        self.set_settings(settings).await
    }

    /// Set device settings
    pub async fn set_settings(&mut self, settings: DeviceSettings) -> Result<()> {
        self.stream.send(Command::SetSettings(settings)).await?;
        match self.stream.next().await {
            Some(Ok(Response::Success(None))) => Ok(()),
            Some(Ok(response)) => Err(response.into()),
            Some(Err(ioerr)) => Err(ioerr.into()),
            None => Err(ProtoError::Abort),
        }
    }

    /// Query beeper state
    pub async fn beeper(&mut self) -> Result<bool> {
        let settings = self.query_settings().await?;
        Ok(settings.beeper())
    }

    /// Set beeper on/off
    pub async fn set_beeper(&mut self, state: bool) -> Result<()> {
        let mut settings = self.query_settings().await?;
        settings.set_beeper(state);
        self.set_settings(settings).await
    }

    /// Query device clock
    pub async fn clock(&mut self) -> Result<NaiveTime> {
        let settings = self.query_settings().await?;
        Ok(settings.clock())
    }

    /// Set device clock
    pub async fn set_clock(&mut self, clock: DateTime<Local>) -> Result<()> {
        let naive = clock.naive_local().time();
        let mut settings = self.query_settings().await?;
        settings.set_clock(naive);
        self.set_settings(settings).await
    }

    /// Clear device memory
    pub async fn clear(&mut self, mem: ClearMemory) -> Result<()> {
        self.stream.send(Command::Clear(mem)).await?;
        match self.stream.next().await {
            Some(Ok(Response::Success(None))) => Ok(()),
            Some(Ok(response)) => Err(response.into()),
            Some(Err(ioerr)) => Err(ioerr.into()),
            None => Err(ProtoError::Abort),
        }
    }

    /// Reset device
    ///
    /// Will reset settings, clock and memory savings,
    /// except calibration and 50/60 Hz configuration.
    pub async fn reset(&mut self) -> Result<()> {
        self.stream.send(Command::ResetDevice).await?;
        match self.stream.next().await {
            Some(Ok(Response::Success(None))) => Ok(()),
            Some(Ok(response)) => Err(response.into()),
            Some(Err(ioerr)) => Err(ioerr.into()),
            None => Err(ProtoError::Abort),
        }
    }

    /// Query temperature offset, result unit depends on device setting.
    pub async fn temp_offset(&mut self) -> Result<f32> {
        let settings = self.query_settings().await?;
        Ok(settings.temp_offset() as f32 / 10.0)
    }

    /// Set temperature offset, unit depends on device setting.
    pub async fn set_temp_offset(&mut self, offset: f32) -> Result<()> {
        let mut settings = self.query_settings().await?;
        settings.set_temp_offset((offset * 10.0) as i16);
        self.set_settings(settings).await
    }

    /// Query temperature unit (C°/F°)
    pub async fn temp_unit(&mut self) -> Result<TempUnit> {
        let settings = self.query_settings().await?;
        Ok(settings.temp_unit())
    }

    /// Set temperature unit (C°/F°)
    pub async fn set_temp_unit(&mut self, unit: TempUnit) -> Result<()> {
        let mut settings = self.query_settings().await?;
        settings.set_temp_unit(unit);
        self.set_settings(settings).await
    }

    /// Query digit count (4/5 digits)
    pub async fn digit_count(&mut self) -> Result<DigitCount> {
        let settings = self.query_settings().await?;
        Ok(settings.digit_count())
    }

    /// Set digit count (4/5 digits)
    pub async fn set_digit_count(&mut self, dc: DigitCount) -> Result<()> {
        let mut settings = self.query_settings().await?;
        settings.set_digit_count(dc);
        self.set_settings(settings).await
    }

    /// Query dBm reference (Ohm)
    pub async fn d_bm_ref(&mut self) -> Result<u16> {
        let settings = self.query_settings().await?;
        Ok(settings.d_bm_ref())
    }

    /// Set dBm reference (Ohm)
    pub async fn set_dbm_ref(&mut self, value: u16) -> Result<()> {
        let mut settings = self.query_settings().await?;
        if !(1..=1999).contains(&value) {
            panic!("Invalid db value");
        }
        settings.set_d_bm_ref(value);
        self.set_settings(settings).await
    }

    /// Query AC frequency (50/60 Hz)
    pub async fn ac_freq(&mut self) -> Result<AcHertz> {
        let settings = self.query_settings().await?;
        Ok(settings.ac_hertz())
    }

    /// Set AC frequency (50/60 Hz)
    pub async fn set_ac_freq(&mut self, hertz: AcHertz) -> Result<()> {
        let mut settings = self.query_settings().await?;
        settings.set_ac_hertz(hertz);
        self.set_settings(settings).await
    }

    /// Query dB mode (dBm/dBV)
    pub async fn db_mode(&mut self) -> Result<DbMode> {
        let settings = self.query_settings().await?;
        Ok(settings.db_mode())
    }

    /// Set dB mode (dBm/dBV)
    pub async fn set_db_mode(&mut self, value: DbMode) -> Result<()> {
        let mut settings = self.query_settings().await?;
        settings.set_db_mode(value);
        self.set_settings(settings).await
    }

    /// Query auto power-off timeout (hh:mm)
    pub async fn auto_poweroff(&mut self) -> Result<Duration> {
        let settings = self.query_settings().await?;
        Ok(Duration::from_secs(settings.autopower_off() as u64 * 60))
    }

    /// Set auto power-off timeout (hh:mm)
    pub async fn set_auto_poweroff(&mut self, value: Duration) -> Result<()> {
        let mut settings = self.query_settings().await?;
        settings.set_autopower_off(value.as_secs() as u16 / 60);
        self.set_settings(settings).await
    }

    /// Query recording interval (mm:ss)
    pub async fn recording_interval(&mut self) -> Result<Duration> {
        let settings = self.query_settings().await?;
        Ok(Duration::from_secs(settings.recording_interval() as u64))
    }

    /// Set recording interval (mm:ss)
    pub async fn set_recording_interval(&mut self, interval: Duration) -> Result<()> {
        let mut settings = self.query_settings().await?;
        settings.set_recording_interval(interval.as_secs() as u16);
        self.set_settings(settings).await
    }

    /// Query recording threshold in percent (0-100%)
    pub async fn recording_event_threshold(&mut self) -> Result<u8> {
        let settings = self.query_settings().await?;
        Ok(settings.recording_thd())
    }

    /// Set recording threshold in percent (0-100%)
    pub async fn set_recording_event_threshold(&mut self, thd: u8) -> Result<()> {
        let mut settings = self.query_settings().await?;
        if thd <= 100 {
            settings.set_recording_thd(thd);
        } else {
            panic!("Recording thd. must be <= 100");
        }
        self.set_settings(settings).await
    }

    /// Get current (live) measurement
    pub async fn live_measurement(&mut self) -> Result<Option<RawMeasurement>> {
        self.stream.send(Command::GetMeasurementBinary).await?;
        match self.stream.next().await {
            Some(Ok(Response::Success(Some(ResponsePayload::MeasurementBinary(m))))) => Ok(Some(m)),
            Some(Ok(Response::NoData)) => Ok(None),
            Some(Ok(response)) => Err(response.into()),
            Some(Err(ioerr)) => Err(ioerr.into()),
            None => Err(ProtoError::Abort),
        }
    }

    //// Query device settings
    pub async fn query_settings(&mut self) -> Result<DeviceSettings> {
        self.stream.send(Command::QuerySettings).await?;
        match self.stream.next().await {
            Some(Ok(Response::Success(Some(ResponsePayload::Settings(settings))))) => Ok(settings),
            Some(Ok(response)) => Err(response.into()),
            Some(Err(ioerr)) => Err(ioerr.into()),
            None => Err(ProtoError::Abort),
        }
    }

    /// Get saved measurements from device memory
    pub async fn saved_measurements(&mut self) -> Result<Vec<RawMeasurement>> {
        self.stream.send(Command::QuerySavedMeasurements).await?;
        match self.stream.next().await {
            Some(Ok(Response::Success(Some(ResponsePayload::SavedMeasurements(m))))) => Ok(m),
            Some(Ok(response)) => Err(response.into()),
            Some(Err(ioerr)) => Err(ioerr.into()),
            None => Err(ProtoError::Abort),
        }
    }

    /// Get saved recordings from device memory
    pub async fn saved_recordings(&mut self) -> Result<RawRecordingSession> {
        self.stream.send(Command::QueryRecordingSession).await?;
        match self.stream.next().await {
            Some(Ok(Response::Success(Some(ResponsePayload::RecordingSession(m))))) => Ok(m),
            Some(Ok(response)) => Err(response.into()),
            Some(Err(ioerr)) => Err(ioerr.into()),
            None => Err(ProtoError::Abort),
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[tokio::test]
    async fn test_get_id() {
        let mut device = Device::new_faked(vec![
            '0', '\r', 'F', 'l', 'u', 'k', 'e', ',', 'x', ',', 'x', '\r',
        ]);
        assert!(device.ident().await.is_ok());
    }

    #[tokio::test]
    async fn test_set_backlight() {
        let mut device = Device::new_faked(vec!['0', '\r']);
        assert!(device
            .set_backlight_off(Duration::from_secs(60 * 15))
            .await
            .is_ok());
    }

    #[tokio::test]
    async fn test_set_backlight_in_settings_mode() {
        let mut device = Device::new_faked(vec!['2', '\r']);
        assert!(device
            .set_backlight_off(Duration::from_secs(60 * 15))
            .await
            .is_err());
    }
}
