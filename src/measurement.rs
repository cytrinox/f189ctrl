use chrono::{Duration, NaiveTime};
use num_enum::TryFromPrimitive;
use serde::{Deserialize, Serialize};
use serde_with::serde_as;
use std::convert::TryFrom;
use std::fmt::{self, Display};

use crate::{
    proto::{conv::unit_prefix, response::clock_to_naive_time},
    rawmea::{RawInfo, RawMeasurement, RawRecording, RawRecordingSession},
};

/// Measurement function
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[allow(non_camel_case_types)]
pub enum Function {
    None,
    V_DC,
    V_AC_HZ,
    MV_AC_HZ,
    V_DC_HZ,
    MV_DC_HZ,
    V_DC_DUTYCYCLE,
    MV_DC_DUTYCYCLE,
    V_AC_DUTYCYCLE,
    MV_AC_DUTYCYCLE,
    TEMPERATURE,
    A_DC,
    A_DC_HZ,
    A_DC_DUTYCYCLE,
    MA_DC_HZ,
    MA_DC_DUTYCYCLE,
    V_DC_OVER_AC,
    V_AC_OVER_DC,
    V_AC_DB,
    MV_AC_DB,
    CAPACITANCE,
    DIODE_TEST,
    OHMS,
    MA_AC,
    MA_AC_HZ,
    V_AC_PLUS_DC,
    MV_AC_PLUS_DC,
    MA_DC_OVER_AC,
    MV_DC_OVER_AC,
    A_AC,
    A_AC_HZ,
    A_AC_DUTYCYCLE,
    MA_AC_DUTYCYCLE,
    CONTINUITY,
    MV_AC,
    MV_DC,
    A_DC_OVER_AC,
    CONDUCTANCE,
    V_AC,
    UA_AC_PLUS_DC,
    UA_DC_OVER_AC,
    UA_DC,
    UA_DC_HZ,
    UA_DC_DUTYCYCLE,
    UA_AC_OVER_DC,
    A_AC_OVER_DC,
    MA_AC_OVER_DC,
    MA_AC_PLUS_DC,
    UA_AC,
    UA_AC_HZ,
    UA_AC_DUTYCYCLE,
    MV_AC_OVER_DC,
    MA_DC,
    A_AC_PLUS_DC,
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Function::None => f.write_str("None"),
            Function::V_DC => f.write_str("V DC"),
            Function::TEMPERATURE => f.write_str("Temperature"),
            Function::A_DC => f.write_str("A DC"),
            Function::V_DC_OVER_AC => f.write_str("V DC,AC"),
            Function::V_AC_OVER_DC => f.write_str("V AC,DC"),
            Function::V_AC_DB => f.write_str("V AC dB"),
            Function::MV_AC_DB => f.write_str("mV AC dB"),
            Function::CAPACITANCE => f.write_str("Capacity"),
            Function::OHMS => f.write_str("Ohms"),
            Function::MA_AC => f.write_str("mA AC"),
            Function::V_AC_PLUS_DC => f.write_str("V AC+DC"),
            Function::MV_AC_PLUS_DC => f.write_str("mV AC+DC"),
            Function::MA_DC_OVER_AC => f.write_str("mA DC,AC"),
            Function::MV_DC_OVER_AC => f.write_str("mV DC,AC"),
            Function::A_AC => f.write_str("A AC"),
            Function::CONTINUITY => f.write_str("Continuity"),
            Function::MV_AC => f.write_str("mV AC"),
            Function::MV_DC => f.write_str("mv DC"),
            Function::A_DC_OVER_AC => f.write_str("A DC,AC"),
            Function::CONDUCTANCE => f.write_str("Conductivity"),
            Function::DIODE_TEST => f.write_str("Diode mV"),
            Function::V_AC => f.write_str("V AC"),
            Function::UA_AC_PLUS_DC => f.write_str("µA AC+DC"),
            Function::UA_DC_OVER_AC => f.write_str("µA DC,AC"),
            Function::UA_DC => f.write_str("µA DC"),
            Function::UA_AC_OVER_DC => f.write_str("µA AC,DC"),
            Function::A_AC_OVER_DC => f.write_str("A AC,DC"),
            Function::MA_AC_OVER_DC => f.write_str("mA AC,DC"),
            Function::MA_AC_PLUS_DC => f.write_str("mA AC+DC"),
            Function::UA_AC => f.write_str("µA AC"),
            Function::MV_AC_OVER_DC => f.write_str("mV AC,DC"),
            Function::MA_DC => f.write_str("mA DC"),
            Function::A_AC_PLUS_DC => f.write_str("A AC+DC"),
            Function::V_AC_HZ => f.write_str("V AC Freq."),
            Function::MV_AC_HZ => f.write_str("mv AC Freq."),
            Function::V_AC_DUTYCYCLE => f.write_str("V AC Duty Cycle"),
            Function::MV_AC_DUTYCYCLE => f.write_str("mV AC Duty Cycle"),
            Function::V_DC_HZ => f.write_str("V DC Freq."),
            Function::MV_DC_HZ => f.write_str("mv DC Freq."),
            Function::V_DC_DUTYCYCLE => f.write_str("V DC Duty Cycle"),
            Function::MV_DC_DUTYCYCLE => f.write_str("mV DC Duty Cycle"),
            Function::A_AC_HZ => f.write_str("A AC Freq."),
            Function::A_AC_DUTYCYCLE => f.write_str("A AC Duty Cycle"),
            Function::MA_AC_HZ => f.write_str("mA AC Freq."),
            Function::MA_AC_DUTYCYCLE => f.write_str("mA AC Duty Cycle"),
            Function::A_DC_HZ => f.write_str("A DC Freq."),
            Function::A_DC_DUTYCYCLE => f.write_str("A DC Duty Cycle"),
            Function::MA_DC_HZ => f.write_str("mA DC Freq."),
            Function::MA_DC_DUTYCYCLE => f.write_str("mA DC Duty Cycle"),
            Function::UA_DC_HZ => f.write_str("µA DC Freq."),
            Function::UA_DC_DUTYCYCLE => f.write_str("µA DC Duty Cycle"),
            Function::UA_AC_HZ => f.write_str("µA AC Freq."),
            Function::UA_AC_DUTYCYCLE => f.write_str("µA DC Duty Cycle"),
        }
    }
}

/// Indicator for V/AC special modes
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, TryFromPrimitive)]
#[allow(non_camel_case_types)]
#[repr(u8)]
pub enum VoltAcSubMode {
    Normal = 0,
    DbOverAc = 0x01,
    AcOverDb = 0x02,
    DbOverHz = 0x03,
    HzOverDb = 0x04,
}

/// List of modes
#[derive(Debug, Clone, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct Modes(Vec<Mode>);

impl Modes {
    pub fn is(&self, mode: Mode) -> bool {
        self.0.contains(&mode)
    }

    pub(crate) fn add(&mut self, mode: Mode) {
        if !self.0.contains(&mode) {
            self.0.push(mode);
        }
    }
}

impl fmt::Display for Modes {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let str = self
            .0
            .iter()
            .map(Mode::to_string)
            .collect::<Vec<_>>()
            .join(", ");
        f.write_str(&str)
    }
}

/// Measurement mode
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[allow(non_camel_case_types)]
pub enum Mode {
    Hold,
    AutoHold,
    MinMaxAvg(MinMaxMode),
    AutoRange,
    FastMode,
    Logging,
    Rel,
    RelPercent,
}

impl fmt::Display for Mode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Mode::Hold => f.write_str("HOLD"),
            Mode::AutoHold => f.write_str("AutoHOLD"),
            Mode::MinMaxAvg(mode) => f.write_fmt(format_args!("MIN MAX ({})", mode)),
            Mode::Logging => f.write_str("LOG"),
            Mode::Rel => f.write_str("Rel."),
            Mode::RelPercent => f.write_str("Rel. %"),
            Mode::FastMode => f.write_str("Fast MN MX"),
            Mode::AutoRange => f.write_str("AutoRANGE"),
        }
    }
}

/// Measurement state
#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Serialize, Deserialize)]
#[allow(non_camel_case_types)]
pub enum State {
    #[serde(rename = "OK")]
    Normal,
    #[serde(rename = "DISPLAY_OFF")]
    DisplayOff,
    #[serde(rename = "OPEN")]
    Open,
    #[serde(rename = "LEADS")]
    Leads,
    #[serde(rename = "-OL")]
    OL_Minus,
    #[serde(rename = "FUSE")]
    Fuse,
    #[serde(rename = "---")]
    Blank,
    #[serde(rename = "OL")]
    OL,
    #[serde(rename = "?")]
    Unknown,
    #[serde(rename = "SETUP")]
    SetupMode,
}

impl From<i32> for State {
    #[allow(clippy::manual_range_contains)]
    fn from(value: i32) -> Self {
        //if value < -99_999 || value > 99_999 {
        if value < -0x0FFFFFFF || value > 0x0FFFFFFF {
            let masked = value & !((1 << 31) | (1 << 30) | (1 << 29) | (1 << 28));
            //println!("m {masked}");
            match masked {
                1 => State::DisplayOff,
                21 | 19 => State::Blank, // 19 is used in saved measurements, not sure why.
                22 => State::Open,
                29 => State::Leads,
                27 => State::Fuse,
                33 => State::OL,
                15 => State::SetupMode,
                0 if value.is_negative() => State::OL_Minus,
                0 => State::Unknown,
                _ => State::Unknown,
            }
        } else {
            State::Normal
        }
    }
}

/// Measurement attribute
#[derive(Debug, Clone, Serialize, Deserialize)]
#[allow(non_camel_case_types)]
pub enum Attribute {
    //ShortCircuit,
    //OpenCircuit,
    //GoodDiode,
    NegativeEdge,
    PositiveEdge,
}

impl fmt::Display for Attribute {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            match self {
                //Attribute::ShortCircuit => f.write_str("ShortC"),
                //Attribute::OpenCircuit => f.write_str("OpenC"),
                //Attribute::GoodDiode => f.write_str("Diode OK"),
                Attribute::NegativeEdge => f.write_str("⭝⭜"),
                Attribute::PositiveEdge => f.write_str("⭜⭝"),
            }
        } else {
            match self {
                //Attribute::ShortCircuit => f.write_str("Short Circuit"),
                //Attribute::OpenCircuit => f.write_str("Open Circuit"),
                //Attribute::GoodDiode => f.write_str("Good Diode"),
                Attribute::NegativeEdge => f.write_str("Negative Edge"),
                Attribute::PositiveEdge => f.write_str("Positive Edge"),
            }
        }
    }
}

/// Measurement unit
#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Unit {
    #[serde(rename = "F")]
    Farad,
    #[serde(rename = "NONE")]
    None,
    #[serde(rename = "%")]
    Percent,
    #[serde(rename = "s")]
    Seconds,
    #[serde(rename = "A/AC")]
    AmpereAC,
    #[serde(rename = "V/AC+DC")]
    VoltAcPlusDc,
    #[serde(rename = "°C")]
    CEL,
    #[serde(rename = "dBV")]
    dBV,
    #[serde(rename = "dBm")]
    dBm,
    #[serde(rename = "dB")]
    dB,
    #[serde(rename = "A/AC+DC")]
    AmpereAcPlusDc,
    #[serde(rename = "V/DC")]
    VoltDC,
    #[serde(rename = "V")]
    Volt,
    #[serde(rename = "A/DC")]
    AmpereDC,
    #[serde(rename = "V/AC")]
    VoltAC,
    #[serde(rename = "°F")]
    Fahrenheit,
    #[serde(rename = "Ohm")]
    Ohm,
    #[serde(rename = "S")]
    Siemens,
    #[serde(rename = "Hz")]
    Hertz,
    #[serde(rename = "A")]
    Ampere,
}

impl fmt::Display for Unit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Unit::Farad => f.write_str("F"),
            Unit::None => f.write_str(""),
            Unit::Percent => f.write_str("%"),
            Unit::Seconds => f.write_str("S"),
            Unit::AmpereAC => f.write_str("AAC"),
            Unit::VoltAcPlusDc => f.write_str("VAC+DC"),
            Unit::CEL => f.write_str("°C"),
            Unit::dBV => f.write_str("dBV"),
            Unit::dBm => f.write_str("dBm"),
            Unit::dB => f.write_str("db"),
            Unit::AmpereAcPlusDc => f.write_str("AAC+DC"),
            Unit::VoltDC => f.write_str("VDC"),
            Unit::Volt => f.write_str("V"),
            Unit::AmpereDC => f.write_str("ADC"),
            Unit::VoltAC => f.write_str("VAC"),
            Unit::Fahrenheit => f.write_str("°F"),
            Unit::Ohm => f.write_str("Ω"),
            Unit::Siemens => f.write_str("S"),
            Unit::Hertz => f.write_str("Hz"),
            Unit::Ampere => f.write_str("A"),
        }
    }
}

/// Single reading value from a measurements
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Reading {
    /// Value in main unit, e.g. 0.00034 V
    pub value: f64,
    /// Unit
    pub unit: Unit,
    /// SI modifier, -3 for milli, -6 for micro...
    pub unit_mul: i8,
    /// Reading state
    pub state: State,
}

impl Reading {
    pub(crate) fn new(value: i32, scale: u8, si: i8, unit: Unit) -> std::io::Result<Self> {
        let state = State::from(value);
        let unit_multiplier = if unit == Unit::Percent { 0 } else { si * 3 };

        Ok(Reading {
            value: {
                if state == State::Normal {
                    if scale & 0x80 > 0 {
                        // High resolution mode for frequency readings (Hz mode)
                        // scale seems to need some correction in this mode, so far
                        // adding 1 to scale is okay.
                        (value as f64) / 10_u32.pow((scale & 0xF) as u32 + 1) as f64
                            * (10_f64.powf(unit_multiplier as f64))
                    } else {
                        (value as f64) / 10_u32.pow((scale & 0xF) as u32) as f64
                            * (10_f64.powf(unit_multiplier as f64))
                    }
                } else {
                    f64::NAN
                }
            },
            unit,
            unit_mul: unit_multiplier,
            state,
        })
    }
}

/// Min/Max/Avg mode
#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive, Serialize, Deserialize)]
#[repr(u8)]
#[allow(non_camel_case_types)]
pub enum MinMaxMode {
    Avg = 3,
    Min = 2,
    Max = 1,
    Normal = 0,
}

impl Display for MinMaxMode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MinMaxMode::Avg => f.write_str("AVG"),
            MinMaxMode::Min => f.write_str("MIN"),
            MinMaxMode::Max => f.write_str("MAX"),
            MinMaxMode::Normal => f.write_str("NORMAL"),
        }
    }
}

/// Internal device mode
///
/// Only useful when parsing raw measurements.
#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive)]
#[repr(u8)]
#[allow(non_camel_case_types)]
pub enum DeviceMode {
    None = 0,
    V_AC = 1,
    MV_AC = 2,
    V_DC = 3,
    V_DC_AC_PLUS_DC = 5,
    MV_DC = 4,
    MV_DC_AC_PLUS_DC = 6,
    OHMS = 9,
    CONTINUITY = 11,
    CONDUCTANCE = 10,
    CAPACITANCE = 12,
    DIODE_TEST = 13,
    TEMPERATURE_C = 26,
    TEMPERATURE_F = 27,
    A_AC = 14,
    MA_AC = 15,
    UA_AC = 16,
    A_DC = 17,
    MA_DC = 18,
    UA_DC = 19,
    A_DC_AC_PLUS_DC = 20,
    MA_DC_AC_PLUS_DC = 21,
    UA_DC_AC_PLUS_DC = 22,
    Unspecified = 99,
}

impl Default for DeviceMode {
    fn default() -> Self {
        Self::Unspecified
    }
}

/// Measurement
///
/// Contains up to 3 readings.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Measurement {
    /// Primary reading on display
    pub pri_reading: Reading,
    /// Primary reading, (internal only)
    pub pri_reading2: Reading,
    /// Secondary reading
    pub sec_reading: Reading,
    /// Tertiary reading
    pub ter_reading: Reading,
    /// Danger voltage
    pub bolt: bool,
    /// Autorange enabled
    pub auto_range: bool,
    /// Primary reading function
    pub pri_function: Function,
    /// Secondary reading function
    pub sec_function: Function,
    /// Optional modes
    pub modes: Modes,
    /// Clock
    pub clock_pri: NaiveTime,
    /// Clock
    pub clock_sec: NaiveTime,
    /// Selected range
    pub selected_range: RangeSelection,
    /// Optional attributes
    pub attribute: Option<Attribute>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FlattenMeasurement {
    pub primary_value: f64,
    pub primary_unit: Unit,
    pub primary_si: i8,
    pub primary_state: State,
    pub secondary_value: f64,
    pub secondary_unit: Unit,
    pub secondary_si: i8,
    pub secondary_state: State,
    pub teriary_value: f64,
    pub teriary_unit: Unit,
    pub teriary_si: i8,
    pub teriary_state: State,
    pub primary_function: Function,
    pub secondary_function: Function,
    pub modes: String,
    pub clock: NaiveTime,
    pub selected_range: RangeSelection,
    pub attribute: Option<Attribute>,
}

impl From<Measurement> for FlattenMeasurement {
    fn from(value: Measurement) -> Self {
        Self {
            primary_value: value.pri_reading.value,
            primary_unit: value.pri_reading.unit,
            primary_si: value.pri_reading.unit_mul,
            primary_state: value.pri_reading.state,
            primary_function: value.pri_function,
            secondary_value: value.sec_reading.value,
            secondary_unit: value.sec_reading.unit,
            secondary_si: value.sec_reading.unit_mul,
            secondary_state: value.sec_reading.state,
            secondary_function: value.sec_function,
            teriary_value: value.ter_reading.value,
            teriary_unit: value.ter_reading.unit,
            teriary_si: value.ter_reading.unit_mul,
            teriary_state: value.ter_reading.state,
            modes: value.modes.to_string(),
            clock: value.clock_pri,
            selected_range: value.selected_range,
            attribute: value.attribute,
        }
    }
}

/// Indicator for special DC modes
#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive)]
#[repr(u8)]
#[allow(non_camel_case_types)]
pub enum DcMode {
    DC = 0,
    AC_OVER_DC = 1,
    DC_OVER_AC = 2,
    AC_PLUS_DC = 3,
}

/// Range selection
#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive, Serialize, Deserialize)]
#[repr(u8)]
#[allow(non_camel_case_types)]
pub enum RangeSelection {
    RANGE_1 = 0x01,
    RANGE_10 = 0x41,
    RANGE_100 = 0x81,
    RANGE_1000 = 0xc1,
    RANGE_5 = 0x21,
    RANGE_50 = 0x61,
    RANGE_500 = 0xA1,
    RANGE_5000 = 0xE1,
    Unknown = 98,
    /// When CLEAR MEM is selected
    ClearMem = 164,
}

impl Default for RangeSelection {
    fn default() -> Self {
        Self::Unknown
    }
}

impl Display for RangeSelection {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RangeSelection::RANGE_1 => f.write_str("0..1"),
            RangeSelection::RANGE_10 => f.write_str("0..10"),
            RangeSelection::RANGE_100 => f.write_str("0..100"),
            RangeSelection::RANGE_1000 => f.write_str("0..1000"),
            RangeSelection::RANGE_5 => f.write_str("0..5"),
            RangeSelection::RANGE_50 => f.write_str("0..50"),
            RangeSelection::RANGE_500 => f.write_str("0..500"),
            RangeSelection::RANGE_5000 => f.write_str("0..5000"),
            RangeSelection::Unknown => f.write_str("NO_RANGE"),
            RangeSelection::ClearMem => f.write_str("CLEAR_MEM"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive)]
#[repr(u8)]
#[allow(non_camel_case_types)]
pub enum RangeMode {
    RANGEMODE_AUTO = 0xA0,
    RANGEMODE_MANUAL = 0xC0,
    RANGEMODE_NONE = 0x80,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive)]
#[repr(u8)]
#[allow(non_camel_case_types)]
enum CurrentView {
    /// Just displaying a reading
    CV_DisplayingMeasure = 0x01,
    /// Displaying setup screen (A time setting: intervall, poweroff etc)
    CV_InSetup_noTimeSet = 0x02,
    /// Displaying setup screen (no time setting)
    CV_InSetup_TimeSet = 0x22,
    /// Clr? (saves) is displayed on screen
    CV_Showing_CLR_Saves = 0x03,
    /// Clr? (log) is displayed on screen
    CV_Showing_CLR_Log = 0x05,
    /// ViewMem with no data is displayed
    CV_ViewMem_NoData = 0x06,
}

impl TryFrom<RawMeasurement> for Measurement {
    type Error = std::io::Error;

    fn try_from(value: RawMeasurement) -> std::io::Result<Self> {
        //println!("UNKNOWN: {:?} {:?} {:?} {:?} {:?}", value.f_ubit1, value.f_ubit2, value.f_ubit3,value.f_ubit4,value.f_ubit5);
        let (pri_function, pri_unit, sec_function, sec_unit, ter_unit) = value.info.get_functions();

        let pri_reading = Reading::new(value.pri_value, value.pri_scale, value.pri_si, pri_unit)?;
        let sec_reading = Reading::new(value.sec_value, value.sec_scale, value.sec_si, sec_unit)?;

        let pri2_reading =
            Reading::new(value.pri2_value, value.pri2_scale, value.pri2_si, pri_unit)?;
        let ter_reading = Reading::new(value.sec2_value, 0, 0, ter_unit)?;

        let clock_pri = clock_to_naive_time(value.clock);
        let clock_sec = clock_to_naive_time(value.clock2);

        let bolt = value.info.f_bolt;
        let auto_range = value.info.f_auto_range;

        let selected_range =
            RangeSelection::try_from_primitive(value.info.selected_range).unwrap_or_default();

        let attribute = value.info.attribute()?;

        let modes = value.info.modes()?;

        Ok(Self {
            clock_pri,
            clock_sec,
            pri_reading,
            sec_reading,
            pri_reading2: pri2_reading,
            ter_reading,
            bolt,
            auto_range,
            pri_function,
            sec_function,
            modes,
            selected_range,
            attribute,
        })
    }
}

/// States for a single logging record.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct RecordingStates(Vec<RecordingState>);

impl From<u8> for RecordingStates {
    fn from(value: u8) -> Self {
        let mut s = Self::default();
        if value & 0b0000_0001 > 0 {
            s.0.push(RecordingState::Interval);
        }
        if value & 0b0000_0010 > 0 {
            s.0.push(RecordingState::U1);
        }
        if value & 0b0000_0100 > 0 {
            s.0.push(RecordingState::Stable);
        }
        if value & 0b0000_1000 > 0 {
            s.0.push(RecordingState::Unstable);
        }
        if value & 0b0001_0000 > 0 {
            s.0.push(RecordingState::U4);
        }
        if value & 0b0010_0000 > 0 {
            s.0.push(RecordingState::U5);
        }
        if value & 0b0100_0000 > 0 {
            s.0.push(RecordingState::NoData);
        }
        if value & 0b1000_0000 > 0 {
            s.0.push(RecordingState::Stopped);
        }
        s
    }
}

impl Display for RecordingStates {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = self
            .0
            .iter()
            .map(RecordingState::to_string)
            .collect::<Vec<_>>()
            .join(", ");
        f.write_str(&s)?;
        Ok(())
    }
}

/// Recording state
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum RecordingState {
    Interval,
    U1,
    Stable,
    Unstable,
    U4,
    U5,
    NoData,
    Stopped,
}

impl Display for RecordingState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RecordingState::Stable => f.write_str("STABLE"),
            RecordingState::Unstable => f.write_str("UNSTABLE"),
            RecordingState::Interval => f.write_str("INTERVAL"),
            RecordingState::U1 => f.write_str("U1"),
            RecordingState::U4 => f.write_str("U4"),
            RecordingState::U5 => f.write_str("U5"),
            RecordingState::NoData => f.write_str("NO_DATA"),
            RecordingState::Stopped => f.write_str("STOPPED"),
        }
    }
}

/// Single logging record
#[serde_with::serde_as]
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Recording {
    pub clock_begin: NaiveTime,
    pub clock_end: NaiveTime,
    #[serde_as(as = "serde_with::DurationSeconds<i64>")]
    pub duration: Duration,
    pub reading_min: Reading,
    pub reading_max: Reading,
    pub reading_avg: Reading,
    pub samples: u32,
    pub states: RecordingStates,
    pub function: Function,
    pub modes: Modes,
    pub attribute: Option<Attribute>,
}

#[serde_with::serde_as]
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FlattenRecording {
    pub avg_value: f64,
    pub avg_unit: Unit,
    pub avg_si: i8,
    pub avg_state: State,
    pub min_value: f64,
    pub min_unit: Unit,
    pub min_si: i8,
    pub min_state: State,
    pub max_value: f64,
    pub max_unit: Unit,
    pub max_si: i8,
    pub max_state: State,
    pub clock_begin: NaiveTime,
    pub clock_end: NaiveTime,
    #[serde_as(as = "serde_with::DurationSeconds<i64>")]
    pub duration: Duration,
    pub samples: u32,
    pub states: String,
    pub function: Function,
    pub modes: String,
    pub attribute: Option<Attribute>,
}

impl From<Recording> for FlattenRecording {
    fn from(value: Recording) -> Self {
        Self {
            avg_value: value.reading_avg.value,
            avg_unit: value.reading_avg.unit,
            avg_si: value.reading_avg.unit_mul,
            avg_state: value.reading_avg.state,
            min_value: value.reading_min.value,
            min_unit: value.reading_min.unit,
            min_si: value.reading_min.unit_mul,
            min_state: value.reading_min.state,
            max_value: value.reading_max.value,
            max_unit: value.reading_max.unit,
            max_si: value.reading_max.unit_mul,
            max_state: value.reading_max.state,
            clock_begin: value.clock_begin,
            clock_end: value.clock_end,
            duration: value.duration,
            samples: value.samples,
            states: value.states.to_string(),
            modes: value.modes.to_string(),
            function: value.function,
            attribute: value.attribute,
        }
    }
}

impl TryFrom<(RawRecording, &RawInfo)> for Recording {
    type Error = std::io::Error;

    fn try_from((value, info): (RawRecording, &RawInfo)) -> Result<Self, Self::Error> {
        let (pri_function, pri_unit, _sec_function, _sec_unit, _ter_unit) = info.get_functions();
        //eprintln!("{:?}", value);
        let clock_begin = clock_to_naive_time(value.clock_begin);
        let clock_end = clock_to_naive_time(value.clock_end);
        let duration = clock_end - clock_begin;
        let reading_min = Reading::new(value.min_value, value.pri_scale, value.pri_si, pri_unit)?;
        let reading_max = Reading::new(value.max_value, value.pri_scale, value.pri_si, pri_unit)?;
        let samples = value.sum_count;
        let mut reading_avg =
            Reading::new(value.sum_value, value.pri_scale, value.pri_si, pri_unit)?;
        let state = RecordingStates::from(value.status);
        reading_avg.value /= samples as f64;
        Ok(Self {
            clock_begin,
            clock_end,
            duration,
            reading_min,
            reading_max,
            reading_avg,
            samples,
            states: state,
            function: pri_function,
            modes: info.modes()?,
            attribute: info.attribute()?,
        })
    }
}

impl Display for Recording {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!(
            "{} duration: {:>5} ms avg: {:#}   min: {:#}   max: {:#}   state: [{}]",
            self.clock_begin.format("%H:%M:%S"),
            self.duration.num_milliseconds(),
            self.reading_avg,
            self.reading_min,
            self.reading_max,
            self.states
        ))
    }
}

/// Recording session
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RecordingSession {
    pub pri_function: Function,
    pub init_reading: Reading,
    pub recordings: Vec<Recording>,
    pub modes: Modes,
    pub selected_range: RangeSelection,
}

impl TryFrom<RawRecordingSession> for RecordingSession {
    type Error = std::io::Error;

    fn try_from(value: RawRecordingSession) -> Result<Self, Self::Error> {
        let (pri_function, pri_unit, _sec_function, _sec_unit, _ter_unit) =
            value.info.get_functions();
        let init_reading = Reading::new(
            value.init_value,
            value.init_scale,
            value.init_prefix,
            pri_unit,
        )?;
        let recordings: Result<Vec<Recording>, Self::Error> = value
            .recordings
            .into_iter()
            .map(|rec| Recording::try_from((rec, &value.info)))
            .collect();
        let selected_range = value.info.range()?;
        let modes = value.info.modes()?;
        Ok(Self {
            pri_function,
            init_reading,
            recordings: recordings?,
            selected_range,
            modes,
        })
    }
}

impl fmt::Display for Reading {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let prec = f.precision().unwrap_or(4);
        let width = 10;
        match self.state {
            State::Normal => {
                let prefix = unit_prefix(self.unit_mul);
                if f.alternate() {
                    // Print with unit and preferred SI
                    let value = self.value / (10_f64.powi(self.unit_mul as i32));
                    f.write_fmt(format_args!(
                        "{:>width$.prec$} {}{}",
                        value, prefix, self.unit
                    ))
                } else {
                    // Just raw value
                    f.write_fmt(format_args!("{:>width$.prec$}", self.value))
                }
            }
            State::OL_Minus => f.write_fmt(format_args!("{:>width$}", "-OL")),
            State::DisplayOff => f.write_fmt(format_args!("{:>width$}", "---")),
            State::Blank => f.write_fmt(format_args!("{:>width$}", "---")),
            State::Open => f.write_fmt(format_args!("{:>width$}", "OPEN")),
            State::Leads => f.write_fmt(format_args!("{:>width$}", "LEADS")),
            State::Fuse => f.write_fmt(format_args!("{:>width$}", "FUSE")),
            State::OL => f.write_fmt(format_args!("{:>width$}", "OL")),
            State::SetupMode => f.write_fmt(format_args!("{:>width$}", "SETUP")),
            State::Unknown => f.write_fmt(format_args!("{:>width$}", "UNKNOWN_ERR")),
        }
    }
}

impl fmt::Display for Measurement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.pri_reading.state {
            State::Normal => {
                if f.alternate() {
                    f.write_fmt(format_args!("{} ", self.clock_pri.format("%H:%M:%S")))?;
                    let bolt = if self.bolt { " 🗲 " } else { "   " };
                    f.write_str(bolt)?;
                }
                if f.alternate() {
                    if let Some(attr) = &self.attribute {
                        f.write_fmt(format_args!(
                            "{:#10}  {:#}  {:#}  {:#}",
                            self.pri_reading, attr, self.sec_reading, self.ter_reading
                        ))?;
                        //f.write_fmt(format_args!("{:#10} {:#} {:#}", self.pri_reading, attr, self.sec_reading))?;
                    } else {
                        f.write_fmt(format_args!(
                            "{:#10}  {:#10}  {:#10.0}",
                            self.pri_reading, self.sec_reading, self.ter_reading
                        ))?;
                        //f.write_fmt(format_args!("{:#10} {:#}", self.pri_reading, self.sec_reading))?;
                    }
                } else {
                    self.pri_reading.fmt(f)?;
                }
                Ok(())
            }
            State::OL_Minus => f.write_str("-OL"),
            State::DisplayOff => f.write_str("DISPLAY OFF"),
            State::Blank => f.write_str("---"),
            State::Open => f.write_str("OPEN"),
            State::Leads => f.write_str("LEADS"),
            State::Fuse => f.write_str("FUSE"),
            State::OL => f.write_str("OL"),
            State::SetupMode => f.write_str("SETUP"),
            State::Unknown => f.write_str("UNKNOWN_ERR"),
            //State::OpenTC => f.write_str("OPEN-TC"),
        }
    }
}
