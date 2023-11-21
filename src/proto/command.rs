use std::fmt::Display;

use super::response::DeviceSettings;

#[derive(Debug, Copy, Clone)]
pub enum ClearMemory {
    Measurements,
    Recordings,
}

impl clap::ValueEnum for ClearMemory {
    fn value_variants<'a>() -> &'a [Self] {
        &[Self::Measurements, Self::Recordings]
    }

    fn to_possible_value(&self) -> Option<clap::builder::PossibleValue> {
        Some(match self {
            Self::Measurements => clap::builder::PossibleValue::new("measurements"),
            Self::Recordings => clap::builder::PossibleValue::new("recordings"),
        })
    }
}

#[derive(Debug, Copy, Clone)]
pub enum DigitCount {
    Digit4,
    Digit5,
}

impl clap::ValueEnum for DigitCount {
    fn value_variants<'a>() -> &'a [Self] {
        &[Self::Digit4, Self::Digit5]
    }

    fn to_possible_value(&self) -> Option<clap::builder::PossibleValue> {
        Some(match self {
            Self::Digit4 => clap::builder::PossibleValue::new("4"),
            Self::Digit5 => clap::builder::PossibleValue::new("5"),
        })
    }
}

#[derive(Debug, Copy, Clone)]
pub enum DbMode {
    Volt,
    Millivolt,
}

impl Display for DbMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DbMode::Volt => f.write_str("dBV"),
            DbMode::Millivolt => f.write_str("dBm"),
        }
    }
}

impl clap::ValueEnum for DbMode {
    fn value_variants<'a>() -> &'a [Self] {
        &[Self::Volt, Self::Millivolt]
    }

    fn to_possible_value(&self) -> Option<clap::builder::PossibleValue> {
        Some(match self {
            Self::Volt => clap::builder::PossibleValue::new("dBV"),
            Self::Millivolt => clap::builder::PossibleValue::new("dBm"),
        })
    }
}

#[derive(Debug, Copy, Clone)]
pub enum TempUnit {
    F,
    C,
}

impl Display for TempUnit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TempUnit::C => f.write_str("C"),
            TempUnit::F => f.write_str("F"),
        }
    }
}

impl clap::ValueEnum for TempUnit {
    fn value_variants<'a>() -> &'a [Self] {
        &[Self::C, Self::F]
    }

    fn to_possible_value(&self) -> Option<clap::builder::PossibleValue> {
        Some(match self {
            Self::C => clap::builder::PossibleValue::new("C"),
            Self::F => clap::builder::PossibleValue::new("F"),
        })
    }
}

#[derive(Debug, Copy, Clone)]
pub enum AcHertz {
    Hertz50,
    Hertz60,
}

impl Display for AcHertz {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AcHertz::Hertz50 => f.write_str("50"),
            AcHertz::Hertz60 => f.write_str("60"),
        }
    }
}

impl clap::ValueEnum for AcHertz {
    fn value_variants<'a>() -> &'a [Self] {
        &[Self::Hertz50, Self::Hertz60]
    }

    fn to_possible_value(&self) -> Option<clap::builder::PossibleValue> {
        Some(match self {
            Self::Hertz50 => clap::builder::PossibleValue::new("50"),
            Self::Hertz60 => clap::builder::PossibleValue::new("60"),
        })
    }
}

#[derive(Debug, Clone)]
pub enum Command {
    Id,
    SetSettings(DeviceSettings),
    QuerySettings,
    GetMeasurementBinary,
    QuerySavedMeasurements,
    QueryRecordingSession,
    StartLogging,
    StopLogging,
    Clear(ClearMemory),
    ResetDevice,
}
