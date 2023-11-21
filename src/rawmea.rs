use byteorder::{LittleEndian, ReadBytesExt};
use num_enum::TryFromPrimitive;
use std::io;
use std::io::Cursor;
use std::io::Read;

use crate::measurement::Attribute;
use crate::measurement::DcMode;
use crate::measurement::DeviceMode;
use crate::measurement::Function;
use crate::measurement::Unit;
use crate::measurement::VoltAcSubMode;
use crate::measurement::{MinMaxMode, Mode, Modes, RangeSelection};

/// Raw Measurement information block
#[derive(Debug, Clone, Default)]
pub struct RawInfo {
    /// 1 on AutoHOLD
    pub f_autohold2: bool,
    /// 1 on HOLD or AutoHOLD
    pub f_hold2: bool,
    /// 1 on logging mode
    pub f_log_mode: bool,
    /// 1 on AVG
    pub f_avg: bool,
    /// 1 on MAX
    pub f_max: bool,
    /// 1 on MIN
    pub f_min: bool,
    /// 1 on Fast-Min/Max
    pub f_fast_mode: bool,
    /// 1 on low battery
    pub f_low_bat: bool,
    /// 1 on Dangerous voltage (bolt), also on Setup or ViewMem screen
    pub f_bolt: bool,
    /// 1 on Delta (rel.), 0 on Delta%
    pub f_delta: bool,
    /// 1 on Delta%, 0 on Delta
    pub f_delta_pct: bool,
    /// 3 if saves or logs in ViewMem screen, otherwise 0, 1 if Clr? is displayed
    pub f_mem_clear: u8,
    /// 1 on Auto Range
    pub f_auto_range: bool,
    /// 1 on Manual Range
    pub f_man_range: bool,
    /// 1 on shift sign on screen (yellow button pressed)
    pub f_shift_sign: bool,
    /// Measurement mode (dial switch + blue button, A/mA pins)
    pub f_mea_mode: u8,
    /// 1 on Hz, 2 on DutyCycle%, 3 on ms
    pub f_hz_mode: u8,
    /// 1 on Min/Max mode
    pub f_mn_mx_mode: bool,
    /// 1 on Fast mode
    pub f_fast_mode2: bool,
    /// Unknown
    pub f_ubit1: bool,
    /// 1 on Max, 2 on Min, 3 on AVG
    pub f_minmaxavg: u8,
    /// Unknown
    pub f_ubit2: bool,
    /// 1 on rising etch
    pub f_rising_etch: bool,
    /// 1 on falling etch
    pub f_fall_etch: bool,
    /// Substates V mV mA uA (DC Mode only?)
    pub f_sub_acdc: u8,
    /// Unknown
    pub f_ubit3: bool,
    /// 1 on HOLD (0 on AutoHOLD)
    pub f_hold: bool,
    /// 1 on AutoHOLD (0 on HOLD)
    pub f_auto_hold: bool,
    /// Unknown
    pub f_ubit4: bool,
    /// 1 on dBV, 0 on dBm
    pub f_db_v_or_m: bool,
    /// 1 on C, 0 on F
    pub f_temp_c_or_f: bool,
    /// 1 on Delta (rel. mode)
    pub f_delta2: bool,
    /// 1 on Delta%
    pub f_delta_pct2: bool,
    /// Unknown
    pub f_ubit5: bool,
    /// Sub-mode for dBm/V (AC) views
    pub f_ac_db_mode: u8,
    /// 1 on 4 digits, 0 on 5 digits
    pub f_digits: bool,
    /// 1 on Range displayed (TODO: check on C/F)
    pub f_range_disp: bool,
    /// Selected range
    pub selected_range: u8,
    /// Current view
    pub current_view: u8,
    /// dBm reference
    pub dbm_ref: u16,
}

pub(crate) const RAW_INFO_LEN: usize = 10;

/// Raw information block
impl RawInfo {
    #[rustfmt::skip]
    pub(crate) fn new(cur: &mut impl Read) -> io::Result<Self> {
        let flags1 = cur.read_u16::<LittleEndian>()?;
        let dbm_ref = cur.read_u16::<LittleEndian>()?;
        let flags2 = cur.read_u16::<LittleEndian>()?;
        let flags3 = cur.read_u16::<LittleEndian>()?;
        let selected_range = cur.read_u8()?;
        let current_view = cur.read_u8()?;

        let f_autohold2   = get_bits16(0b0000_0000_0000_0001, flags1) > 0;
        let f_hold2       = get_bits16(0b0000_0000_0000_0010, flags1) > 0;
        let f_log_mode    = get_bits16(0b0000_0000_0000_0100, flags1) > 0;
        let f_avg         = get_bits16(0b0000_0000_0000_1000, flags1) > 0;
        let f_max         = get_bits16(0b0000_0000_0001_0000, flags1) > 0;
        let f_min         = get_bits16(0b0000_0000_0010_0000, flags1) > 0;
        let f_fast_mode   = get_bits16(0b0000_0000_0100_0000, flags1) > 0;
        let f_low_bat     = get_bits16(0b0000_0000_1000_0000, flags1) > 0;
        let f_bolt        = get_bits16(0b0000_0001_0000_0000, flags1) > 0;
        let f_delta2      = get_bits16(0b0000_0010_0000_0000, flags1) > 0;
        let f_delta_pct2  = get_bits16(0b0000_0100_0000_0000, flags1) > 0;
        let f_mem_clear   = get_bits16(0b0001_1000_0000_0000, flags1) as u8;
        let f_auto_range  = get_bits16(0b0010_0000_0000_0000, flags1) > 0;
        let f_man_range   = get_bits16(0b0100_0000_0000_0000, flags1) > 0;
        let f_shift_sign  = get_bits16(0b1000_0000_0000_0000, flags1) > 0;

        let f_mea_mode    = get_bits16(0b0000_0000_0011_1111, flags2) as u8;
        let f_hz_mode     = get_bits16(0b0000_0000_1100_0000, flags2) as u8;
        let f_mn_mx_mode  = get_bits16(0b0000_0001_0000_0000, flags2) > 0;
        let f_fast_mode2  = get_bits16(0b0000_0010_0000_0000, flags2) > 0;
        let f_ubit1       = get_bits16(0b0000_0100_0000_0000, flags2) > 0;
        let f_minmaxavg   = get_bits16(0b0001_1000_0000_0000, flags2) as u8;
        let f_ubit2       = get_bits16(0b0010_0000_0000_0000, flags2) > 0;
        let f_rising_etch = get_bits16(0b0100_0000_0000_0000, flags2) > 0;
        let f_fall_etch   = get_bits16(0b1000_0000_0000_0000, flags2) > 0;

        let f_sub_acdc    = get_bits16(0b0000_0000_0000_0011, flags3) as u8;
        let f_ubit3       = get_bits16(0b0000_0000_0000_0100, flags3) > 0;
        let f_hold        = get_bits16(0b0000_0000_0000_1000, flags3) > 0;
        let f_auto_hold   = get_bits16(0b0000_0000_0001_0000, flags3) > 0;
        let f_ubit4       = get_bits16(0b0000_0000_0010_0000, flags3) > 0;
        let f_db_v_or_m   = get_bits16(0b0000_0000_0100_0000, flags3) > 0;
        let f_temp_c_or_f = get_bits16(0b0000_0000_1000_0000, flags3) > 0;
        let f_delta       = get_bits16(0b0000_0001_0000_0000, flags3) > 0;
        let f_delta_pct   = get_bits16(0b0000_0010_0000_0000, flags3) > 0;
        let f_ubit5       = get_bits16(0b0000_0100_0000_0000, flags3) > 0;
        let f_ac_db_mode  = get_bits16(0b0011_1000_0000_0000, flags3) as u8;
        //let f_unit_db_ac  = get_bits16(0b0000_1000_0000_0000, flags3) > 0;
        //let f_unit_ac_db  = get_bits16(0b0001_0000_0000_0000, flags3) > 0;
        //let f_unit_hz_db  = get_bits16(0b0010_0000_0000_0000, flags3) > 0;
        let f_digits      = get_bits16(0b0100_0000_0000_0000, flags3) > 0;
        let f_range_disp  = get_bits16(0b1000_0000_0000_0000, flags3) > 0;

        Ok(Self {
            f_autohold2,
            f_hold2,
            f_log_mode,
            f_avg,
            f_max,
            f_min,
            f_fast_mode,
            f_low_bat,
            f_bolt,
            f_delta,
            f_delta_pct,
            f_mem_clear,
            f_auto_range,
            f_man_range,
            f_shift_sign,
            f_mea_mode,
            f_hz_mode,
            f_mn_mx_mode,
            f_fast_mode2,
            f_ubit1,
            f_minmaxavg,
            f_ubit2,
            f_rising_etch,
            f_fall_etch,
            f_sub_acdc,
            f_ubit3,
            f_hold,
            f_auto_hold,
            f_ubit4,
            f_db_v_or_m,
            f_temp_c_or_f,
            f_delta2,
            f_delta_pct2,
            f_ubit5,
            f_ac_db_mode,
            f_digits,
            f_range_disp,
            dbm_ref,
            selected_range,
            current_view,
        })
    }
    pub fn db_mode(&self) -> bool {
        //self.f_unit_ac_db | self.f_unit_db_ac | self.f_unit_hz_db
        self.f_ac_db_mode != 0
    }

    pub fn range(&self) -> io::Result<RangeSelection> {
        RangeSelection::try_from_primitive(self.selected_range)
            .map_err(|err| io::Error::new(io::ErrorKind::InvalidData, err))
    }

    pub fn attribute(&self) -> io::Result<Option<Attribute>> {
        Ok(if self.f_rising_etch {
            Some(Attribute::PositiveEdge)
        } else if self.f_fall_etch {
            Some(Attribute::NegativeEdge)
        } else {
            None
        })
    }

    pub fn volt_ac_submode(&self) -> io::Result<VoltAcSubMode> {
        VoltAcSubMode::try_from_primitive(self.f_ac_db_mode)
            .map_err(|_| std::io::Error::new(io::ErrorKind::InvalidData, "Unknown V/AC sub-mode"))
    }

    pub fn modes(&self) -> io::Result<Modes> {
        let minmax = MinMaxMode::try_from_primitive(self.f_minmaxavg)
            .map_err(|err| io::Error::new(io::ErrorKind::InvalidData, err))?;

        let mut modes = Modes::default();
        if self.f_auto_range {
            modes.add(Mode::AutoRange);
        }
        if self.f_fast_mode {
            modes.add(Mode::FastMode);
        }
        if self.f_mn_mx_mode {
            modes.add(Mode::MinMaxAvg(minmax));
        }
        if self.f_auto_hold {
            modes.add(Mode::AutoHold);
        }
        if self.f_hold {
            modes.add(Mode::Hold);
        }
        if self.f_log_mode {
            modes.add(Mode::Logging);
        }
        if self.f_delta {
            modes.add(Mode::Rel);
        }
        if self.f_delta_pct {
            modes.add(Mode::RelPercent);
        }
        Ok(modes)
    }

    pub fn get_functions(&self) -> (Function, Unit, Function, Unit, Unit) {
        let dbm_or_v = || {
            if self.f_db_v_or_m {
                Unit::dBV
            } else {
                Unit::dBm
            }
        };
        let dc_mode: DcMode = DcMode::try_from(self.f_sub_acdc).expect("Error");
        //println!("fun: {}, {}, {:?}, {}, {}, {}", value.f_mea_mode, value.f_hz_mode, dc_mode, value.f_unit_ac_db, value.f_unit_db_ac, value.f_unit_hz_db);
        let mea_mode = DeviceMode::try_from_primitive(self.f_mea_mode).unwrap_or_default();
        let vac_sm = self.volt_ac_submode().unwrap();
        let (pri_f, pri_unit, sec_f, sec_unit) = match (mea_mode, self.f_hz_mode) {
            (DeviceMode::None, _) => (Function::None, Unit::None, Function::None, Unit::None),
            // V/AC = 1
            (DeviceMode::V_AC, 0) if vac_sm == VoltAcSubMode::Normal => {
                (Function::V_AC, Unit::VoltAC, Function::None, Unit::None)
            }
            (DeviceMode::V_AC, 0) if vac_sm == VoltAcSubMode::AcOverDb => {
                (Function::V_AC, Unit::VoltAC, Function::V_AC_DB, dbm_or_v())
            }
            (DeviceMode::V_AC, 0) if vac_sm == VoltAcSubMode::DbOverAc => {
                (Function::V_AC_DB, dbm_or_v(), Function::V_AC, Unit::VoltAC)
            }
            (DeviceMode::V_AC, 1) if vac_sm == VoltAcSubMode::Normal => {
                (Function::V_AC_HZ, Unit::Hertz, Function::V_AC, Unit::VoltAC)
            }
            (DeviceMode::V_AC, 1) if vac_sm == VoltAcSubMode::HzOverDb => (
                Function::V_AC_HZ,
                Unit::Hertz,
                Function::V_AC_DB,
                dbm_or_v(),
            ),
            (DeviceMode::V_AC, 1) if vac_sm == VoltAcSubMode::DbOverHz => (
                Function::V_AC_DB,
                dbm_or_v(),
                Function::V_AC_HZ,
                Unit::Hertz,
            ),
            (DeviceMode::V_AC, 2) => (
                Function::V_AC_DUTYCYCLE,
                Unit::Percent,
                Function::V_AC_HZ,
                Unit::Hertz,
            ),
            (DeviceMode::V_AC, 3) => (
                Function::V_AC_DUTYCYCLE,
                Unit::Seconds,
                Function::V_AC_HZ,
                Unit::Hertz,
            ),
            // mV/AC = 2
            (DeviceMode::MV_AC, 0) if vac_sm == VoltAcSubMode::Normal => {
                (Function::MV_AC, Unit::VoltAC, Function::None, Unit::None)
            }
            (DeviceMode::MV_AC, 0) if vac_sm == VoltAcSubMode::AcOverDb => (
                Function::MV_AC,
                Unit::VoltAC,
                Function::MV_AC_DB,
                dbm_or_v(),
            ),
            (DeviceMode::MV_AC, 0) if vac_sm == VoltAcSubMode::DbOverAc => (
                Function::MV_AC_DB,
                dbm_or_v(),
                Function::MV_AC,
                Unit::VoltAC,
            ),
            (DeviceMode::MV_AC, 1) if vac_sm == VoltAcSubMode::Normal => (
                Function::MV_AC_HZ,
                Unit::Hertz,
                Function::MV_AC,
                Unit::VoltAC,
            ),
            (DeviceMode::MV_AC, 1) if vac_sm == VoltAcSubMode::HzOverDb => (
                Function::MV_AC_HZ,
                Unit::Hertz,
                Function::MV_AC_DB,
                dbm_or_v(),
            ),
            (DeviceMode::MV_AC, 1) if vac_sm == VoltAcSubMode::DbOverHz => (
                Function::MV_AC_DB,
                dbm_or_v(),
                Function::MV_AC_HZ,
                Unit::Hertz,
            ),
            (DeviceMode::MV_AC, 2) => (
                Function::MV_AC_DUTYCYCLE,
                Unit::Percent,
                Function::MV_AC_HZ,
                Unit::Hertz,
            ),
            (DeviceMode::MV_AC, 3) => (
                Function::MV_AC_DUTYCYCLE,
                Unit::Seconds,
                Function::MV_AC_HZ,
                Unit::Hertz,
            ),

            // V/DC = 3
            (DeviceMode::V_DC, 0) if dc_mode == DcMode::DC => {
                (Function::V_DC, Unit::VoltDC, Function::None, Unit::None)
            }
            (DeviceMode::V_DC, 1) if dc_mode == DcMode::DC => {
                (Function::V_DC_HZ, Unit::Hertz, Function::V_DC, Unit::VoltDC)
            }
            (DeviceMode::V_DC, 2) if dc_mode == DcMode::DC => (
                Function::V_DC_DUTYCYCLE,
                Unit::Percent,
                Function::V_DC_HZ,
                Unit::Hertz,
            ),
            (DeviceMode::V_DC, 3) if dc_mode == DcMode::DC => (
                Function::V_DC_DUTYCYCLE,
                Unit::Seconds,
                Function::V_DC_HZ,
                Unit::Hertz,
            ),
            // V/DC ac+dc = 5
            (DeviceMode::V_DC_AC_PLUS_DC, _) if dc_mode == DcMode::DC_OVER_AC => (
                Function::V_DC_OVER_AC,
                Unit::VoltDC,
                Function::V_AC,
                Unit::VoltAC,
            ),
            (DeviceMode::V_DC_AC_PLUS_DC, _) if dc_mode == DcMode::AC_OVER_DC => (
                Function::V_AC_OVER_DC,
                Unit::VoltAC,
                Function::V_DC,
                Unit::VoltDC,
            ),
            (DeviceMode::V_DC_AC_PLUS_DC, _) if dc_mode == DcMode::AC_PLUS_DC => (
                Function::V_AC_PLUS_DC,
                Unit::Volt,
                Function::None,
                Unit::None,
            ),

            // mV/DC = 4
            (DeviceMode::MV_DC, 0) if dc_mode == DcMode::DC => {
                (Function::MV_DC, Unit::VoltDC, Function::None, Unit::None)
            }
            (DeviceMode::MV_DC, 1) if dc_mode == DcMode::DC => (
                Function::MV_DC_HZ,
                Unit::Hertz,
                Function::MV_DC,
                Unit::VoltDC,
            ),
            (DeviceMode::MV_DC, 2) if dc_mode == DcMode::DC => (
                Function::MV_DC_DUTYCYCLE,
                Unit::Percent,
                Function::MV_DC_HZ,
                Unit::Hertz,
            ),
            (DeviceMode::MV_DC, 3) if dc_mode == DcMode::DC => (
                Function::MV_DC_DUTYCYCLE,
                Unit::Seconds,
                Function::MV_DC_HZ,
                Unit::Hertz,
            ),
            // mV/DC ac+dc = 6
            (DeviceMode::MV_DC_AC_PLUS_DC, _) if dc_mode == DcMode::DC_OVER_AC => (
                Function::MV_DC_OVER_AC,
                Unit::VoltDC,
                Function::MV_AC,
                Unit::VoltAC,
            ),
            (DeviceMode::MV_DC_AC_PLUS_DC, _) if dc_mode == DcMode::AC_OVER_DC => (
                Function::MV_AC_OVER_DC,
                Unit::VoltAC,
                Function::MV_DC,
                Unit::VoltDC,
            ),
            (DeviceMode::MV_DC_AC_PLUS_DC, _) if dc_mode == DcMode::AC_PLUS_DC => (
                Function::MV_AC_PLUS_DC,
                Unit::Volt,
                Function::None,
                Unit::None,
            ),

            // Ohm = 9
            (DeviceMode::OHMS, _) => (Function::OHMS, Unit::Ohm, Function::None, Unit::None),

            // Beeper = 11
            (DeviceMode::CONTINUITY, _) => {
                (Function::CONTINUITY, Unit::None, Function::OHMS, Unit::Ohm)
            }

            // Siemens = 10
            (DeviceMode::CONDUCTANCE, _) => (
                Function::CONDUCTANCE,
                Unit::Siemens,
                Function::None,
                Unit::None,
            ),

            // Capacitor = 12
            (DeviceMode::CAPACITANCE, _) => (
                Function::CAPACITANCE,
                Unit::Farad,
                Function::None,
                Unit::None,
            ),

            // Diode = 13
            (DeviceMode::DIODE_TEST, _) => (
                Function::DIODE_TEST,
                Unit::VoltDC,
                Function::None,
                Unit::None,
            ),

            // Temp.C = 26
            (DeviceMode::TEMPERATURE_C, _) => {
                (Function::TEMPERATURE, Unit::CEL, Function::None, Unit::CEL)
            }
            // Temp.F = 27
            (DeviceMode::TEMPERATURE_F, _) => (
                Function::TEMPERATURE,
                Unit::Fahrenheit,
                Function::None,
                Unit::Fahrenheit,
            ),

            // A/AC = 14
            (DeviceMode::A_AC, 0) => (Function::A_AC, Unit::AmpereAC, Function::None, Unit::None),
            (DeviceMode::A_AC, 1) => (
                Function::A_AC_HZ,
                Unit::Hertz,
                Function::A_AC,
                Unit::AmpereAC,
            ),
            (DeviceMode::A_AC, 2) => (
                Function::A_AC_DUTYCYCLE,
                Unit::Percent,
                Function::A_AC_HZ,
                Unit::Hertz,
            ),
            (DeviceMode::A_AC, 3) => (
                Function::A_AC_DUTYCYCLE,
                Unit::Seconds,
                Function::A_AC_HZ,
                Unit::Hertz,
            ),

            // mA/AC = 15
            (DeviceMode::MA_AC, 0) => (Function::MA_AC, Unit::AmpereAC, Function::None, Unit::None),
            (DeviceMode::MA_AC, 1) => (
                Function::MA_AC_HZ,
                Unit::Hertz,
                Function::MA_AC,
                Unit::AmpereAC,
            ),
            (DeviceMode::MA_AC, 2) => (
                Function::MA_AC_DUTYCYCLE,
                Unit::Percent,
                Function::MA_AC_HZ,
                Unit::Hertz,
            ),
            (DeviceMode::MA_AC, 3) => (
                Function::MA_AC_DUTYCYCLE,
                Unit::Seconds,
                Function::MA_AC_HZ,
                Unit::Hertz,
            ),

            // µA/AC = 16
            (DeviceMode::UA_AC, 0) => (Function::UA_AC, Unit::AmpereAC, Function::None, Unit::None),
            (DeviceMode::UA_AC, 1) => (
                Function::UA_AC_HZ,
                Unit::Hertz,
                Function::UA_AC,
                Unit::AmpereAC,
            ),
            (DeviceMode::UA_AC, 2) => (
                Function::UA_AC_DUTYCYCLE,
                Unit::Percent,
                Function::UA_AC_HZ,
                Unit::Hertz,
            ),
            (DeviceMode::UA_AC, 3) => (
                Function::UA_AC_DUTYCYCLE,
                Unit::Seconds,
                Function::UA_AC_HZ,
                Unit::Hertz,
            ),

            // A/DC = 17
            (DeviceMode::A_DC, 0) if dc_mode == DcMode::DC => {
                (Function::A_DC, Unit::AmpereDC, Function::None, Unit::None)
            }
            (DeviceMode::A_DC, 1) if dc_mode == DcMode::DC => (
                Function::A_DC_HZ,
                Unit::Hertz,
                Function::A_DC,
                Unit::AmpereDC,
            ),
            (DeviceMode::A_DC, 2) if dc_mode == DcMode::DC => (
                Function::A_DC_DUTYCYCLE,
                Unit::Percent,
                Function::A_DC_HZ,
                Unit::Hertz,
            ),
            (DeviceMode::A_DC, 3) if dc_mode == DcMode::DC => (
                Function::A_DC_DUTYCYCLE,
                Unit::Seconds,
                Function::A_DC_HZ,
                Unit::Hertz,
            ),
            // mA/DC = 18
            (DeviceMode::MA_DC, 0) if dc_mode == DcMode::DC => {
                (Function::MA_DC, Unit::AmpereDC, Function::None, Unit::None)
            }
            (DeviceMode::MA_DC, 1) if dc_mode == DcMode::DC => (
                Function::MA_DC_HZ,
                Unit::Hertz,
                Function::MA_DC,
                Unit::AmpereDC,
            ),
            (DeviceMode::MA_DC, 2) if dc_mode == DcMode::DC => (
                Function::MA_DC_DUTYCYCLE,
                Unit::Percent,
                Function::MA_DC_HZ,
                Unit::Hertz,
            ),
            (DeviceMode::MA_DC, 3) if dc_mode == DcMode::DC => (
                Function::MA_DC_DUTYCYCLE,
                Unit::Seconds,
                Function::MA_DC_HZ,
                Unit::Hertz,
            ),
            // A/DC ac+dc = 20
            (DeviceMode::A_DC_AC_PLUS_DC, _) if dc_mode == DcMode::DC_OVER_AC => (
                Function::A_DC_OVER_AC,
                Unit::AmpereDC,
                Function::A_AC,
                Unit::AmpereAC,
            ),
            (DeviceMode::A_DC_AC_PLUS_DC, _) if dc_mode == DcMode::AC_OVER_DC => (
                Function::A_AC_OVER_DC,
                Unit::AmpereAC,
                Function::A_DC,
                Unit::AmpereDC,
            ),
            (DeviceMode::A_DC_AC_PLUS_DC, _) if dc_mode == DcMode::AC_PLUS_DC => (
                Function::A_AC_PLUS_DC,
                Unit::Ampere,
                Function::None,
                Unit::None,
            ),
            // mA/DC ac+dc = 21
            (DeviceMode::MA_DC_AC_PLUS_DC, _) if dc_mode == DcMode::DC_OVER_AC => (
                Function::MA_DC_OVER_AC,
                Unit::AmpereDC,
                Function::MA_AC,
                Unit::AmpereAC,
            ),
            (DeviceMode::MA_DC_AC_PLUS_DC, _) if dc_mode == DcMode::AC_OVER_DC => (
                Function::MA_AC_OVER_DC,
                Unit::AmpereAC,
                Function::MA_DC,
                Unit::AmpereDC,
            ),
            (DeviceMode::MA_DC_AC_PLUS_DC, _) if dc_mode == DcMode::AC_PLUS_DC => (
                Function::MA_AC_PLUS_DC,
                Unit::Ampere,
                Function::None,
                Unit::None,
            ),

            // µA/DC = 19
            (DeviceMode::UA_DC, 0) if dc_mode == DcMode::DC => {
                (Function::UA_DC, Unit::AmpereDC, Function::None, Unit::None)
            }
            (DeviceMode::UA_DC, 1) if dc_mode == DcMode::DC => (
                Function::UA_DC_HZ,
                Unit::Hertz,
                Function::UA_DC,
                Unit::AmpereDC,
            ),
            (DeviceMode::UA_DC, 2) if dc_mode == DcMode::DC => (
                Function::UA_DC_DUTYCYCLE,
                Unit::Percent,
                Function::UA_DC_HZ,
                Unit::Hertz,
            ),
            (DeviceMode::UA_DC, 3) if dc_mode == DcMode::DC => (
                Function::UA_DC_DUTYCYCLE,
                Unit::Seconds,
                Function::UA_DC_HZ,
                Unit::Hertz,
            ),
            // µA/DC ac+dc = 22
            (DeviceMode::UA_DC_AC_PLUS_DC, _) if dc_mode == DcMode::DC_OVER_AC => (
                Function::UA_DC_OVER_AC,
                Unit::AmpereDC,
                Function::UA_AC,
                Unit::AmpereAC,
            ),
            (DeviceMode::UA_DC_AC_PLUS_DC, _) if dc_mode == DcMode::AC_OVER_DC => (
                Function::UA_AC_OVER_DC,
                Unit::AmpereAC,
                Function::UA_DC,
                Unit::AmpereDC,
            ),
            (DeviceMode::UA_DC_AC_PLUS_DC, _) if dc_mode == DcMode::AC_PLUS_DC => (
                Function::UA_AC_PLUS_DC,
                Unit::Ampere,
                Function::None,
                Unit::None,
            ),

            _ => {
                eprintln!(
                    "Unknown function: {}, {}, {:?}, {}",
                    self.f_mea_mode, self.f_hz_mode, dc_mode, self.f_ac_db_mode,
                );
                (Function::None, Unit::None, Function::None, Unit::None)
            }
        };

        let ter_unit = match (self.f_mea_mode, self.f_hz_mode, self.db_mode()) {
            (1, 0, true) => Unit::Ohm,
            (1, 1, true) => Unit::Ohm,
            (2, 0, true) => Unit::Ohm,
            (2, 1, true) => Unit::Ohm,
            _ => Unit::None,
        };

        if (self.f_auto_hold || self.f_hold || self.f_delta || self.f_fast_mode)
            && !self.f_delta_pct
            && !self.db_mode()
            && sec_f == Function::None
            && (dc_mode == DcMode::DC || dc_mode == DcMode::AC_PLUS_DC)
        {
            (pri_f, pri_unit, pri_f, pri_unit, ter_unit)
        } else if (self.f_delta_pct) && !self.db_mode() {
            (pri_f, Unit::Percent, pri_f, pri_unit, ter_unit)
        } else {
            (pri_f, pri_unit, sec_f, sec_unit, ter_unit)
        }
    }
}

/// Raw measurement
///
/// Internal representation of measurements
#[derive(Debug, Clone, Default)]
pub struct RawMeasurement {
    /// 1/10 seconds since reference
    pub clock: u32,
    /// Primary reading value
    pub pri_value: i32,
    /// Primary reading scale
    pub pri_scale: u8,
    /// Primary reading SI unit multiplier, 1 = 10^3, 2 = 10^6, 3 = 10^9, -1 = 10^-3 etc.
    pub pri_si: i8,
    /// Secondary reading value
    pub sec_value: i32,
    /// Secondary reading scale
    pub sec_scale: u8,
    /// Primary reading SI unit multiplier, 1 = 10^3, 2 = 10^6, 3 = 10^9, -1 = 10^-3 etc.
    pub sec_si: i8,
    /// Secondary reading value, 2. occour.
    pub sec2_value: i32,
    /// Clock, 2. occour., maybe it's read start and read-stop clock?
    pub clock2: u32,
    /// Primary reading value, 2. occour.
    pub pri2_value: i32,
    /// Primary reading scale, 2. occour.
    pub pri2_scale: u8,
    /// Primary reading scale, 2. occour.
    pub pri2_si: i8,
    /// Reading information
    pub info: RawInfo,
    /// Unknown byte 0
    pub byte0: u8,
    /// Unknown byte 1
    pub byte1: u8,
}

impl RawMeasurement {
    pub fn db_mode(&self) -> bool {
        self.info.db_mode()
    }

    pub fn low_battery(&self) -> bool {
        self.info.f_low_bat
    }
}

impl TryFrom<&[u8]> for RawMeasurement {
    type Error = std::io::Error;

    fn try_from(value: &[u8]) -> std::result::Result<Self, Self::Error> {
        assert!(value.len() >= 32 + RAW_INFO_LEN);
        let mut cur = Cursor::new(value);
        let clock = cur.read_u32::<LittleEndian>()?;
        let pri_value = cur.read_i32::<LittleEndian>()?;
        let pri_scale = cur.read_u8()?;

        let pri_si = cur.read_i8()?;
        let sec_value = cur.read_i32::<LittleEndian>()?;
        let sec_scale = cur.read_u8()?;

        let sec_si = cur.read_i8()?;
        let sec2_value = cur.read_i32::<LittleEndian>()?;
        let clock2 = cur.read_u32::<LittleEndian>()?;
        let pri2_value = cur.read_i32::<LittleEndian>()?;
        let pri2_scale = cur.read_u8()?;
        let pri2_si = cur.read_i8()?;

        let info = RawInfo::new(&mut cur)?;

        let byte0 = cur.read_u8()?;
        let byte1 = cur.read_u8()?;

        //eprintln!("clock1:{clock} {pri_value:011}/{pri_scale:03} pri_si:{pri_si:03} {sec_value:011}/{sec_scale:03} sec_si:{sec_si:03} sec2:{sec2_value:011} clock2:{clock2} pri2:{pri2_value:011}/{pri2_scale:04} si:{pri2_si:03} flags1:{flags1:#018b} db:{dbref:04} flags2:{flags2:#018b} flags3:{flags3:#018b} range:0x{selected_range:x} view:{current_view}");

        //eprintln!("IN: {:?}", &value[2+26..]);
        //eprintln!("IN: {:?}", &value[2..]);

        Ok(Self {
            pri_value,
            pri_scale,
            sec_value,
            sec_scale,
            info,
            clock,
            pri_si,
            sec_si,
            sec2_value,
            clock2,
            pri2_value,
            pri2_scale,
            pri2_si,
            byte0,
            byte1,
        })
    }
}

/// Raw saved measurement
#[derive(Debug, Clone)]
pub struct RawSavedMeasurement {
    /// 1/10 seconds since reference
    pub clock: u32,
    /// Primary reading value
    pub pri_value: i32,
    /// Primary reading scale
    pub pri_scale: u8,
    /// Primary reading SI unit multiplier, 1 = 10^3, 2 = 10^6, 3 = 10^9, -1 = 10^-3 etc.
    pub pri_si: i8,
    /// Secondary reading value
    pub sec_value: i32,
    /// Secondary reading scale
    pub sec_scale: u8,
    /// Primary reading SI unit multiplier, 1 = 10^3, 2 = 10^6, 3 = 10^9, -1 = 10^-3 etc.
    pub sec_si: i8,
    /// Secondary reading value, 2. occour.
    pub sec2_value: i32,
    /// Clock, 2. occour., maybe it's read start and read-stop clock?
    pub clock2: u32,
    /// Primary reading value, 2. occour.
    pub pri2_value: i32,
    /// Primary reading scale, 2. occour.
    pub pri2_scale: u8,
    /// Primary reading scale, 2. occour.
    pub pri2_si: i8,
    /// Reading information
    pub info: RawInfo,
    /// Unknown byte 0
    pub byte0: u8,
    /// Unknown byte 1
    pub byte1: u8,
}

/// Raw recording
#[derive(Debug, Clone)]
pub struct RawRecording {
    /// 1/10 seconds since reference
    pub clock_begin: u32,
    /// Primary reading scale
    pub pri_scale: u8,
    /// Primary reading SI unit multiplier, 1 = 10^3, 2 = 10^6, 3 = 10^9, -1 = 10^-3 etc.
    pub pri_si: i8,
    /// Min reading value
    pub min_value: i32,
    /// Max reading value
    pub max_value: i32,
    /// Summed reading value
    pub sum_value: i32,
    /// Unknown
    pub int0: i32,
    /// Count of values included in sum_value
    pub sum_count: u32,
    /// Status
    pub status: u8,
    /// Unknown
    pub byte1: u8,
    /// 1/10 seconds since reference
    pub clock_end: u32,
}

impl TryFrom<&[u8]> for RawRecording {
    type Error = std::io::Error;

    fn try_from(value: &[u8]) -> std::result::Result<Self, Self::Error> {
        assert_eq!(value.len(), 32);
        let mut cur = Cursor::new(value);
        let clock_begin = cur.read_u32::<LittleEndian>()?;
        let pri_scale = cur.read_u8()?;
        let pri_si = cur.read_i8()?;
        let min_value = cur.read_i32::<LittleEndian>()?;
        let max_value = cur.read_i32::<LittleEndian>()?;
        let sum_value = cur.read_i32::<LittleEndian>()?;
        let int0 = cur.read_i32::<LittleEndian>()?;
        let sum_count = cur.read_u32::<LittleEndian>()?;
        let status = cur.read_u8()?;
        let byte1 = cur.read_u8()?;
        let clock_end = cur.read_u32::<LittleEndian>()?;
        Ok(Self {
            clock_begin,
            pri_scale,
            pri_si,
            min_value,
            max_value,
            sum_value,
            int0,
            sum_count,
            status,
            byte1,
            clock_end,
        })
    }
}

/// Raw recording session
#[derive(Debug, Clone)]
pub struct RawRecordingSession {
    pub recordings: Vec<RawRecording>,
    pub init_value: i32,
    pub init_scale: u8,
    pub init_prefix: i8,
    pub info: RawInfo,
}

impl TryFrom<&[u8]> for RawRecordingSession {
    type Error = std::io::Error;

    fn try_from(value: &[u8]) -> std::result::Result<Self, Self::Error> {
        const REC_SIZE: usize = 32;
        let mut cur = Cursor::new(value);
        let data_count = cur.read_u16::<LittleEndian>()?;
        let init_value = cur.read_i32::<LittleEndian>()?;
        let init_scale = cur.read_u8()?;
        let init_prefix = cur.read_i8()?;
        let info = RawInfo::new(&mut cur)?;
        let mut recordings = Vec::with_capacity(data_count as usize);
        let mut buf = [0; REC_SIZE];
        for _ in 0..data_count {
            cur.read_exact(&mut buf)?;
            recordings.push(RawRecording::try_from(&buf[..])?);
        }
        Ok(Self {
            recordings,
            init_value,
            init_scale,
            init_prefix,
            info,
        })
    }
}

/// Helper function to extract bits
fn get_bits16(mask: u16, value: u16) -> u16 {
    assert_ne!(mask, 0);
    let shift = mask.trailing_zeros();
    (value & mask) >> shift
}
