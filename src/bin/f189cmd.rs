#![deny(clippy::unwrap_used)]

use byteorder::{LittleEndian, WriteBytesExt};
use chrono::Local;
use clap::builder::{BoolishValueParser, NonEmptyStringValueParser};
use clap::{arg, command, value_parser};
use f189ctrl::device::Device;
use f189ctrl::measurement::{FlattenMeasurement, FlattenRecording, Measurement, RecordingSession};
use f189ctrl::proto::command::{AcHertz, ClearMemory, DbMode, DigitCount, TempUnit};
use f189ctrl::proto::response::Response;
use f189ctrl::proto::Result;
use f189ctrl::{proto, DEFAULT_BAUDRATE, DEFAULT_TTY};
use std::fmt;
use std::fs::File;
use std::io::{BufWriter, ErrorKind, Write};
use std::process::exit;
use std::sync::Arc;
use std::{env, path::PathBuf, str, time::Duration};
use tokio::sync::Mutex;

#[derive(Debug, Copy, Clone)]
pub enum OutputFormat {
    Text,
    Json,
    Csv,
}

impl fmt::Display for OutputFormat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Text => f.write_str("text"),
            Self::Json => f.write_str("json"),
            Self::Csv => f.write_str("csv"),
        }
    }
}

impl clap::ValueEnum for OutputFormat {
    fn value_variants<'a>() -> &'a [Self] {
        &[Self::Text, Self::Json, Self::Csv]
    }

    fn to_possible_value(&self) -> Option<clap::builder::PossibleValue> {
        Some(match self {
            Self::Text => clap::builder::PossibleValue::new("text"),
            Self::Json => clap::builder::PossibleValue::new("json"),
            Self::Csv => clap::builder::PossibleValue::new("csv"),
        })
    }
}

#[tokio::main]
async fn main() -> tokio_serial::Result<()> {
    let matches =
        command!() // requires `cargo` feature
            .arg(
                arg!(
                    -p --device <PORT> "Port for USB adapter"
                )
                .default_value(DEFAULT_TTY)
                .required(false)
                .value_parser(value_parser!(PathBuf)),
            )
            .arg(arg!(
                -d --debug ... "Turn debugging information on"
            ))
            .arg(
                arg!(
                    -b --baudrate <BAUDRATE> "Baudrate"
                )
                .default_value(DEFAULT_BAUDRATE.to_string())
                .value_parser(value_parser!(u32)),
            )
            .subcommand(
                clap::Command::new("backlight")
                    .about("Auto Backlight Timeout")
                    .arg(
                        arg!([seconds] "Set time in mm:ss for auto backlight timeout")
                            .value_parser(NonEmptyStringValueParser::new()),
                    ),
            )
            .subcommand(
                clap::Command::new("poweroff").about("Auto Power Off").arg(
                    arg!([minutes] "Set time in hh:mm for auto power off")
                        .value_parser(NonEmptyStringValueParser::new()),
                ),
            )
            .subcommand(clap::Command::new("reset-device").about("Reset device"))
            .subcommand(
                clap::Command::new("temp-offset")
                    .about("Temperature offset")
                    .arg(arg!([offset] "Set custom offset").value_parser(value_parser!(f32))),
            )
            .subcommand(
                clap::Command::new("temp-unit")
                    .about("Temperature unit")
                    .arg(arg!([unit] "Set unit").value_parser(value_parser!(TempUnit))),
            )
            .subcommand(clap::Command::new("digits").about("Digit count").arg(
                arg!([digits] "Set display digit count").value_parser(value_parser!(DigitCount)),
            ))
            .subcommand(
                clap::Command::new("recording-interval")
                    .about("Interval in seconds")
                    .arg(
                        arg!([seconds] "Set time in mm:ss for recording interval")
                            .value_parser(NonEmptyStringValueParser::new()),
                    ),
            )
            .subcommand(
                clap::Command::new("recording-thd")
                    .about("Recording threshold in %")
                    .arg(
                        arg!([percent] "Set threshold")
                            .value_parser(clap::value_parser!(u8).range(0..=100)),
                    ),
            )
            .subcommand(
                clap::Command::new("dBm-reference")
                    .about("dBm reference in Ohm")
                    .arg(
                        arg!([reference] "Set dBm reference")
                            .value_parser(clap::value_parser!(u16).range(1..=1999)),
                    ),
            )
            .subcommand(
                clap::Command::new("powergrid-freq")
                    .about("Powergrid AC frequency")
                    .arg(arg!([freq] "Set AC frequency").value_parser(value_parser!(AcHertz))),
            )
            .subcommand(
                clap::Command::new("dB-mode")
                    .about("Decibel unit")
                    .arg(arg!([unit] "Set decibel unit").value_parser(value_parser!(DbMode))),
            )
            .subcommand(clap::Command::new("ident").about("Device identification"))
            .subcommand(
                clap::Command::new("beeper")
                    .about("Beeper")
                    .arg(arg!([state] "Set beeper").value_parser(BoolishValueParser::new())),
            )
            .subcommand(
                clap::Command::new("clock")
                    .about("Internal clock")
                    .arg(arg!(
                        --"sync-with-host" "Sync DMM clock with local host"
                    )),
            )
            .subcommand(
                clap::Command::new("mea")
                    .about("Get current measurement")
                    .arg(arg!(
                        --"loop" "Poll current measurement forever"
                    ))
                    .arg(arg!(
                        --"fifo" <FIFO> "Write measurement as f64 values (LE) to FIFO file, order is [PRI][SEC][TER] reading"
                    ))
                    .arg(
                        arg!(--"format" <fmt> "Output format")
                            .value_parser(value_parser!(OutputFormat)),
                    ),
            )
            .subcommand(
                clap::Command::new("clear").about("Clear memory").arg(
                    arg!(<memory> "Memory type to clear")
                    .required(false)
                        .value_parser(value_parser!(ClearMemory))
                ),
            )
            .subcommand(
                clap::Command::new("dump-measurements")
                    .about("Dump memory measurements")
                    .alias("dump-mea")
                    .arg(
                        arg!(output: --"output" <PATH> "Output path")
                            .value_parser(value_parser!(PathBuf)),
                    )
                    .arg(
                        arg!(format: --"format" <fmt> "Output format")
                            .value_parser(value_parser!(OutputFormat)),
                    ),
            )
            .subcommand(
                clap::Command::new("dump-recordings")
                    .about("Dump memory recordings")
                    .alias("dump-rec")
                    .arg(
                        arg!(output: --"output" <PATH> "Output path")
                            .value_parser(value_parser!(PathBuf)),
                    )
                    .arg(
                        arg!(format: --"format" <fmt> "Output format")
                            .value_parser(value_parser!(OutputFormat)),
                    ),
            )
            .subcommand_required(true)
            .get_matches();

    match handle_args(&matches).await {
        Ok(()) => {}
        Err(e) => {
            //eprintln!("{:?}", e);
            match e {
                proto::ProtoError::Serial(err) => {
                    let port = matches
                        .get_one::<PathBuf>("device")
                        .expect("Requires device parameter")
                        .display();

                    if err.kind() == tokio_serial::ErrorKind::NoDevice
                        || matches!(err.kind(), tokio_serial::ErrorKind::Io(ErrorKind::NotFound))
                    {
                        eprintln!("{}: File not found", port);
                    } else {
                        eprintln!("I/O Error: {} [device: {}]", err, port,);
                    }
                    exit(-1);
                }
                proto::ProtoError::Io(err) => {
                    let port = matches
                        .get_one::<PathBuf>("device")
                        .expect("Requires device parameter")
                        .display();

                    if err.kind() == ErrorKind::NotFound {
                        eprintln!("{}: File not found", port);
                    } else {
                        eprintln!("I/O Error: {} [device: {}]", err, port,);
                    }
                    exit(-1);
                }
                proto::ProtoError::SyntaxError => {
                    eprintln!("Command was not recognized by device, aborting!");
                    exit(-1);
                }
                proto::ProtoError::ExecutionError => {
                    eprintln!("Command was not executed, maybe device is locked? Try to exit the current screen mode.");
                    exit(-1);
                }
                proto::ProtoError::Abort => {
                    eprintln!("Failed to communicate with device, aborting!");
                    exit(-1);
                }
                proto::ProtoError::Unexpected(err) if *err == Response::NoData => {
                    eprintln!("No data returned");
                    exit(-2);
                }
                proto::ProtoError::Unexpected(err) => {
                    eprintln!(
                        "Received an unexpected response from device, aborting!: {:?}",
                        err
                    );
                    exit(-1);
                }
            }
        }
    }

    Ok(())
}

async fn handle_args(matches: &clap::ArgMatches) -> Result<()> {
    let baud_rate = matches
        .get_one::<u32>("baudrate")
        .unwrap_or(&DEFAULT_BAUDRATE);

    if let Some(port_path) = matches.get_one::<PathBuf>("device") {
        let mut device = Device::new(port_path.to_string_lossy(), *baud_rate)?;

        eprintln!("Connected to: {}\n", port_path.display());

        //let x = device.query_settings().await?;
        //eprintln!("{:?}", x);

        /*
        let mut x = device.query_settings().await?;
        eprintln!("{:?}", x);
        x.beep = 1;

        eprintln!("Clock: {}", x.clock());

        //x.set_clock(NaiveTime::from_hms_milli_opt(14, 56, 23, 0).expect("invalid time"));
        //x.clock = 10 * (14 * 60 * 60 + 36 * 60) ;
        device.set_settings(x).await?;
        //sleep(Duration::from_secs(60)).await;
        let x = device.query_settings().await?;
        eprintln!("{:?}", x);

        return Ok(());
         */

        match matches.subcommand() {
            // Device ID
            Some(("ident", _args)) => {
                let ident = device.ident().await?;
                println!("Model: {}", ident.model);
                println!("Firmware: {}", ident.firmware);
                println!("Serial: {}", ident.serial);
            }
            // Auto Backlight Timeout
            Some(("backlight", args)) => {
                if let Some(seconds) = args.get_one::<String>("seconds") {
                    let d = duration_from_mmss(seconds).expect("Invalid argument");
                    if d.as_secs() > 99 * 60 + 59 {
                        eprintln!("Duration is too big");
                    } else {
                        // Write value
                        device.set_backlight_off(d).await?;
                        println!("OK");
                    }
                } else {
                    // Read value
                    let backlight = device.backlight_off().await?;
                    if backlight.is_zero() {
                        println!("Auto Backlight Timeout: OFF");
                    } else {
                        println!(
                            "Auto Backlight Timeout: {} mm:ss",
                            pretty_print_mmss(&backlight)
                        );
                    }
                }
            }
            // Auto poweroff
            Some(("poweroff", args)) => {
                if let Some(minutes) = args.get_one::<String>("minutes") {
                    let d = duration_from_hhmm(minutes).expect("Invalid argument");
                    if d.as_secs() > 23 * 60 * 60 + 59 * 60 {
                        eprintln!("Duration is too big");
                    } else {
                        // Write value
                        device.set_auto_poweroff(d).await?;
                        println!("OK");
                    }
                } else {
                    // Read value
                    let poweroff = device.auto_poweroff().await?;
                    if poweroff.is_zero() {
                        println!("Auto Poweroff Timeout: OFF");
                    } else {
                        println!(
                            "Auto Poweroff Timeout: {} hh:mm",
                            pretty_print_hhmm(&poweroff)
                        );
                    }
                }
            }

            // Recording interval
            Some(("recording-interval", args)) => {
                if let Some(seconds) = args.get_one::<String>("seconds") {
                    let d = duration_from_mmss(seconds).expect("Invalid argument");
                    if d.as_secs() > 99 * 60 + 59 {
                        eprintln!("Duration is too big");
                    } else {
                        // Write value
                        device.set_recording_interval(d).await?;
                        println!("OK");
                    }
                } else {
                    // Read value
                    let interval = device.recording_interval().await?;
                    if interval.is_zero() {
                        println!("Recording interval: OFF");
                    } else {
                        println!("Recording interval: {} mm:ss", pretty_print_mmss(&interval));
                    }
                }
            }

            // Powergrid AC frequency
            Some(("powergrid-freq", args)) => {
                if let Some(freq) = args.get_one::<AcHertz>("freq") {
                    // Write value
                    device.set_ac_freq(*freq).await?;
                    println!("OK");
                } else {
                    // Read value
                    let freq = device.ac_freq().await?;
                    println!("AC frequency: {}", freq);
                }
            }

            // dB mode
            Some(("dB-mode", args)) => {
                if let Some(mode) = args.get_one::<DbMode>("unit") {
                    // Write value
                    device.set_db_mode(*mode).await?;
                    println!("OK");
                } else {
                    // Read value
                    let mode = device.db_mode().await?;
                    println!("dB mode: {}", mode);
                }
            }

            // Clock
            Some(("clock", args)) => {
                if let Some(true) = args.get_one::<bool>("sync-with-host") {
                    // Write value
                    device.set_clock(Local::now()).await?;
                    println!("OK");
                } else {
                    // Read value
                    let clock = device.clock().await?;
                    println!("Device clock: {}", clock);
                }
            }
            // Reset
            Some(("reset", _)) => {
                device.reset().await?;
                println!("OK");
            }
            // Beeper
            Some(("beeper", args)) => {
                if let Some(state) = args.get_one::<bool>("state") {
                    // Write value
                    device.set_beeper(*state).await?;
                    println!("OK");
                } else {
                    // Read value
                    let state = device.beeper().await?;
                    println!("Beeper: {}", state);
                }
            }

            // dBm-Ref
            Some(("dBm-reference", args)) => {
                if let Some(dbm) = args.get_one::<u16>("reference") {
                    if *dbm > 0 && *dbm <= 1999 {
                        // Write value
                        device.set_dbm_ref(*dbm).await?;
                        println!("OK");
                    } else {
                        println!("dBm reference must be in range 1 .. 1999 Ohm");
                    }
                } else {
                    // Read value
                    let dbm = device.d_bm_ref().await?;
                    println!("dBm reference: {}", dbm);
                }
            }
            // Temp Offset
            Some(("temp-offset", args)) => {
                if let Some(offset) = args.get_one::<f32>("offset") {
                    // Write value
                    if *offset >= -199.9 && *offset <= 199.9 {
                        device.set_temp_offset(*offset).await?;
                        println!("OK");
                    } else {
                        println!("Offset must be in range -199.9 .. 199.9");
                    }
                } else {
                    // Read value
                    let offset = device.temp_offset().await?;
                    println!("Temp. offset: {}", offset);
                }
            }
            // Temp Offset
            Some(("temp-unit", args)) => {
                if let Some(unit) = args.get_one::<TempUnit>("unit") {
                    // Write value
                    device.set_temp_unit(*unit).await?;
                    println!("OK");
                } else {
                    // Read value
                    let unit = device.temp_unit().await?;
                    println!("Temp. unit: {}", unit);
                }
            }
            // Digit count
            Some(("digits", args)) => {
                if let Some(count) = args.get_one::<DigitCount>("digits") {
                    // Write value
                    device.set_digit_count(*count).await?;
                    println!("OK");
                } else {
                    // Read value
                    let count = device.digit_count().await?;
                    match count {
                        DigitCount::Digit4 => println!("Digit count: 4",),
                        DigitCount::Digit5 => println!("Digit count: 5",),
                    }
                }
            }

            // Recording event thd
            Some(("recording-thd", args)) => {
                if let Some(thd) = args.get_one::<u8>("percent") {
                    // Write value
                    device.set_recording_event_threshold(*thd).await?;
                    println!("OK");
                } else {
                    // Read value
                    let thd = device.recording_event_threshold().await?;
                    println!("Recording event threshold: {}%", thd);
                }
            }
            // Clear
            Some(("clear", args)) => {
                if let Some(memory) = args.get_one::<ClearMemory>("memory") {
                    device.clear(*memory).await?;
                    println!("OK");
                } else {
                    device.clear(ClearMemory::Measurements).await?;
                    device.clear(ClearMemory::Recordings).await?;
                    println!("OK");
                }
            }
            // Measurement
            Some(("mea", args)) => {
                let endless_loop = args.get_one::<bool>("loop").unwrap_or(&false);

                let mut fifo = args
                    .get_one::<PathBuf>("fifo")
                    .map(File::open)
                    .transpose()?;

                let mut c = 1;

                let mut prifunction = None;
                let mut secfunction = None;
                let mut modes = None;
                let mut selected_range = None;

                let format = args
                    .get_one::<OutputFormat>("format")
                    .unwrap_or(&OutputFormat::Text);

                let csv_output = Arc::new(Mutex::new(None));
                loop {
                    let mut output = std::io::stdout();
                    match device.live_measurement().await {
                        Ok(Some(mea_raw)) => {
                            /*
                            eprintln!(
                                "{:?}, {}, {}",
                                (
                                    mea_raw.info.f_ubit1,
                                    mea_raw.info.f_ubit2,
                                    mea_raw.info.f_ubit3,
                                    mea_raw.info.f_ubit4,
                                    mea_raw.info.f_ubit5
                                ),
                                mea_raw.byte0,
                                mea_raw.byte1
                            );
                             */
                            let low_bat = mea_raw.low_battery();
                            let mea = Measurement::try_from(mea_raw)?;

                            match format {
                                OutputFormat::Text => {
                                    if prifunction != Some(mea.pri_function)
                                        || secfunction != Some(mea.sec_function)
                                        || modes.as_ref() != Some(&mea.modes)
                                        || selected_range.as_ref() != Some(&mea.selected_range)
                                    {
                                        prifunction = Some(mea.pri_function);
                                        secfunction = Some(mea.sec_function);
                                        modes = Some(mea.modes.clone());
                                        selected_range = Some(mea.selected_range);
                                        println!(
                                    "Measurement primary: [{}], secondary: [{}], modes: [{}], range: [{}]",
                                    mea.pri_function, mea.sec_function, mea.modes, mea.selected_range);
                                    }
                                    if low_bat {
                                        eprintln!("Warning: low battery!");
                                    }
                                    println!("#{:0>4} {:#}", c, mea,);
                                }
                                OutputFormat::Json => {
                                    output.write_all(
                                        serde_json::to_string_pretty(&mea)
                                            .expect("JSON serializaion failed")
                                            .as_bytes(),
                                    )?;
                                    output.write_fmt(format_args!("\n"))?;
                                }
                                OutputFormat::Csv => {
                                    let mut guard = csv_output.lock().await;
                                    if guard.is_none() {
                                        *guard = Some(csv::Writer::from_writer(output));
                                    }

                                    if let Some(writer) = guard.as_mut() {
                                        writer
                                            .serialize(FlattenMeasurement::from(mea.clone()))
                                            .expect("Writing CSV failed");
                                        writer.flush()?;
                                    }
                                }
                            }

                            if let Some(binout) = &mut fifo {
                                binout.write_f64::<LittleEndian>(mea.pri_reading.value)?;
                                binout.write_f64::<LittleEndian>(mea.sec_reading.value)?;
                                binout.write_f64::<LittleEndian>(mea.ter_reading.value)?;
                            }

                            //println!("{:?}", r);
                        }
                        Ok(None) => {
                            println!("--- NO DATA ---");
                        }
                        Err(err) => {
                            eprintln!("Error: {}", err);
                        }
                    }

                    if !endless_loop {
                        break;
                    }
                    tokio::time::sleep(Duration::from_millis(1000)).await;
                    c += 1;
                }
            }

            Some(("dump-measurements", args)) => {
                //let watch = args.get_one::<bool>("watch").unwrap_or(&false);

                let raw_meas = device.saved_measurements().await?;

                let meas: Vec<Measurement> = raw_meas
                    .into_iter()
                    .flat_map(Measurement::try_from)
                    .collect();

                let format = args
                    .get_one::<OutputFormat>("format")
                    .unwrap_or(&OutputFormat::Text);

                let mut output = if let Some(fpath) = args.get_one::<PathBuf>("output") {
                    BufWriter::new(Box::from(File::create(fpath)?) as Box<dyn Write>)
                } else {
                    BufWriter::new(Box::from(std::io::stdout().lock()) as Box<dyn Write>)
                };

                match format {
                    OutputFormat::Text => {
                        for (c, mea) in meas.iter().enumerate() {
                            output.write_fmt(format_args!("#{:0>3} {:#}", c + 1, mea))?;
                            output.write_fmt(format_args!("\n"))?;
                        }
                    }
                    OutputFormat::Json => {
                        output.write_all(
                            serde_json::to_string_pretty(&meas)
                                .expect("JSON serializaion failed")
                                .as_bytes(),
                        )?;
                    }
                    OutputFormat::Csv => {
                        let mut wtr = csv::Writer::from_writer(output);
                        for mea in meas {
                            wtr.serialize(FlattenMeasurement::from(mea))
                                .expect("Writing CSV failed");
                        }
                        wtr.flush()?;
                    }
                }
            }

            Some(("dump-recordings", args)) => {
                let raw_meas = device.saved_recordings().await?;
                let meas = RecordingSession::try_from(raw_meas)?;

                let format = args
                    .get_one::<OutputFormat>("format")
                    .unwrap_or(&OutputFormat::Text);

                let mut output = if let Some(fpath) = args.get_one::<PathBuf>("output") {
                    BufWriter::new(Box::from(File::create(fpath)?) as Box<dyn Write>)
                } else {
                    BufWriter::new(Box::from(std::io::stdout().lock()) as Box<dyn Write>)
                };

                match format {
                    OutputFormat::Text => {
                        output.write_fmt(format_args!(
                            "Measurement primary: [{}], modes: [{}], range: [{}]",
                            meas.pri_function, meas.modes, meas.selected_range
                        ))?;

                        for (c, mea) in meas.recordings.iter().enumerate() {
                            output.write_fmt(format_args!("#{:0>3} {:#}", c + 1, mea))?;
                            output.write_fmt(format_args!("\n"))?;
                        }
                    }
                    OutputFormat::Json => {
                        output.write_all(
                            serde_json::to_string_pretty(&meas)
                                .expect("JSON serializaion failed")
                                .as_bytes(),
                        )?;
                    }
                    OutputFormat::Csv => {
                        let mut wtr = csv::Writer::from_writer(output);
                        for mea in meas.recordings {
                            wtr.serialize(FlattenRecording::from(mea))
                                .expect("Writing CSV failed");
                        }
                        wtr.flush()?;
                    }
                }
            }
            _ => {
                return Err(std::io::Error::new(
                    ErrorKind::Unsupported,
                    "Unsupported command line argument",
                )
                .into());
            }
        }
    }

    Ok(())
}

fn duration_from_hhmm(s: &str) -> std::result::Result<Duration, &str> {
    if s.to_lowercase() == "off" {
        Ok(Duration::default())
    } else if let Some((hh, mm)) = s.split_once(':') {
        let hh = hh.parse::<u64>().map_err(|_| "Invalid value")?;
        let mm = mm.parse::<u64>().map_err(|_| "Invalid value")?;
        if mm > 59 {
            return Err("Overflow");
        }
        Ok(Duration::from_secs(mm * 60 + hh * 60 * 60))
    } else {
        let mm = s.parse::<u64>().map_err(|_| "Invalid value")?;
        Ok(Duration::from_secs(mm * 60))
    }
}

fn duration_from_mmss(s: &str) -> std::result::Result<Duration, &str> {
    if s.to_lowercase() == "off" {
        Ok(Duration::default())
    } else if let Some((mm, ss)) = s.split_once(':') {
        let mm = mm.parse::<u64>().map_err(|_| "Invalid value")?;
        let ss = ss.parse::<u64>().map_err(|_| "Invalid value")?;
        if ss > 59 {
            return Err("Overflow");
        }
        Ok(Duration::from_secs(ss + mm * 60))
    } else {
        let mm = s.parse::<u64>().map_err(|_| "Invalid value")?;
        Ok(Duration::from_secs(mm))
    }
}

fn pretty_print_hhmm(d: &Duration) -> String {
    format!("{}:{:02}", d.as_secs() / 60 / 60, (d.as_secs() / 60) % 60)
}

fn pretty_print_mmss(d: &Duration) -> String {
    format!("{}:{:02}", d.as_secs() / 60, d.as_secs() % 60)
}
