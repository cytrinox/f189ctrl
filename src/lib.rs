//!
//! This library provides communication with a Fluke 89 IV/187/189 digital multimeter.
//!
//! <br>
//!
//! # Details
//!
//! - You need a Fluke IR cable attached to your DMM.
//!
//! - Basic setup and connection
//!
//!   ```
//!   use f289ctrl::{Device, DEFAULT_BAUDRATE};
//!   #[tokio::main]
//!   async fn main() -> f289ctrl::Result<()> {
//!       let path = "/dev/ttyUSB0".to_string();
//!       let mut device = Device::new(&path, DEFAULT_BAUDRATE)?;
//!       eprintln!("Connected to: {}\n", device.ident().await?.model);
//!       Ok(())
//!   }
//!   ```
//!
//! # Supported devices
//!
//!  * Fluke 187
//!  * Fluke 189
//!  * Fluke 89 IV
//!

pub mod device;
pub mod measurement;
pub mod proto;
pub mod rawmea;

pub use device::Device;
pub use proto::Result;

#[cfg(unix)]
pub const DEFAULT_TTY: &str = "/dev/ttyUSB0";
#[cfg(windows)]
pub const DEFAULT_TTY: &str = "COM1";

/// Default Baudrate for Fluke 187 and 189.
pub const DEFAULT_BAUDRATE: u32 = 9600;
