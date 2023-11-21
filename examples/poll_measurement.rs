use f189ctrl::{measurement::Measurement, Device, DEFAULT_BAUDRATE};

#[tokio::main]
async fn main() -> f189ctrl::Result<()> {
    let path = "/dev/ttyUSB0".to_string();
    let mut device = Device::new(&path, DEFAULT_BAUDRATE)?;

    loop {
        let raw = device.live_measurement().await?;
        match raw {
            Some(data) => {
                let mea = Measurement::try_from(data).unwrap();
                println!("Value: {}", mea.pri_reading.value);
            }
            None => {
                println!("NO_DATA");
            }
        }
    }
}
