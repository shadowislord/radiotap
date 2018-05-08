extern crate hex;
extern crate pcap;
extern crate radiotap;

use radiotap::{Modulation, Radiotap};
use pcap::{Active, Capture, Device, Linktype};

fn find_capture_device() -> Option<Capture<Active>> {
    let list = Device::list().expect("cannot get device list");
    for device in list {
        let capture = Capture::from_device(device)
            .expect("failed to create capture from device")
            .snaplen(100);

        let capture = {
            match capture.open() {
                Ok(c) => c,
                Err(e) => {
                    println!("WARN: failed to open capture device: {}", e);
                    continue;
                }
            }
        };

        if capture.get_datalink() == Linktype(127) {
            return Some(capture);
        }
    }
    None
}

fn main() {
    let mut capture = find_capture_device().expect(
        "Cannot find network device with radiotap header.
        Enable monitor mode on a 802.11 device and make
        sure the example has the CAP_NET_RAW and CAP_NET_ADMIN capabilities",
    );

    capture
        .filter("wlan[0] == 0x88 or wlan[0] == 0x08")
        .expect("failed to set capture filter");

    loop {
        let packet = capture.next().expect("failed to read packet");
        let radiotap = Radiotap::parse(&packet).expect("failed to parse radiotap header");
        println!("{}", radiotap);
    }
}
