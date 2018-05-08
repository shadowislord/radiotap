#[macro_use]
extern crate bitflags;
extern crate byteorder;

#[macro_use]
extern crate lazy_static;

use std::fmt;
use std::fmt::{Display, Formatter};
use std::io::{Cursor, Seek, SeekFrom};
use std::io;
use std::io::Write;
use std::collections::HashMap;

use byteorder::{ReadBytesExt, WriteBytesExt, LE};

bitflags! {
    struct Fields : u32 {
        const TSFT = 1 << 0;
        const FLAGS = 1 << 1;
        const RATE = 1 << 2;
        const CHANNEL = 1 << 3;
        const ANTENNA_SIGNAL = 1 << 5;
        const ANTENNA = 1 << 11;
        const RX_FLAGS = 1 << 14;
        const TX_FLAGS = 1 << 15;
        const DATA_RETRIES = 1 << 17;
        const MCS = 1 << 19;
        const AMPDU = 1 << 20;
        const MORE_PRESENT = 1 << 31;
    }
}

bitflags! {
    struct Flags : u8 {
        const SHORT_PREAMBLE = 0x02;
        const WEP = 0x04;
        const FRAGMENTATION = 0x08;
        const FCS_AT_END = 0x10;
        const FCS_FAIL = 0x40;
    }
}

bitflags! {
    struct ChannelFlags : u16 {
        const CCK = 0x0020;
        const OFDM = 0x0040;
        const SPECTRUM_2GHZ = 0x0080;
        const SPECTRUM_5GHZ = 0x0100;
        const CCK_OFDM = 0x0400;
    }
}

bitflags! {
    struct RxFlags : u16 {
        const PLCP_FAIL = 0x0002;
    }
}

bitflags! {
    struct TxFlags: u16 {
        const NO_ACK = 0x0008;
        const REAL_SEQUENCE = 0x0010;
    }
}

bitflags! {
    struct McsPresent: u8 {
        const BANDWIDTH = 0x01;
        const MCS_INDEX = 0x02;
        const GUARD_INTERVAL = 0x04;
        const FORMAT = 0x08;
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Bandwidth {
    _20,
    _40,
}

impl Display for Bandwidth {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            Bandwidth::_20 => write!(f, "20 Mhz"),
            Bandwidth::_40 => write!(f, "40 Mhz"),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
enum HtFormat {
    Mixed,
    Greenfield,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
enum GuardInterval {
    Long,
    Short,
}

#[derive(Debug, Copy, Clone)]
struct MCS {
    bandwidth: Option<Bandwidth>,
    index: Option<u8>,
    guard_interval: Option<GuardInterval>,
    ht_format: Option<HtFormat>,
}

impl Display for MCS {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let index = self.index.ok_or(fmt::Error)?;
        write!(f, "{}", index)?;
        if let Some(gi) = self.guard_interval {
            if gi == GuardInterval::Short {
                write!(f, " short GI")?;
            }
        }
        if let Some(htf) = self.ht_format {
            if htf == HtFormat::Greenfield {
                write!(f, " (GF)")?;
            }
        }
        Ok(())
    }
}

lazy_static! {
    static ref MCS_TO_RATE: HashMap<(u8, Bandwidth, GuardInterval), f64> = {
        let mut m = HashMap::new();

        m.insert((0, Bandwidth::_20, GuardInterval::Long), 6.5);
        m.insert((0, Bandwidth::_20, GuardInterval::Short), 7.2);
        m.insert((0, Bandwidth::_40, GuardInterval::Long), 13.5);
        m.insert((0, Bandwidth::_40, GuardInterval::Short), 15.0);

        m.insert((1, Bandwidth::_20, GuardInterval::Long), 13.0);
        m.insert((1, Bandwidth::_20, GuardInterval::Short), 14.4);
        m.insert((1, Bandwidth::_40, GuardInterval::Long), 27.0);
        m.insert((1, Bandwidth::_40, GuardInterval::Short), 30.0);

        m.insert((2, Bandwidth::_20, GuardInterval::Long), 19.5);
        m.insert((2, Bandwidth::_20, GuardInterval::Short), 21.7);
        m.insert((2, Bandwidth::_40, GuardInterval::Long), 40.5);
        m.insert((2, Bandwidth::_40, GuardInterval::Short), 45.0);

        m.insert((3, Bandwidth::_20, GuardInterval::Long), 26.0);
        m.insert((3, Bandwidth::_20, GuardInterval::Short), 28.9);
        m.insert((3, Bandwidth::_40, GuardInterval::Long), 54.0);
        m.insert((3, Bandwidth::_40, GuardInterval::Short), 60.0);

        m.insert((4, Bandwidth::_20, GuardInterval::Long), 39.0);
        m.insert((4, Bandwidth::_20, GuardInterval::Short), 43.3);
        m.insert((4, Bandwidth::_40, GuardInterval::Long), 81.0);
        m.insert((4, Bandwidth::_40, GuardInterval::Short), 90.0);

        m.insert((5, Bandwidth::_20, GuardInterval::Long), 52.0);
        m.insert((5, Bandwidth::_20, GuardInterval::Short), 57.8);
        m.insert((5, Bandwidth::_40, GuardInterval::Long), 108.0);
        m.insert((5, Bandwidth::_40, GuardInterval::Short), 120.0);

        m.insert((6, Bandwidth::_20, GuardInterval::Long), 58.5);
        m.insert((6, Bandwidth::_20, GuardInterval::Short), 65.0);
        m.insert((6, Bandwidth::_40, GuardInterval::Long), 121.5);
        m.insert((6, Bandwidth::_40, GuardInterval::Short), 135.0);

        m.insert((7, Bandwidth::_20, GuardInterval::Long), 65.0);
        m.insert((7, Bandwidth::_20, GuardInterval::Short), 72.2);
        m.insert((7, Bandwidth::_40, GuardInterval::Long), 135.0);
        m.insert((7, Bandwidth::_40, GuardInterval::Short), 150.0);

        m.insert((8, Bandwidth::_20, GuardInterval::Long), 13.0);
        m.insert((8, Bandwidth::_20, GuardInterval::Short), 14.4);
        m.insert((8, Bandwidth::_40, GuardInterval::Long), 27.0);
        m.insert((8, Bandwidth::_40, GuardInterval::Short), 30.0);

        m.insert((9, Bandwidth::_20, GuardInterval::Long), 26.0);
        m.insert((9, Bandwidth::_20, GuardInterval::Short), 28.9);
        m.insert((9, Bandwidth::_40, GuardInterval::Long), 54.0);
        m.insert((9, Bandwidth::_40, GuardInterval::Short), 60.0);

        m.insert((10, Bandwidth::_20, GuardInterval::Long), 39.0);
        m.insert((10, Bandwidth::_20, GuardInterval::Short), 43.3);
        m.insert((10, Bandwidth::_40, GuardInterval::Long), 81.0);
        m.insert((10, Bandwidth::_40, GuardInterval::Short), 90.0);

        m.insert((11, Bandwidth::_20, GuardInterval::Long), 52.0);
        m.insert((11, Bandwidth::_20, GuardInterval::Short), 57.8);
        m.insert((11, Bandwidth::_40, GuardInterval::Long), 108.0);
        m.insert((11, Bandwidth::_40, GuardInterval::Short), 120.0);

        m.insert((12, Bandwidth::_20, GuardInterval::Long), 78.0);
        m.insert((12, Bandwidth::_20, GuardInterval::Short), 86.7);
        m.insert((12, Bandwidth::_40, GuardInterval::Long), 162.0);
        m.insert((12, Bandwidth::_40, GuardInterval::Short), 180.0);

        m.insert((13, Bandwidth::_20, GuardInterval::Long), 104.0);
        m.insert((13, Bandwidth::_20, GuardInterval::Short), 115.6);
        m.insert((13, Bandwidth::_40, GuardInterval::Long), 216.0);
        m.insert((13, Bandwidth::_40, GuardInterval::Short), 240.0);

        m.insert((14, Bandwidth::_20, GuardInterval::Long), 117.0);
        m.insert((14, Bandwidth::_20, GuardInterval::Short), 130.3);
        m.insert((14, Bandwidth::_40, GuardInterval::Long), 243.0);
        m.insert((14, Bandwidth::_40, GuardInterval::Short), 270.0);

        m.insert((15, Bandwidth::_20, GuardInterval::Long), 130.0);
        m.insert((15, Bandwidth::_20, GuardInterval::Short), 144.4);
        m.insert((15, Bandwidth::_40, GuardInterval::Long), 270.0);
        m.insert((15, Bandwidth::_40, GuardInterval::Short), 300.0);

        m
    };
}

impl MCS {
    fn into_known_flags_mcs(&self) -> (u8, u8, u8) {
        let mut present = McsPresent::empty();
        let mut flags = 0;
        let mut mcs = 0;
        if let Some(bandwidth) = self.bandwidth {
            present |= McsPresent::BANDWIDTH;
            // may not be correct...
            if bandwidth == Bandwidth::_40 {
                flags |= 0x1;
            }
        }
        if let Some(index) = self.index {
            present |= McsPresent::MCS_INDEX;
            mcs = index;
        }
        if let Some(gi) = self.guard_interval {
            present |= McsPresent::GUARD_INTERVAL;
            if gi == GuardInterval::Short {
                flags |= 0x4;
            }
        }
        if let Some(htf) = self.ht_format {
            present |= McsPresent::FORMAT;
            if htf == HtFormat::Greenfield {
                flags |= 0x8;
            }
        }
        (present.bits(), flags, mcs)
    }

    fn new(mcs_index: u8, short_gi: bool, gf: bool, bandwidth: Bandwidth) -> MCS {
        let ht_format = if gf {
            HtFormat::Greenfield
        } else {
            HtFormat::Mixed
        };
        let guard_interval = if short_gi {
            GuardInterval::Short
        } else {
            GuardInterval::Long
        };
        MCS {
            bandwidth: Some(bandwidth),
            index: Some(mcs_index),
            ht_format: Some(ht_format),
            guard_interval: Some(guard_interval),
        }
    }

    fn from_radiotap(known: u8, flags: u8, mcs: u8) -> MCS {
        let present = McsPresent::from_bits_truncate(known);
        let mut index = None;
        let mut bandwidth = None;
        let mut guard_interval = None;
        let mut ht_format = None;
        if present.intersects(McsPresent::BANDWIDTH) {
            match flags & 0x3 {
                0 | 2 | 3 => bandwidth = Some(Bandwidth::_20),
                1 => bandwidth = Some(Bandwidth::_40),
                _ => unreachable!(),
            }
        }
        if present.intersects(McsPresent::MCS_INDEX) {
            index = Some(mcs);
        }
        if present.intersects(McsPresent::GUARD_INTERVAL) {
            guard_interval = Some(if flags & 0x4 != 0 {
                GuardInterval::Short
            } else {
                GuardInterval::Long
            });
        }
        if present.intersects(McsPresent::FORMAT) {
            ht_format = Some(if flags & 0x8 != 0 {
                HtFormat::Greenfield
            } else {
                HtFormat::Mixed
            });
        }
        MCS {
            bandwidth,
            index,
            guard_interval,
            ht_format,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Mhz(u16);

impl Mhz {
    fn new(mhz: u16) -> Mhz {
        Mhz(mhz)
    }
}

impl Display for Mhz {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{} Mhz", self.0)
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Mbps(f64);

impl Mbps {
    pub fn new(mbps: f64) -> Mbps {
        Mbps(mbps)
    }
}

impl Display for Mbps {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{:.1} Mbps", self.0)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Dbm(i8);

impl Dbm {
    fn new(dbm: i8) -> Self {
        Dbm(dbm)
    }
}

impl Display for Dbm {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}dBm", self.0)
    }
}

impl Fields {
    fn info(&self) -> (u64, u64) {
        match *self {
            Fields::TSFT => (8, 8),
            Fields::FLAGS => (1, 1),
            Fields::RATE => (1, 1),
            Fields::CHANNEL => (2, 4),
            Fields::ANTENNA_SIGNAL => (1, 1),
            Fields::ANTENNA => (1, 1),
            Fields::RX_FLAGS => (2, 2),
            Fields::TX_FLAGS => (2, 2),
            Fields::DATA_RETRIES => (1, 1),
            Fields::MCS => (1, 3),
            Fields::AMPDU => (4, 8),
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Standard {
    A,
    B,
    G,
    N,
    AC,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Modulation {
    Cck,
    DynamicCckOfdm,
    Ofdm,
}

impl Display for Modulation {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            Modulation::Cck => write!(f, "CCK"),
            Modulation::DynamicCckOfdm => write!(f, "Dynamic CCK/OFDM"),
            Modulation::Ofdm => write!(f, "OFDM"),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Spectrum {
    TwoG,
    FiveG,
}

impl Display for Spectrum {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            Spectrum::TwoG => write!(f, "2.4 Ghz"),
            Spectrum::FiveG => write!(f, "5 Ghz"),
        }
    }
}

#[derive(Debug)]
pub enum Error {
    InvalidVersion,
    InvalidPresent,
    InvalidLength,
    IOError(io::Error),
}

impl From<io::Error> for Error {
    fn from(e: io::Error) -> Error {
        Error::IOError(e)
    }
}

type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub struct Radiotap {
    length: usize,
    tsft: Option<u64>,
    flags: Option<Flags>,
    rate: Option<u8>,
    channel: Option<(Mhz, ChannelFlags)>,
    antenna: Option<u8>,
    signal: Option<Dbm>,
    rxflags: Option<RxFlags>,
    txflags: Option<TxFlags>,
    retries: Option<u8>,
    mcs: Option<MCS>,
    ampdu: Option<u64>,
}

trait Align: Seek {
    fn align(&mut self, amount: u64) -> io::Result<()> {
        let p = self.seek(SeekFrom::Current(0))?;
        let new_p = align_up_to(p, amount);
        if p != new_p {
            self.seek(SeekFrom::Start(new_p))?;
        }
        Ok(())
    }
}

impl Align for Seek {}
impl<T> Align for T
where
    T: Seek,
{
}

fn align_up_to(x: u64, a: u64) -> u64 {
    if x % a != 0 {
        x + a - (x % a)
    } else {
        x
    }
}

impl Display for Radiotap {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "Std: {:?}", self.standard().unwrap())?;
        Self::fmt_field(f, "Signal", self.signal_strength())?;
        Self::fmt_field(f, "Antenna", self.antenna())?;
        Self::fmt_field(f, "Bandwidth", self.bandwidth())?;
        Self::fmt_field(f, "Frequency", self.frequency())?;
        Self::fmt_field(f, "FCS", Some(if self.fcs_fail() { "NG" } else { "OK" }))?;
        Self::fmt_field(f, "Rate", self.rate())?;
        if let Some(mcs) = self.mcs {
            Self::fmt_field(f, "MCS", Some(mcs))?;
        }
        Self::fmt_field(f, "Modulation", self.modulation())?;
        Ok(())
    }
}

impl Radiotap {
    fn fmt_field<T: Display>(f: &mut Formatter, name: &str, value: Option<T>) -> fmt::Result {
        if let Some(value) = value {
            write!(f, ", {}: {}", name, value)
        } else {
            write!(f, ", {}: ?", name)
        }
    }

    fn write_field<T: Write + Seek>(&self, cur: &mut T, field: Fields) -> io::Result<Fields> {
        let (align, _) = field.info();
        let mut has_field = false;
        match field {
            Fields::TSFT => if let Some(tsft) = self.tsft {
                cur.align(align)?;
                cur.write_u64::<LE>(tsft)?;
                has_field = true;
            },
            Fields::FLAGS => if let Some(flags) = self.flags {
                cur.align(align)?;
                cur.write_u8(flags.bits() as u8)?;
                has_field = true;
            },
            Fields::RATE => if let Some(rate) = self.rate {
                cur.align(align)?;
                cur.write_u8(rate)?;
                has_field = true;
            },
            Fields::CHANNEL => if let Some(channel) = self.channel {
                cur.align(align)?;
                cur.write_u16::<LE>((channel.0).0 as u16)?;
                cur.write_u16::<LE>(channel.1.bits() as u16)?;
                has_field = true;
            },
            Fields::ANTENNA_SIGNAL => if let Some(signal) = self.signal {
                cur.align(align)?;
                cur.write_i8(signal.0 as i8)?;
                has_field = true;
            },
            Fields::ANTENNA => if let Some(antenna) = self.antenna {
                cur.align(align)?;
                cur.write_u8(antenna)?;
                has_field = true;
            },
            Fields::RX_FLAGS => if let Some(rxflags) = self.rxflags {
                cur.align(align)?;
                cur.write_u16::<LE>(rxflags.bits())?;
                has_field = true;
            },
            Fields::TX_FLAGS => if let Some(txflags) = self.txflags {
                cur.align(align)?;
                cur.write_u16::<LE>(txflags.bits())?;
                has_field = true;
            },
            Fields::DATA_RETRIES => if let Some(retries) = self.retries {
                cur.align(align)?;
                cur.write_u8(retries)?;
                has_field = true;
            },
            Fields::MCS => if let Some(mcs) = self.mcs {
                let (known, flags, index) = mcs.into_known_flags_mcs();
                cur.align(align)?;
                cur.write_u8(known)?;
                cur.write_u8(flags)?;
                cur.write_u8(index)?;
                has_field = true;
            },
            Fields::AMPDU => if let Some(ampdu) = self.ampdu {
                cur.align(align)?;
                cur.write_u64::<LE>(ampdu)?;
                has_field = true;
            },
            _ => unreachable!(),
        }
        if has_field {
            Ok(field)
        } else {
            Ok(Fields::empty())
        }
    }

    pub fn write_into<T: Write + Seek>(&self, cur: &mut T) -> io::Result<usize> {
        cur.write_u8(0)?;
        cur.write_u8(0)?;
        cur.write_u16::<LE>(0)?; // skip length for now...
        cur.write_u32::<LE>(0)?; // skip fields for now...

        let mut fields = Fields::empty();
        fields |= self.write_field(cur, Fields::TSFT)?;
        fields |= self.write_field(cur, Fields::FLAGS)?;
        fields |= self.write_field(cur, Fields::RATE)?;
        fields |= self.write_field(cur, Fields::CHANNEL)?;
        fields |= self.write_field(cur, Fields::ANTENNA_SIGNAL)?;
        fields |= self.write_field(cur, Fields::ANTENNA)?;
        fields |= self.write_field(cur, Fields::RX_FLAGS)?;
        fields |= self.write_field(cur, Fields::TX_FLAGS)?;
        fields |= self.write_field(cur, Fields::DATA_RETRIES)?;
        fields |= self.write_field(cur, Fields::MCS)?;
        fields |= self.write_field(cur, Fields::AMPDU)?;

        let length = cur.seek(SeekFrom::Current(0))?;
        cur.seek(SeekFrom::Start(2))?;
        cur.write_u16::<LE>(length as u16)?;
        cur.write_u32::<LE>(fields.bits())?;
        cur.seek(SeekFrom::Start(length))?;

        Ok(length as usize)
    }

    pub fn new_ht(mcs_index: u8) -> Radiotap {
        Radiotap {
            mcs: Some(MCS::new(mcs_index, false, true, Bandwidth::_20)),
            txflags: Some(TxFlags::NO_ACK),
            length: 0,
            flags: Some(Flags::empty()),
            retries: Some(0),
            tsft: None,
            rate: None,
            channel: None,
            signal: None,
            antenna: None,
            rxflags: None,
            ampdu: None,
        }
    }

    pub fn new_legacy(rate: Mbps) -> Radiotap {
        let rate = (rate.0 * 2.0) as u8;
        Radiotap {
            rate: Some(rate),
            txflags: Some(TxFlags::NO_ACK),
            length: 0,
            flags: Some(Flags::empty()),
            retries: Some(0),
            tsft: None,
            channel: None,
            signal: None,
            antenna: None,
            rxflags: None,
            mcs: None,
            ampdu: None,
        }
    }

    pub fn parse(header: &[u8]) -> Result<Radiotap> {
        let mut cur = Cursor::new(header);

        if cur.read_u8()? != 0 {
            return Err(Error::InvalidVersion);
        }

        cur.read_u8()?;

        let length = cur.read_u16::<LE>()? as usize;
        if header.len() < length {
            return Err(Error::InvalidLength);
        }

        let present = Fields::from_bits_truncate(cur.read_u32::<LE>()?);

        if present.intersects(Fields::MORE_PRESENT) {
            cur.read_u32::<LE>()?;
        }

        let mut tsft = None;
        let mut flags = None;
        let mut rate = None;
        let mut channel = None;
        let mut signal = None;
        let mut antenna = None;
        let mut rxflags = None;
        let mut txflags = None;
        let mut retries = None;
        let mut mcs = None;
        let mut ampdu = None;

        for i in 0..31 {
            let field = Fields::from_bits_truncate(1 << i);
            if present.intersects(field) {
                let (align, size) = field.info();
                cur.seek(SeekFrom::Current(0))?;
                cur.align(align)?;
                match field {
                    Fields::TSFT => {
                        tsft = Some(cur.read_u64::<LE>()?);
                    }
                    Fields::FLAGS => {
                        flags = Some(Flags::from_bits_truncate(cur.read_u8()?));
                    }
                    Fields::RATE => {
                        rate = Some(cur.read_u8()?);
                    }
                    Fields::CHANNEL => {
                        channel = Some((
                            Mhz::new(cur.read_u16::<LE>()?),
                            ChannelFlags::from_bits_truncate(cur.read_u16::<LE>()?),
                        ));
                    }
                    Fields::ANTENNA_SIGNAL => {
                        signal = Some(Dbm::new(cur.read_i8()?));
                    }
                    Fields::ANTENNA => {
                        antenna = Some(cur.read_u8()?);
                    }
                    Fields::RX_FLAGS => {
                        rxflags = Some(RxFlags::from_bits_truncate(cur.read_u16::<LE>()?));
                    }
                    Fields::TX_FLAGS => {
                        txflags = Some(TxFlags::from_bits_truncate(cur.read_u16::<LE>()?));
                    }
                    Fields::DATA_RETRIES => {
                        retries = Some(cur.read_u8()?);
                    }
                    Fields::MCS => {
                        mcs = Some(MCS::from_radiotap(
                            cur.read_u8()?,
                            cur.read_u8()?,
                            cur.read_u8()?,
                        ));
                    }
                    Fields::AMPDU => {
                        ampdu = Some(cur.read_u64::<LE>()?);
                    }
                    _ => {
                        cur.seek(SeekFrom::Current(size as i64))?;
                    }
                }
            }
        }

        Ok(Radiotap {
            length,
            tsft,
            flags,
            rate,
            mcs,
            signal,
            channel,
            antenna,
            rxflags,
            txflags,
            retries,
            ampdu,
        })
    }

    pub fn len(&self) -> usize {
        self.length
    }

    pub fn fcs_at_end(&self) -> bool {
        if let Some(flags) = self.flags {
            flags.intersects(Flags::FCS_AT_END)
        } else {
            false
        }
    }

    pub fn fcs_fail(&self) -> bool {
        if let Some(flags) = self.flags {
            flags.intersects(Flags::FCS_FAIL)
        } else {
            false
        }
    }

    pub fn standard(&self) -> Option<Standard> {
        // TODO: AC support
        if self.mcs.is_some() {
            Some(Standard::N)
        } else if let Some((_, cf)) = self.channel {
            if cf.intersects(ChannelFlags::SPECTRUM_2GHZ) {
                if cf.intersects(ChannelFlags::CCK) {
                    Some(Standard::B)
                } else {
                    Some(Standard::G)
                }
            } else if cf.intersects(ChannelFlags::SPECTRUM_5GHZ) {
                Some(Standard::A)
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn spectrum(&self) -> Option<Spectrum> {
        if let Some((_, cf)) = self.channel {
            if cf.intersects(ChannelFlags::SPECTRUM_2GHZ) {
                Some(Spectrum::TwoG)
            } else if cf.intersects(ChannelFlags::SPECTRUM_5GHZ) {
                Some(Spectrum::FiveG)
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn modulation(&self) -> Option<Modulation> {
        if let Some((_, cf)) = self.channel {
            if cf.intersects(ChannelFlags::CCK) {
                Some(Modulation::Cck)
            } else if cf.intersects(ChannelFlags::CCK_OFDM) {
                Some(Modulation::DynamicCckOfdm)
            } else if cf.intersects(ChannelFlags::OFDM) {
                Some(Modulation::Ofdm)
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn bandwidth(&self) -> Option<Bandwidth> {
        if let Some(mcs) = self.mcs {
            mcs.bandwidth
        } else {
            // TODO: this is wrong...
            Some(Bandwidth::_20)
        }
    }

    pub fn is_ht(&self) -> bool {
        // TODO: not sure if correct...
        self.mcs.is_some()
    }

    pub fn mcs_index(&self) -> Option<u8> {
        if let Some(mcs) = self.mcs {
            mcs.index
        } else {
            None
        }
    }

    pub fn short_guard_interval(&self) -> Option<bool> {
        if let Some(mcs) = self.mcs {
            if let Some(gi) = mcs.guard_interval {
                Some(gi == GuardInterval::Short)
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn greenfield(&self) -> Option<bool> {
        if let Some(mcs) = self.mcs {
            if let Some(fmt) = mcs.ht_format {
                Some(fmt == HtFormat::Greenfield)
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn signal_strength(&self) -> Option<Dbm> {
        self.signal
    }

    pub fn frequency(&self) -> Option<Mhz> {
        if let Some((f, _)) = self.channel {
            Some(f)
        } else {
            None
        }
    }

    pub fn antenna(&self) -> Option<u8> {
        self.antenna
    }

    pub fn short_preamble(&self) -> Option<bool> {
        if let Some(flags) = self.flags {
            Some(flags.intersects(Flags::SHORT_PREAMBLE))
        } else {
            None
        }
    }

    /// Rate in Mbps
    pub fn rate(&self) -> Option<Mbps> {
        // TODO: units
        if let Some(mcs) = self.mcs {
            let index = mcs.index?;
            let bandwidth = mcs.bandwidth?;
            let gi = mcs.guard_interval?;
            if let Some(rate) = MCS_TO_RATE.get(&(index, bandwidth, gi)) {
                Some(Mbps::new(*rate))
            } else {
                None
            }
        } else if let Some(rate) = self.rate {
            Some(Mbps::new((rate as f64) * 0.5))
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    extern crate hex;

    use super::*;

    #[test]
    fn parse_11n_2g_40mhz() {
        let bytes = &hex::decode(
            "000020002a48180010006c098004c40300000f01070000009b4e040000000000",
        ).unwrap()[..];

        let radiotap = Radiotap::parse(bytes).unwrap();

        assert_eq!(Bandwidth::_40, radiotap.bandwidth().unwrap());
        assert_eq!(true, radiotap.is_ht());
        assert_eq!(7, radiotap.mcs_index().unwrap());
        assert_eq!(Mbps::new(135.0), radiotap.rate().unwrap());
        assert_eq!(false, radiotap.greenfield().unwrap());
        assert_eq!(Dbm::new(-60), radiotap.signal_strength().unwrap());
        assert_eq!(Mhz::new(2412), radiotap.frequency().unwrap());
        assert_eq!(3, radiotap.antenna().unwrap());
        assert_eq!(false, radiotap.short_preamble().unwrap());
        assert!(radiotap.fcs_at_end());

        assert_serialized_match(bytes, &radiotap);
    }

    #[test]
    fn parse_11b_2g_20mhz_short_preamble() {
        let bytes = &hex::decode("000012002e480000120b6c09a000ab030000").unwrap()[..];
        let radiotap = Radiotap::parse(bytes).unwrap();

        assert_eq!(Bandwidth::_20, radiotap.bandwidth().unwrap());
        assert_eq!(false, radiotap.is_ht());
        assert_eq!(Dbm::new(-85), radiotap.signal_strength().unwrap());
        assert_eq!(Mhz::new(2412), radiotap.frequency().unwrap());
        assert_eq!(3, radiotap.antenna().unwrap());
        assert_eq!(true, radiotap.short_preamble().unwrap());
        assert_eq!(Mbps::new(5.5), radiotap.rate().unwrap());
        assert!(radiotap.fcs_at_end());

        assert_serialized_match(bytes, &radiotap);
    }

    #[test]
    fn parse_11g_2g() {
        let bytes = &hex::decode("000012002e48000010486c09c000c5030000").unwrap()[..];
        let radiotap = Radiotap::parse(bytes).unwrap();

        assert_eq!(Bandwidth::_20, radiotap.bandwidth().unwrap());
        assert_eq!(false, radiotap.is_ht());
        assert_eq!(Dbm::new(-59), radiotap.signal_strength().unwrap());
        assert_eq!(Mhz::new(2412), radiotap.frequency().unwrap());
        assert_eq!(3, radiotap.antenna().unwrap());
        assert_eq!(false, radiotap.short_preamble().unwrap());
        assert_eq!(Mbps::new(36.0), radiotap.rate().unwrap());
        assert!(radiotap.fcs_at_end());

        assert_serialized_match(bytes, &radiotap);
    }

    #[test]
    fn parse_11n_ath9k() {
        let bytes = &hex::decode(
            "000027002b4008a02008000000000000fec224150000000050009e098004ad000000070407ad00",
        ).unwrap()[..];

        let radiotap = Radiotap::parse(bytes).unwrap();

        assert_eq!(Bandwidth::_20, radiotap.bandwidth().unwrap());
        assert_eq!(true, radiotap.is_ht());
        assert_eq!(Dbm::new(-83), radiotap.signal_strength().unwrap());
        assert_eq!(Mhz::new(2462), radiotap.frequency().unwrap());
        assert_eq!(false, radiotap.short_preamble().unwrap());
        assert_eq!(Mbps::new(72.2), radiotap.rate().unwrap());
        assert!(radiotap.fcs_at_end());
        assert!(radiotap.fcs_fail());

        // assert_serialized_match(bytes, &radiotap);
    }

    #[test]
    fn new_legacy_11mbps() {
        let bytes = &hex::decode("00000d00068002000016080000").unwrap()[..];
        let radiotap = Radiotap::new_legacy(Mbps::new(11.0));
        assert_serialized_match(bytes, &radiotap);
    }

    #[test]
    fn new_legacy_5_5mbps() {
        let bytes = &hex::decode("00000d0006800200000b080000").unwrap()[..];
        let radiotap = Radiotap::new_legacy(Mbps::new(5.5));
        assert_serialized_match(bytes, &radiotap);
    }

    #[test]
    fn new_ht_mcs0() {
        let bytes = &hex::decode("0000100002800a0000000800000f0800").unwrap()[..];
        let radiotap = Radiotap::new_ht(0);
        assert_serialized_match(bytes, &radiotap);
    }

    fn assert_serialized_match(bytes: &[u8], radiotap: &Radiotap) {
        let (p, v) = {
            let mut c = Cursor::new(vec![]);
            radiotap.write_into(&mut c).unwrap();
            let p = c.position() as usize;
            let v = c.into_inner();
            (p, v)
        };
        println!("hex: {}", hex::encode(&v[..]));
        assert_eq!(bytes.len(), p);
        assert_eq!(bytes, &v[..]);
    }
}
