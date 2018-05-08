#!/bin/sh
cargo build --examples
sudo setcap cap_net_raw,cap_net_admin=eip target/debug/examples/monitor
exec ./target/debug/examples/monitor
