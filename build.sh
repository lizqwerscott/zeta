#!/bin/bash

cargo build --release

ln -sv ./target/release/libzeta_dyn.so ./zeta-dyn.so
