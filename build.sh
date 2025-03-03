#!/bin/bash

cargo build --release &&

if [[ "$(uname -s)" == "Darwin" ]]; then
    rm ./zeta-dyn.dylib
    ln -sv ./target/release/libzeta_dyn.dylib ./zeta-dyn.dylib
else
    rm ./zeta-dyn.so
    ln -sv ./target/release/libzeta_dyn.so ./zeta-dyn.so
fi
