#!/bin/sh
exec erl \
    -pa ebin deps/*/ebin \
    -boot start_sasl \
    -sname recon_monitor_dev \
    -s recon_monitor \
    -s reloader
