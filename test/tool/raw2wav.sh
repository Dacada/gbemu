#!/bin/sh
ffmpeg -f f32le -ar 1048576 -ac 1 -i $1 -af "aresample=48000:resampler=soxr:precision=28:cutoff=0.97" -c:a pcm_f32le -ar 48000 $2
