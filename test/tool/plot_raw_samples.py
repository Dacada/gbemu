#!/usr/bin/env python3
# plot_raw_le_f32_headless.py
import sys
import numpy as np

import matplotlib
matplotlib.use("Agg")              # headless backend (no DISPLAY needed)
import matplotlib.pyplot as plt

if len(sys.argv) < 2:
    print(f"Usage: {sys.argv[0]} <input.raw> [output.png]")
    sys.exit(1)

inp = sys.argv[1]
out = sys.argv[2] if len(sys.argv) > 2 else "plot.png"

samples = np.fromfile(inp, dtype="<f4")  # little-endian float32

plt.figure()
plt.plot(samples)
plt.xlabel("Sample index")
plt.ylabel("Amplitude (float32)")
plt.title(inp)
plt.tight_layout()
plt.savefig(out, dpi=150)
print(f"Wrote {out} with {len(samples)} samples")
