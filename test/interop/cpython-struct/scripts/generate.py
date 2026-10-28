import csv
import json
import pathlib
import platform
import struct
import sys

if sys.version_info[:3] != (3, 15, 0) or platform.python_implementation() != "CPython":
    raise SystemExit("framing vectors require CPython 3.15.0")
output = pathlib.Path(sys.argv[1])
output.mkdir(parents=True, exist_ok=True)
rows = []
for width, format_code in ((16, "!H"), (32, "!I")):
    lengths = [0, 1, 100, 1000, 65535]
    if width == 32:
        lengths.extend([65536, 1000000])
    for length in lengths:
        rows.append(("encode", width, length, struct.pack(format_code, length).hex()))
try:
    struct.pack("!H", 65536)
except struct.error:
    rows.append(("refuse_encode", 16, 65536, ""))
else:
    raise SystemExit("struct unexpectedly accepted an overflowing uint16")
for length in (2147483647, 2147483648, 4294967295):
    rows.append(("refuse_declared", 32, length, struct.pack("!I", length).hex()))
with (output / "vectors.csv").open("w", newline="", encoding="ascii") as handle:
    writer = csv.writer(handle)
    writer.writerow(("operation", "length_field", "length", "prefix_hex"))
    writer.writerows(rows)
with (output / "oracle.json").open("w", encoding="ascii") as handle:
    json.dump({"implementation": "CPython", "version": "3.15.0", "module": "struct",
               "source": "https://docs.python.org/3.15/library/struct.html",
               "formats": ["!H", "!I"]}, handle, sort_keys=True, indent=2)
    handle.write("\n")
