#!/usr/bin/env python3
"""The synthetic series the merge cases run on, and what they are expected to
produce, drawn here so that both can be checked by eye.

    ./draw.py          writes merge_<t>.png and merge-<case>.expected.png here

A '#' is a lesion voxel (255) and a '.' is background (0). The series is three
frames of a co-registered 16x5 domain: two lesions, a bridge that merges them,
and the merged lesion persisting one frame further, shifted by a voxel so that
the propagation has to follow it by overlap rather than by position.

The expected images are computed by hand, not by any implementation of the
semantics, which is the point of them: a run of VoxLogicA 1 on the programs
the pipeline emits (see ../cases/merge-*.imgql) has to reproduce them, up to
the encoding of a boolean image.
"""

import struct
import zlib

FRAMES = {
    0: """
................
.###......###...
.###......###...
.###......###...
................
""",
    1: """
................
.############...
.############...
.############...
................
""",
    2: """
................
..############..
..############..
..############..
................
""",
}

# merge-footprint (option A): the footprint of the whole series is one
# component, so at frame 0 "region 1" is both lesions, no lesion lies outside
# it, and the merge is invisible: false everywhere.
#
# merge-initially (option B, first frame only): the labels of frame 0 tell the
# two lesions apart, and the component of frame 1 touches region 1 and a lesion
# outside it: the merge is the whole component of frame 1.
#
# merge-tracked (option B, propagated): what descends from both lesions of
# frame 0 by the time of frame 2 is the merged lesion there, shifted and all.
# Under A, and under B without propagation, the two regions never overlap and
# the result is empty.
EXPECTED = {
    "merge-footprint": """
................
................
................
................
................
""",
    "merge-initially": FRAMES[1],
    "merge-tracked": FRAMES[2],
}


def png(rows):
    """A greyscale 8-bit PNG of the rows of '.' and '#', no library needed."""
    rows = [r for r in rows.strip("\n").split("\n")]
    width, height = len(rows[0]), len(rows)
    assert all(len(r) == width for r in rows), "the rows differ in length"
    raw = b"".join(b"\0" + bytes(255 if c == "#" else 0 for c in r) for r in rows)

    def chunk(kind, data):
        body = kind + data
        return struct.pack(">I", len(data)) + body + struct.pack(">I", zlib.crc32(body) & 0xFFFFFFFF)

    return (
        b"\x89PNG\r\n\x1a\n"
        + chunk(b"IHDR", struct.pack(">IIBBBBB", width, height, 8, 0, 0, 0, 0))
        + chunk(b"IDAT", zlib.compress(raw, 9))
        + chunk(b"IEND", b"")
    )


if __name__ == "__main__":
    for t, rows in FRAMES.items():
        with open(f"merge_{t}.png", "wb") as f:
            f.write(png(rows))
    for case, rows in EXPECTED.items():
        with open(f"{case}.expected.png", "wb") as f:
            f.write(png(rows))
