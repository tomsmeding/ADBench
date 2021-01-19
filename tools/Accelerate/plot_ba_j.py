#!/usr/bin/env python3
import sys

with open(sys.argv[1]) as f:  # Takes Jacobian file name as input
    def read_vals(f, cvt): return [cvt(x) for x in f.readline().split(" ")]
    h, w = read_vals(f, int)
    f.readline()
    rowindices = read_vals(f, int)
    f.readline()
    columnvals = read_vals(f, int)
    valuearray = read_vals(f, float)

full = [[0] * w for _ in range(h)]
for i in range(len(rowindices) - 1):
    starti = rowindices[i]
    endi = rowindices[i + 1]
    for j, val in zip(columnvals[starti:endi], valuearray[starti:endi]):
        full[i][j] = val

def toimgspace(x, y, height=None):
    y2 = 3 + 4 * y
    return (3 + 4 * x, height - y2 if height is not None else y2)

width, height = toimgspace(len(full[0]), len(full))
print("%!PS")
print("%%BoundingBox: 0 0 {} {}".format(width, height))
print("/c {newpath 2 0 360 arc fill} bind def")
print("/d {newpath 0.3 0 360 arc fill} bind def")
print("0.95 0.95 0.95 setrgbcolor")
print("newpath 0 1 moveto {0} 1 lineto {0} {1} lineto 0 {1} lineto closepath fill".format(width - 1, height))
print("0 0 0 setrgbcolor")
for iy, row in enumerate(full):
    for ix, x in enumerate(row):
        if x != 0:
            print("{} {} c".format(*toimgspace(ix, iy, height)))
        else:
            print("{} {} d".format(*toimgspace(ix, iy, height)))
