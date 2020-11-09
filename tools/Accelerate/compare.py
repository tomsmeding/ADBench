#!/usr/bin/env python3
import sys
from collections import namedtuple

# Relative difference computation formula:
#   abs(x - y) / max(1, abs(x), abs(y))
# This is the relative difference if |x| and |y| are at both at least 1, and the absolute difference otherwise.
Result = namedtuple("Result", ["absdiffs", "reldiffs"])
def absdiff(x, y): return abs(x - y)
def reldiff(x, y): return abs(x - y) / max(1, abs(x), abs(y))

class GMM:
    def load_file(fpath):
        with open(fpath) as f:
            return [float(x) for x in f.read().split()]

    def compare(fname1, fname2):
        data1 = GMM.load_file(fname1)
        data2 = GMM.load_file(fname2)

        if len(data1) != len(data2):
            print("Sizes of jacobians do not match!", file=sys.stderr)
            sys.exit(1)

        absdiffs = [absdiff(x, y) for x, y in zip(data1, data2)]
        reldiffs = [reldiff(x, y) for x, y in zip(data1, data2)]
        return Result(absdiffs, reldiffs)

class BA:
    Data = namedtuple("BA_Data", ["ys", "xs", "data"])

    def load_file(fpath):
        with open(fpath) as f:
            [height, width] = [int(x) for x in f.readline().split()]
            numys = int(f.readline())
            assert numys == height + 1
            ys = [int(x) for x in f.readline().split()]
            numvals = int(f.readline())
            xs = [int(x) for x in f.readline().split()]
            assert len(xs) == numvals
            data = [float(x) for x in f.readline().split()]
            assert len(data) == numvals
            return BA.Data(ys, xs, data)

    def compare(fname1, fname2):
        data1 = BA.load_file(fname1)
        data2 = BA.load_file(fname2)

        if len(data1.ys) != len(data2.ys) or len(data1.xs) != len(data2.xs):
            print("Sizes of jacobians do not match!", file=sys.stderr)
            sys.exit(1)

        absdiffs = []
        reldiffs = []
        def record_pair(x, y):
            absdiffs.append(absdiff(x, y))
            reldiffs.append(reldiff(x, y))

        for yi in range(len(data1.ys) - 1):
            i1, iend1 = data1.ys[yi], data1.ys[yi+1]
            i2, iend2 = data2.ys[yi], data2.ys[yi+1]
            # Perform a mergesort-like pairing of the non-zero entries of these rows
            while i1 < iend1 and i2 < iend2:
                if data1.xs[i1] == data2.xs[i2]:
                    record_pair(data1.data[i1], data2.data[i2])
                    i1, i2 = i1 + 1, i2 + 1
                elif data1.xs[i1] < data2.xs[i2]:
                    record_pair(data1.data[i1], 0)
                    i1 = i1 + 1
                else:
                    record_pair(0, data2.data[i2])
                    i2 = i2 + 1
            # At most one of these two while-loops will run
            while i1 < iend1:
                record_pair(data1.data[i1], 0)
                i1 = i1 + 1
            while i2 < iend2:
                record_pair(data2.data[i2], 0)
                i2 = i2 + 1

        return Result(absdiffs, reldiffs)

def main():
    if len(sys.argv) != 4:
        print("Usage: {} <GMM|BA> <gmm_dX_KY_J_a.txt> <gmm_dX_KY_J_b.txt>".format(sys.argv[0]), file=sys.stderr)
        sys.exit(1)

    kind = sys.argv[1]
    if kind == "GMM":
        result = GMM.compare(sys.argv[2], sys.argv[3])
    elif kind == "BA":
        result = BA.compare(sys.argv[2], sys.argv[3])
    else:
        print("Unknown problem name '{}'".format(kind), file=sys.stderr)
        sys.exit(1)

    print("Max abs diff:  {}".format(max(result.absdiffs)))
    print("Mean abs diff: {}".format(sum(result.absdiffs) / len(result.absdiffs)))
    print("Max rel diff:  {}".format(max(result.reldiffs)))
    print("Mean rel diff: {}".format(sum(result.reldiffs) / len(result.reldiffs)))

if __name__ == "__main__":
    main()
