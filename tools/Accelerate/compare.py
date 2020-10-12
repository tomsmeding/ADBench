#!/usr/bin/env python3
import sys

def load_file(fpath):
    with open(fpath) as f:
        return [float(x) for x in f.read().split()]

def checkeddiv(x, y):
    return float("nan") if y == 0 else x / y

def isnan(x): return x != x

def main():
    if len(sys.argv) != 3:
        print("Usage: {} <gmm_dX_KY_J_a.txt> <gmm_dX_KY_J_b.txt>".format(sys.argv[0]), file=sys.stderr)
        sys.exit(1)

    data1 = load_file(sys.argv[1])
    data2 = load_file(sys.argv[2])

    absdiffs = [x - y for x, y in zip(data1, data2)]
    reldiffs = [max(abs(checkeddiv(x - y, x)), abs(checkeddiv(x - y, y))) for x, y in zip(data1, data2)]
    reldiffs = [x for x in reldiffs if not isnan(x)]

    print("Max abs diff:  {}".format(max(absdiffs)))
    print("Mean abs diff: {}".format(sum(absdiffs) / len(absdiffs)))
    print("Max rel diff:  {}".format(max(reldiffs)))
    print("Mean rel diff: {}".format(sum(reldiffs) / len(reldiffs)))

if __name__ == "__main__":
    main()
