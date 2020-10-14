#!/usr/bin/env bash
set -euo pipefail

sourcedir="$(dirname "$0")"
binarydir="$sourcedir"

[[ $# -ge 1 && ( $1 = "-h" || $1 = "--help" ) ]] && {
    echo "Usage: $0 [sourcedir] [binarydir]"
    echo "If unspecified, uses current directory."
    exit
}

[[ $# -ge 1 ]] && { sourcedir="$1"; binarydir="$1"; }
[[ $# -ge 2 ]] && binarydir="$2"

cd "$sourcedir"

tempdir="$(mktemp -d)"
trap "rm -rf '$tempdir'" EXIT

stack build --copy-bins --local-bin-path="$tempdir"
mv "$tempdir"/adbench-accelerate-gmm "$binarydir"/Tools-Accelerate-GMM-FULL.exe
