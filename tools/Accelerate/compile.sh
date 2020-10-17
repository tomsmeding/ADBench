#!/usr/bin/env bash
set -euo pipefail

sourcedir="$(dirname "$0")"
binarydir="$sourcedir"
binarydir1=""
binarydirGPU=""

[[ $# -ge 1 && ( $1 = "-h" || $1 = "--help" ) ]] && {
    echo "Usage: $0 [sourcedir] [binarydir]"
    echo "If unspecified, uses current directory."
    exit
}

if [[ $# -ge 1 ]]; then
    sourcedir="$1"
    binarydir="$1"
fi

if [[ $# -ge 2 ]]; then
    binarydir="$2"
    binarydirPrefix="$(sed 's,Accelerate/\?$,,' <<<"$binarydir")"
    if [[ $binarydirPrefix != $binarydir ]]; then
        binarydir1="$binarydirPrefix/Accelerate1"
        binarydirGPU="$binarydirPrefix/AccelerateGPU"
    fi
fi

echo "Source directory: '$sourcedir'"
echo "Binary directory: CPU: '$binarydir'"
[[ -n $binarydir1 ]] && echo "                  1-thread: '$binarydir1'"
[[ -n $binarydirGPU ]] && echo "                  GPU: '$binarydirGPU'"

cd "$sourcedir"

tempdir="$(mktemp -d)"
trap "rm -rf '$tempdir'" EXIT

stack build --copy-bins --local-bin-path="$tempdir"
mv "$tempdir"/adbench-accelerate-gmm "$binarydir"/Tools-Accelerate-GMM-FULL.exe

if [[ -n $binarydir1 ]]; then
    mkdir -p "$binarydir1"
    printf >"$binarydir1/Tools-Accelerate1-GMM-FULL.exe" '#!/usr/bin/env bash\nenv ACCELERATE_LLVM_NATIVE_THREADS=1 "$(dirname "$0")"/../Accelerate/Tools-Accelerate-GMM-FULL.exe "$@"'
    chmod +x "$binarydir1/Tools-Accelerate1-GMM-FULL.exe"
fi
if [[ -n $binarydirGPU ]]; then
    mkdir -p "$binarydirGPU"
    printf >"$binarydirGPU/Tools-AccelerateGPU-GMM-FULL.exe" '#!/usr/bin/env bash\n"$(dirname "$0")"/../Accelerate/Tools-Accelerate-GMM-FULL.exe -gpu "$@"'
    chmod +x "$binarydirGPU/Tools-AccelerateGPU-GMM-FULL.exe"
fi
