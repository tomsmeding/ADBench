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
        binarydirRecomp="$binarydirPrefix/AccelerateRecomp"
    fi
fi

echo "Source directory: '$sourcedir'"
echo "Binary directory: CPU: '$binarydir'"
[[ -n $binarydir1 ]] && echo "                  1-thread: '$binarydir1'"
[[ -n $binarydirGPU ]] && echo "                  GPU: '$binarydirGPU'"
[[ -n $binarydirRecomp ]] && echo "                  Recomp: '$binarydirRecomp'"

cd "$sourcedir"

tempdir="$(mktemp -d)"
trap "rm -rf '$tempdir'" EXIT

stack build --copy-bins --local-bin-path="$tempdir"

mv "$tempdir"/adbench-accelerate-gmm "$binarydir"/adbench-accelerate-GMM-FULL
mv "$tempdir"/adbench-accelerate-ba "$binarydir"/adbench-accelerate-BA

function create_normal_launcher() {
    local id="$1"
    printf >"$binarydir/Tools-Accelerate-$id.exe" '#!/usr/bin/env bash\nenv ACCELERATE_AD_SMALLFUNSIZE=0 "$(dirname "$0")"/../Accelerate/adbench-accelerate-'"$id"' "$@"'
    chmod +x "$binarydir/Tools-Accelerate-$id.exe"
}
create_normal_launcher "GMM-FULL"
create_normal_launcher "BA"

if [[ -n $binarydir1 ]]; then
    mkdir -p "$binarydir1"
    function create_1thread_launcher() {
        local id="$1"
        printf >"$binarydir1/Tools-Accelerate1-$id.exe" '#!/usr/bin/env bash\nenv ACCELERATE_LLVM_NATIVE_THREADS=1 "$(dirname "$0")"/../Accelerate/adbench-accelerate-'"$id"' "$@"'
        chmod +x "$binarydir1/Tools-Accelerate1-$id.exe"
    }
    create_1thread_launcher "GMM-FULL"
    create_1thread_launcher "BA"
fi
if [[ -n $binarydirGPU ]]; then
    mkdir -p "$binarydirGPU"
    function create_gpu_launcher() {
        local id="$1"
        printf >"$binarydirGPU/Tools-AccelerateGPU-$id.exe" '#!/usr/bin/env bash\n"$(dirname "$0")"/../Accelerate/adbench-accelerate-'"$id"' -gpu "$@"'
        chmod +x "$binarydirGPU/Tools-AccelerateGPU-$id.exe"
    }
    create_gpu_launcher "GMM-FULL"
    create_gpu_launcher "BA"
fi
if [[ -n $binarydirRecomp ]]; then
    mkdir -p "$binarydirRecomp"
    function create_recomp_launcher() {
        local id="$1"
        printf >"$binarydirRecomp/Tools-AccelerateRecomp-$id.exe" '#!/usr/bin/env bash\nenv ACCELERATE_AD_SMALLFUNSIZE=9999 "$(dirname "$0")"/../Accelerate/adbench-accelerate-'"$id"' "$@"'
        chmod +x "$binarydirRecomp/Tools-AccelerateRecomp-$id.exe"
    }
    create_recomp_launcher "GMM-FULL"
    create_recomp_launcher "BA"
fi
