#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"

do_plot=0
do_html=1

function show_usage() {
    echo "Usage: $1 [--plot] [--html] [-h|--help]"
    echo "Creates a tarball of plots with short filenames, from the result of"
    echo "plot_graphs.py."
    echo "  --plot   Invokes plot_graphs.py --save before creating the tarball."
    echo "  --html   Adds index.html to the tarball, listing all plots."
}

for arg; do
    case "$arg" in
        --plot) do_plot=1; ;;
        --html) do_html=1; ;;
        -h|--help) show_usage "$0"; exit; ;;
        *)
            echo >&2 "Unrecognised argument '$arg'"
            show_usage "$0" >&2
            exit 1
    esac
done

[[ $do_plot -eq 1 ]] && python3 ./plot_graphs.py --save

tempdir="$(mktemp -d)"
trap "rm -rf '$tempdir'" EXIT

mkdir "$tempdir/graphs"

# Close the filelist.txt file descriptor on subshell closure
(
    if [[ $do_html -eq 1 ]]; then
        exec 10>"$tempdir/graphs/filelist.txt"
    fi

    for fname in ../tmp/graphs/static/Release/*/*.png; do
        newname="$(sed -n -e 's,.*/,,' \
                          -e 's/Jacobian ÷ objective/JdivO/' \
                          -e 's/Objective ÷ manual/OdivM/' \
                          -e 's/Objective/O/' \
                          -e 's/Jacobian/J/' \
                          -e 's/^\([^ ]\+\)\( ([^)]*)\)\? \[\([^]]\+\)\] - Release Graph\.png$/\1 \3\2.png/p' \
                        <<<"$fname" | tr -d '()' )"
        testname="${newname/ */}"
        if [[ $testname == "GMM" || $testname == "BA" ]]; then
            if [[ -n $newname ]]; then
                cp "$fname" "$tempdir/graphs/$newname"
                [[ $do_html -eq 1 ]] && echo >&10 "$newname"
            fi
        fi
    done
)

if [[ $do_html -eq 1 ]]; then
    {
        echo "<!doctype html>"
        echo "<html><head><meta charset=\"utf-8\"><title>ADBench graphs</title></head>"
        echo "<body>"
        sort -k1,2 -k3n,3n "$tempdir/graphs/filelist.txt" | while read fname; do
            echo "<a href=\"$fname\">$fname</a><br>"
        done
        echo "</body></html>"
    } >"$tempdir/graphs/index.html"
    rm "$tempdir/graphs/filelist.txt"
fi

outpath="../tmp/graphs/graphs.tar.gz"
tar -C "$tempdir" -czf "$outpath" graphs
echo "Wrote $(realpath "$outpath")"
