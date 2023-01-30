#!/bin/bash

outName=scrolling

writeToChip=false
flags=""

while getopts "we" opt; do
    case $opt in
        w)
            echo "ok good"
            writeToChip=true
            ;;
        e)
            echo "building for emulator"
            flags="-D EMU"
            ;;
        \?)
            echo "Invalid option: -$OPTARG" >&2
            ;;
    esac
done

rgbasm -o memory.o memory.asm $flags &&
rgbasm -o main.o main.asm $flags &&
rgblink -d -n $outName.sym -o $outName.gb main.o memory.o &&
#rgbfix -v -p 0 -m 0x08 -r 0x1 $outName.gb
rgbfix -v -p 0 -m MBC5 $outName.gb

if [ $? -ne 0 ] ; then
    echo "build failed"
    echo "exiting"
    exit $?
fi
echo "done build"



if [ "$writeToChip" != true ] ; then
    echo "not writing"
    exit 0
fi
echo "Writing to chip"
minipro -p "SST39SF020A" -w $outName.gb -s

