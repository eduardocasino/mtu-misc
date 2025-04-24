#!/bin/bash

name=$(basename $1)
output=${name%.*}.z
entry=${2#*x}
entrylo=\\x${entry#??}
entryhi=\\x${entry%??}
load=${3#*x}
loadlo=\\x${load#??}
loadhi=\\x${load%??}
size=$(printf '%x' $(stat -c"%s" $1))
sizelo=\\x${size#??}
sizehi=\\x${size%??}

echo -n -e '\x58\x00\x00\x00' > $output
eval echo -n -e '$entrylo$entryhi$loadlo$loadhi$sizelo$sizehi' >> $output
cat $1 >> $output
