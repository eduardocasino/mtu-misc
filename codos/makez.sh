#!/bin/bash

#name=$(basename $1)
entry=${3#*x}
entrylo=\\x${entry#??}
entryhi=\\x${entry%??}
load=${4#*x}
loadlo=\\x${load#??}
loadhi=\\x${load%??}
size=$(printf '%4.4x' $(stat -c"%s" $1))
sizelo=\\x${size#??}
sizehi=\\x${size%??}

echo -n -e '\x58\x00\x00\x00' > $2
eval echo -n -e '$entrylo$entryhi$loadlo$loadhi$sizelo$sizehi' >> $2
cat $1 >> $2
