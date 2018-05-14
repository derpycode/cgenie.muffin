#!/bin/bash

for FILE in `cat files`
do
  python ../config2xml.py genie_${FILE}.config > ${FILE}.xml
done
