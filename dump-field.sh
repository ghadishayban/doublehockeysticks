#!/bin/bash
echo FIELD DUMP OF "$1"
xzcat /mnt/raidx/home/cloverleaf/output/MACT_ADTInbound.030[1-5].xz | java -jar /mnt/raidx/home/musc/Hl7Snip.jar "$1"  &>  /tmp/"$1".dump

