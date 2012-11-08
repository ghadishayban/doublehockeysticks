#!/bin/bash

medfiles=/mnt/raidx/home/cloverleaf/output/{HMM,HCI}*Inb*030[0-5].xz
hl7jar=/mnt/raidx/home/musc/Hl7Snip.jar

for x in /mnt/raidx/home/cloverleaf/output/{HMM,HCI}*Inb*0301.xz; do
	echo $x
	xzcat $x | java -jar $hl7jar -d'|' MSH:9-1-1 $1 $2 $3 | sort | uniq
done
