xzcat MACT_ADTInbound.0314.xz  | java -jar ../../musc/Hl7Snip.jar    PID:18-1-1 PID:18-1-2 | grep -e  '^[0-9]' | awk ' {print "[" "\x22" $1 "\x22 \x22" $2 "\x22]"}'
