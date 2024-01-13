#/usr/bin/env bash

echo "1. set-local-rtc"
echo "Else: quit"
read input
if [[ "$input" == "1" ]]; then
	timedatectl set-local-rtc 1 --adjust-system-clock
	printf "changed time setting\n"
else
    printf "$input: quit\n"
    exit 0
fi

