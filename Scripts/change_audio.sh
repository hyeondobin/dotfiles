#!/bin/bash

# Small script to switch between two hardcoded sound output devices

# Set patterns for devices to be toggled. Have to match to second column
# in output of "pactl list short sinks".
# (Could also hardcode the full names, but this might be a little more robust)
#TOGGLE1="alsa_output.pci-0000_2d_00.4.3.analog-stereo"
#TOGGLE2="alsa_output.usb-Macronix_Razer_Barracuda_X_2.4_1234-00.3.analog-stereo"
TOGGLE1="pci-0000_2d_00.4"
TOGGLE2="usb-Macronix_Razer_Barracuda_X"

# Discover which sink (output device) is active, and set up toggles
while read -r stream; do
	echo "$stream"
	if echo "$stream" | grep -q "RUNNING"; then
		currentSink=$(echo "$stream" | cut -f2)
	fi
	if echo "$stream" | grep -q "$TOGGLE1"; then
		toggle1Sink=$(echo "$stream" | cut -f2)
	fi
	if echo "$stream" | grep -q "$TOGGLE2"; then
		toggle2Sink=$(echo "$stream" | cut -f2)
	fi
done < <(pactl list short sinks)

# Set up where to switch to
if [ "$currentSink" = "$toggle1Sink" ]; then
	newSink=$toggle2Sink
else
	newSink=$toggle1Sink
fi

# Switch streams AND default sink
pactl list short sink-inputs | while read -r stream; do
	streamId=$(echo "$stream" | cut '-d ' -f1)
	# exclude echo cancellation module, but switch all other streams
	# You can, but don't have to remove if condition if no such module present
	if [ "$streamId" != "0" ]; then
		echo "moving stream $streamId"
		pactl move-sink-input "$streamId" "$newSink"
	fi
	# Also switch default sink, so media control keys work correctly
	pactl set-default-sink "$newSink"
done
