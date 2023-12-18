#!/bin/bash

if [ -d "~/.config/fish" ]; then
	echo "copy FISH config"
	rm -rf fish
	ln -s ./fish ~/.config/fish
	echo "DONE:copy FISH config"
else
	echo "FISH config missing"
fi
