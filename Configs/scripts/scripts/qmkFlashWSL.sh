#!/bin/bash
cd ~/repo/qmk_userspace/
qmk compile
cmd.exe /C C:/QMK_MSYS/shell_connector.cmd -c C:/QMK_MSYS/moonlander_flash.sh
