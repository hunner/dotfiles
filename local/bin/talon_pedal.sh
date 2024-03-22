#!/bin/bash

pedal_down="enable"
pedal_up="disable"

if [ xsleep = x"$1" ]; then
  pedal_down="disable"
  pedal_up="enable"
fi

line=""
aseqdump -p "USB Uno MIDI Interface" | while true ; do
  read -r line
  case $(echo "$line" | tr -s " " | cut -d " " -f 4-8) in
    "0, controller 67, value 0") echo "actions.speech.${pedal_up}()" | ~/.talon/bin/repl ;;
    "0, controller 67, value 127") echo "actions.speech.${pedal_down}()" | ~/.talon/bin/repl ;;
    *) echo "other stuff: $(echo "$line" | tr -s " " | cut -d " " -f 4-8)" ;;
  esac
done
