#!/bin/sh

if [ x = x"$1" ]; then
  echo "wrong"
  exit 1
fi
if [ "wake" = "$1" ]; then
  echo 'actions.speech.enable()' | ~/.talon/bin/repl
  exit 0
fi
if [ "sleep" = "$1" ]; then
  echo 'actions.speech.disable()' | ~/.talon/bin/repl
  exit 0
fi
echo "what?"
exit 1
