#!/bin/sh

echo Need the sudo pass first...
sudo echo OK

if [ ! -f "${HOME}/Downloads/firmware.zip" ]; then
  echo Could not find firmware.zip
  exit 1
fi

FILES=$(unzip -l ~/Downloads/firmware.zip | awk '{print $4}' | rg -v '(Name|^$|----)')
FILE=$(echo $FILES | head -n1)
if [ -z "$FILE" ]; then
  echo Could not find firmware file in firmware.zip
  exit 1
fi
echo Found firmware file: $FILE
unzip -o ~/Downloads/firmware.zip "$FILE" -d /tmp
echo Reset the keeb.....
sleep 5

sudo mount /dev/sdb /mnt/keeb
sudo cp /tmp/$FILE /mnt/keeb
sudo umount /mnt/keeb
echo Flashed!

echo "Remove firmware.zip? (y/n)"
read -r REMOVE
if [ "$REMOVE" = "y" ]; then
  rm ~/Downloads/firmware.zip
fi
