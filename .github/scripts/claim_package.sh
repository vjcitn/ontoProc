#!/bin/bash
set -xe
git pull origin main || git reset --hard origin/main

mkdir -p lists
touch tobuild.txt
if [ ! -s tobuild.txt ]; then
      grep -Pzo "(?s)\s*\"\N*\":\s*\[\s*\]" packages.json | awk -F'"' '{print $2}' | grep -v '^$' | xargs -i bash -c 'touch lists/{} && if ! [ -s "lists/failed/{}" ]; then if ! grep -i ".tar.gz$" lists/{}; then echo "readytobuild" > lists/{}; fi; fi'

      grep -lr "readytobuild" lists/ | sed 's#lists/##g' > tobuild.txt
      git add lists
      git add tobuild.txt
      git commit -m "Adding tobuild list"
      git push

fi

PKGTOBUILD=$(head -n 1 tobuild.txt)
sed -i 's/readytobuild/claimed/g' "lists/$PKGTOBUILD" && grep -lr "readytobuild" lists/ | sed 's#lists/##g' > tobuild.txt
git add lists
git add tobuild.txt
git commit -m "Claim $PKGTOBUILD"
echo "$PKGTOBUILD" > "tmp$1"
git push
