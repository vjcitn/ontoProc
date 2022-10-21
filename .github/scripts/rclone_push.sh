#!/bin/bash
set -xe
git pull origin main || git reset --hard origin/main

PKGTOMARK=$1
TAR=$2

#rclone ls js2:/gha-build | grep "tar" | awk '{print $2}' > /tmp/tars
#cat /tmp/tars | awk -F'_' '{print $1}' | sed 's#lists/##g' | xargs -i bash -c 'grep "{}" /tmp/tars | head -n1 > lists/{}'
echo "$TAR" > "lists/$PKGTOMARK"

bash .github/scripts/mark_done.sh $PKGTOMARK

git add lists
git add packages.json
git commit -m "Mark pushed $PKGTOMARK"
git push
