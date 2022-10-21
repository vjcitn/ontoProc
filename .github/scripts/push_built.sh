#!/bin/bash
set -xe
git pull origin main || git reset --hard origin/main

PKG=$1
sed -i 's/runidpushed/built/g' "lists/$PKG"
git add lists
#cp /tmp/tars/*.tar.gz tars/
mkdir -p logs/
#git add tars
git add logs
#git add built
git commit -m "Built $PKG"
git push
