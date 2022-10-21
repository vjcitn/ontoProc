#!/bin/bash
set -e

PKGTOMARK=$1

sed -i "s/    \"$PKGTOMARK\": \[\],\?//g" packages.json
sed -i "s/        \"$PKGTOMARK\",\?//g" packages.json
sed -i -z 's/,\n\n\+}/}/g' packages.json
sed -i -z 's/,\n\n\+ *]/]/g' packages.json
python3 -c 'import json; f = open("packages.json", "r"); pkgs = json.load(f); f.close(); f = open("packages.json", "w"); f.write(json.dumps(pkgs, indent=4)); f.close()'
