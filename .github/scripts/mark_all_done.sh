#!/bin/bash
set -xe

ORIGINAL="0"
AFTER=$(cat packages.json | wc -l)

until [ $ORIGINAL = $AFTER ];
do
	ORIGINAL=$AFTER
	rclone ls js2:/gha-build | grep "tar" | awk '{print $2}' > /tmp/tars
	cat /tmp/tars | awk -F'_' '{print $1}' | xargs -i bash .github/scripts/mark_done.sh {}

	cat /tmp/tars | awk -F'_' '{print $1}' | xargs -i bash -c 'grep "^{}_" /tmp/tars | head -n1 > lists/{}'
	AFTER=$(cat packages.json | wc -l)
done
