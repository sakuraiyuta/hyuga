#!/usr/bin/env bash

poetry build && poetry publish
curl https://github.com/sakuraiyuta/hyuga/blob/master/README.md | \
	grep -Eo '<img src="[^"]+"' | grep camo | grep -Eo 'https[^"]+' | \
	xargs -I {} curl -w "\n" -s -X PURGE {}
