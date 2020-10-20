#!/bin/bash

mapfile -t haskell < <( git ls-files | grep -E "*.hs" )
for i in "${haskell[@]}"; do
	stylish-haskell $i > temp
	[ ! -z "`diff $i temp`" ] && echo "$i is not formatted" && rm temp && exit 1
done

rm temp
