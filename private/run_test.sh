#!/bin/sh

if [ -z "$MAKEFILE_PRIVATE" ]; then
    echo "DO NOT RUN THIS SCRIPT EXPLICITLY!"
    echo "If you want to test the solutions just use command:"
    echo " make test"
    exit 1
fi

for i in `find -name p\*.ml | sort`; do
    echo "Testing $i"
    ocaml simpletest.cmo $i
done
