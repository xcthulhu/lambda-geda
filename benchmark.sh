#!/bin/bash

# Some benchmarks/tests to make sure everything is working and check how
# fast...


echo "Parsing gschem test-suite schematics/symbols..."
echo 'time ./parse-test tests/gschem-bench/* > /dev/null'
time ./parse-test tests/gschem-bench/*.sch > /dev/null
echo

echo "Parsing the SXI real world schematics..."
echo 'time ./parse-test tests/SXI/*.sch > /dev/null'
time ./parse-test tests/SXI/* > /dev/null
echo

echo "Converting gschem test-suite schematics/symbols into S-Expression..."
echo 'time (for i in tests/SXI/*; do ./geda-sexpress $i > /dev/null ; done)'
time (for i in tests/SXI/*; do ./geda-sexpress $i > /dev/null ; done)
echo

echo "Converting SXI real world schematics into S-Expressions..."
echo 'time (for i in tests/SXI/*; do ./geda-sexpress $i > /dev/null ; done)'
time (for i in tests/SXI/*; do ./geda-sexpress $i > /dev/null ; done)
echo
