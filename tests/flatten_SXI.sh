#/bin/bash

echo rm -rf SXI/flatten
rm -rf SXI/flatten
echo mkdir SXI/flatten
mkdir SXI/flatten
echo cd SXI
cd SXI
echo ../../flatten-hierarchy flatten DriverBoard.*.sch J?.sch TCE.sch
time ../../flatten-hierarchy flatten DriverBoard.*.sch J?.sch TCE.sch
