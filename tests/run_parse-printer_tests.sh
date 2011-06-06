#/bin/bash

for i in SXI/*.sch gschem-bench/*.sym gschem-bench/*.sch ; do
	d=`../parse-print-test $i | diff $i -`
	if [ ! $dd ] ; then
		echo $i PASSED
	else
		echo "$i FAILED; Differences:"
		echo $d
	fi
done
