#/bin/bash

for i in SXI/* gschem-bench/* ; do
	d=`../parse-print-test $i | diff $i -`
	if [ ! $dd ] ; then
		echo $i PASSED
	else
		echo "$i FAILED; Differences:"
		echo $d
	fi
done
