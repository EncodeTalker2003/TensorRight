SHELL=/bin/bash

.PHONY: build verify maxMinToClampBefore maxMinToClampAfter generalize plot	clean

build:
	stack build

verify: build
	./runall.sh 2> >(tee ./plot/result.txt);

maxMinToClampBefore: build
	stack exec rules-maxMinToClampBefore

maxMinToClampAfter: build
	stack exec rules-maxMinToClampAfter

generalize: build
	stack exec rules-generalize

plot: build
	if [ ! -f ./plot/result.txt ]; then ./runall.sh 2> >(tee ./plot/result.txt); fi
	cd plot && python3 timing_plot.py

clean:
	stack clean
	rm -rf .stack-work
	rm -f ./plot/result.txt ./plot/timing_plot.pdf
