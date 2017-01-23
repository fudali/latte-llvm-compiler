all:
	cd src && $(MAKE)
	cp src/bash/insc_llvm .

clean:
	cd src && $(MAKE) clean
	rm insc_llvm

compile-all:
	for test in `find lattests/good -name "*.lat"`; do \
	       	./insc_llvm $$test;\
	done

clean-tests:
	rm lattests/good/*.ll
	rm lattests/good/*.bc

