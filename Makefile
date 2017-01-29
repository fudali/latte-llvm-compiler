all:
	cd src && $(MAKE)
	cp src/bash/latc_llvm .

clean:
	cd src && $(MAKE) clean
	rm latc_llvm

compile-all:
	for test in `find lattests/good -name "*.lat"`; do \
	       	./latc_llvm $$test;\
	done

clean-tests:
	rm lattests/good/*.ll
	rm lattests/good/*.bc

