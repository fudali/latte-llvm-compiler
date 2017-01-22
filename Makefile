all:
	cd src && $(MAKE)
	cp src/bash/insc_llvm .

clean:
	cd src && $(MAKE) clean
	rm insc_llvm
