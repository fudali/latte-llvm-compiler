all:
	happy -gca ParLatte.y
	alex -g LexLatte.x
	ghc --make Latte.hs -o LlvmLatte

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi


