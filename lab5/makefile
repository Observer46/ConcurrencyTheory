# Makefile
RUNGHC = runghc
GHC = ghc

# Better to runghc - .o and .hi files generated, also generates graph
run: trace.hs
	$(RUNGHC) trace.hs
	dot -Tps graph.gv -o graph.pdf	

runExample0: trace.hs example0.txt
	cat example0.txt | $(RUNGHC) trace.hs
	dot -Tps graph.gv -o graph.pdf	

runExample1: trace.hs example1.txt
	cat example1.txt | $(RUNGHC) trace.hs
	dot -Tps graph.gv -o graph.pdf	

runExample2: trace.hs example2.txt
	cat example2.txt | $(RUNGHC) trace.hs
	dot -Tps graph.gv -o graph.pdf	

compile: trace.hs
	$(GHC) trace.hs

compileAndRunExample0: example0.txt
	make compile
	cat example0.txt | ./trace
	dot -Tps graph.gv -o graph.pdf

compileAndRunExample1: example1.txt
	make compile
	cat example1.txt | ./trace
	dot -Tps graph.gv -o graph.pdf

compileAndRunExample2: example2.txt
	make compile
	cat example2.txt | ./trace
	dot -Tps graph.gv -o graph.pdf
		


clean:
	rm -f trace.o trace.hi trace
	rm -f freshResults.txt graph.gv graph.pdf