# "make test" Compiles everything and runs the regression tests

.PHONY : test
test : all testall.sh
	./testall.sh

# "make all" builds the executable as well as the "printbig" library designed
# to test linking external code

.PHONY : all
all : scic.native printbig.o

# "make microc.native" compiles the compiler
#
# The _tags file controls the operation of ocamlbuild, e.g., by including
# packages, enabling warnings
#
# See https://github.com/ocaml/ocamlbuild/blob/master/manual/manual.adoc

scic.native :
	opam config exec -- \
	ocamlbuild -use-ocamlfind scic.native -pkgs str
	
# 

# "make clean" removes all generated files

.PHONY : clean
clean :
	ocamlbuild -clean
	rm -rf testall.log ocamlllvm *.diff *.ll *.exe *.out *.s *.err printbig.o

# Testing the "printbig" example

printbig : printbig.c
	cc -o printbig -DBUILD_TEST printbig.c 

# Building the tarball

TESTS = \
  printbig helloWorld printftest printb printC

FAILS = \
#   assign1 assign2 assign3 dead1 dead2 expr1 expr2 expr3 float1 float2 \
#   for1 for2 for3 for4 for5 func1 func2 func3 func4 func5 func6 func7 \
#   func8 func9 global1 global2 if1 if2 if3 nomain printbig print \
#   return1 return2 while1 while2

TESTFILES = $(TESTS:%=test-%.sc) $(TESTS:%=test-%.out) \
	    $(FAILS:%=fail-%.sc) $(FAILS:%=fail-%.err)

TARFILES = ast.ml sast.ml codegen.ml Makefile _tags scic.ml parser.mly \
	README scanner.mll semant.ml testall.sh \
	printbig.c  \
	Dockerfile \
	$(TESTFILES:%=tests/%) 

microc.tar.gz : $(TARFILES)
	cd .. && tar czf SCIC/SCIC.tar.gz \
		$(TARFILES:%=SCIC/%)
