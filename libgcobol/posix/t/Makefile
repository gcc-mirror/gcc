.SUFFIXES: .scr .cbl

#
# Ensure UDFs compile and run without crashing. 
#

#      COBCFLAGS is defined by the user

COBC = gcobol
LDFLAGS = -L $$(pwd) -Wl,-rpath -Wl,$$(pwd)

TESTS = errno exit localtime stat

# Default target builds the tests
all: $(TESTS)

% : %.cbl
	$(COBC) -o $@ $(COBCFLAGS) -I. -I../cpy -I../udf $(LDFLAGS) $<


exit: ../udf/posix-exit.cbl 

errno: ../udf/posix-mkdir.cbl

stat: ../udf/posix-stat.cbl 

localtime: ../udf/posix-stat.cbl 

# Run the tests
test: $(TESTS)
	@$(foreach P,$(TESTS),echo $(P):; ./$(P);)

clean:
	rm -f *.o $(basename $(wildcard *.cbl))


