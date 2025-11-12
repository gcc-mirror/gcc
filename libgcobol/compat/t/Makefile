#
# A simple Makefile to demonstrate how the compat/lib programs are used.  
#

COBC = gcobol -g -O0

INCLUDE = ../../posix/cpy  ../../posix/udf 

FLAGS = -dialect mf $(addprefix -I,$(INCLUDE))

COMPAT = $(subst .cbl,.o,$(wildcard ../lib/gnu/*.cbl))

test: smoke 
	./$^

smoke: smoke.cbl $(COMPAT)
	$(ENV) $(COBC) -o $@ \
		$(FLAGS) $(COBCFLAGS) $(LDFLAGS) $^

%.o : %.cbl
	$(ENV) $(COBC) -c -o $@ $(FLAGS) $(COBCFLAGS) $^

% : %.cbl
	$(ENV) $(COBC) -o $@ $(FLAGS) $(COBCFLAGS) $(LDFLAGS) $^



