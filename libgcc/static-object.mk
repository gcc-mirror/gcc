# This file is included several times in a row, once for each element of
# $(iter-items).  On each inclusion, we advance $o to the next element.

o := $(firstword $(iter-items))
iter-items := $(filter-out $o,$(iter-items))

base := $(basename $(notdir $o))

# Copy c_flags to a rule-specific copy and use the copy, to avoid the
# following rules being affected by later changes to c_flags in the
# including file.
c_flags-$o := $(c_flags)

ifeq ($(suffix $o),.c)

$(base)$(objext): $o
	$(gcc_compile) $(c_flags-$<) -c $< $(vis_hide)

else

ifneq ($(suffix $o),.S)
ifneq ($(suffix $o),.asm)
$(error Unsupported file type: $o)
endif
endif

as_flags-$o := -xassembler$(if $(filter .S,$(suffix $o)),-with-cpp)

$(base)$(objext): $o $(base).vis
	$(gcc_compile) -c $(as_flags-$<) -include $*.vis $<

$(base).vis: $(base)_s$(objext)
	$(gen-hide-list)

$(base)_s$(objext): $o
	$(gcc_s_compile) -c $(as_flags-$<) $<

endif
