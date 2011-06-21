# This file is included several times in a row, once for each element of
# $(iter-items).  On each inclusion, we advance $o to the next element.

o := $(firstword $(iter-items))
iter-items := $(filter-out $o,$(iter-items))

base := $(basename $(notdir $o))

c_flags-$o := $(c_flags)

ifeq ($(suffix $o),.c)

$(base)$(objext): $o
	$(gcc_compile) $(c_flags-$<) -c $< $(vis_hide)

$(base)_s$(objext): $o
	$(gcc_s_compile) $(c_flags-$<) -c $<

else

ifneq ($(suffix $o),.S)
ifneq ($(suffix $o),.asm)
$(error Unsupported file type: $o)
endif
endif

$(base)$(objext): $o $(base).vis
	$(gcc_compile) -c -xassembler-with-cpp -include $*.vis $<

$(base).vis: $(base)_s$(objext)
	$(gen-hide-list)

$(base)_s$(objext): $o
	$(gcc_s_compile) -c -xassembler-with-cpp $<

endif
