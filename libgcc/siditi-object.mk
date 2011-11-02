# This file is included several times in a row, once for each element of
# $(iter-items).  On each inclusion, we advance $o to the next element.
# $(iter-labels) and $(iter-sizes) are also advanced.

o := $(firstword $(iter-items))
iter-items := $(filter-out $o,$(iter-items))

$o-label := $(firstword $(iter-labels))
iter-labels := $(wordlist 2,$(words $(iter-labels)),$(iter-labels))

$o-size := $(firstword $(iter-sizes))
iter-sizes := $(wordlist 2,$(words $(iter-sizes)),$(iter-sizes))

$o$(objext): %$(objext): $(srcdir)/libgcc2.c
	$(gcc_compile) -DL$($*-label) -c $< $(vis_hide) \
		-DLIBGCC2_UNITS_PER_WORD=$($*-size)

ifeq ($(enable_shared),yes)
$(o)_s$(objext): %_s$(objext): $(srcdir)/libgcc2.c
	$(gcc_s_compile) -DL$($*-label) -c $< \
		-DLIBGCC2_UNITS_PER_WORD=$($*-size)
endif
