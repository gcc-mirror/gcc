# This file is included several times in a row, once for each element of
# $(iter-items).  On each inclusion, we advance $o to the next element.
# $(iter-labels) and $(iter-flags) are also advanced.
# This works similar to $(srcdir)/siditi-object.mk.

o := $(firstword $(iter-items))
iter-items := $(filter-out $o,$(iter-items))

$o-label := $(firstword $(iter-labels))
iter-labels := $(wordlist 2,$(words $(iter-labels)),$(iter-labels))

$o-flag := $(firstword $(iter-flags))
iter-flags := $(wordlist 2,$(words $(iter-flags)),$(iter-flags))

$o$(objext): %$(objext): $(srcdir)/config/avr/lib2funcs.c
	$(gcc_compile) -DL_$($*-label) -DL_LABEL=$($*-label) $($*-flag) \
		-c $< $(vis_hide)

ifeq ($(enable_shared),yes)
$(o)_s$(objext): %_s$(objext): $(srcdir)/config/avr/lib2funcs.c
	$(gcc_s_compile) -DL_$($*-label) -DL_LABEL=$($*-label) $($*-flag) \
		-c $<
endif
