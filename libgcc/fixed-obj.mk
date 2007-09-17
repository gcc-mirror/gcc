# This file is included several times in a row, once for each element of
# $(iter-items).  On each inclusion, we advance $o to the next element.
# $(iter-labels) and $(iter-from) and $(iter-to) are also advanced.

o := $(firstword $(iter-items))
iter-items := $(filter-out $o,$(iter-items))

$o-label := $(firstword $(iter-labels))
iter-labels := $(wordlist 2,$(words $(iter-labels)),$(iter-labels))

$o-from := $(firstword $(iter-from))
iter-from := $(wordlist 2,$(words $(iter-from)),$(iter-from))

$o-to := $(firstword $(iter-to))
iter-to := $(wordlist 2,$(words $(iter-to)),$(iter-to))

ifeq ($($o-from),$($o-to))
$o-opt := -D$($o-from)_MODE
else
$o-opt := -DFROM_$($o-from) -DTO_$($o-to)
endif

#$(info $o$(objext): -DL$($o-label) $($o-opt))

$o$(objext): %$(objext): $(gcc_srcdir)/config/fixed-bit.c
	$(gcc_compile) -DL$($*-label) $($*-opt) -c $(gcc_srcdir)/config/fixed-bit.c $(vis_hide)

ifeq ($(enable_shared),yes)
$(o)_s$(objext): %_s$(objext): $(gcc_srcdir)/config/fixed-bit.c
	$(gcc_s_compile) -DL$($*-label) $($*-opt) -c $(gcc_srcdir)/config/fixed-bit.c
endif
