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

ifneq ($o,$(filter $o,$(LIB2FUNCS_EXCLUDE)))
$o$(objext): %$(objext): $(srcdir)/fixed-bit.c
	$(gcc_compile) -DL$($*-label) $($*-opt) -c $(srcdir)/fixed-bit.c $(vis_hide)

ifeq ($(enable_shared),yes)
$(o)_s$(objext): %_s$(objext): $(srcdir)/fixed-bit.c
	$(gcc_s_compile) -DL$($*-label) $($*-opt) -c $(srcdir)/fixed-bit.c
endif
endif
