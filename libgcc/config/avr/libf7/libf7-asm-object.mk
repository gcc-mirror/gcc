# This file is included several times in a row, once for each element of
# $(iter-items).  On each inclusion, we advance $o to the next element.
# $(iter-labels) is also advanced.
# This works similar to $(srcdir)/siditi-object.mk.

o := $(firstword $(iter-items))
iter-items := $(filter-out $o,$(iter-items))

$o-label := $(firstword $(iter-labels))
iter-labels := $(wordlist 2,$(words $(iter-labels)),$(iter-labels))

f7_asm_$o$(objext): f7_asm_%$(objext): $(libf7)/libf7-asm.sx
	$(gcc_compile) -DF7MOD_$($*-label)_ $(F7_ASM_FLAGS) \
		-c $<

ifeq ($(enable_shared),yes)
f7_asm_$(o)_s$(objext): f7_asm_%_s$(objext): $(libf7)/libf7-asm.sx
	$(gcc_s_compile) -DF7MOD_$($*-label)_ $(F7_ASM_FLAGS) \
		-c $<
endif
