## Process this file with automake to produce Makefile.in


ACLOCAL_AMFLAGS = -I ../config

## May be used by toolexeclibdir.
gcc_version := $(shell cat $(top_srcdir)/../gcc/BASE-VER)

toolexeclib_LTLIBRARIES = libgfortran.la libgfortranbegin.la

libgfortran_la_LDFLAGS = -version-info `grep -v '^\#' $(srcdir)/libtool-version` -lm $(extra_ldflags_libgfortran)

## This should really go in the compiler lib dir, not the system lib dir.
libgfortranbegin_la_SOURCES = fmain.c
libgfortranbegin_la_LDFLAGS = -static

## io.h conflicts with some a system header on some platforms, so
## use -iquote
AM_CPPFLAGS = -iquote$(srcdir)/io

gfor_io_src= \
io/close.c \
io/file_pos.c \
io/format.c \
io/inquire.c \
io/list_read.c \
io/lock.c \
io/open.c \
io/read.c \
io/size_from_kind.c \
io/transfer.c \
io/unit.c \
io/unix.c \
io/write.c

gfor_io_headers= \
io/io.h

gfor_helper_src= \
intrinsics/associated.c \
intrinsics/abort.c \
intrinsics/args.c \
intrinsics/bessel.c \
intrinsics/c99_functions.c \
intrinsics/chdir.c \
intrinsics/cpu_time.c \
intrinsics/cshift0.c \
intrinsics/ctime.c \
intrinsics/date_and_time.c \
intrinsics/env.c \
intrinsics/erf.c \
intrinsics/eoshift0.c \
intrinsics/eoshift2.c \
intrinsics/etime.c \
intrinsics/exit.c \
intrinsics/flush.c \
intrinsics/fnum.c \
intrinsics/gerror.c \
intrinsics/getcwd.c \
intrinsics/getlog.c \
intrinsics/getXid.c \
intrinsics/hyper.c \
intrinsics/hostnm.c \
intrinsics/kill.c \
intrinsics/ierrno.c \
intrinsics/ishftc.c \
intrinsics/link.c \
intrinsics/malloc.c \
intrinsics/mvbits.c \
intrinsics/pack_generic.c \
intrinsics/perror.c \
intrinsics/signal.c \
intrinsics/size.c \
intrinsics/sleep.c \
intrinsics/spread_generic.c \
intrinsics/string_intrinsics.c \
intrinsics/system.c \
intrinsics/rand.c \
intrinsics/random.c \
intrinsics/rename.c \
intrinsics/reshape_generic.c \
intrinsics/reshape_packed.c \
intrinsics/selected_int_kind.f90 \
intrinsics/selected_real_kind.f90 \
intrinsics/stat.c \
intrinsics/symlnk.c \
intrinsics/system_clock.c \
intrinsics/time.c \
intrinsics/transpose_generic.c \
intrinsics/tty.c \
intrinsics/umask.c \
intrinsics/unlink.c \
intrinsics/unpack_generic.c \
runtime/in_pack_generic.c \
runtime/in_unpack_generic.c \
runtime/normalize.c

gfor_src= \
runtime/compile_options.c \
runtime/environ.c \
runtime/error.c \
runtime/fpu.c \
runtime/main.c \
runtime/memory.c \
runtime/pause.c \
runtime/stop.c \
runtime/string.c \
runtime/select.c \
gfortypes.h \
libgfortran.h

i_all_c= \
generated/all_l4.c \
generated/all_l8.c \
generated/all_l16.c

i_any_c= \
generated/any_l4.c \
generated/any_l8.c \
generated/any_l16.c

i_count_c= \
generated/count_4_l4.c \
generated/count_8_l4.c \
generated/count_16_l4.c \
generated/count_4_l8.c \
generated/count_8_l8.c \
generated/count_16_l8.c \
generated/count_4_l16.c \
generated/count_8_l16.c \
generated/count_16_l16.c

i_maxloc0_c= \
generated/maxloc0_4_i4.c \
generated/maxloc0_8_i4.c \
generated/maxloc0_16_i4.c \
generated/maxloc0_4_i8.c \
generated/maxloc0_8_i8.c \
generated/maxloc0_16_i8.c \
generated/maxloc0_4_i16.c \
generated/maxloc0_8_i16.c \
generated/maxloc0_16_i16.c \
generated/maxloc0_4_r4.c \
generated/maxloc0_8_r4.c \
generated/maxloc0_16_r4.c \
generated/maxloc0_4_r8.c \
generated/maxloc0_8_r8.c \
generated/maxloc0_16_r8.c \
generated/maxloc0_4_r10.c \
generated/maxloc0_8_r10.c \
generated/maxloc0_16_r10.c \
generated/maxloc0_4_r16.c \
generated/maxloc0_8_r16.c \
generated/maxloc0_16_r16.c

i_maxloc1_c= \
generated/maxloc1_4_i4.c \
generated/maxloc1_8_i4.c \
generated/maxloc1_16_i4.c \
generated/maxloc1_4_i8.c \
generated/maxloc1_8_i8.c \
generated/maxloc1_16_i8.c \
generated/maxloc1_4_i16.c \
generated/maxloc1_8_i16.c \
generated/maxloc1_16_i16.c \
generated/maxloc1_4_r4.c \
generated/maxloc1_8_r4.c \
generated/maxloc1_16_r4.c \
generated/maxloc1_4_r8.c \
generated/maxloc1_8_r8.c \
generated/maxloc1_16_r8.c \
generated/maxloc1_4_r10.c \
generated/maxloc1_8_r10.c \
generated/maxloc1_16_r10.c \
generated/maxloc1_4_r16.c \
generated/maxloc1_8_r16.c \
generated/maxloc1_16_r16.c

i_maxval_c= \
generated/maxval_i4.c \
generated/maxval_i8.c \
generated/maxval_i16.c \
generated/maxval_r4.c \
generated/maxval_r8.c \
generated/maxval_r10.c \
generated/maxval_r16.c

i_minloc0_c= \
generated/minloc0_4_i4.c \
generated/minloc0_8_i4.c \
generated/minloc0_16_i4.c \
generated/minloc0_4_i8.c \
generated/minloc0_8_i8.c \
generated/minloc0_16_i8.c \
generated/minloc0_4_i16.c \
generated/minloc0_8_i16.c \
generated/minloc0_16_i16.c \
generated/minloc0_4_r4.c \
generated/minloc0_8_r4.c \
generated/minloc0_16_r4.c \
generated/minloc0_4_r8.c \
generated/minloc0_8_r8.c \
generated/minloc0_16_r8.c \
generated/minloc0_4_r10.c \
generated/minloc0_8_r10.c \
generated/minloc0_16_r10.c \
generated/minloc0_4_r16.c \
generated/minloc0_8_r16.c \
generated/minloc0_16_r16.c

i_minloc1_c= \
generated/minloc1_4_i4.c \
generated/minloc1_8_i4.c \
generated/minloc1_16_i4.c \
generated/minloc1_4_i8.c \
generated/minloc1_8_i8.c \
generated/minloc1_16_i8.c \
generated/minloc1_4_i16.c \
generated/minloc1_8_i16.c \
generated/minloc1_16_i16.c \
generated/minloc1_4_r4.c \
generated/minloc1_8_r4.c \
generated/minloc1_16_r4.c \
generated/minloc1_4_r8.c \
generated/minloc1_8_r8.c \
generated/minloc1_16_r8.c \
generated/minloc1_4_r10.c \
generated/minloc1_8_r10.c \
generated/minloc1_16_r10.c \
generated/minloc1_4_r16.c \
generated/minloc1_8_r16.c \
generated/minloc1_16_r16.c

i_minval_c= \
generated/minval_i4.c \
generated/minval_i8.c \
generated/minval_i16.c \
generated/minval_r4.c \
generated/minval_r8.c \
generated/minval_r10.c \
generated/minval_r16.c

i_sum_c= \
generated/sum_i4.c \
generated/sum_i8.c \
generated/sum_i16.c \
generated/sum_r4.c \
generated/sum_r8.c \
generated/sum_r10.c \
generated/sum_r16.c \
generated/sum_c4.c \
generated/sum_c8.c \
generated/sum_c10.c \
generated/sum_c16.c

i_product_c= \
generated/product_i4.c \
generated/product_i8.c \
generated/product_i16.c \
generated/product_r4.c \
generated/product_r8.c \
generated/product_r10.c \
generated/product_r16.c \
generated/product_c4.c \
generated/product_c8.c \
generated/product_c10.c \
generated/product_c16.c

i_dotprod_c= \
generated/dotprod_i4.c \
generated/dotprod_i8.c \
generated/dotprod_i16.c \
generated/dotprod_r4.c \
generated/dotprod_r8.c \
generated/dotprod_r10.c \
generated/dotprod_r16.c

i_dotprodl_c= \
generated/dotprod_l4.c \
generated/dotprod_l8.c \
generated/dotprod_l16.c

i_dotprodc_c= \
generated/dotprod_c4.c \
generated/dotprod_c8.c \
generated/dotprod_c10.c \
generated/dotprod_c16.c

i_matmul_c= \
generated/matmul_i4.c \
generated/matmul_i8.c \
generated/matmul_i16.c \
generated/matmul_r4.c \
generated/matmul_r8.c \
generated/matmul_r10.c \
generated/matmul_r16.c \
generated/matmul_c4.c \
generated/matmul_c8.c \
generated/matmul_c10.c \
generated/matmul_c16.c

i_matmull_c= \
generated/matmul_l4.c \
generated/matmul_l8.c \
generated/matmul_l16.c

i_transpose_c= \
generated/transpose_i4.c \
generated/transpose_i8.c \
generated/transpose_i16.c \
generated/transpose_c4.c \
generated/transpose_c8.c \
generated/transpose_c10.c \
generated/transpose_c16.c

i_shape_c= \
generated/shape_i4.c \
generated/shape_i8.c \
generated/shape_i16.c

i_reshape_c= \
generated/reshape_i4.c \
generated/reshape_i8.c \
generated/reshape_i16.c \
generated/reshape_c4.c \
generated/reshape_c8.c \
generated/reshape_c10.c \
generated/reshape_c16.c

i_eoshift1_c= \
generated/eoshift1_4.c \
generated/eoshift1_8.c \
generated/eoshift1_16.c

i_eoshift3_c= \
generated/eoshift3_4.c \
generated/eoshift3_8.c \
generated/eoshift3_16.c

i_cshift1_c= \
generated/cshift1_4.c \
generated/cshift1_8.c \
generated/cshift1_16.c

in_pack_c = \
generated/in_pack_i4.c \
generated/in_pack_i8.c \
generated/in_pack_i16.c \
generated/in_pack_c4.c \
generated/in_pack_c8.c \
generated/in_pack_c10.c \
generated/in_pack_c16.c

in_unpack_c = \
generated/in_unpack_i4.c \
generated/in_unpack_i8.c \
generated/in_unpack_i16.c \
generated/in_unpack_c4.c \
generated/in_unpack_c8.c \
generated/in_unpack_c10.c \
generated/in_unpack_c16.c

i_exponent_c = \
generated/exponent_r4.c \
generated/exponent_r8.c \
generated/exponent_r10.c \
generated/exponent_r16.c

i_fraction_c = \
generated/fraction_r4.c \
generated/fraction_r8.c \
generated/fraction_r10.c \
generated/fraction_r16.c

i_nearest_c = \
generated/nearest_r4.c \
generated/nearest_r8.c \
generated/nearest_r10.c \
generated/nearest_r16.c

i_set_exponent_c = \
generated/set_exponent_r4.c \
generated/set_exponent_r8.c \
generated/set_exponent_r10.c \
generated/set_exponent_r16.c

i_pow_c = \
generated/pow_i4_i4.c \
generated/pow_i8_i4.c \
generated/pow_i16_i4.c \
generated/pow_r4_i4.c \
generated/pow_r8_i4.c \
generated/pow_r10_i4.c \
generated/pow_r16_i4.c \
generated/pow_c4_i4.c \
generated/pow_c8_i4.c \
generated/pow_c10_i4.c \
generated/pow_c16_i4.c \
generated/pow_i4_i8.c \
generated/pow_i8_i8.c \
generated/pow_i16_i8.c \
generated/pow_r4_i8.c \
generated/pow_r8_i8.c \
generated/pow_r10_i8.c \
generated/pow_r16_i8.c \
generated/pow_c4_i8.c \
generated/pow_c8_i8.c \
generated/pow_c10_i8.c \
generated/pow_c16_i8.c \
generated/pow_i4_i16.c \
generated/pow_i8_i16.c \
generated/pow_i16_i16.c \
generated/pow_r4_i16.c \
generated/pow_r8_i16.c \
generated/pow_r10_i16.c \
generated/pow_r16_i16.c \
generated/pow_c4_i16.c \
generated/pow_c8_i16.c \
generated/pow_c10_i16.c \
generated/pow_c16_i16.c

m4_files= m4/iparm.m4 m4/ifunction.m4 m4/iforeach.m4 m4/all.m4 \
    m4/any.m4 m4/count.m4 m4/maxloc0.m4 m4/maxloc1.m4 m4/maxval.m4 \
    m4/minloc0.m4 m4/minloc1.m4 m4/minval.m4 m4/product.m4 m4/sum.m4 \
    m4/dotprod.m4 m4/dotprodl.m4 m4/dotprodc.m4 m4/matmul.m4 m4/matmull.m4 \
    m4/ctrig.m4 m4/cexp.m4 m4/chyp.m4 m4/mtype.m4 \
    m4/specific.m4 m4/specific2.m4 m4/head.m4 m4/shape.m4 m4/reshape.m4 \
    m4/transpose.m4 m4/eoshift1.m4 m4/eoshift3.m4 m4/exponent.m4 \
    m4/fraction.m4 m4/nearest.m4 m4/set_exponent.m4 m4/pow.m4

gfor_built_src= $(i_all_c) $(i_any_c) $(i_count_c) $(i_maxloc0_c) \
    $(i_maxloc1_c) $(i_maxval_c) $(i_minloc0_c) $(i_minloc1_c) $(i_minval_c) \
    $(i_product_c) $(i_sum_c) $(i_dotprod_c) $(i_dotprodl_c) $(i_dotprodc_c) \
    $(i_matmul_c) $(i_matmull_c) $(i_transpose_c) $(i_shape_c) $(i_eoshift1_c) \
    $(i_eoshift3_c) $(i_cshift1_c) $(i_reshape_c) $(in_pack_c) $(in_unpack_c) \
    $(i_exponent_c) $(i_fraction_c) $(i_nearest_c) $(i_set_exponent_c) \
    $(i_pow_c) \
    selected_int_kind.inc selected_real_kind.inc kinds.h \
    kinds.inc c99_protos.inc fpu-target.h

# Machine generated specifics
gfor_built_specific_src= \
generated/_abs_c4.F90 \
generated/_abs_c8.F90 \
generated/_abs_c10.F90 \
generated/_abs_c16.F90 \
generated/_abs_i4.F90 \
generated/_abs_i8.F90 \
generated/_abs_i16.F90 \
generated/_abs_r4.F90 \
generated/_abs_r8.F90 \
generated/_abs_r10.F90 \
generated/_abs_r16.F90 \
generated/_exp_r4.F90 \
generated/_exp_r8.F90 \
generated/_exp_r10.F90 \
generated/_exp_r16.F90 \
generated/_exp_c4.F90 \
generated/_exp_c8.F90 \
generated/_exp_c10.F90 \
generated/_exp_c16.F90 \
generated/_log_r4.F90 \
generated/_log_r8.F90 \
generated/_log_r10.F90 \
generated/_log_r16.F90 \
generated/_log_c4.F90 \
generated/_log_c8.F90 \
generated/_log_c10.F90 \
generated/_log_c16.F90 \
generated/_log10_r4.F90 \
generated/_log10_r8.F90 \
generated/_log10_r10.F90 \
generated/_log10_r16.F90 \
generated/_sqrt_r4.F90 \
generated/_sqrt_r8.F90 \
generated/_sqrt_r10.F90 \
generated/_sqrt_r16.F90 \
generated/_sqrt_c4.F90 \
generated/_sqrt_c8.F90 \
generated/_sqrt_c10.F90 \
generated/_sqrt_c16.F90 \
generated/_asin_r4.F90 \
generated/_asin_r8.F90 \
generated/_asin_r10.F90 \
generated/_asin_r16.F90 \
generated/_acos_r4.F90 \
generated/_acos_r8.F90 \
generated/_acos_r10.F90 \
generated/_acos_r16.F90 \
generated/_atan_r4.F90 \
generated/_atan_r8.F90 \
generated/_atan_r10.F90 \
generated/_atan_r16.F90 \
generated/_sin_r4.F90 \
generated/_sin_r8.F90 \
generated/_sin_r10.F90 \
generated/_sin_r16.F90 \
generated/_sin_c4.F90 \
generated/_sin_c8.F90 \
generated/_sin_c10.F90 \
generated/_sin_c16.F90 \
generated/_cos_r4.F90 \
generated/_cos_r8.F90 \
generated/_cos_r10.F90 \
generated/_cos_r16.F90 \
generated/_cos_c4.F90 \
generated/_cos_c8.F90 \
generated/_cos_c10.F90 \
generated/_cos_c16.F90 \
generated/_tan_r4.F90 \
generated/_tan_r8.F90 \
generated/_tan_r10.F90 \
generated/_tan_r16.F90 \
generated/_sinh_r4.F90 \
generated/_sinh_r8.F90 \
generated/_sinh_r10.F90 \
generated/_sinh_r16.F90 \
generated/_cosh_r4.F90 \
generated/_cosh_r8.F90 \
generated/_cosh_r10.F90 \
generated/_cosh_r16.F90 \
generated/_tanh_r4.F90 \
generated/_tanh_r8.F90 \
generated/_tanh_r10.F90 \
generated/_tanh_r16.F90 \
generated/_conjg_c4.F90 \
generated/_conjg_c8.F90 \
generated/_conjg_c10.F90 \
generated/_conjg_c16.F90 \
generated/_aint_r4.F90 \
generated/_aint_r8.F90 \
generated/_aint_r10.F90 \
generated/_aint_r16.F90 \
generated/_anint_r4.F90 \
generated/_anint_r8.F90 \
generated/_anint_r10.F90 \
generated/_anint_r16.F90

gfor_built_specific2_src= \
generated/_sign_i4.F90 \
generated/_sign_i8.F90 \
generated/_sign_i16.F90 \
generated/_sign_r4.F90 \
generated/_sign_r8.F90 \
generated/_sign_r10.F90 \
generated/_sign_r16.F90 \
generated/_dim_i4.F90 \
generated/_dim_i8.F90 \
generated/_dim_i16.F90 \
generated/_dim_r4.F90 \
generated/_dim_r8.F90 \
generated/_dim_r10.F90 \
generated/_dim_r16.F90 \
generated/_atan2_r4.F90 \
generated/_atan2_r8.F90 \
generated/_atan2_r10.F90 \
generated/_atan2_r16.F90 \
generated/_mod_i4.F90 \
generated/_mod_i8.F90 \
generated/_mod_i16.F90 \
generated/_mod_r4.F90 \
generated/_mod_r8.F90
# There are commented out due to a bug in the way the front-end
# handles MOD
#generated/_mod_r10.F90
#generated/_mod_r16.F90

gfor_specific_src= \
$(gfor_built_specific_src) \
$(gfor_built_specific2_src) \
intrinsics/dprod_r8.f90 \
intrinsics/f2c_specifics.F90

BUILT_SOURCES=$(gfor_built_src) $(gfor_built_specific_src) \
    $(gfor_built_specific2_src)
libgfortran_la_SOURCES = $(gfor_src) $(gfor_built_src) $(gfor_io_src) \
    $(gfor_helper_src) $(gfor_io_headers) $(gfor_specific_src)

I_M4_DEPS=m4/iparm.m4
I_M4_DEPS0=$(I_M4_DEPS) m4/iforeach.m4
I_M4_DEPS1=$(I_M4_DEPS) m4/ifunction.m4

kinds.h: $(srcdir)/mk-kinds-h.sh
	$(SHELL) $(srcdir)/mk-kinds-h.sh '$(FCCOMPILE)' > $@ || rm $@

kinds.inc: kinds.h
	grep '^#' < kinds.h > $@

c99_protos.inc: $(srcdir)/c99_protos.h
	grep '^#' < $(srcdir)/c99_protos.h > $@

selected_int_kind.inc: $(srcdir)/mk-sik-inc.sh
	$(SHELL) $(srcdir)/mk-sik-inc.sh '$(FCCOMPILE)' > $@ || rm $@

selected_real_kind.inc: $(srcdir)/mk-srk-inc.sh
	$(SHELL) $(srcdir)/mk-srk-inc.sh '$(FCCOMPILE)' > $@ || rm $@

fpu-target.h: $(srcdir)/$(FPU_HOST_HEADER)
	cp $(srcdir)/$(FPU_HOST_HEADER) $@

## A 'normal' build shouldn't need to regenerate these
## so we only include them in maintainer mode

if MAINTAINER_MODE
$(i_all_c): m4/all.m4 $(I_M4_DEPS1)
	m4 -Dfile=$@ -I$(srcdir)/m4 all.m4 > $(srcdir)/$@

$(i_any_c): m4/any.m4 $(I_M4_DEPS1)
	m4 -Dfile=$@ -I$(srcdir)/m4 any.m4> $(srcdir)/$@

$(i_count_c): m4/count.m4 $(I_M4_DEPS1)
	m4 -Dfile=$@ -I$(srcdir)/m4 count.m4> $(srcdir)/$@

$(i_maxloc0_c): m4/maxloc0.m4 $(I_M4_DEPS0)
	m4 -Dfile=$@ -I$(srcdir)/m4 maxloc0.m4 > $(srcdir)/$@

$(i_maxloc1_c): m4/maxloc1.m4 $(I_M4_DEPS1)
	m4 -Dfile=$@ -I$(srcdir)/m4 maxloc1.m4 > $(srcdir)/$@

$(i_maxval_c): m4/maxval.m4 $(I_M4_DEPS1)
	m4 -Dfile=$@ -I$(srcdir)/m4 maxval.m4 > $(srcdir)/$@

$(i_minloc0_c): m4/minloc0.m4 $(I_M4_DEPS0)
	m4 -Dfile=$@ -I$(srcdir)/m4 minloc0.m4 > $(srcdir)/$@

$(i_minloc1_c): m4/minloc1.m4 $(I_M4_DEPS1)
	m4 -Dfile=$@ -I$(srcdir)/m4 minloc1.m4 > $(srcdir)/$@

$(i_minval_c): m4/minval.m4 $(I_M4_DEPS1)
	m4 -Dfile=$@ -I$(srcdir)/m4 minval.m4 > $(srcdir)/$@

$(i_product_c): m4/product.m4 $(I_M4_DEPS1)
	m4 -Dfile=$@ -I$(srcdir)/m4 product.m4 > $(srcdir)/$@

$(i_sum_c): m4/sum.m4 $(I_M4_DEPS1)
	m4 -Dfile=$@ -I$(srcdir)/m4 sum.m4 > $(srcdir)/$@

$(i_dotprod_c): m4/dotprod.m4 $(I_M4_DEPS)
	m4 -Dfile=$@ -I$(srcdir)/m4 dotprod.m4 > $(srcdir)/$@

$(i_dotprodl_c): m4/dotprodl.m4 $(I_M4_DEPS)
	m4 -Dfile=$@ -I$(srcdir)/m4 dotprodl.m4 > $(srcdir)/$@

$(i_dotprodc_c): m4/dotprodc.m4 $(I_M4_DEPS)
	m4 -Dfile=$@ -I$(srcdir)/m4 dotprodc.m4 > $(srcdir)/$@

$(i_matmul_c): m4/matmul.m4 $(I_M4_DEPS)
	m4 -Dfile=$@ -I$(srcdir)/m4 matmul.m4 > $(srcdir)/$@

$(i_matmull_c): m4/matmull.m4 $(I_M4_DEPS)
	m4 -Dfile=$@ -I$(srcdir)/m4 matmull.m4 > $(srcdir)/$@

$(i_transpose_c): m4/transpose.m4 $(I_M4_DEPS)
	m4 -Dfile=$@ -I$(srcdir)/m4 transpose.m4 > $(srcdir)/$@

$(i_shape_c): m4/shape.m4 $(I_M4_DEPS)
	m4 -Dfile=$@ -I$(srcdir)/m4 shape.m4 > $(srcdir)/$@

$(i_reshape_c): m4/reshape.m4 $(I_M4_DEPS)
	m4 -Dfile=$@ -I$(srcdir)/m4 reshape.m4 > $(srcdir)/$@

$(i_eoshift1_c): m4/eoshift1.m4 $(I_M4_DEPS)
	m4 -Dfile=$@ -I$(srcdir)/m4 eoshift1.m4 > $(srcdir)/$@

$(i_eoshift3_c): m4/eoshift3.m4 $(I_M4_DEPS)
	m4 -Dfile=$@ -I$(srcdir)/m4 eoshift3.m4 > $(srcdir)/$@

$(i_cshift1_c): m4/cshift1.m4 $(I_M4_DEPS)
	m4 -Dfile=$@ -I$(srcdir)/m4 cshift1.m4 > $(srcdir)/$@

$(in_pack_c): m4/in_pack.m4 $(I_M4_DEPS)
	m4 -Dfile=$@ -I$(srcdir)/m4 in_pack.m4 > $(srcdir)/$@

$(in_unpack_c): m4/in_unpack.m4 $(I_M4_DEPS)
	m4 -Dfile=$@ -I$(srcdir)/m4 in_unpack.m4 > $(srcdir)/$@

$(i_exponent_c): m4/exponent.m4 m4/mtype.m4
	m4 -Dfile=$@ -I$(srcdir)/m4 exponent.m4 > $(srcdir)/$@

$(i_fraction_c): m4/fraction.m4 m4/mtype.m4
	m4 -Dfile=$@ -I$(srcdir)/m4 fraction.m4 > $(srcdir)/$@

$(i_nearest_c): m4/nearest.m4 m4/mtype.m4
	m4 -Dfile=$@ -I$(srcdir)/m4 nearest.m4 > $(srcdir)/$@

$(i_set_exponent_c): m4/set_exponent.m4 m4/mtype.m4
	m4 -Dfile=$@ -I$(srcdir)/m4 set_exponent.m4 > $(srcdir)/$@

$(i_pow_c): m4/pow.m4 $(I_M4_DEPS)
	m4 -Dfile=$@ -I$(srcdir)/m4 pow.m4 > $(srcdir)/$@

$(gfor_built_specific_src): m4/specific.m4 m4/head.m4
	m4 -Dfile=$@ -I$(srcdir)/m4 specific.m4 > $(srcdir)/$@

$(gfor_built_specific2_src): m4/specific2.m4 m4/head.m4
	m4 -Dfile=$@ -I$(srcdir)/m4 specific2.m4 > $(srcdir)/$@
## end of maintainer mode only rules
endif

EXTRA_DIST = $(m4_files)
