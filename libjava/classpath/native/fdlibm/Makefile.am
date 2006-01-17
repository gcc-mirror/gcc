noinst_LTLIBRARIES = libfdlibm.la 

libfdlibm_la_SOURCES =  \
			dtoa.c \
			e_acos.c \
			e_asin.c \
			e_atan2.c \
			e_exp.c \
			e_fmod.c \
			e_log.c \
			e_pow.c \
			e_remainder.c \
			e_rem_pio2.c \
			e_scalb.c \
			e_sqrt.c \
			fdlibm.h \
			ieeefp.h \
			k_cos.c \
			k_rem_pio2.c \
			k_sin.c \
			k_tan.c \
			mprec.c \
			mprec.h \
			s_atan.c \
			s_ceil.c \
			s_copysign.c \
			s_cos.c \
			s_fabs.c \
			sf_fabs.c \
			s_finite.c \
			s_floor.c \
			sf_rint.c \
			s_rint.c \
			s_scalbn.c \
			s_sin.c \
			s_tan.c \
			strtod.c \
			w_acos.c \
			w_asin.c \
			w_atan2.c \
			w_exp.c \
			w_fmod.c \
			w_log.c \
			w_pow.c \
			w_remainder.c \
			w_sqrt.c \
			namespace.h

AM_LDFLAGS = @CLASSPATH_MODULE@ 

# We just want the standard flags for fdlibm since it is an upstream lib
# and our normal -pedantic -Wall -Werror breaks this lib. So no AM_CFLAGS.
# We also don't need extra includes, so no AM_CPPFLAGS either.
