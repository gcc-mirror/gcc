dnl Copyright (C) 2021-2025 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl This program is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY, to the extent permitted by law; without
dnl even the implied warranty of MERCHANTABILITY or FITNESS FOR A
dnl PARTICULAR PURPOSE.

m4_include(../config/acx.m4)
m4_include(../config/no-executables.m4)
m4_include(../config/enable.m4)
m4_include(../config/tls.m4)
m4_include(../config/bitfields.m4)

m4_include(../libtool.m4)
dnl The lines below arrange for aclocal not to bring an installed
dnl libtool.m4 into aclocal.m4, while still arranging for automake to
dnl add a definition of LIBTOOL to Makefile.in.
ifelse(yes,no,[
AC_DEFUN([AM_PROG_LIBTOOL],)
AC_DEFUN([AC_LIBTOOL_DLOPEN],)
AC_DEFUN([AC_LIBLTDL_CONVENIENCE],)
AC_SUBST(LIBTOOL)
])

dnl Check whether we have a __float128 and _Float128 type
dnl Unashamedly plagiarized from libgfortran.

AC_DEFUN([LIBGCOBOL_CHECK_FLOAT128], [
  LIBQUADSPEC=
  LIBQUADLIB=
  LIBQUADLIB_DEP=
  LIBQUADINCLUDE=
  USE_IEC_60559=no

  if test "x$enable_libquadmath_support" = "xno"; then
    if test "x$have_iec_60559_libc_support" = "xyes"; then
      AC_DEFINE(USE_IEC_60559, 1, [Define if IEC 60559 *f128 APIs should be used for _Float128.])
    fi
  else

  AC_CACHE_CHECK([whether we have a usable _Float128 type],
                 libgcob_cv_have_float128, [
   GCC_TRY_COMPILE_OR_LINK([
#if __LDBL_MANT_DIG__ == 113 && __LDBL_MIN_EXP__ == -16381
#error "long double is IEEE quad, no need for libquadmath"
#endif

    _Float128 foo (_Float128 x)
    {
     _Complex _Float128 z1, z2;

     z1 = x;
     z2 = x / 7.F128;
     z2 /= z1;

     return __real__ z2;
    }

    _Float128 bar (_Float128 x)
    {
      return x * __builtin_huge_valf128 ();
    }

    __float128 baz (__float128 x)
    {
      return x * __builtin_huge_valf128 ();
    }
  ],[
    foo (1.2F128);
    bar (1.2F128);
    baz (1.2F128);
    foo (1.2Q);
    bar (1.2Q);
    baz (1.2Q);
  ],[
    libgcob_cv_have_float128=yes
  ],[
    libgcob_cv_have_float128=no
])])

    if test "x$have_iec_60559_libc_support$enable_libquadmath_support$libgcob_cv_have_float128" = xyesdefaultyes; then
      USE_IEC_60559=yes
    fi

  if test "x$libgcob_cv_have_float128" = xyes; then

    if test "x$USE_IEC_60559" = xyes; then
      AC_DEFINE(USE_IEC_60559, 1, [Define if IEC 60559 *f128 APIs should be used for _Float128.])
    else
      AC_DEFINE(USE_QUADMATH, 1, [Define if *q APIs should be used for __float128.])
    fi
    AC_DEFINE(HAVE_FLOAT128, 1, [Define if target has usable _Float128 and __float128 types.])

    if test "x$USE_IEC_60559" != xyes; then
      dnl Check whether -Wl,--as-needed resp. -Wl,-zignore is supported
      dnl 
      dnl Turn warnings into error to avoid testsuite breakage.  So enable
      dnl AC_LANG_WERROR, but there's currently (autoconf 2.64) no way to turn
      dnl it off again.  As a workaround, save and restore werror flag like
      dnl AC_PATH_XTRA.
      dnl Cf. http://gcc.gnu.org/ml/gcc-patches/2010-05/msg01889.html
      ac_xsave_[]_AC_LANG_ABBREV[]_werror_flag=$ac_[]_AC_LANG_ABBREV[]_werror_flag
      AC_CACHE_CHECK([whether --as-needed/-z ignore works],
	[libgcob_cv_have_as_needed],
	[
	# Test for native Solaris options first.
	# No whitespace after -z to pass it through -Wl.
	libgcob_cv_as_needed_option="-zignore"
	libgcob_cv_no_as_needed_option="-zrecord"
	save_LDFLAGS="$LDFLAGS"
	LDFLAGS="$LDFLAGS -Wl,$libgcob_cv_as_needed_option -lm -Wl,$libgcob_cv_no_as_needed_option"
	libgcob_cv_have_as_needed=no
	AC_LANG_WERROR
	AC_LINK_IFELSE([AC_LANG_PROGRAM([])],
		       [libgcob_cv_have_as_needed=yes],
		       [libgcob_cv_have_as_needed=no])
	LDFLAGS="$save_LDFLAGS"
	if test "x$libgcob_cv_have_as_needed" = xno; then
	  libgcob_cv_as_needed_option="--as-needed"
	  libgcob_cv_no_as_needed_option="--no-as-needed"
	  save_LDFLAGS="$LDFLAGS"
	  LDFLAGS="$LDFLAGS -Wl,$libgcob_cv_as_needed_option -lm -Wl,$libgcob_cv_no_as_needed_option"
	  libgcob_cv_have_as_needed=no
	  AC_LANG_WERROR
	  AC_LINK_IFELSE([AC_LANG_PROGRAM([])],
			 [libgcob_cv_have_as_needed=yes],
			 [libgcob_cv_have_as_needed=no])
	  LDFLAGS="$save_LDFLAGS"
	fi
	ac_[]_AC_LANG_ABBREV[]_werror_flag=$ac_xsave_[]_AC_LANG_ABBREV[]_werror_flag
      ])

      dnl Determine -Bstatic ... -Bdynamic etc. support from gfortran -### stderr.
      touch conftest1.$ac_objext conftest2.$ac_objext
      LQUADMATH=-lquadmath
      $CXX -static-libgcobol -### -o conftest \
	  conftest1.$ac_objext -lgcobol conftest2.$ac_objext 2>&1 >/dev/null \
	  | grep "conftest1.$ac_objext.*conftest2.$ac_objext" > conftest.cmd
      if grep "conftest1.$ac_objext.* -Bstatic -lgcobol -Bdynamic .*conftest2.$ac_objext" \
	 conftest.cmd >/dev/null 2>&1; then
	LQUADMATH="%{static-libquadmath:-Bstatic} -lquadmath %{static-libquadmath:-Bdynamic}"
      elif grep "conftest1.$ac_objext.* -bstatic -lgcobol -bdynamic .*conftest2.$ac_objext" \
	  conftest.cmd >/dev/null 2>&1; then
	LQUADMATH="%{static-libquadmath:-bstatic} -lquadmath %{static-libquadmath:-bdynamic}"
      elif grep "conftest1.$ac_objext.* -aarchive_shared -lgcobol -adefault .*conftest2.$ac_objext" \
	  conftest.cmd >/dev/null 2>&1; then
	LQUADMATH="%{static-libquadmath:-aarchive_shared} -lquadmath %{static-libquadmath:-adefault}"
      elif grep "conftest1.$ac_objext.*libgcobol.a .*conftest2.$ac_objext" \
         conftest.cmd >/dev/null 2>&1; then
	LQUADMATH="%{static-libquadmath:libquadmath.a%s;:-lquadmath}"
      fi
      rm -f conftest1.$ac_objext conftest2.$ac_objext conftest conftest.cmd

      if test "x$libgcob_cv_have_as_needed" = xyes; then
	if test "x$USE_IEC_60559" = xyes; then
	  LIBQUADSPEC="$libgcob_cv_as_needed_option $LQUADMATH $libgcob_cv_no_as_needed_option"
	else
	  LIBQUADSPEC="%{static-libgcobol:$libgcob_cv_as_needed_option} $LQUADMATH %{static-libgcobol:$libgcob_cv_no_as_needed_option}"
	fi
      else
	LIBQUADSPEC="$LQUADMATH"
      fi
      if test -f ../libquadmath/libquadmath.la; then
	LIBQUADLIB=../libquadmath/libquadmath.la
	LIBQUADLIB_DEP=../libquadmath/libquadmath.la
	LIBQUADINCLUDE='-I$(srcdir)/../libquadmath'
      else
	LIBQUADLIB="-lquadmath"
      fi
    fi
  else
    if test "x$USE_IEC_60559" = xyes; then
      AC_DEFINE(USE_IEC_60559, 1, [Define if IEC 60559 *f128 APIs should be used for _Float128.])
    fi
  fi
  
  fi

  dnl For the spec file
  AC_SUBST(LIBQUADSPEC)
  AC_SUBST(LIBQUADLIB)
  AC_SUBST(LIBQUADLIB_DEP)
  AC_SUBST(LIBQUADINCLUDE)
  AC_SUBST(USE_IEC_60559)
])
