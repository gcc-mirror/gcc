dnl
dnl Unconditionally define a preprocessor macro, translating the shell
dnl macro from yes/no to 1/0.
dnl
AC_DEFUN([LIBAT_DEFINE_YESNO], [
  yesno=`echo $2 | tr 'yesno' '1  0 '`
  AC_DEFINE_UNQUOTED([$1], $yesno, [$3])
])
dnl
dnl Iterate over all of the modes we're prepared to check.
dnl
AC_DEFUN([LIBAT_FORALL_MODES],
  [$1(QI,1)
  $1(HI,2)
  $1(SI,4)
  $1(DI,8)
  $1(TI,16)]
)
dnl
dnl Check for builtin types by mode.
dnl
dnl A less interesting of size checking than autoconf normally provides.
dnl We know that gcc always provides <stdint.h>, but we don't often
dnl provide a builtin type for TImode.
dnl
AC_DEFUN([LIBAT_HAVE_INT_MODE],[
  AC_CACHE_CHECK([for $2 byte integer],[libat_cv_have_mode_$1],
    [AC_COMPILE_IFELSE([AC_LANG_SOURCE([int x __attribute__((mode($1)));])],
      [libat_cv_have_mode_$1=yes],[libat_cv_have_mode_$1=no])])
  LIBAT_DEFINE_YESNO([HAVE_INT$2], [$libat_cv_have_mode_$1],
      [Have support for $2 byte integers.])
  if test x$libat_cv_have_mode_$1 = xyes; then
    SIZES="$SIZES $2"
  fi
])
dnl
dnl Check for atomic builtins.
dnl See:
dnl http://gcc.gnu.org/onlinedocs/gcc/_005f_005fatomic-Builtins.html
dnl
dnl This checks to see if the host supports the compiler-generated
dnl builtins for atomic operations for various integral sizes.
dnl
AC_DEFUN([LIBAT_TEST_ATOMIC_INIT],[
  # Do link tests if possible, instead asm tests, limited to some platforms
  # see discussion in PR target/40134, PR libstdc++/40133 and the thread
  # starting at http://gcc.gnu.org/ml/gcc-patches/2009-07/msg00322.html
  atomic_builtins_link_tests=no
  if test x$gcc_no_link != xyes; then
    # Can do link tests. Limit to some tested platforms
    case "$host" in
      *-*-linux* | *-*-uclinux* | *-*-kfreebsd*-gnu | *-*-gnu*)
	atomic_builtins_link_tests=yes
	;;
    esac
  fi
])
AC_DEFUN([LIBAT_TEST_ATOMIC_BUILTIN],[
  AC_CACHE_CHECK([$1],[$2],[
    AC_LANG_CONFTEST([AC_LANG_PROGRAM([],[$3])])
    if test x$atomic_builtins_link_tests = xyes; then
      if AC_TRY_EVAL(ac_link); then
        eval $2=yes
      else
        eval $2=no
      fi
    else
      old_CFLAGS="$CFLAGS"
      # Compile unoptimized.
      CFLAGS="$CFLAGS -O0 -S"
      if AC_TRY_EVAL(ac_compile); then
        if grep __atomic_ conftest.s >/dev/null 2>&1 ; then
	  eval $2=no
        else
	  eval $2=yes
        fi
      else
        eval $2=no
      fi
      CFLAGS="$old_CFLAGS"
    fi
    rm -f conftest*
  ])
])

dnl
dnl Test if we have __atomic_load and __atomic_store for mode $1, size $2
dnl
AC_DEFUN([LIBAT_HAVE_ATOMIC_LOADSTORE],[
  LIBAT_TEST_ATOMIC_BUILTIN([for __atomic_load/store for size $2],
    [libat_cv_have_at_ldst_$2],
    [typedef int T __attribute__((mode($1)));
     T *x; volatile T sink; asm("" : "=g"(x));
     sink = __atomic_load_n(x, 0);
     __atomic_store_n(x, sink, 0);])
  LIBAT_DEFINE_YESNO([HAVE_ATOMIC_LDST_$2], [$libat_cv_have_at_ldst_$2],
	[Have __atomic_load/store for $2 byte integers.])
  AH_BOTTOM([#define MAYBE_HAVE_ATOMIC_LDST_$2 HAVE_ATOMIC_LDST_$2])
  AH_BOTTOM([#define FAST_ATOMIC_LDST_$2 HAVE_ATOMIC_LDST_$2])
])

dnl
dnl Test if we have __atomic_test_and_set for mode $1, size $2
dnl
AC_DEFUN([LIBAT_HAVE_ATOMIC_TAS],[
  LIBAT_TEST_ATOMIC_BUILTIN([for __atomic_test_and_set for size $2],
    [libat_cv_have_at_tas_$2],
    [typedef int T __attribute__((mode($1))); T *x; asm("" : "=g"(x));
     __atomic_test_and_set(x, 0);])
  LIBAT_DEFINE_YESNO([HAVE_ATOMIC_TAS_$2], [$libat_cv_have_at_tas_$2],
	[Have __atomic_test_and_set for $2 byte integers.])
  AH_BOTTOM([#define MAYBE_HAVE_ATOMIC_TAS_$2 HAVE_ATOMIC_TAS_$2])
])

dnl
dnl Test if we have __atomic_exchange for mode $1, size $2
dnl
AC_DEFUN([LIBAT_HAVE_ATOMIC_EXCHANGE],[
  LIBAT_TEST_ATOMIC_BUILTIN([for __atomic_exchange for size $2],
    [libat_cv_have_at_exch_$2],
    [typedef int T __attribute__((mode($1))); T *x; asm("" : "=g"(x));
     __atomic_exchange_n(x, 0, 0);])
  LIBAT_DEFINE_YESNO([HAVE_ATOMIC_EXCHANGE_$2], [$libat_cv_have_at_exch_$2],
	[Have __atomic_exchange for $2 byte integers.])
  AH_BOTTOM([#define MAYBE_HAVE_ATOMIC_EXCHANGE_$2 HAVE_ATOMIC_EXCHANGE_$2])
])

dnl
dnl Test if we have __atomic_compare_exchange for mode $1, size $2
dnl
AC_DEFUN([LIBAT_HAVE_ATOMIC_CAS],[
  LIBAT_TEST_ATOMIC_BUILTIN([for __atomic_compare_exchange for size $2],
    [libat_cv_have_at_cas_$2],
    [typedef int T __attribute__((mode($1))); T *x, *y;
     asm("" : "=g"(x), "=g"(y));
     __atomic_compare_exchange_n(x, y, 0, 0, 0, 0);])
  LIBAT_DEFINE_YESNO([HAVE_ATOMIC_CAS_$2], [$libat_cv_have_at_cas_$2],
	[Have __atomic_compare_exchange for $2 byte integers.])
  AH_BOTTOM([#define MAYBE_HAVE_ATOMIC_CAS_$2 HAVE_ATOMIC_CAS_$2])
])

dnl
dnl Test if we have __atomic_fetch_add for mode $1, size $2
dnl
AC_DEFUN([LIBAT_HAVE_ATOMIC_FETCH_ADD],[
  LIBAT_TEST_ATOMIC_BUILTIN([for __atomic_fetch_add for size $2],
    [libat_cv_have_at_fadd_$2],
    [typedef int T __attribute__((mode($1))); T *x, y;
     asm("" : "=g"(x), "=g"(y));
     __atomic_fetch_add (x, y, 0);
     __atomic_add_fetch (x, y, 0);])
  LIBAT_DEFINE_YESNO([HAVE_ATOMIC_FETCH_ADD_$2], [$libat_cv_have_at_fadd_$2],
	[Have __atomic_fetch_add for $2 byte integers.])
  AH_BOTTOM([#define MAYBE_HAVE_ATOMIC_FETCH_ADD_$2 HAVE_ATOMIC_FETCH_ADD_$2])
])

dnl
dnl Test if we have __atomic_fetch_op for all op for mode $1, size $2
dnl
AC_DEFUN([LIBAT_HAVE_ATOMIC_FETCH_OP],[
  LIBAT_TEST_ATOMIC_BUILTIN([for __atomic_fetch_op for size $2],
    [libat_cv_have_at_fop_$2],
    [typedef int T __attribute__((mode($1))); T *x, y;
     asm("" : "=g"(x), "=g"(y));
     __atomic_fetch_add (x, y, 0); __atomic_add_fetch (x, y, 0);
     __atomic_fetch_sub (x, y, 0); __atomic_sub_fetch (x, y, 0);
     __atomic_fetch_and (x, y, 0); __atomic_and_fetch (x, y, 0);
     __atomic_fetch_nand (x, y, 0); __atomic_nand_fetch (x, y, 0);
     __atomic_fetch_xor (x, y, 0); __atomic_xor_fetch (x, y, 0);
     __atomic_fetch_or (x, y, 0);  __atomic_or_fetch (x, y, 0); ])
  LIBAT_DEFINE_YESNO([HAVE_ATOMIC_FETCH_OP_$2], [$libat_cv_have_at_fop_$2],
	[Have __atomic_fetch_op for all op for $2 byte integers.])
  AH_BOTTOM([#define MAYBE_HAVE_ATOMIC_FETCH_OP_$2 HAVE_ATOMIC_FETCH_OP_$2])
])

dnl
dnl Test for the size of the target word.
dnl
AC_DEFUN([LIBAT_WORDSIZE],[
  AC_CACHE_CHECK([for the word size],[libat_cv_wordsize],
    [AC_COMPUTE_INT(libat_cv_wordsize,
      [sizeof(word)], [typedef int word __attribute__((mode(word)));],
      AC_ERROR([Could not determine word size.]))])
  AC_DEFINE_UNQUOTED(WORDSIZE, $libat_cv_wordsize,
    [The word size in bytes of the machine.])
])

dnl
dnl Check whether the target supports the ifunc attribute.
dnl
AC_DEFUN([LIBAT_CHECK_IFUNC], [
  AC_CACHE_CHECK([whether the target supports the ifunc attribute],
		 libat_cv_have_ifunc, [
  save_CFLAGS="$CFLAGS"
  CFLAGS="$CFLAGS -Werror"
  AC_TRY_LINK([
    int foo_alt(void) { return 0; }
    typedef int F (void);
    F *foo_sel(void) { return foo_alt; }
    int foo(void) __attribute__((ifunc("foo_sel")));],
    [return foo();], libat_cv_have_ifunc=yes, libat_cv_have_ifunc=no)])
  LIBAT_DEFINE_YESNO([HAVE_IFUNC], [$libat_cv_have_ifunc],
      [Define to 1 if the target supports __attribute__((ifunc(...))).])
])

dnl ----------------------------------------------------------------------
dnl This whole bit snagged from libitm.

dnl Check whether the target supports hidden visibility.
AC_DEFUN([LIBAT_CHECK_ATTRIBUTE_VISIBILITY], [
  AC_CACHE_CHECK([whether the target supports hidden visibility],
		 libat_cv_have_attribute_visibility, [
  save_CFLAGS="$CFLAGS"
  CFLAGS="$CFLAGS -Werror"
  AC_TRY_COMPILE([void __attribute__((visibility("hidden"))) foo(void) { }],
		 [], libat_cv_have_attribute_visibility=yes,
		 libat_cv_have_attribute_visibility=no)
  CFLAGS="$save_CFLAGS"])
  if test $libat_cv_have_attribute_visibility = yes; then
    AC_DEFINE(HAVE_ATTRIBUTE_VISIBILITY, 1,
      [Define to 1 if the target supports __attribute__((visibility(...))).])
  fi])

dnl Check whether the target supports dllexport
AC_DEFUN([LIBAT_CHECK_ATTRIBUTE_DLLEXPORT], [
  AC_CACHE_CHECK([whether the target supports dllexport],
		 libat_cv_have_attribute_dllexport, [
  save_CFLAGS="$CFLAGS"
  CFLAGS="$CFLAGS -Werror"
  AC_TRY_COMPILE([void __attribute__((dllexport)) foo(void) { }],
		 [], libat_cv_have_attribute_dllexport=yes,
		 libat_cv_have_attribute_dllexport=no)
  CFLAGS="$save_CFLAGS"])
  if test $libat_cv_have_attribute_dllexport = yes; then
    AC_DEFINE(HAVE_ATTRIBUTE_DLLEXPORT, 1,
      [Define to 1 if the target supports __attribute__((dllexport)).])
  fi])

dnl Check whether the target supports symbol aliases.
AC_DEFUN([LIBAT_CHECK_ATTRIBUTE_ALIAS], [
  AC_CACHE_CHECK([whether the target supports symbol aliases],
		 libat_cv_have_attribute_alias, [
  AC_TRY_LINK([
void foo(void) { }
extern void bar(void) __attribute__((alias("foo")));],
    [bar();], libat_cv_have_attribute_alias=yes, libat_cv_have_attribute_alias=no)])
  if test $libat_cv_have_attribute_alias = yes; then
    AC_DEFINE(HAVE_ATTRIBUTE_ALIAS, 1,
      [Define to 1 if the target supports __attribute__((alias(...))).])
  fi])

dnl ----------------------------------------------------------------------
dnl This whole bit snagged from libstdc++-v3.

dnl
dnl LIBAT_ENABLE
dnl    (FEATURE, DEFAULT, HELP-ARG, HELP-STRING)
dnl    (FEATURE, DEFAULT, HELP-ARG, HELP-STRING, permit a|b|c)
dnl    (FEATURE, DEFAULT, HELP-ARG, HELP-STRING, SHELL-CODE-HANDLER)
dnl
dnl See docs/html/17_intro/configury.html#enable for documentation.
dnl
m4_define([LIBAT_ENABLE],[dnl
m4_define([_g_switch],[--enable-$1])dnl
m4_define([_g_help],[AC_HELP_STRING(_g_switch$3,[$4 @<:@default=$2@:>@])])dnl
 AC_ARG_ENABLE($1,_g_help,
  m4_bmatch([$5],
   [^permit ],
     [[
      case "$enableval" in
       m4_bpatsubst([$5],[permit ])) ;;
       *) AC_MSG_ERROR(Unknown argument to enable/disable $1) ;;
          dnl Idea for future:  generate a URL pointing to
          dnl "onlinedocs/configopts.html#whatever"
      esac
     ]],
   [^$],
     [[
      case "$enableval" in
       yes|no) ;;
       *) AC_MSG_ERROR(Argument to enable/disable $1 must be yes or no) ;;
      esac
     ]],
   [[$5]]),
  [enable_]m4_bpatsubst([$1],-,_)[=][$2])
m4_undefine([_g_switch])dnl
m4_undefine([_g_help])dnl
])

dnl
dnl If GNU ld is in use, check to see if tricky linker opts can be used.  If
dnl the native linker is in use, all variables will be defined to something
dnl safe (like an empty string).
dnl
dnl Defines:
dnl  SECTION_LDFLAGS='-Wl,--gc-sections' if possible
dnl  OPT_LDFLAGS='-Wl,-O1' if possible
dnl  LD (as a side effect of testing)
dnl Sets:
dnl  with_gnu_ld
dnl  libat_ld_is_gold (possibly)
dnl  libat_gnu_ld_version (possibly)
dnl
dnl The last will be a single integer, e.g., version 1.23.45.0.67.89 will
dnl set libat_gnu_ld_version to 12345.  Zeros cause problems.
dnl
AC_DEFUN([LIBAT_CHECK_LINKER_FEATURES], [
  # If we're not using GNU ld, then there's no point in even trying these
  # tests.  Check for that first.  We should have already tested for gld
  # by now (in libtool), but require it now just to be safe...
  test -z "$SECTION_LDFLAGS" && SECTION_LDFLAGS=''
  test -z "$OPT_LDFLAGS" && OPT_LDFLAGS=''
  AC_REQUIRE([AC_PROG_LD])
  AC_REQUIRE([AC_PROG_AWK])

  # The name set by libtool depends on the version of libtool.  Shame on us
  # for depending on an impl detail, but c'est la vie.  Older versions used
  # ac_cv_prog_gnu_ld, but now it's lt_cv_prog_gnu_ld, and is copied back on
  # top of with_gnu_ld (which is also set by --with-gnu-ld, so that actually
  # makes sense).  We'll test with_gnu_ld everywhere else, so if that isn't
  # set (hence we're using an older libtool), then set it.
  if test x${with_gnu_ld+set} != xset; then
    if test x${ac_cv_prog_gnu_ld+set} != xset; then
      # We got through "ac_require(ac_prog_ld)" and still not set?  Huh?
      with_gnu_ld=no
    else
      with_gnu_ld=$ac_cv_prog_gnu_ld
    fi
  fi

  # Start by getting the version number.  I think the libtool test already
  # does some of this, but throws away the result.
  libat_ld_is_gold=no
  if $LD --version 2>/dev/null | grep 'GNU gold'> /dev/null 2>&1; then
    libat_ld_is_gold=yes
  fi
  changequote(,)
  ldver=`$LD --version 2>/dev/null |
         sed -e 's/GNU gold /GNU ld /;s/GNU ld version /GNU ld /;s/GNU ld ([^)]*) /GNU ld /;s/GNU ld \([0-9.][0-9.]*\).*/\1/; q'`
  changequote([,])
  libat_gnu_ld_version=`echo $ldver | \
         $AWK -F. '{ if (NF<3) [$]3=0; print ([$]1*100+[$]2)*100+[$]3 }'`

  # Set --gc-sections.
  if test "$with_gnu_ld" = "notbroken"; then
    # GNU ld it is!  Joy and bunny rabbits!

    # All these tests are for C++; save the language and the compiler flags.
    # Need to do this so that g++ won't try to link in libstdc++
    ac_test_CFLAGS="${CFLAGS+set}"
    ac_save_CFLAGS="$CFLAGS"
    CFLAGS='-x c++  -Wl,--gc-sections'

    # Check for -Wl,--gc-sections
    # XXX This test is broken at the moment, as symbols required for linking
    # are now in libsupc++ (not built yet).  In addition, this test has
    # cored on solaris in the past.  In addition, --gc-sections doesn't
    # really work at the moment (keeps on discarding used sections, first
    # .eh_frame and now some of the glibc sections for iconv).
    # Bzzzzt.  Thanks for playing, maybe next time.
    AC_MSG_CHECKING([for ld that supports -Wl,--gc-sections])
    AC_TRY_RUN([
     int main(void)
     {
       try { throw 1; }
       catch (...) { };
       return 0;
     }
    ], [ac_sectionLDflags=yes],[ac_sectionLDflags=no], [ac_sectionLDflags=yes])
    if test "$ac_test_CFLAGS" = set; then
      CFLAGS="$ac_save_CFLAGS"
    else
      # this is the suspicious part
      CFLAGS=''
    fi
    if test "$ac_sectionLDflags" = "yes"; then
      SECTION_LDFLAGS="-Wl,--gc-sections $SECTION_LDFLAGS"
    fi
    AC_MSG_RESULT($ac_sectionLDflags)
  fi

  # Set linker optimization flags.
  if test x"$with_gnu_ld" = x"yes"; then
    OPT_LDFLAGS="-Wl,-O1 $OPT_LDFLAGS"
  fi

  AC_SUBST(SECTION_LDFLAGS)
  AC_SUBST(OPT_LDFLAGS)
])


dnl
dnl If GNU ld is in use, check to see if tricky linker opts can be used.  If
dnl the native linker is in use, all variables will be defined to something
dnl safe (like an empty string).
dnl
dnl Defines:
dnl  SECTION_LDFLAGS='-Wl,--gc-sections' if possible
dnl  OPT_LDFLAGS='-Wl,-O1' if possible
dnl  LD (as a side effect of testing)
dnl Sets:
dnl  with_gnu_ld
dnl  libat_ld_is_gold (possibly)
dnl  libat_gnu_ld_version (possibly)
dnl
dnl The last will be a single integer, e.g., version 1.23.45.0.67.89 will
dnl set libat_gnu_ld_version to 12345.  Zeros cause problems.
dnl
AC_DEFUN([LIBAT_CHECK_LINKER_FEATURES], [
  # If we're not using GNU ld, then there's no point in even trying these
  # tests.  Check for that first.  We should have already tested for gld
  # by now (in libtool), but require it now just to be safe...
  test -z "$SECTION_LDFLAGS" && SECTION_LDFLAGS=''
  test -z "$OPT_LDFLAGS" && OPT_LDFLAGS=''
  AC_REQUIRE([AC_PROG_LD])
  AC_REQUIRE([AC_PROG_AWK])

  # The name set by libtool depends on the version of libtool.  Shame on us
  # for depending on an impl detail, but c'est la vie.  Older versions used
  # ac_cv_prog_gnu_ld, but now it's lt_cv_prog_gnu_ld, and is copied back on
  # top of with_gnu_ld (which is also set by --with-gnu-ld, so that actually
  # makes sense).  We'll test with_gnu_ld everywhere else, so if that isn't
  # set (hence we're using an older libtool), then set it.
  if test x${with_gnu_ld+set} != xset; then
    if test x${ac_cv_prog_gnu_ld+set} != xset; then
      # We got through "ac_require(ac_prog_ld)" and still not set?  Huh?
      with_gnu_ld=no
    else
      with_gnu_ld=$ac_cv_prog_gnu_ld
    fi
  fi

  # Start by getting the version number.  I think the libtool test already
  # does some of this, but throws away the result.
  libat_ld_is_gold=no
  if $LD --version 2>/dev/null | grep 'GNU gold'> /dev/null 2>&1; then
    libat_ld_is_gold=yes
  fi
  changequote(,)
  ldver=`$LD --version 2>/dev/null |
         sed -e 's/GNU gold /GNU ld /;s/GNU ld version /GNU ld /;s/GNU ld ([^)]*) /GNU ld /;s/GNU ld \([0-9.][0-9.]*\).*/\1/; q'`
  changequote([,])
  libat_gnu_ld_version=`echo $ldver | \
         $AWK -F. '{ if (NF<3) [$]3=0; print ([$]1*100+[$]2)*100+[$]3 }'`

  # Set --gc-sections.
  if test "$with_gnu_ld" = "notbroken"; then
    # GNU ld it is!  Joy and bunny rabbits!

    # All these tests are for C++; save the language and the compiler flags.
    # Need to do this so that g++ won't try to link in libstdc++
    ac_test_CFLAGS="${CFLAGS+set}"
    ac_save_CFLAGS="$CFLAGS"
    CFLAGS='-x c++  -Wl,--gc-sections'

    # Check for -Wl,--gc-sections
    # XXX This test is broken at the moment, as symbols required for linking
    # are now in libsupc++ (not built yet).  In addition, this test has
    # cored on solaris in the past.  In addition, --gc-sections doesn't
    # really work at the moment (keeps on discarding used sections, first
    # .eh_frame and now some of the glibc sections for iconv).
    # Bzzzzt.  Thanks for playing, maybe next time.
    AC_MSG_CHECKING([for ld that supports -Wl,--gc-sections])
    AC_TRY_RUN([
     int main(void)
     {
       try { throw 1; }
       catch (...) { };
       return 0;
     }
    ], [ac_sectionLDflags=yes],[ac_sectionLDflags=no], [ac_sectionLDflags=yes])
    if test "$ac_test_CFLAGS" = set; then
      CFLAGS="$ac_save_CFLAGS"
    else
      # this is the suspicious part
      CFLAGS=''
    fi
    if test "$ac_sectionLDflags" = "yes"; then
      SECTION_LDFLAGS="-Wl,--gc-sections $SECTION_LDFLAGS"
    fi
    AC_MSG_RESULT($ac_sectionLDflags)
  fi

  # Set linker optimization flags.
  if test x"$with_gnu_ld" = x"yes"; then
    OPT_LDFLAGS="-Wl,-O1 $OPT_LDFLAGS"
  fi

  AC_SUBST(SECTION_LDFLAGS)
  AC_SUBST(OPT_LDFLAGS)
])


dnl
dnl Add version tags to symbols in shared library (or not), additionally
dnl marking other symbols as private/local (or not).
dnl
dnl --enable-symvers=style adds a version script to the linker call when
dnl       creating the shared library.  The choice of version script is
dnl       controlled by 'style'.
dnl --disable-symvers does not.
dnl  +  Usage:  LIBAT_ENABLE_SYMVERS[(DEFAULT)]
dnl       Where DEFAULT is either 'yes' or 'no'.  Passing `yes' tries to
dnl       choose a default style based on linker characteristics.  Passing
dnl       'no' disables versioning.
dnl
AC_DEFUN([LIBAT_ENABLE_SYMVERS], [

LIBAT_ENABLE(symvers,yes,[=STYLE],
  [enables symbol versioning of the shared library],
  [permit yes|no|gnu*|sun])

# If we never went through the LIBAT_CHECK_LINKER_FEATURES macro, then we
# don't know enough about $LD to do tricks...
AC_REQUIRE([LIBAT_CHECK_LINKER_FEATURES])

# Turn a 'yes' into a suitable default.
if test x$enable_symvers = xyes ; then
  # FIXME  The following test is too strict, in theory.
  if test $enable_shared = no || test "x$LD" = x; then
    enable_symvers=no
  else
    if test $with_gnu_ld = yes ; then
      enable_symvers=gnu
    else
      case ${target_os} in
        # Sun symbol versioning exists since Solaris 2.5.
        solaris2.[[5-9]]* | solaris2.1[[0-9]]*)
          enable_symvers=sun ;;
        *)
          enable_symvers=no ;;
      esac
    fi
  fi
fi

# Check if 'sun' was requested on non-Solaris 2 platforms.
if test x$enable_symvers = xsun ; then
  case ${target_os} in
    solaris2*)
      # All fine.
      ;;
    *)
      # Unlikely to work.
      AC_MSG_WARN([=== You have requested Sun symbol versioning, but])
      AC_MSG_WARN([=== you are not targetting Solaris 2.])
      AC_MSG_WARN([=== Symbol versioning will be disabled.])
      enable_symvers=no
      ;;
  esac
fi

# Check to see if libgcc_s exists, indicating that shared libgcc is possible.
if test $enable_symvers != no; then
  AC_MSG_CHECKING([for shared libgcc])
  ac_save_CFLAGS="$CFLAGS"
  CFLAGS=' -lgcc_s'
  AC_TRY_LINK(, [return 0;], libat_shared_libgcc=yes, libat_shared_libgcc=no)
  CFLAGS="$ac_save_CFLAGS"
  if test $libat_shared_libgcc = no; then
    cat > conftest.c <<EOF
int main (void) { return 0; }
EOF
changequote(,)dnl
    libat_libgcc_s_suffix=`${CC-cc} $CFLAGS $CPPFLAGS $LDFLAGS \
			     -shared -shared-libgcc -o conftest.so \
			     conftest.c -v 2>&1 >/dev/null \
			     | sed -n 's/^.* -lgcc_s\([^ ]*\) .*$/\1/p'`
changequote([,])dnl
    rm -f conftest.c conftest.so
    if test x${libat_libgcc_s_suffix+set} = xset; then
      CFLAGS=" -lgcc_s$libat_libgcc_s_suffix"
      AC_TRY_LINK(, [return 0;], libat_shared_libgcc=yes)
      CFLAGS="$ac_save_CFLAGS"
    fi
  fi
  AC_MSG_RESULT($libat_shared_libgcc)
fi

# For GNU ld, we need at least this version.  The format is described in
# LIBAT_CHECK_LINKER_FEATURES above.
libat_min_gnu_ld_version=21400
# XXXXXXXXXXX libat_gnu_ld_version=21390

# Check to see if unspecified "yes" value can win, given results above.
# Change "yes" into either "no" or a style name.
if test $enable_symvers != no && test $libat_shared_libgcc = yes; then
  if test $with_gnu_ld = yes; then
    if test $libat_gnu_ld_version -ge $libat_min_gnu_ld_version ; then
      enable_symvers=gnu
    elif test $libat_ld_is_gold = yes ; then
      enable_symvers=gnu
    else
      # The right tools, the right setup, but too old.  Fallbacks?
      AC_MSG_WARN(=== Linker version $libat_gnu_ld_version is too old for)
      AC_MSG_WARN(=== full symbol versioning support in this release of GCC.)
      AC_MSG_WARN(=== You would need to upgrade your binutils to version)
      AC_MSG_WARN(=== $libat_min_gnu_ld_version or later and rebuild GCC.)
      if test $libat_gnu_ld_version -ge 21200 ; then
        # Globbing fix is present, proper block support is not.
        dnl AC_MSG_WARN([=== Dude, you are soooo close.  Maybe we can fake it.])
        dnl enable_symvers=???
        AC_MSG_WARN([=== Symbol versioning will be disabled.])
        enable_symvers=no
      else
        # 2.11 or older.
        AC_MSG_WARN([=== Symbol versioning will be disabled.])
        enable_symvers=no
      fi
    fi
  elif test $enable_symvers = sun; then
    : All interesting versions of Sun ld support sun style symbol versioning.
  else
    # just fail for now
    AC_MSG_WARN([=== You have requested some kind of symbol versioning, but])
    AC_MSG_WARN([=== either you are not using a supported linker, or you are])
    AC_MSG_WARN([=== not building a shared libgcc_s (which is required).])
    AC_MSG_WARN([=== Symbol versioning will be disabled.])
    enable_symvers=no
  fi
fi
if test $enable_symvers = gnu; then
  AC_DEFINE(LIBAT_GNU_SYMBOL_VERSIONING, 1,
	    [Define to 1 if GNU symbol versioning is used for libatomic.])
fi

AM_CONDITIONAL(LIBAT_BUILD_VERSIONED_SHLIB, test $enable_symvers != no)
AM_CONDITIONAL(LIBAT_BUILD_VERSIONED_SHLIB_GNU, test $enable_symvers = gnu)
AM_CONDITIONAL(LIBAT_BUILD_VERSIONED_SHLIB_SUN, test $enable_symvers = sun)
AC_MSG_NOTICE(versioning on shared library symbols is $enable_symvers)
])

dnl ----------------------------------------------------------------------
sinclude(../libtool.m4)
sinclude(../config/enable.m4)
sinclude(../config/cet.m4)
dnl The lines below arrange for aclocal not to bring an installed
dnl libtool.m4 into aclocal.m4, while still arranging for automake to
dnl add a definition of LIBTOOL to Makefile.in.
ifelse(,,,[AC_SUBST(LIBTOOL)
AC_DEFUN([AM_PROG_LIBTOOL])
AC_DEFUN([AC_LIBTOOL_DLOPEN])
AC_DEFUN([AC_PROG_LD])
])
