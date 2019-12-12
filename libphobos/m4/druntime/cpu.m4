#
# Contains macros to detect CPU features.
#


# DRUNTIME_CPU_SOURCES
# -------------------
# Detect target CPU and add DRUNTIME_CPU_XXX conditionals.
AC_DEFUN([DRUNTIME_CPU_SOURCES],
[
  druntime_target_cpu_parsed=""
  case "$target_cpu" in
      aarch64*)
               druntime_target_cpu_parsed="aarch64"
               ;;
      arm*)    druntime_target_cpu_parsed="arm"
               ;;
      mips*)   druntime_target_cpu_parsed="mips"
               ;;
      powerpc) druntime_target_cpu_parsed="powerpc"
               ;;
      powerpc64)
               druntime_target_cpu_parsed="powerpc64"
               ;;
      i[[34567]]86|x86_64)
               druntime_target_cpu_parsed="x86"
               ;;
  esac
  AM_CONDITIONAL([DRUNTIME_CPU_AARCH64],
                 [test "$druntime_target_cpu_parsed" = "aarch64"])
  AM_CONDITIONAL([DRUNTIME_CPU_ARM],
                 [test "$druntime_target_cpu_parsed" = "arm"])
  AM_CONDITIONAL([DRUNTIME_CPU_MIPS],
                 [test "$druntime_target_cpu_parsed" = "mips"])
  AM_CONDITIONAL([DRUNTIME_CPU_POWERPC],
                 [test "$druntime_target_cpu_parsed" = "powerpc"])
  AM_CONDITIONAL([DRUNTIME_CPU_POWERPC64],
                 [test "$druntime_target_cpu_parsed" = "powerpc64"])
  AM_CONDITIONAL([DRUNTIME_CPU_X86],
                 [test "$druntime_target_cpu_parsed" = "x86"])
])


# DRUNTIME_ENABLE_ATOMIC_BUILTINS
# -------------------------
# Check support for atomic builtins up to 64 bit.
AC_DEFUN([DRUNTIME_ENABLE_ATOMIC_BUILTINS],
[
  # This checks to see if the host supports the compiler-generated builtins
  # for atomic operations for various integral sizes. Note, this is intended
  # to be an all-or-nothing switch, so all the atomic operations that are
  # used should be checked.
  AC_MSG_CHECKING([for atomic builtins for byte])
  AC_CACHE_VAL(druntime_cv_atomic_byte, [
    AC_TRY_LINK(
      [import gcc.builtins;], [
      shared(byte) c1;
       byte c2, c3;
       __atomic_compare_exchange_1(&c1, &c2, c3, false, 5, 5);
       __atomic_load_1(&c1, 5);
       __atomic_store_1(&c1, c2, 5);
       return 0;
      ],
      [druntime_cv_atomic_byte=yes],
      [druntime_cv_atomic_byte=no])
  ])
  AC_MSG_RESULT($druntime_cv_atomic_byte)

  AC_MSG_CHECKING([for atomic builtins for short])
  AC_CACHE_VAL(druntime_cv_atomic_short, [
    AC_TRY_LINK(
      [import gcc.builtins;], [
      shared(short) c1;
       short c2, c3;
       __atomic_compare_exchange_2(&c1, &c2, c3, false, 5, 5);
       __atomic_load_2(&c1, 5);
       __atomic_store_2(&c1, c2, 5);
       return 0;
      ],
      [druntime_cv_atomic_short=yes],
      [druntime_cv_atomic_short=no])
  ])
  AC_MSG_RESULT($druntime_cv_atomic_short)

  AC_MSG_CHECKING([for atomic builtins for int])
  AC_CACHE_VAL(druntime_cv_atomic_int, [
    AC_TRY_LINK(
      [import gcc.builtins;], [
      shared(int) c1;
       int c2, c3;
       __atomic_compare_exchange_4(&c1, &c2, c3, false, 5, 5);
       __atomic_load_4(&c1, 5);
       __atomic_store_4(&c1, c2, 5);
       return 0;
      ],
      [druntime_cv_atomic_int=yes],
      [druntime_cv_atomic_int=no])
  ])
  AC_MSG_RESULT($druntime_cv_atomic_int)

  AC_MSG_CHECKING([for atomic builtins for long])
  AC_CACHE_VAL(druntime_cv_atomic_long, [
    AC_TRY_LINK(
      [import gcc.builtins;], [
       shared(long) c1;
       long c2, c3;
       __atomic_compare_exchange_8(&c1, &c2, c3, false, 5, 5);
       __atomic_load_8(&c1, 5);
       __atomic_store_8(&c1, c2, 5);
       return 0;
      ],
      [druntime_cv_atomic_long=yes],
      [druntime_cv_atomic_long=no])
  ])
  AC_MSG_RESULT($druntime_cv_atomic_long)

  # Have atomic builtin support if all but the long test above passes.
  DCFG_HAVE_ATOMIC_BUILTINS=false
  if test "$druntime_cv_atomic_byte" = yes \
     && test "$druntime_cv_atomic_short" = yes \
     && test "$druntime_cv_atomic_int" = yes; then \
    DCFG_HAVE_ATOMIC_BUILTINS=true
  fi

  # Have 64-bit atomic support if the long test above passes.
  DCFG_HAVE_64BIT_ATOMICS=false
  if test "$druntime_cv_atomic_long" = yes; then
    DCFG_HAVE_64BIT_ATOMICS=true
  fi

  AC_SUBST(DCFG_HAVE_ATOMIC_BUILTINS)
  AC_SUBST(DCFG_HAVE_64BIT_ATOMICS)
])
