#
# Contains macros to allow building libphobos as part of GCC.
# This includes macros to locate directories and do configure checks
# without an installed libdruntime.
#


# PHOBOS_ABS_SRCDIR
# -----------------
# Find absolute top level source directory and set phobos_cv_abs_srcdir
AC_DEFUN([PHOBOS_ABS_SRCDIR], [
  dnl Not sure if 100% portable, but we need the absolute dir as the _LT_COMPILER_C_O
  dnl test changes the directory
  AC_CACHE_CHECK([[for absolute libphobos source path]],
    [[phobos_cv_abs_srcdir]],
    [[phobos_cv_abs_srcdir=`cd $srcdir && pwd`]])
    AS_IF([[test -d "$phobos_cv_abs_srcdir"]],
      [],
      [AC_MSG_ERROR([[can't find absolute libphobos source path]])])
])


# WITH_LOCAL_DRUNTIME(CALL, EXTRAFLAGS)
# -------------------------------------
# Execute CALL with GDCFLAGS adjusted to use the local druntime includes.
# Flags contains extra arguments to append to GDCFLAGS (e.g. -nophoboslib).
AC_DEFUN([WITH_LOCAL_DRUNTIME], [
  AC_REQUIRE([PHOBOS_ABS_SRCDIR])
  gdc_save_DFLAGS=$GDCFLAGS
  GDCFLAGS="-fno-moduleinfo -nostdinc -I $phobos_cv_abs_srcdir/libdruntime $2 $GDCFLAGS"
  $1
  GDCFLAGS=$gdc_save_DFLAGS
])
