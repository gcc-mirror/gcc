dnl See whether we have struct timezone
dnl LIBU77_HAVE_STRUCT_TIMEZONE
AC_DEFUN(LIBU77_HAVE_STRUCT_TIMEZONE,
[AC_MSG_CHECKING([whether struct timezone exists])
AC_CACHE_VAL(libu77_cv_have_struct_timezone,
[AC_TRY_COMPILE([#include <sys/time.h>],
[struct timezone tz;],
libu77_ac_have_struct_timezone=yes, libu77_ac_have_struct_timezone=no)])
if test $libu77_ac_have_struct_timezone = yes; then
  AC_MSG_RESULT(yes)
  AC_DEFINE_UNQUOTED(HAVE_STRUCT_TIMEZONE)
else
  AC_MSG_RESULT(no)
fi
])dnl

