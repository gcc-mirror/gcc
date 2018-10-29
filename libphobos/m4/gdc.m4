#
# GDC feature checking and sanity check macros
#


# GDC_CHECK_COMPILE
# -----------------
# Check if compiler can compile D code
AC_DEFUN([GDC_CHECK_COMPILE],
[
  AC_LANG_PUSH(D)
    AC_MSG_CHECKING([[If $GDC can compile D sources]])
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([], [[return 0;]])],
      [AC_MSG_RESULT([[yes]])],
      [AC_MSG_RESULT([[no]])
       AC_MSG_ERROR([[can't compile D sources!]])])
  AC_LANG_POP(D)
])
