#
# Minimal autoconf support for the D language.
# Adapted from the Go language support files.
#

# ------------------- #
# Language selection.
# ------------------- #

# AC_LANG(D)
# -----------
AC_LANG_DEFINE([D], [d], [GDC], [GDC], [],
[ac_ext=d
ac_compile='$GDC -c $GDCFLAGS conftest.$ac_ext >&AS_MESSAGE_LOG_FD'
ac_link='$GDC -o conftest$ac_exeext $GDCFLAGS $LDFLAGS conftest.$ac_ext $LIBS >&AS_MESSAGE_LOG_FD'
ac_compiler_gnu=yes
])

# AC_LANG_D
# ----------
AU_DEFUN([AC_LANG_D], [AC_LANG(D)])

# ------------------- #
# Producing programs.
# ------------------- #

# AC_LANG_PROGRAM(D)([PROLOGUE], [BODY])
# ---------------------------------------
m4_define([AC_LANG_PROGRAM(D)],
[module object;
$1

extern(C) int main() {
  $2
}])

# _AC_LANG_IO_PROGRAM(D)
# -----------------------
# Produce source that performs I/O.
m4_define([_AC_LANG_IO_PROGRAM(D)],
[AC_LANG_PROGRAM([import core.stdc.stdio;],
[FILE *f = fopen ("conftest.out", "w");
 return ferror (f) || fclose (f) != 0;
])])

# AC_LANG_CALL(D)(PROLOGUE, FUNCTION)
# ------------------------------------
# TODO: Avoid conflicting decl of main?
# Used by AC_SEARCH_LIBS.
m4_define([AC_LANG_CALL(D)],
[AC_LANG_PROGRAM([$1 extern(C) int $2();], [$2(); return 0;])])

# AC_LANG_FUNC_LINK_TRY(D)(FUNCTION)
# -----------------------------------
# Try to link a program which calls FUNCTION.
# This only works for extern(C) functions.
m4_define([AC_LANG_FUNC_LINK_TRY(D)],
[AC_LANG_PROGRAM([extern(C) int $1();], [return $1();])])

# AC_LANG_BOOL_COMPILE_TRY(D)(PROLOGUE, EXPRESSION)
# --------------------------------------------------
# Return a program which is valid if EXPRESSION is nonzero.
# Probably not that useful for D, we can extract any information
# we need using CTFE.
m4_define([AC_LANG_BOOL_COMPILE_TRY(D)],
[AC_LANG_PROGRAM([$1],
[static assert($2); return 0;])])

# AC_LANG_INT_SAVE(D)(PROLOGUE, EXPRESSION)
# ------------------------------------------
m4_define([AC_LANG_INT_SAVE(D)],
[AC_LANG_PROGRAM([$1
import core.stdc.stdio, core.stdc.stdlib;
],
[
  FILE *f = fopen ("conftest.val", "w");
  if (! f)
    return 1;
  if (($2) < 0)
    {
      fprintf (f, "%ld", $2);
    }
  else
    {
      fprintf (f, "%lu", $2);
    }
  /* Do not output a trailing newline, as this causes \r\n confusion
     on some platforms.  */
  return ferror (f) || fclose (f) != 0;
])])

# ---------------------- #
# Looking for compilers. #
# ---------------------- #

# AC_LANG_COMPILER(D)
# --------------------
AC_DEFUN([AC_LANG_COMPILER(D)],
[AC_REQUIRE([AC_PROG_GDC])])

# AC_PROG_GDC
# ----------
AN_MAKEVAR([GDC], [AC_PROG_GDC])
AN_PROGRAM([gdc], [AC_PROG_GDC])
AC_DEFUN([AC_PROG_GDC],
[AC_LANG_PUSH(D)dnl
AC_ARG_VAR([GDC],     [D compiler command])dnl
AC_ARG_VAR([GDCFLAGS],  [D compiler flags])dnl
_AC_ARG_VAR_LDFLAGS()dnl
m4_ifval([$1],
      [AC_CHECK_TOOLS(GDC, [$1])],
[AC_CHECK_TOOL(GDC, gdc)
if test -z "$GDC"; then
  if test -n "$ac_tool_prefix"; then
    AC_CHECK_PROG(GDC, [${ac_tool_prefix}gdc], [$ac_tool_prefix}gdc])
  fi
fi
if test -z "$GDC"; then
  AC_CHECK_PROG(GDC, gdc, gdc, , , false)
fi
])

# Provide some information about the compiler.
_AS_ECHO_LOG([checking for _AC_LANG compiler version])
set X $ac_compile
ac_compiler=$[2]
_AC_DO_LIMIT([$ac_compiler --version >&AS_MESSAGE_LOG_FD])
m4_expand_once([_AC_COMPILER_EXEEXT])[]dnl
m4_expand_once([_AC_COMPILER_OBJEXT])[]dnl
AC_LANG_POP(D)dnl
])# AC_PROG_D

