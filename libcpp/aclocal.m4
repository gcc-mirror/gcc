# generated automatically by aclocal 1.11.1 -*- Autoconf -*-

# Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
# 2005, 2006, 2007, 2008, 2009  Free Software Foundation, Inc.
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY, to the extent permitted by law; without
# even the implied warranty of MERCHANTABILITY or FITNESS FOR A
# PARTICULAR PURPOSE.

m4_include([../config/acx.m4])
m4_include([../config/codeset.m4])
m4_include([../config/depstand.m4])
m4_include([../config/gettext-sister.m4])
m4_include([../config/iconv.m4])
m4_include([../config/lead-dot.m4])
m4_include([../config/lib-ld.m4])
m4_include([../config/lib-link.m4])
m4_include([../config/lib-prefix.m4])
m4_include([../config/override.m4])
m4_include([../config/warnings.m4])

## ---------------------------------------------------------------- ##
## Checking for declared symbols.                                   ##
## This is like *AC_CHECK_DECL*, except that for c++, we may use a  ##
## prototype to check for a (possibly overloaded) function.         ##
## ---------------------------------------------------------------- ##


# _AC_CHECK_PROTO_BODY
# -------------------
# Shell function body for AC_CHECK_PROTO.
m4_define([_AC_CHECK_PROTO_BODY],
[  AS_LINENO_PUSH([$[]1])
  [as_decl_name=`echo $][2|sed 's/(.*//'`]
  [as_decl_use=`echo $][2|sed -e 's/(/((/' -e 's/)/) 0&/' -e 's/,/) 0& (/g'`]
  AC_CACHE_CHECK([whether $as_decl_name is declared], [$[]3],
  [AC_COMPILE_IFELSE([AC_LANG_PROGRAM([$[]4],
[@%:@ifndef $[]as_decl_name
@%:@ifdef __cplusplus
  (void) $[]as_decl_use;
@%:@else
  (void) $[]as_decl_name;
@%:@endif
@%:@endif
])],
		   [AS_VAR_SET([$[]3], [yes])],
		   [AS_VAR_SET([$[]3], [no])])])
  AS_LINENO_POP
])# _AC_CHECK_PROTO_BODY

# AC_CHECK_PROTO(SYMBOL,
#               [ACTION-IF-FOUND], [ACTION-IF-NOT-FOUND],
#               [INCLUDES = DEFAULT-INCLUDES])
# -------------------------------------------------------
# Check whether SYMBOL (a function, variable, or constant) is declared.
AC_DEFUN([AC_CHECK_PROTO],
[AC_REQUIRE_SHELL_FN([ac_fn_]_AC_LANG_ABBREV[_check_proto],
  [AS_FUNCTION_DESCRIBE([ac_fn_]_AC_LANG_ABBREV[_check_proto],
    [LINENO SYMBOL VAR],
    [Tests whether SYMBOL is declared, setting cache variable VAR accordingly.])],
  [_$0_BODY])]dnl
[AS_VAR_PUSHDEF([ac_Symbol], [ac_cv_have_decl_$1])]dnl
[ac_fn_[]_AC_LANG_ABBREV[]_check_proto ]dnl
["$LINENO" "$1" "ac_Symbol" "AS_ESCAPE([AC_INCLUDES_DEFAULT([$4])], [""])"
AS_VAR_IF([ac_Symbol], [yes], [$2], [$3])
AS_VAR_POPDEF([ac_Symbol])dnl
])# AC_CHECK_PROTO


# _AC_CHECK_PROTOS(SYMBOL, ACTION-IF_FOUND, ACTION-IF-NOT-FOUND,
#                  INCLUDES)
# -------------------------------------------------------------
# Helper to AC_CHECK_PROTOS, which generates the check for a single
# SYMBOL with INCLUDES, performs the AC_DEFINE, then expands
# ACTION-IF-FOUND or ACTION-IF-NOT-FOUND.
m4_define([_AC_CHECK_PROTOS],
[AC_CHECK_PROTO([$1], [ac_have_decl=1], [ac_have_decl=0], [$4])]dnl
[AC_DEFINE_UNQUOTED(AS_TR_CPP(patsubst(HAVE_DECL_[$1],[(.*])), [$ac_have_decl],
  [Define to 1 if you have the declaration of `$1',
   and to 0 if you don't.])]dnl
[m4_ifvaln([$2$3], [AS_IF([test $ac_have_decl = 1], [$2], [$3])])])

# AC_CHECK_PROTOS(SYMBOLS,
#                 [ACTION-IF-FOUND], [ACTION-IF-NOT-FOUND],
#                 [INCLUDES = DEFAULT-INCLUDES])
# --------------------------------------------------------
# Defines HAVE_DECL_SYMBOL to 1 if declared, 0 otherwise.  See the
# documentation for a detailed explanation of this difference with
# other AC_CHECK_*S macros.  SYMBOLS is an m4 list.
AC_DEFUN([AC_CHECK_PROTOS],
[m4_map_args_sep([_$0(], [, [$2], [$3], [$4])], [], $1)])
