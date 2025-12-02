#
# Minimal autoconf support for the Algol 68 language.
# Adapted from the Algol 68 language support files.
#

# ------------------- #
# Language selection.
# ------------------- #

# AC_LANG(A68)
# ------------
AC_LANG_DEFINE([Algol 68], [a68], [A68], [A68], [],
[ac_ext=a68
ac_compile='$A68 -c $A68FLAGS conftest.$ac_ext >&AS_MESSAGE_LOG_FD'
ac_link='$A68 -o conftest$ac_exeext $A68FLAGS $LDFLAGS conftest.$ac_ext $LIBS >&AS_MESSAGE_LOG_FD'
ac_compiler_gnu=yes
])

# AC_LANG_A68
# -----------
AU_DEFUN([AC_LANG_A68], [AC_LANG(Algol 68)])

# ------------------- #
# Producing programs.
# ------------------- #

# AC_LANG_PROGRAM(Algol 68)([PROLOGUE], [BODY])
# ---------------------------------------------
m4_define([AC_LANG_PROGRAM(Algol 68)],
[$1
begin $2 m4_ifnblank([$2],[;])
      skip
end])

# _AC_LANG_IO_PROGRAM(Algol 68)
# -----------------------------
# Produce source that performs I/O.
m4_define([_AC_LANG_IO_PROGRAM(Algol 68)],
[AC_LANG_PROGRAM([],
[if int fd = fcreate ("conftest.out", 8r0777); fd = -1
 then perror ("error creating conftest.out")
 elif fclose (fd) = -1 then perror ("error closing conftest.out")
 fi
])])

# AC_LANG_CALL(Algol 68)(PROLOGUE, FUNCTION)
# ------------------------------------------
# Avoid conflicting decl of main.
m4_define([AC_LANG_CALL(Algol 68)],
[AC_LANG_PROGRAM([],[])])

# AC_LANG_FUNC_LINK_TRY(Algol 68)(FUNCTION)
# -----------------------------------------
# Try to link a program which calls FUNCTION.
m4_define([AC_LANG_FUNC_LINK_TRY(Algol 68)],
[AC_LANG_PROGRAM([],[])])

# AC_LANG_BOOL_COMPILE_TRY(Algol 68)(PROLOGUE, EXPRESSION)
# --------------------------------------------------------
# Return a program which is valid if EXPRESSION is nonzero.
m4_define([AC_LANG_BOOL_COMPILE_TRY(Algol 68)],
[AC_LANG_PROGRAM([], [@<:@$2@:>@INT test multiple;
		      0])])

# AC_LANG_INT_SAVE(Algol 68)(PROLOGUE, EXPRESSION)
# ------------------------------------------------
m4_define([AC_LANG_INT_SAVE(Algol 68)],
[AC_LANG_PROGRAM([
proc itoa = (int i) string:
      begin if i = 0
	    then "0"
	    else int n := ABS i;
		 string res;
		 while n /= 0
		 do int rem = n %* 10;
		    res := (REPR (rem > 9 | (rem - 10) + ABS "a"
				  | rem + ABS "0")
			    + res);
		    n %:= 10
		 od;
		 (i < 0 | "-" + res | res)
	    fi
      end;

int ret := 0;
int fd = fopen ("conftest.val", file o wronly)/
(fd = -1 | ret := 1; stop);
(fputs (fd, itoa ($2)) = 0 | ret := 1; stop)
])])

# ---------------------- #
# Looking for compilers. #
# ---------------------- #

# AC_LANG_COMPILER(Algol 68)
# --------------------------
AC_DEFUN([AC_LANG_COMPILER(Algol 68)],
[AC_REQUIRE([AC_PROG_A68])])

# AC_PROG_A68
# ------------
AN_MAKEVAR([A68], [AC_PROG_A68])
AN_PROGRAM([ga68], [AC_PROG_A68])
AC_DEFUN([AC_PROG_A68],
[AC_LANG_PUSH(Algol 68)dnl
AC_ARG_VAR([A68], [Algol 68 compiler command])dnl
AC_ARG_VAR([A68FLAGS], [Algol 68 compiler flags])dnl
_AC_ARG_VAR_LDFLAGS()dnl
m4_ifval([$1],
      [AC_CHECK_TOOLS(A68, [$1])],
[AC_CHECK_TOOL(A68, ga68)
if test -z "$A68"; then
  if test -n "$ac_tool_prefix"; then
    AC_CHECK_PROG(A68, [${ac_tool_prefix}ga68], [$ac_tool_prefix}ga68])
  fi
fi
if test -z "$A68"; then
  AC_CHECK_PROG(A68, ga68, ga68, , , false)
fi
])

# Provide some information about the compiler.
_AS_ECHO_LOG([checking for _AC_LANG compiler version])
set X $ac_compile
ac_compiler=$[2]
_AC_DO_LIMIT([$ac_compiler --version >&AS_MESSAGE_LOG_FD])
m4_expand_once([_AC_COMPILER_EXEEXT])[]dnl
m4_expand_once([_AC_COMPILER_OBJEXT])[]dnl
A68FLAGS="-g -O2"
AC_LANG_POP(Algol 68)dnl
])# AC_PROG_A68
