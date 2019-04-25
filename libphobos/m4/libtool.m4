#
# Minimal libtool support for the D language.
# Adapted from the Go language support files.
#

# _LT_LANG_D_CONFIG([TAG])
# --------------------------
# Ensure that the configuration variables for the GNU D compiler
# are suitably defined.  These variables are subsequently used by _LT_CONFIG
# to write the compiler configuration to `libtool'.
m4_defun([_LT_LANG_D_CONFIG],
[AC_REQUIRE([AC_PROG_GDC])dnl
AC_LANG_SAVE

# Source file extension for D test sources.
ac_ext=d

# Object file extension for compiled D test sources.
objext=o
_LT_TAGVAR(objext, $1)=$objext

# Code to be used in simple compile tests
lt_simple_compile_test_code="\
  module object;
  shared int some_variable = 0;
"

# Code to be used in simple link tests
lt_simple_link_test_code="\
  module object;
  extern(C) int main() { return 0; }
"

# ltmain only uses $CC for tagged configurations so make sure $CC is set.
_LT_TAG_COMPILER

# save warnings/boilerplate of simple test code
_LT_COMPILER_BOILERPLATE
_LT_LINKER_BOILERPLATE

# Allow CC to be a program name with arguments.
lt_save_CC=$CC
lt_save_CFLAGS=$CFLAGS
lt_save_GCC=$GCC
GCC=yes
CC=${GDC-"gdc"}
CFLAGS=$GDCFLAGS
compiler=$CC
_LT_TAGVAR(compiler, $1)=$CC
_LT_TAGVAR(LD, $1)=$CC
_LT_CC_BASENAME([$compiler])

# GDC did not exist at the time GCC didn't implicitly link libc in.
_LT_TAGVAR(archive_cmds_need_lc, $1)=no

_LT_TAGVAR(old_archive_cmds, $1)=$old_archive_cmds
_LT_TAGVAR(reload_flag, $1)=$reload_flag
_LT_TAGVAR(reload_cmds, $1)=$reload_cmds

## CAVEAT EMPTOR:
## There is no encapsulation within the following macros, do not change
## the running order or otherwise move them around unless you know exactly
## what you are doing...
if test -n "$compiler"; then
  _LT_COMPILER_NO_RTTI($1)
  _LT_COMPILER_PIC($1)
  _LT_COMPILER_C_O($1)
  _LT_COMPILER_FILE_LOCKS($1)
  _LT_LINKER_SHLIBS($1)
  _LT_LINKER_HARDCODE_LIBPATH($1)

  _LT_CONFIG($1)
fi

AC_LANG_RESTORE

GCC=$lt_save_GCC
CC=$lt_save_CC
CFLAGS=$lt_save_CFLAGS
])# _LT_LANG_D_CONFIG
