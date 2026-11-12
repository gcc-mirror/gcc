# ax_python_interpreter.m4 detect a versioned python interpreter.
#
#  Copyright (C) 2026 Free Software Foundation, Inc.
#
#   This program is free software: you can redistribute it and/or modify it
#   under the terms of the GNU General Public License as published by the
#   Free Software Foundation, either version 3 of the License, or (at your
#   option) any later version.
#
#   This program is distributed in the hope that it will be useful, but
#   WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
#   Public License for more details.
#
#   You should have received a copy of the GNU General Public License along
#   with this program. If not, see <https://www.gnu.org/licenses/>.
#
#   As a special exception, the respective Autoconf Macro's copyright owner
#   gives unlimited permission to copy, distribute and modify the configure
#   scripts that are the output of Autoconf when processing the Macro. You
#   need not follow the terms of the GNU General Public License when using
#   or distributing such scripts, even though portions of the text of the
#   Macro appear in them. The GNU General Public License (GPL) does govern
#   all other use of the material that constitutes the Autoconf Macro.
#
#   This special exception to the GPL applies to versions of the Autoconf
#   Macro released by the Autoconf Archive. When you make and distribute a
#   modified version of the Autoconf Macro, you may extend this special
#   exception to the GPL to apply to your modified version as well.
#
#  This code is heavily derived and pruned from:
#
# ===========================================================================
#     https://www.gnu.org/software/autoconf-archive/ax_python_devel.html
# ===========================================================================
#
# SYNOPSIS
#
#   AX_PYTHON_INTERPRETER([version])
#
# DESCRIPTION
#
#   This macro checks for a particular version of Python.
#   The parameter is manditory, for example ">= '2.3.1'", or "== '2.4'".
#   Please note that you also *have* to pass an operator along with the
#   version to match, and pay special attention to the single quotes
#   surrounding the version number.
#
#   The ax_python_interpreter_found variable will be "no" if the incorrect
#   version of Python is found.  It will be "yes" if found.
#
#   PYTHON_VERSION can be set to force the interpreter program name.
#   For example PYTHON_VERSION=2.7.1 which searches for python2.7.1.
#   Otherwise it searches for a program named python, python2 or python3.
#
#   For example:
#
#   AX_PYTHON_INTERPRETER([>= '3.0']) searches for a Python3 interpreter.
#
#   This macro should work for all versions of Python >= 2.1.0.
#

AC_DEFUN([AC_PYTHON_INTERPRETER],[
	# Default to no.
        ax_python_interpreter_found=no
        AC_MSG_CHECKING([do we have a Python interpreter])
	#
	# Allow the use of a (user set) custom python version
	#
	AC_ARG_VAR([PYTHON_VERSION],[The installed Python
		version to use, for example '2.3'. This string
		will be appended to the Python interpreter
		canonical name.])
	for version in $PYTHON_VERSION 3 2 ""; do
            AC_PATH_PROG([PYTHON],[python[$version]])
            if test -z "$PYTHON"; then
                AC_MSG_RESULT([no])
            else
                AC_MSG_RESULT([yes])
                # Note: omitted the test for Python version < 2.1.0.
                #
                # If the macro parameter ``version'' is set, honour it.
                # A Python shim class, VPy, is used to implement correct version comparisons via
                # string expressions, since e.g. a naive textual ">= 2.7.3" won't work for
                # Python 2.7.10 (the ".1" being evaluated as less than ".3").
                if test -n "$1"; then
                    AC_MSG_CHECKING([whether internal python version string is $1])
                    cat << EOF > ax_python_interpreter_vpy.py
class VPy:
    def vtup(self, s):
        return tuple(map(int, s.strip().replace("rc", ".").split(".")))
    def __init__(self):
        import sys
        self.vpy = tuple(sys.version_info)[[:3]]
    def __eq__(self, s):
        return self.vpy == self.vtup(s)
    def __ne__(self, s):
        return self.vpy != self.vtup(s)
    def __lt__(self, s):
        return self.vpy < self.vtup(s)
    def __gt__(self, s):
        return self.vpy > self.vtup(s)
    def __le__(self, s):
        return self.vpy <= self.vtup(s)
    def __ge__(self, s):
        return self.vpy >= self.vtup(s)
EOF
		ac_supports_python_ver=`$PYTHON -c """import ax_python_interpreter_vpy; \
  ver = ax_python_interpreter_vpy.VPy(); \
  print (ver $1) """`
                    rm -rf ax_python_interpreter_vpy*.py* __pycache__/ax_python_interpreter_vpy*.py*
                    if test "$ac_supports_python_ver" = "True"; then
			# Found correct version, now break from the loop.
                        AC_MSG_RESULT([yes])
                        PYTHON_VERSION=$version
                        ax_python_interpreter_found=yes
			break
                    else
                        AC_MSG_RESULT([no])
			PYTHON_VERSION=""
                    fi
                fi
	    fi
	done
        AC_SUBST([ax_python_interpreter_found])
        AC_MSG_CHECKING([result of having a Python $1 interpreter])
	if test "$ax_python_interpreter_found" = "yes"; then
           AC_MSG_RESULT([yes, all done.])
        else
	   AC_MSG_RESULT([no])
           PYTHON_VERSION=""
	fi
	#
	# all done!
	#
])
