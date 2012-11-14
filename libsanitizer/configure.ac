#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.64])
AC_INIT(package-unused, version-unused, libsanitizer)
AC_CONFIG_SRCDIR([include/sanitizer/common_interface_defs.h])
AC_CONFIG_AUX_DIR(..)
AM_INIT_AUTOMAKE(foreign)

# Checks for programs.
AC_PROG_CC
AC_PROG_CXX
AM_PROG_AS

AC_LIBTOOL_DLOPEN
AM_PROG_LIBTOOL

AC_SUBST(enable_shared)
AC_SUBST(enable_static)

#AM_ENABLE_MULTILIB(, ..)
target_alias=${target_alias-$host_alias}
AC_SUBST(target_alias)

toolexecdir='$(libdir)/gcc-lib/$(target_alias)'
toolexeclibdir='$(libdir)'
AC_SUBST(toolexecdir)
AC_SUBST(toolexeclibdir)

AC_CONFIG_FILES([Makefile])

AC_CONFIG_FILES(AC_FOREACH([DIR], [interception sanitizer_common asan], [DIR/Makefile ]),
  [cat > vpsed$$ << \_EOF
s!`test -f '$<' || echo '$(srcdir)/'`!!
_EOF
   sed -f vpsed$$ $ac_file > tmp$$
   mv tmp$$ $ac_file
   rm vpsed$$
   echo 'MULTISUBDIR =' >> $ac_file
])

AC_OUTPUT
