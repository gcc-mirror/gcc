AC_INIT([libgrust], version-unused,,libgrust)
AC_CONFIG_SRCDIR(Makefile.am)
AC_CONFIG_FILES([Makefile])

if test -n "${with_target_subdir}"; then
  AM_ENABLE_MULTILIB(, ..)
fi

# Do not delete or change the following two lines.  For why, see
# http://gcc.gnu.org/ml/libstdc++/2003-07/msg00451.html
AC_CANONICAL_SYSTEM
target_alias=${target_alias-$host_alias}
AC_SUBST(target_alias)

# Automake should never attempt to rebuild configure
AM_MAINTAINER_MODE

AM_INIT_AUTOMAKE([1.15.1 foreign no-dist -Wall])

# Make sure we don't test executables when making cross-tools.
GCC_NO_EXECUTABLES


# Add the ability to change LIBTOOL directory
GCC_WITH_TOOLEXECLIBDIR

# Use system specific extensions
AC_USE_SYSTEM_EXTENSIONS


# Checks for header files.
AC_HEADER_STDC
AC_HEADER_SYS_WAIT
AC_CHECK_HEADERS(limits.h stddef.h string.h strings.h stdlib.h \
                 time.h sys/stat.h wchar.h)

AC_ARG_ENABLE([werror],
  [AS_HELP_STRING([--disable-werror], [disable building with -Werror])])

# Add CET specific flags if CET is enabled
GCC_CET_FLAGS(CET_FLAGS)
XCFLAGS="$XCFLAGS $CET_FLAGS"

# Check for tools
AM_PROG_AR
AC_PROG_CC
AC_PROG_CXX
AM_PROG_AS
AC_PROG_MAKE_SET
AC_PROG_INSTALL

# Enable libtool
LT_INIT

# target_noncanonical variables...
AC_CANONICAL_HOST
ACX_NONCANONICAL_HOST
ACX_NONCANONICAL_TARGET
GCC_TOPLEV_SUBDIRS

AC_MSG_CHECKING([for --enable-version-specific-runtime-libs])
AC_ARG_ENABLE(version-specific-runtime-libs,
[  --enable-version-specific-runtime-libs    Specify that runtime libraries should be installed in a compiler-specific directory ],
[case "$enableval" in
 yes) version_specific_libs=yes ;;
 no)  version_specific_libs=no ;;
 *)   AC_MSG_ERROR([Unknown argument to enable/disable version-specific libs]);;
 esac],
[version_specific_libs=no])
AC_MSG_RESULT($version_specific_libs)

toolexecdir=no
toolexeclibdir=no

# Calculate toolexeclibdir
# Also toolexecdir, though it's only used in toolexeclibdir
case ${version_specific_libs} in
  yes)
    # Need the gcc compiler version to know where to install libraries
    # and header files if --enable-version-specific-runtime-libs option
    # is selected.
    toolexecdir='$(libdir)/gcc/$(target_noncanonical)'
    toolexeclibdir='$(toolexecdir)/$(gcc_version)$(MULTISUBDIR)'
    ;;
  no)
    if test -n "$with_cross_host" &&
       test x"$with_cross_host" != x"no"; then
      # Install a library built with a cross compiler in tooldir, not libdir.
      toolexecdir='$(exec_prefix)/$(target_noncanonical)'
      toolexeclibdir='$(toolexecdir)/lib'
    else
      toolexecdir='$(libdir)/gcc-lib/$(target_noncanonical)'
      toolexeclibdir='$(libdir)'
    fi
    multi_os_directory=`$CC -print-multi-os-directory`
    case $multi_os_directory in
      .) ;; # Avoid trailing /.
      *) toolexeclibdir=$toolexeclibdir/$multi_os_directory ;;
    esac
    ;;
esac

AC_SUBST(toolexecdir)
AC_SUBST(toolexeclibdir)


AC_CONFIG_FILES(AC_FOREACH([DIR], [libproc_macro_internal], [DIR/Makefile ]),
 [ cat > vpsed$$ << \_EOF
s!`test -f '$<' || echo '$(srcdir)/'`!!
_EOF
   sed -f vpsed$$ $ac_file > tmp$$
   mv tmp$$ $ac_file
   rm vpsed$$
   echo 'MULTISUBDIR =' >> $ac_file
   ml_norecursion=yes
   AS_UNSET([ml_norecursion])
])

GCC_BASE_VER

AC_MSG_NOTICE([libgrust has been configured.])

AC_OUTPUT
