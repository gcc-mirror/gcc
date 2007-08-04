#!/bin/sh
# Run this to generate all the initial makefiles, etc.

srcdir=`dirname $0`
test -z "$srcdir" && srcdir=.

ORIGDIR=`pwd`
cd $srcdir
PROJECT=classpath
TEST_TYPE=-f
FILE=java/lang/Object.java

DIE=0

LIBTOOLIZE=libtoolize

have_libtool=false
if ${LIBTOOLIZE} --version < /dev/null > /dev/null 2>&1 ; then
	libtool_version=`${LIBTOOLIZE} --version | sed 's/^.*[^0-9.]\([0-9]\{1,\}\.[0-9.]\{1,\}\).*/\1/'`
	case $libtool_version in
	    1.5*)
		have_libtool=true
		;;
	esac
fi
if $have_libtool ; then : ; else
	echo
	echo "You must have libtool 1.5 installed to compile $PROJECT."
	echo "Install the appropriate package for your distribution,"
	echo "or get the source tarball at http://ftp.gnu.org/gnu/libtool/"
	echo "For Darwin you need the latest stable (1.5.22) to support"
	echo "Frameworks linking. Also, you have to point"
	echo "LOCAL_AUTORECONF_FLAGS to this libtool/share/aclocal."
	DIE=1
fi

if test "$DIE" -eq 1; then
	exit 1
fi

test $TEST_TYPE $FILE || {
	echo "You must run this script in the top-level $PROJECT directory"
	exit 1
}

if test "x$AUTOGEN_SUBDIR_MODE" = "xyes"; then
        if test -z "$*"; then
                echo "I am going to run ./configure with no arguments - if you wish "
                echo "to pass any to it, please specify them on the $0 command line."
        fi
fi

autoreconf --force --install --warnings=no-portability || exit $?

cd $ORIGDIR || exit $?

if test "x$AUTOGEN_SUBDIR_MODE" = "xyes"; then
        $srcdir/configure --enable-maintainer-mode $AUTOGEN_CONFIGURE_ARGS "$@" || exit $?

        echo 
        echo "Now type 'make' to compile $PROJECT."
fi
