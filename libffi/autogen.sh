#!/bin/sh
#exec autoreconf -v -i

rm -rf autom4te.cache
aclocal  -I .. -I ../config
autoheader -I .. -I ../config
autoconf
automake --foreign --add-missing --copy Makefile
automake --foreign include/Makefile
automake --foreign man/Makefile
automake --foreign testsuite/Makefile
