#!/bin/sh -
#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that the above copyright notice and this paragraph are
# duplicated in all such forms and that any documentation,
# advertising materials, and other materials related to such
# distribution and use acknowledge that the software was developed
# by the University of California, Berkeley.  The name of the
# University may not be used to endorse or promote products derived
# from this software without specific prior written permission.
# THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
# WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)mkdep.sh	5.12 (Berkeley) 6/30/88
#

MAKE=Makefile			# default makefile name is "Makefile"

while :
	do case "$1" in
		# -f allows you to select a makefile name
		-f)
			MAKE=$2
			shift; shift ;;

		# the -p flag produces "program: program.c" style dependencies
		# so .o's don't get produced
		-p)
			SED='s;\.o;;'
			shift ;;
		*)
			break ;;
	esac
done

if [ $# = 0 ] ; then
	echo 'usage: mkdep [-p] [-f makefile] [flags] file ...'
	exit 1
fi

if [ ! -w $MAKE ]; then
	echo "mkdep: no writeable file \"$MAKE\""
	exit 1
fi

TMP=/tmp/mkdep$$

trap 'rm -f $TMP ; exit 1' 1 2 3 13 15

cp $MAKE ${MAKE}.bak

sed -e '/DO NOT DELETE THIS LINE/,$d' < $MAKE > $TMP

cat << _EOF_ >> $TMP
# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

_EOF_

# If your compiler doesn't have -M, add it.  If you can't, the next two
# lines will try and replace the "cc -M".  The real problem is that this
# hack can't deal with anything that requires a search path, and doesn't
# even try for anything using bracket (<>) syntax.
#
# egrep '^#include[ 	]*".*"' /dev/null $* |
# sed -e 's/:[^"]*"\([^"]*\)".*/: \1/' -e 's/\.c/.o/' |

gcc -MM $* |
sed "
	s; \./; ;g
	$SED" >> $TMP

cat << _EOF_ >> $TMP

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
_EOF_

# copy to preserve permissions
cp $TMP $MAKE
rm -f ${MAKE}.bak $TMP
exit 0


