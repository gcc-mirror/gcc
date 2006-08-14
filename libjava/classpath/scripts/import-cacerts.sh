#!/bin/sh

function visitFile() {
	install/bin/gkeytool \
	-cacert \
	-v \
	-storepass changeit \
	-keystore resource/java/security/cacerts.gkr \
	-file "$1"
}

function visitDir() {
	local d
	d=$1
	for f in "$d/"*
	do
		if [ -d "$f" ] ; then
			visitDir "$f"
		else
			visitFile "$f"
		fi
	done
}

if [ "$#" -lt "1" ] ; then
	echo "Usage: import-cacerts DIR"
	echo "Import CA trusted certificates into a 'cacerts.gkr' key store"
	echo "under resource/java/security using 'changeit' as its password,"
	echo "and constructing the Alias from the certificate's file name."
	echo
	echo "  DIR  the 'ca-certificates' deb package installation directory"
	echo "         containing trusted CA certificates."
	echo
else
	caDir=$1
	if [ ! -d $caDir ] ; then
		echo "Argument MUST be a directory."
		echo "Type command with no arguments for usage string."
		exit 1
	fi
	visitDir $caDir
fi
exit 0