## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign dejagnu

EXPECT = `if [ -f ../../expect/expect ] ; then \
	  echo ../../expect/expect ; \
	  else echo expect ; fi`

RUNTEST = `if [ -f ${srcdir}/../../dejagnu/runtest ] ; then \
	   echo ${srcdir}/../../dejagnu/runtest ; \
	   else echo runtest ;  fi`

all-local: site.exp
if LIBMUDFLAPTH
	echo 'set libmudflapth 1' >> site.exp
else
	echo 'set libmudflapth 0' >> site.exp
endif
