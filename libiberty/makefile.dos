CFLAGS=-O2

OBJS = \
	argv.o \
	basename.o \
	concat.o \
	cplus-dem.o \
	fdmatch.o \
	floatformat.o \
	getopt.o \
	getopt1.o \
	getruntime.o \
	hex.o \
	msdos.o \
	obstack.o \
	spaces.o \
	strerror.o \
	strsignal.o \
	xatexit.o \
	xexit.o \
	xmalloc.o \
	$E

.c.o:
	gcc -I../include $(CFLAGS) -c $<

libiberty.a : $(OBJS)
	-rm libiberty.a
	ar rvs libiberty.a $(OBJS)
