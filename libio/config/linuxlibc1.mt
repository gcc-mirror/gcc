# Use the libio which comes with the local libc.

# Comment this out to avoid including the stdio functions in libiostream.a:
# LIBIOSTREAM_OBJECTS = $(IO_OBJECTS) $(IOSTREAM_OBJECTS) $(STDIO_WRAP_OBJECTS) $(OSPRIM_OBJECTS)
# LIBIOSTREAM_DEP = $(LIBIOSTREAM_OBJECTS) stdio.list
# LIBIOSTREAM_USE = $(LIBIOSTREAM_OBJECTS) `cat stdio.list`

# We must not see the libio.h file from this library.
LIBIO_INCLUDE=

# We have those in libc.a.
IO_OBJECTS=iogetc.o ioputc.o iofeof.o ioferror.o \
	filedoalloc.o fileops.o genops.o iofclose.o \
	iovsprintf.o iovsscanf.o strops.o iogetline.o
STDIO_WRAP_OBJECTS=
OSPRIM_OBJECTS=
STDIO_OBJECTS=

# We have the rest in /usr/include.
USER_INCLUDES=PlotFile.h SFile.h builtinbuf.h editbuf.h fstream.h \
	indstream.h iomanip.h iostream.h istream.h ostream.h \
	parsestream.h pfstream.h procbuf.h stdiostream.h stream.h \
	streambuf.h strfile.h strstream.h libio.h

# A bad kludge
MT_CFLAGS=-D_G_HAVE_MMAP -D_G_STDIO_USES_LIBIO -D_G_HAVE_WEAK_SYMBOL
