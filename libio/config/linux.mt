# Use the libio which comes with the local libc.

# Comment this out to avoid including the stdio functions in libiostream.a:
# LIBIOSTREAM_OBJECTS = $(IO_OBJECTS) $(IOSTREAM_OBJECTS) $(STDIO_WRAP_OBJECTS) $(OSPRIM_OBJECTS)
# LIBIOSTREAM_DEP = $(LIBIOSTREAM_OBJECTS) stdio.list
# LIBIOSTREAM_USE = $(LIBIOSTREAM_OBJECTS) `cat stdio.list`

# Comment the above and uncomment the below to use the code in the Linux libc:
# We have _G_config.h in /usr/include.
_G_CONFIG_H=

# We must not see the libio.h file from this library.
LIBIO_INCLUDE=

# We have those in libc.a.
IO_OBJECTS= iogetline.o
STDIO_WRAP_OBJECTS=
OSPRIM_OBJECTS=
STDIO_OBJECTS=

# We have the rest in /usr/include.
USER_INCLUDES=PlotFile.h SFile.h builtinbuf.h editbuf.h fstream.h \
	indstream.h iomanip.h iostream.h istream.h ostream.h \
	parsestream.h pfstream.h procbuf.h stdiostream.h stream.h \
	streambuf.h strfile.h strstream.h
