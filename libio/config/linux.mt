# Since the Linux C library has libio, we have to be very careful.

# By default, we build libio and use it.  If someone wants to not
# build it, let them go to extra work.  The reason is that the user
# may want a newer, bug fixed libio, also on a linux 1.0.8 system
# things just won't build with the bottom section uncommented.

# Comment this out to avoid including the stdio functions in libiostream.a:
LIBIOSTREAM_OBJECTS = $(IO_OBJECTS) $(IOSTREAM_OBJECTS) $(STDIO_WRAP_OBJECTS) $(OSPRIM_OBJECTS)
LIBIOSTREAM_DEP = $(LIBIOSTREAM_OBJECTS) stdio.list
LIBIOSTREAM_USE = $(LIBIOSTREAM_OBJECTS) `cat stdio.list`

# Comment the above and uncomment the below to use the code in the Linux libc:
# We have _G_config.h in /usr/include.
# _G_CONFIG_H=

# We have those in libc.a.
# IO_OBJECTS=
# STDIO_WRAP_OBJECTS=
# OSPRIM_OBJECTS=

# We have the rest in /usr/include.
# USER_INCLUDES=PlotFile.h SFile.h builtinbuf.h editbuf.h fstream.h \
# 	indstream.h iomanip.h iostream.h istream.h ostream.h \
# 	parsestream.h pfstream.h procbuf.h stdiostream.h stream.h \
# 	streambuf.h strfile.h strstream.h
