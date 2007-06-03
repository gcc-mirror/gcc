nativeexeclib_LTLIBRARIES = libgjsmdssi.la 

libgjsmdssi_la_SOURCES = gnu_javax_sound_midi_dssi_DSSIMidiDeviceProvider.c \
			 gnu_javax_sound_midi_dssi_DSSISynthesizer.c \
			 dssi_data.h

libgjsmdssi_la_LIBADD = $(top_builddir)/native/jni/classpath/jcl.lo -ljack
libgjsmdssi_la_LDFLAGS = $(AM_LDFLAGS) -avoid-version

AM_LDFLAGS = @CLASSPATH_MODULE@
AM_CPPFLAGS = @CLASSPATH_INCLUDES@ 
# No STRICT_WARNING_CFLAGS here as we use dlsym to load the address of 
# a function,and ISO C prohibits casting void pointers, like those returned
# by dlsym, to function pointers.
AM_CFLAGS = @WARNING_CFLAGS@ @ERROR_CFLAGS@
