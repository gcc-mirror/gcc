## Input file for automake to generate the Makefile.in used by configure

if CREATE_CORE_JNI_LIBRARIES
  JNIDIRS = native-lib java-io java-lang java-net java-nio java-util
endif

if CREATE_ALSA_LIBRARIES
  ALSADIR = midi-alsa
endif

if CREATE_DSSI_LIBRARIES
  DSSIDIR = midi-dssi
endif

if CREATE_GTK_PEER_LIBRARIES
  GTKDIR = gtk-peer
endif

if CREATE_QT_PEER_LIBRARIES
  CLASSPATH_QT_PEER_DIR = qt-peer
endif

if CREATE_GCONF_PEER_LIBRARIES
  CLASSPATH_GCONF_PEER_DIR = gconf-peer
endif

if CREATE_GSTREAMER_PEER_LIBRARIES
  CLASSPATH_GSTREAMER_PEER_DIR = gstreamer-peer
endif

if CREATE_XMLJ_LIBRARY
  XMLJDIR = xmlj
endif

if CREATE_GMPBI_LIBRARY
  GMPDIR = java-math
endif

SUBDIRS = classpath $(JNIDIRS) \
  $(ALSADIR) $(DSSIDIR) $(GTKDIR) $(CLASSPATH_QT_PEER_DIR) $(XMLJDIR) \
  $(CLASSPATH_GCONF_PEER_DIR) $(CLASSPATH_GSTREAMER_PEER_DIR) $(GMPDIR)
DIST_SUBDIRS = classpath java-io java-lang java-math java-net java-nio java-util \
               gtk-peer gconf-peer gstreamer-peer qt-peer xmlj midi-alsa \
               midi-dssi native-lib

all-local:
	$(SHELL) $(top_builddir)/scripts/check_jni_methods.sh
