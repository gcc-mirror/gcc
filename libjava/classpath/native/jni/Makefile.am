## Input file for automake to generate the Makefile.in used by configure

if CREATE_CORE_JNI_LIBRARIES
  JNIDIRS = java-io java-lang java-net java-nio java-util
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

if CREATE_XMLJ_LIBRARY
  XMLJDIR = xmlj
endif

SUBDIRS = classpath $(JNIDIRS) \
  $(ALSADIR) $(DSSIDIR) $(GTKDIR) $(CLASSPATH_QT_PEER_DIR) $(XMLJDIR)
DIST_SUBDIRS = classpath java-io java-lang java-net java-nio java-util \
               gtk-peer qt-peer xmlj midi-alsa midi-dssi

all-local:
	cd $(top_srcdir) && $(SHELL) ./scripts/check_jni_methods.sh
