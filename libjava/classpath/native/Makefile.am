## Input file for automake to generate the Makefile.in used by configure

if CREATE_JNI_LIBRARIES
  JNIDIR = jni
endif

if CREATE_GTK_PEER_LIBRARIES
  JAWTDIR = jawt
endif

if CREATE_PLUGIN
  PLUGINDIR = plugin
endif

SUBDIRS = fdlibm $(JNIDIR) $(JAWTDIR) $(PLUGINDIR)
DIST_SUBDIRS = fdlibm jni jawt plugin

