## GCJ LOCAL: install this library in GCJ's versioned library
## directory
gcc_version := $(shell cat $(top_srcdir)/../../gcc/BASE-VER)
gcjversionedlibdir = $(libdir)/gcj-$(gcc_version)
gcjversionedlib_LTLIBRARIES = libgcjwebplugin.la

libgcjwebplugin_la_SOURCES = gcjwebplugin.cc

libgcjwebplugin_la_CXXFLAGS = \
	-Wall -DAPPLETVIEWER_EXECUTABLE="\"$(bindir)/gappletviewer\"" \
	-DPLUGIN_DATA_DIRECTORY="\"$(PLUGIN_DIR)/gcjwebplugin-data\"" \
	$(MOZILLA_CFLAGS) $(GLIB_CFLAGS) $(GTK_CFLAGS)

## GCJ LOCAL: encode the library path and  use GCJ's library version
libgcjwebplugin_la_LDFLAGS =  -rpath $(gcjversionedlibdir) \
	-version-info `grep -v '^\#' $(top_srcdir)/../libtool-version` \
	$(GLIB_LIBS) $(GTK_LIBS) \
	-lstdc++

## GCJ LOCAL: depend on gcjversionedlib_LTLIBRARIES
install-plugin: $(gcjversionedlib_LTLIBRARIES)
	$(INSTALL) -d -m0755 $(DESTDIR)$(PLUGIN_DIR)
	$(INSTALL) .libs/libgcjwebplugin.so $(DESTDIR)$(PLUGIN_DIR)

uninstall-plugin:
	rm -f $(DESTDIR)$(PLUGIN_DIR)/libgcjwebplugin.so
