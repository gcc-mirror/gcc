lib_LTLIBRARIES = libgcjwebplugin.la

libgcjwebplugin_la_SOURCES = gcjwebplugin.cc

libgcjwebplugin_la_CXXFLAGS = \
	-Wall -DAPPLETVIEWER_EXECUTABLE="\"$(bindir)/appletviewer\"" \
	-DPLUGIN_DATA_DIRECTORY="\"$(PLUGIN_DIR)/gcjwebplugin-data\"" \
	$(MOZILLA_CFLAGS) $(GLIB_CFLAGS) $(GTK_CFLAGS)

libgcjwebplugin_la_LDFLAGS = -avoid-version \
	$(GLIB_LIBS) $(GTK_LIBS) \
	-lstdc++

install-plugin: $(lib_LTLIBRARIES)
	$(INSTALL) -d -m0755 $(DESTDIR)$(PLUGIN_DIR)
	$(INSTALL) .libs/libgcjwebplugin.so $(DESTDIR)$(PLUGIN_DIR)

uninstall-plugin:
	rm -f $(DESTDIR)$(PLUGIN_DIR)/libgcjwebplugin.so
