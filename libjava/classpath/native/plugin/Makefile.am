nativeexeclib_LTLIBRARIES = libgcjwebplugin.la

libgcjwebplugin_la_SOURCES = gcjwebplugin.cc

libgcjwebplugin_la_CXXFLAGS = \
	-Wall -DAPPLETVIEWER_EXECUTABLE="\"$(bindir)/`echo gappletviewer | sed '$(program_transform_name)'`\"" \
	$(MOZILLA_CFLAGS) $(GLIB_CFLAGS) $(GTK_CFLAGS)

libgcjwebplugin_la_LDFLAGS = -avoid-version \
	$(GLIB_LIBS) $(GTK_LIBS) \
	-lstdc++ $(AM_LDFLAGS)

install-plugin: $(nativeexeclib_LTLIBRARIES)
	$(INSTALL) -d -m0755 $(DESTDIR)$(PLUGIN_DIR)
	$(INSTALL) .libs/libgcjwebplugin.so $(DESTDIR)$(PLUGIN_DIR)

uninstall-plugin:
	rm -f $(DESTDIR)$(PLUGIN_DIR)/libgcjwebplugin.so
