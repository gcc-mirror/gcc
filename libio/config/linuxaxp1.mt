# We _do_ need a new _G_config.h
_G_CONFIG_H=_G_config.h

# And the two bits files.
all:
install: install-axp-mt-headers

install-axp-mt-headers:
	$(INSTALL_DATA) libc-lock.h $(gxx_include_dir)/libc-lock.h
	$(INSTALL_DATA) stdio-lock.h $(gxx_include_dir)/stdio-lock.h
