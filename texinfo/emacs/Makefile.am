## Makefile.am for texinfo/emacs.
## $Id: Makefile.am,v 1.1.1.1 1998/03/23 04:42:12 law Exp $
## Run automake in .. to produce Makefile.in from this.

# Do not byte compile our Elisp files by default.
ELCFILES =

# Do not install our Elisp files by default, either.
noinst_LISP = info.el informat.el makeinfo.el \
              texinfmt.el texinfo.el texnfo-upd.el

EXTRA_DIST = README elisp-comp new-useful-setqs $(noinst_LISP)

install-data-local:
	@echo "WARNING: You must (compile and) install the Emacs Lisp files"
	@echo "WARNING: manually.  See ./emacs/README for some considerations."

# For some reason these do not get defined.
distclean-lisp:
clean-lisp:
mostlyclean-lisp:
