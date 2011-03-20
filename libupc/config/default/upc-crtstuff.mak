
ALL_CRT_CFLAGS = $(CFLAGS) $(CRTSTUFF_CFLAGS) $(INCLUDES) $(UPC_CRTSTUFF_CFLAGS)
GCC_SRCDIR_CONF_INC = -I$(srcdir)/../gcc/config
GCC_OBJDIR_INC = -I$(gcc_objdir)

crt_compile = $(CC) $(ALL_CRT_CFLAGS) $(AM_CPPFLAGS) \
              $(GCC_SRCDIR_CONF_INC) $(GCC_OBJDIR_INC) 

upc_crtstuff_src = $(top_srcdir)/upc-crtstuff.c

# UPC related begin/end files
upc-crtbegin.$(OBJEXT): $(upc_crtstuff_src)
	$(crt_compile) -DCRT_BEGIN $(CRTSTUFF_T_CFLAGS) -c $< -o $@

upc-crtend.$(OBJEXT): $(upc_crtstuff_src)
	$(crt_compile) -DCRT_END $(CRTSTUFF_T_CFLAGS) -c $< -o $@

# upc-crtbegin and upc-crtend for shared libraries
upc-crtbeginS.$(OBJEXT): $(upc_crtstuff_src)
	$(crt_compile) -DCRT_BEGIN $(CRTSTUFF_T_CFLAGS_S) -c $< -o $@

upc-crtendS.$(OBJEXT): $(upc_crtstuff_src)
	$(crt_compile) -DCRT_END $(CRTSTUFF_T_CFLAGS_S) -c $< -o $@

# upc-crtbegin and upc-crtend for -static links
upc-crtbeginT.$(OBJEXT): $(upc_crtstuff_src)
	$(crt_compile) -DCRT_BEGIN $(CRTSTUFF_T_CFLAGS) -c $< -o $@

upc-crtendT.$(OBJEXT): $(upc_crtstuff_src)
	$(crt_compile) -DCRT_END $(CRTSTUFF_T_CFLAGS) -c $< -o $@
