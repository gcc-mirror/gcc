/* upc-lang.c: UPC language-specific hooks
   Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011
   Free Software Foundation, Inc.
   Contributed by Gary Funck <gary@intrepid.com>
     and Nenad Vukicevic <nenad@intrepid.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "c-tree.h"
#include "c-family/c-common.h"
#include "ggc.h"
#include "upc-act.h"
#include "upc-pts.h"
#include "upc-genericize.h"
#include "upc-gasp.h"
#include "upc-pts.h"
#include "langhooks.h"
#include "langhooks-def.h"
#include "c-objc-common.h"
#include "toplev.h"
#include "diagnostic.h"
#include "c-family/c-pretty-print.h"
#include "c-family/c-pragma.h"
#include "c-family/c-upc.h"
#include "flags.h"
#include "opts.h"
#include "options.h"

/* Non-zero if the current compilation context is UPC */
int compiling_upc;

/* Non-zero if dwarf2 debugging information should
   encode UPC specific information. */
int use_upc_dwarf2_extensions;

/* Nonzero whenever UPC functionality is being used.  */
int flag_upc;

enum c_language_kind c_language = clk_upc;

static void upc_initialize_diagnostics (diagnostic_context *);
static void upc_init_options (unsigned int, struct cl_decoded_option *);
static bool upc_post_options (const char **);
static alias_set_type upc_get_alias_set (tree);
static void upc_init_ts (void);

/* UPC inherits hook definitions from "c-objc-common.h"
   and adds to them.  */

#undef LANG_HOOKS_NAME
#define LANG_HOOKS_NAME "GNU UPC"
#undef LANG_HOOKS_EXPAND_CONSTANT
#define LANG_HOOKS_EXPAND_CONSTANT upc_pts_build_constant
#undef LANG_HOOKS_GET_ALIAS_SET
#define LANG_HOOKS_GET_ALIAS_SET upc_get_alias_set
#undef LANG_HOOKS_GENERICIZE
#define LANG_HOOKS_GENERICIZE upc_genericize
#undef LANG_HOOKS_HANDLE_OPTION
#define LANG_HOOKS_HANDLE_OPTION upc_handle_option
#undef LANG_HOOKS_INIT
#define LANG_HOOKS_INIT upc_lang_init
#undef LANG_HOOKS_FINISH
#define LANG_HOOKS_FINISH upc_finish
#undef LANG_HOOKS_INITIALIZE_DIAGNOSTICS
#define LANG_HOOKS_INITIALIZE_DIAGNOSTICS upc_initialize_diagnostics
#undef LANG_HOOKS_INIT_OPTIONS
#define LANG_HOOKS_INIT_OPTIONS upc_init_options
#undef LANG_HOOKS_POST_OPTIONS
#define LANG_HOOKS_POST_OPTIONS upc_post_options
#undef LANG_HOOKS_TYPES_COMPATIBLE_P
#define LANG_HOOKS_TYPES_COMPATIBLE_P upc_types_compatible_p
#undef LANG_HOOKS_INIT_TS
#define LANG_HOOKS_INIT_TS upc_init_ts

/* Each front end provides its own hooks, for toplev.c.  */
struct lang_hooks lang_hooks = LANG_HOOKS_INITIALIZER;

static void
upc_initialize_diagnostics (diagnostic_context *context)
{
  pretty_printer *base = context->printer;
  c_pretty_printer *pp = (c_pretty_printer *)
                         xmalloc (sizeof (c_pretty_printer));
  memcpy (pp_base (pp), base, sizeof (pretty_printer));
  pp_c_pretty_printer_init (pp);
  context->printer = (pretty_printer *) pp;
  /* It is safe to free this object because it was previously malloc()'d.  */
  free (base);
}

/* Set the C 99 standard (without GNU extensions if ISO).
   (borrowed from c-opts.c) */

static void
set_std_c99 (int iso)
{
  cpp_set_lang (parse_in, iso ? CLK_STDC99: CLK_GNUC99);
  flag_no_asm = iso;
  flag_no_nonansi_builtin = iso;
  flag_iso = iso;
  flag_isoc99 = 1;
  flag_isoc94 = 1;
}

static void
upc_init_options (unsigned int decoded_options_count,
		  struct cl_decoded_option *decoded_options)
{
  struct cl_option_handlers handlers;

  c_common_init_options (decoded_options_count, decoded_options);

  /* UPC is based upon the C99 dialect. Assert it here.
   * We'll let the user override these options as he/she
   * sees fit. For example, -traditional will disable
   * prototype checking */
  set_std_c99 ( 0 /* iso=0 */ );

  /* The consensus of the UPC community seems to be that
     arithmetic on (void *) pointers and sizeof (void)
     are compilation errors.  Enable this warning-as-error
     mode by default.  */
  warn_pointer_arith = 1;
  set_default_handlers (&handlers);
  control_warning_option (OPT_Wpointer_arith, (int) DK_ERROR, true,
			  UNKNOWN_LOCATION, CL_C | CL_ObjC | CL_UPC,
			  &handlers, &global_options, &global_options_set,
			  global_dc);

#ifdef ENABLE_UPC_DWARF2_SUPPORT
  /* Some targets support UPC's DWARF2 extensions by default. */
  use_upc_dwarf2_extensions = 1;
#else
  use_upc_dwarf2_extensions = 0;
#endif

  flag_upc = 1;
  flag_upc_threads = 0;
  flag_upc_pthreads = 0;
  /* We begin in the state where we assume that we're compiling UPC 
     The 'compiling_upc' flag is queried when compiling for a
     pthreads environment to determine whether global static
     variables should be allocated to thread local storage. */
  compiling_upc = 1;
  /* By default, don't map UPC threads to POSIX threads. */
  flag_upc_pthreads = 0;
  upc_pthreads_model = upc_pthreads_no_model;
  flag_upc_pthreads_per_process = 0;
  /* By default, GASP profiling is off.  */
  flag_upc_instrument = 0;
  flag_upc_instrument_functions = 0;
  /* By default, optimization level > 0 defines shared access routines
     inlining, otherwise use the user specified flag for unconditional 
     enable/disable of inlining (0 - disable, 1 - enable) */
  flag_upc_inline_lib = -1;
  /* Disable section anchors. The presence of an unshared equivalent of the
     shared variables causes a double definition of the symbol names in the
     assembly code. */
  flag_section_anchors = 0;
}

static bool upc_post_options (const char **pfilename)
{
  return c_common_post_options (pfilename);
}

static alias_set_type
upc_get_alias_set (tree t)
{

  /* For the time being, make UPC pointers-to-shared conflict
     with everything else. Ideally, UPC pointers-to-shared should
     only conflict with the internal type used to represent
     the UPC pointer-to-shared (i.e., upc_pts_rep_type_node).  */

  if (TYPE_P (t) ? (TREE_CODE (t) == POINTER_TYPE
		    && upc_shared_type_p (TREE_TYPE (t)))
                 : (TREE_TYPE(t)
		    && TREE_CODE (TREE_TYPE (t)) == POINTER_TYPE
		    && upc_shared_type_p (TREE_TYPE (TREE_TYPE (t)))))
    return 0;


  /* Otherwise, do the default thing. */

  return c_common_get_alias_set (t);
}

static void
upc_init_ts (void)
{
  c_common_init_ts ();
  MARK_TS_COMMON (UPC_FORALL_STMT);
  MARK_TS_COMMON (UPC_SYNC_STMT);
}

#include "gtype-upc.h"
