/* Defs for interface to demanglers.
   Copyright 1992, 1993, 1994, 1995, 1996, 1997, 1998, 2000, 2001, 2002
   Free Software Foundation, Inc.
   
   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */


#if !defined (DEMANGLE_H)
#define DEMANGLE_H

#include "ansidecl.h"

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/* Options passed to cplus_demangle (in 2nd parameter). */

#define DMGL_NO_OPTS	 0		/* For readability... */
#define DMGL_PARAMS	 (1 << 0)	/* Include function args */
#define DMGL_ANSI	 (1 << 1)	/* Include const, volatile, etc */
#define DMGL_JAVA	 (1 << 2)	/* Demangle as Java rather than C++. */
#define DMGL_VERBOSE	 (1 << 3)	/* Include implementation details.  */
#define DMGL_TYPES	 (1 << 4)	/* Also try to demangle type encodings.  */

#define DMGL_AUTO	 (1 << 8)
#define DMGL_GNU	 (1 << 9)
#define DMGL_LUCID	 (1 << 10)
#define DMGL_ARM	 (1 << 11)
#define DMGL_HP 	 (1 << 12)       /* For the HP aCC compiler;
                                            same as ARM except for
                                            template arguments, etc. */
#define DMGL_EDG	 (1 << 13)
#define DMGL_GNU_V3	 (1 << 14)
#define DMGL_GNAT	 (1 << 15)

/* If none of these are set, use 'current_demangling_style' as the default. */
#define DMGL_STYLE_MASK (DMGL_AUTO|DMGL_GNU|DMGL_LUCID|DMGL_ARM|DMGL_HP|DMGL_EDG|DMGL_GNU_V3|DMGL_JAVA|DMGL_GNAT)

/* Enumeration of possible demangling styles.

   Lucid and ARM styles are still kept logically distinct, even though
   they now both behave identically.  The resulting style is actual the
   union of both.  I.E. either style recognizes both "__pt__" and "__rf__"
   for operator "->", even though the first is lucid style and the second
   is ARM style. (FIXME?) */

extern enum demangling_styles
{
  no_demangling = -1,
  unknown_demangling = 0,
  auto_demangling = DMGL_AUTO,
  gnu_demangling = DMGL_GNU,
  lucid_demangling = DMGL_LUCID,
  arm_demangling = DMGL_ARM,
  hp_demangling = DMGL_HP,
  edg_demangling = DMGL_EDG,
  gnu_v3_demangling = DMGL_GNU_V3,
  java_demangling = DMGL_JAVA,
  gnat_demangling = DMGL_GNAT
} current_demangling_style;

/* Define string names for the various demangling styles. */

#define NO_DEMANGLING_STYLE_STRING            "none"
#define AUTO_DEMANGLING_STYLE_STRING	      "auto"
#define GNU_DEMANGLING_STYLE_STRING    	      "gnu"
#define LUCID_DEMANGLING_STYLE_STRING	      "lucid"
#define ARM_DEMANGLING_STYLE_STRING	      "arm"
#define HP_DEMANGLING_STYLE_STRING	      "hp"
#define EDG_DEMANGLING_STYLE_STRING	      "edg"
#define GNU_V3_DEMANGLING_STYLE_STRING        "gnu-v3"
#define JAVA_DEMANGLING_STYLE_STRING          "java"
#define GNAT_DEMANGLING_STYLE_STRING          "gnat"

/* Some macros to test what demangling style is active. */

#define CURRENT_DEMANGLING_STYLE current_demangling_style
#define AUTO_DEMANGLING (((int) CURRENT_DEMANGLING_STYLE) & DMGL_AUTO)
#define GNU_DEMANGLING (((int) CURRENT_DEMANGLING_STYLE) & DMGL_GNU)
#define LUCID_DEMANGLING (((int) CURRENT_DEMANGLING_STYLE) & DMGL_LUCID)
#define ARM_DEMANGLING (((int) CURRENT_DEMANGLING_STYLE) & DMGL_ARM)
#define HP_DEMANGLING (((int) CURRENT_DEMANGLING_STYLE) & DMGL_HP)
#define EDG_DEMANGLING (((int) CURRENT_DEMANGLING_STYLE) & DMGL_EDG)
#define GNU_V3_DEMANGLING (((int) CURRENT_DEMANGLING_STYLE) & DMGL_GNU_V3)
#define JAVA_DEMANGLING (((int) CURRENT_DEMANGLING_STYLE) & DMGL_JAVA)
#define GNAT_DEMANGLING (((int) CURRENT_DEMANGLING_STYLE) & DMGL_GNAT)

/* Provide information about the available demangle styles. This code is
   pulled from gdb into libiberty because it is useful to binutils also.  */

extern const struct demangler_engine
{
  const char *const demangling_style_name;
  const enum demangling_styles demangling_style;
  const char *const demangling_style_doc;
} libiberty_demanglers[];

extern char *
cplus_demangle PARAMS ((const char *mangled, int options));

extern int
cplus_demangle_opname PARAMS ((const char *opname, char *result, int options));

extern const char *
cplus_mangle_opname PARAMS ((const char *opname, int options));

/* Note: This sets global state.  FIXME if you care about multi-threading. */

extern void
set_cplus_marker_for_demangling PARAMS ((int ch));

extern enum demangling_styles 
cplus_demangle_set_style PARAMS ((enum demangling_styles style));

extern enum demangling_styles 
cplus_demangle_name_to_style PARAMS ((const char *name));

/* V3 ABI demangling entry points, defined in cp-demangle.c.  */
extern char*
cplus_demangle_v3 PARAMS ((const char* mangled, int options));

extern char*
java_demangle_v3 PARAMS ((const char* mangled));


enum gnu_v3_ctor_kinds {
  gnu_v3_complete_object_ctor = 1,
  gnu_v3_base_object_ctor,
  gnu_v3_complete_object_allocating_ctor
};

/* Return non-zero iff NAME is the mangled form of a constructor name
   in the G++ V3 ABI demangling style.  Specifically, return an `enum
   gnu_v3_ctor_kinds' value indicating what kind of constructor
   it is.  */
extern enum gnu_v3_ctor_kinds
	is_gnu_v3_mangled_ctor PARAMS ((const char *name));


enum gnu_v3_dtor_kinds {
  gnu_v3_deleting_dtor = 1,
  gnu_v3_complete_object_dtor,
  gnu_v3_base_object_dtor
};

/* Return non-zero iff NAME is the mangled form of a destructor name
   in the G++ V3 ABI demangling style.  Specifically, return an `enum
   gnu_v3_dtor_kinds' value, indicating what kind of destructor
   it is.  */
extern enum gnu_v3_dtor_kinds
	is_gnu_v3_mangled_dtor PARAMS ((const char *name));

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif	/* DEMANGLE_H */
