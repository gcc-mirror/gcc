/* ANSI and traditional C compatability macros
   Copyright 1991, 1992, 1993, 1994, 1995, 1996, 1998, 1999, 2000
   Free Software Foundation, Inc.
   This file is part of the GNU C Library.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

/* ANSI and traditional C compatibility macros

   ANSI C is assumed if __STDC__ is #defined.

   Macro	ANSI C definition	Traditional C definition
   -----	---- - ----------	----------- - ----------
   PTR		`void *'		`char *'
   LONG_DOUBLE	`long double'		`double'
   VOLATILE	`volatile'		`'
   SIGNED	`signed'		`'
   PTRCONST	`void *const'		`char *'
   ANSI_PROTOTYPES  1			not defined

   CONST is also defined, but is obsolete.  Just use const.

   obsolete --     DEFUN (name, arglist, args)

	Defines function NAME.

	ARGLIST lists the arguments, separated by commas and enclosed in
	parentheses.  ARGLIST becomes the argument list in traditional C.

	ARGS list the arguments with their types.  It becomes a prototype in
	ANSI C, and the type declarations in traditional C.  Arguments should
	be separated with `AND'.  For functions with a variable number of
	arguments, the last thing listed should be `DOTS'.

   obsolete --     DEFUN_VOID (name)

	Defines a function NAME, which takes no arguments.

   obsolete --     EXFUN (name, (prototype))	-- obsolete.

	Replaced by PARAMS.  Do not use; will disappear someday soon.
	Was used in external function declarations.
	In ANSI C it is `NAME PROTOTYPE' (so PROTOTYPE should be enclosed in
	parentheses).  In traditional C it is `NAME()'.
	For a function that takes no arguments, PROTOTYPE should be `(void)'.

   obsolete --     PROTO (type, name, (prototype)    -- obsolete.

	This one has also been replaced by PARAMS.  Do not use.

   PARAMS ((args))

	We could use the EXFUN macro to handle prototype declarations, but
	the name is misleading and the result is ugly.  So we just define a
	simple macro to handle the parameter lists, as in:

	      static int foo PARAMS ((int, char));

	This produces:  `static int foo();' or `static int foo (int, char);'

	EXFUN would have done it like this:

	      static int EXFUN (foo, (int, char));

	but the function is not external...and it's hard to visually parse
	the function name out of the mess.   EXFUN should be considered
	obsolete; new code should be written to use PARAMS.

   DOTS is also obsolete.

   Examples:

	extern int printf PARAMS ((const char *format, ...));
*/

#ifndef	_ANSIDECL_H

#define	_ANSIDECL_H	1


/* Every source file includes this file,
   so they will all get the switch for lint.  */
/* LINTLIBRARY */


#if defined (__STDC__) || defined (_AIX) || (defined (__mips) && defined (_SYSTYPE_SVR4)) || defined(_WIN32)
/* All known AIX compilers implement these things (but don't always
   define __STDC__).  The RISC/OS MIPS compiler defines these things
   in SVR4 mode, but does not define __STDC__.  */

#define	PTR		void *
#define	PTRCONST	void *CONST
#define	LONG_DOUBLE	long double

#ifndef IN_GCC
#define	AND		,
#define	NOARGS		void
#define	VOLATILE	volatile
#define	SIGNED		signed
#endif /* ! IN_GCC */

#define PARAMS(paramlist)		paramlist
#define ANSI_PROTOTYPES			1

#define VPARAMS(ARGS)			ARGS
#define VA_START(va_list,var)		va_start(va_list,var)

/* These are obsolete.  Do not use.  */
#ifndef IN_GCC
#define CONST				const
#define DOTS				, ...
#define PROTO(type, name, arglist)	type name arglist
#define EXFUN(name, proto)		name proto
#define DEFUN(name, arglist, args)	name(args)
#define DEFUN_VOID(name)		name(void)
#endif /* ! IN_GCC */

#else	/* Not ANSI C.  */

#define	PTR		char *
#define	PTRCONST	PTR
#define	LONG_DOUBLE	double

#ifndef IN_GCC
#define	AND		;
#define	NOARGS
#define	VOLATILE
#define	SIGNED
#endif /* !IN_GCC */

#ifndef const /* some systems define it in header files for non-ansi mode */
#define	const
#endif

#define PARAMS(paramlist)		()

#define VPARAMS(ARGS)			(va_alist) va_dcl
#define VA_START(va_list,var)		va_start(va_list)

/* These are obsolete.  Do not use.  */
#ifndef IN_GCC
#define CONST
#define DOTS
#define PROTO(type, name, arglist)	type name ()
#define EXFUN(name, proto)		name()
#define DEFUN(name, arglist, args)	name arglist args;
#define DEFUN_VOID(name)		name()
#endif /* ! IN_GCC */

#endif	/* ANSI C.  */


/* Using MACRO(x,y) in cpp #if conditionals does not work with some
   older preprocessors.  Thus we can't define something like this:

#define HAVE_GCC_VERSION(MAJOR, MINOR) \
  (__GNUC__ > (MAJOR) || (__GNUC__ == (MAJOR) && __GNUC_MINOR__ >= (MINOR)))

and then test "#if HAVE_GCC_VERSION(2,7)".

So instead we use the macro below and test it against specific values.  */

/* This macro simplifies testing whether we are using gcc, and if it
   is of a particular minimum version. (Both major & minor numbers are
   significant.)  This macro will evaluate to 0 if we are not using
   gcc at all.  */
#ifndef GCC_VERSION
#define GCC_VERSION (__GNUC__ * 1000 + __GNUC_MINOR__)
#endif /* GCC_VERSION */

/* Define macros for some gcc attributes.  This permits us to use the
   macros freely, and know that they will come into play for the
   version of gcc in which they are supported.  */

#if (GCC_VERSION < 2007)
# define __attribute__(x)
#endif

/* Attribute __malloc__ on functions was valid as of gcc 2.96. */
#ifndef ATTRIBUTE_MALLOC
# if (GCC_VERSION >= 2096)
#  define ATTRIBUTE_MALLOC __attribute__ ((__malloc__))
# else
#  define ATTRIBUTE_MALLOC
# endif /* GNUC >= 2.96 */
#endif /* ATTRIBUTE_MALLOC */

/* Attributes on labels were valid as of gcc 2.93. */
#ifndef ATTRIBUTE_UNUSED_LABEL
# if (GCC_VERSION >= 2093)
#  define ATTRIBUTE_UNUSED_LABEL ATTRIBUTE_UNUSED
# else
#  define ATTRIBUTE_UNUSED_LABEL
# endif /* GNUC >= 2.93 */
#endif /* ATTRIBUTE_UNUSED_LABEL */

#ifndef ATTRIBUTE_UNUSED
#define ATTRIBUTE_UNUSED __attribute__ ((__unused__))
#endif /* ATTRIBUTE_UNUSED */

#ifndef ATTRIBUTE_NORETURN
#define ATTRIBUTE_NORETURN __attribute__ ((__noreturn__))
#endif /* ATTRIBUTE_NORETURN */

#ifndef ATTRIBUTE_PRINTF
#define ATTRIBUTE_PRINTF(m, n) __attribute__ ((__format__ (__printf__, m, n)))
#define ATTRIBUTE_PRINTF_1 ATTRIBUTE_PRINTF(1, 2)
#define ATTRIBUTE_PRINTF_2 ATTRIBUTE_PRINTF(2, 3)
#define ATTRIBUTE_PRINTF_3 ATTRIBUTE_PRINTF(3, 4)
#define ATTRIBUTE_PRINTF_4 ATTRIBUTE_PRINTF(4, 5)
#define ATTRIBUTE_PRINTF_5 ATTRIBUTE_PRINTF(5, 6)
#endif /* ATTRIBUTE_PRINTF */

/* We use __extension__ in some places to suppress -pedantic warnings
   about GCC extensions.  This feature didn't work properly before
   gcc 2.8.  */
#if GCC_VERSION < 2008
#define __extension__
#endif

#endif	/* ansidecl.h	*/
