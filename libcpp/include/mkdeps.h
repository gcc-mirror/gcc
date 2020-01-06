/* Dependency generator for Makefile fragments.
   Copyright (C) 2000-2020 Free Software Foundation, Inc.
   Contributed by Zack Weinberg, Mar 2000

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.

 In other words, you are welcome to use, share and improve this program.
 You are forbidden to forbid anyone else to use, share and improve
 what you give them.   Help stamp out software-hoarding!  */

#ifndef LIBCPP_MKDEPS_H
#define LIBCPP_MKDEPS_H

/* This is the data structure used by all the functions in mkdeps.c.
   It's quite straightforward, but should be treated as opaque.  */

class mkdeps;

/* Create a deps buffer.  */
extern class mkdeps *deps_init (void);

/* Destroy a deps buffer.  */
extern void deps_free (class mkdeps *);

/* Add a set of "vpath" directories. The second argument is a colon-
   separated list of pathnames, like you would set Make's VPATH
   variable to.  If a dependency or target name begins with any of
   these pathnames (and the next path element is not "..") that
   pathname is stripped off.  */
extern void deps_add_vpath (class mkdeps *, const char *);

/* Add a target (appears on left side of the colon) to the deps list.  Takes
   a boolean indicating whether to quote the target for MAKE.  */
extern void deps_add_target (class mkdeps *, const char *, int);

/* Sets the default target if none has been given already.  An empty
   string as the default target is interpreted as stdin.  */
extern void deps_add_default_target (class mkdeps *, const char *);

/* Add a dependency (appears on the right side of the colon) to the
   deps list.  Dependencies will be printed in the order that they
   were entered with this function.  By convention, the first
   dependency entered should be the primary source file.  */
extern void deps_add_dep (class mkdeps *, const char *);

/* Write out a deps buffer to a specified file.  The third argument
   is the number of columns to word-wrap at (0 means don't wrap).  */
extern void deps_write (const class mkdeps *, FILE *, bool, unsigned int);

/* Write out a deps buffer to a file, in a form that can be read back
   with deps_restore.  Returns nonzero on error, in which case the
   error number will be in errno.  */
extern int deps_save (class mkdeps *, FILE *);

/* Read back dependency information written with deps_save into
   the deps buffer.  The third argument may be NULL, in which case
   the dependency information is just skipped, or it may be a filename,
   in which case that filename is skipped.  */
extern int deps_restore (class mkdeps *, FILE *, const char *);

#endif /* ! LIBCPP_MKDEPS_H */
