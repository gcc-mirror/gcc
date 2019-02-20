/* Dependency generator for Makefile fragments.
   Copyright (C) 2000-2019 Free Software Foundation, Inc.
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

#include "config.h"
#include "system.h"
#include "mkdeps.h"

/* Not set up to just include std::vector et al, here's a simple
   implementation.  */

/* Keep this structure local to this file, so clients don't find it
   easy to start making assumptions.  */
struct mrules
{
public:
  /* T has trivial cctor & dtor.  */
  template <typename T>
  class vec
  {
  private:
    T *ary;
    unsigned num;
    unsigned alloc;

  public:
    vec ()
      : ary (NULL), num (0), alloc (0)
      {}
    ~vec ()
      {
	XDELETEVEC (ary);
      }

  public:
    unsigned size () const
    {
      return num;
    }
    const T &operator[] (unsigned ix) const
    {
      return ary[ix];
    }
    void push (const T &elt)
    {
      if (num == alloc)
	{
	  alloc = alloc ? alloc * 2 : 16;
	  ary = XRESIZEVEC (T, ary, alloc);
	}
      ary[num++] = elt;
    }
  };
  struct velt
  {
    const char *str;
    size_t len;
  };

  mrules ()
    : module_name (NULL), bmi_name (NULL), is_legacy (false)
  {
  }
  ~mrules ()
  {
    unsigned int i;

    for (i = targets.size (); i--;)
      XDELETEVEC (targets[i]);
    for (i = deps.size (); i--;)
      XDELETEVEC (deps[i]);
    for (i = vpath.size (); i--;)
      XDELETEVEC (vpath[i].str);
    for (i = modules.size (); i--;)
      XDELETEVEC (modules[i]);
    XDELETEVEC (module_name);
    XDELETEVEC (bmi_name);
  }

public:
  vec<const char *> targets;
  vec<const char *> deps;
  vec<velt> vpath;
  vec<const char *> modules;

public:
  const char *module_name;
  const char *bmi_name;
  bool is_legacy;
};

/* Given a filename, quote characters in that filename which are
   significant to Make.  Note that it's not possible to quote all such
   characters - e.g. \n, %, *, ?, [, \ (in some contexts), and ~ are
   not properly handled.  It isn't possible to get this right in any
   current version of Make.  (??? Still true?  Old comment referred to
   3.76.1.)  */

static const char *
munge (const char *str, const char *trail = NULL, ...)
{
  static unsigned alloc;
  static char *buf;
  unsigned dst = 0;
  va_list args;
  if (trail)
    va_start (args, trail);

  for (bool first = true; str; first = false)
    {
      unsigned slashes = 0;
      char c;
      for (const char *probe = str; (c = *probe++);)
	{
	  if (alloc < dst + 4 + slashes)
	    {
	      alloc = alloc * 2 + 32;
	      buf = XRESIZEVEC (char, buf, alloc);
	    }

	  switch (c)
	    {
	    case '\\':
	      slashes++;
	      break;

	    case '$':
	      buf[dst++] = '$';
	      goto def;

	    case ' ':
	    case '\t':
	      /* GNU make uses a weird quoting scheme for white space.
		 A space or tab preceded by 2N+1 backslashes
		 represents N backslashes followed by space; a space
		 or tab preceded by 2N backslashes represents N
		 backslashes at the end of a file name; and
		 backslashes in other contexts should not be
		 doubled.  */
	      while (slashes--)
		buf[dst++] = '\\';
	      /* FALLTHROUGH  */

	    case '#':
	      buf[dst++] = '\\';
	      /* FALLTHROUGH  */

	    default:
	    def:
	      slashes = 0;
	      break;
	    }

	  buf[dst++] = c;
	}

      if (first)
	str = trail;
      else
	str = va_arg (args, const char *);
    }
  if (trail)
    va_end (args);

  buf[dst] = 0;
  char *res = XNEWVEC (char, dst + 1);
  memcpy (res, buf, dst + 1);
  return res;
}

/* If T begins with any of the partial pathnames listed in d->vpathv,
   then advance T to point beyond that pathname.  */
static const char *
apply_vpath (struct mrules *d, const char *t)
{
  if (unsigned len = d->vpath.size ())
    for (unsigned i = len; i--;)
      {
	if (!filename_ncmp (d->vpath[i].str, t, d->vpath[i].len))
	  {
	    const char *p = t + d->vpath[i].len;
	    if (!IS_DIR_SEPARATOR (*p))
	      goto not_this_one;

	    /* Do not simplify $(vpath)/../whatever.  ??? Might not
	       be necessary. */
	    if (p[1] == '.' && p[2] == '.' && IS_DIR_SEPARATOR (p[3]))
	      goto not_this_one;

	    /* found a match */
	    t = t + d->vpath[i].len + 1;
	    break;
	  }
      not_this_one:;
      }

  /* Remove leading ./ in any case.  */
  while (t[0] == '.' && IS_DIR_SEPARATOR (t[1]))
    {
      t += 2;
      /* If we removed a leading ./, then also remove any /s after the
	 first.  */
      while (IS_DIR_SEPARATOR (t[0]))
	++t;
    }

  return t;
}

/* Public routines.  */

struct mrules *
deps_init (void)
{
  return new mrules ();
}

void
deps_free (struct mrules *d)
{
  delete d;
}

/* Adds a target T.  We make a copy, so it need not be a permanent
   string.  QUOTE is true if the string should be quoted.  */
void
deps_add_target (struct mrules *d, const char *t, int quote)
{
  t = apply_vpath (d, t);
  if (quote)
    t = munge (t);  /* Also makes permanent copy.  */
  else
    t = xstrdup (t);

  d->targets.push (t);
}

/* Sets the default target if none has been given already.  An empty
   string as the default target in interpreted as stdin.  The string
   is quoted for MAKE.  */
void
deps_add_default_target (struct mrules *d, const char *tgt)
{
  /* Only if we have no targets.  */
  if (d->targets.size ())
    return;

  if (tgt[0] == '\0')
    deps_add_target (d, "-", 1);
  else
    {
#ifndef TARGET_OBJECT_SUFFIX
# define TARGET_OBJECT_SUFFIX ".o"
#endif
      const char *start = lbasename (tgt);
      char *o = (char *) alloca (strlen (start)
                                 + strlen (TARGET_OBJECT_SUFFIX) + 1);
      char *suffix;

      strcpy (o, start);

      suffix = strrchr (o, '.');
      if (!suffix)
        suffix = o + strlen (o);
      strcpy (suffix, TARGET_OBJECT_SUFFIX);

      deps_add_target (d, o, 1);
    }
}

void
deps_add_dep (struct mrules *d, const char *t)
{
  t = munge (apply_vpath (d, t));  /* Also makes permanent copy.  */

  d->deps.push (t);
}

void
deps_add_vpath (struct mrules *d, const char *vpath)
{
  const char *elem, *p;

  for (elem = vpath; *elem; elem = p)
    {
      for (p = elem; *p && *p != ':'; p++)
	continue;
      mrules::velt elt;
      elt.len = p - elem;
      char *str = XNEWVEC (char, elt.len + 1);
      elt.str = str;
      memcpy (str, elem, elt.len);
      str[elt.len] = '\0';
      if (*p == ':')
	p++;

      d->vpath.push (elt);
    }
}

void
deps_add_module (struct mrules *d, const char *m, const char *p,
		 const char *bmi, bool is_legacy)
{
  m = munge (p, m, ".c++m", NULL);
  if (bmi)
    {
      d->module_name = m;
      d->is_legacy = is_legacy;
      d->bmi_name = munge (bmi);
    }
  else
    d->modules.push (m);
}

static unsigned
write_name (const char *name, FILE *fp, unsigned col, unsigned colmax)
{
  unsigned size = strlen (name);

  if (col)
    {
      if (colmax && col + size> colmax)
	{
	  fputs (" \\\n", fp);
	  col = 0;
	}
      col++;
      fputs (" ", fp);
    }

  col += size;
  fputs (name, fp);

  return col;
}

static unsigned
write_vec (const mrules::vec<const char *> &vec, FILE *fp,
	   unsigned col, unsigned colmax)
{
  for (unsigned ix = 0; ix != vec.size (); ix++)
    col = write_name (vec[ix], fp, col, colmax);
  return col;
}

void
deps_write (const struct mrules *d, FILE *fp, unsigned int colmax)
{
  unsigned column = 0;
  if (colmax && colmax < 34)
    colmax = 34;

  column = write_vec (d->targets, fp, column, colmax);
  if (d->bmi_name)
    column = write_name (d->bmi_name, fp, column, colmax);
  fputs (":", fp);
  column++;
  column = write_vec (d->deps, fp, column, colmax);
  column = write_vec (d->modules, fp, column, colmax);
  fputs ("\n", fp);
  column = 0;

  if (d->module_name)
    {
      /* module-name : bmi-name */
      column = write_name (d->module_name, fp, column, colmax);
      fputs (":", fp);
      column++;
      column = write_name (d->bmi_name, fp, column, colmax);
      fputs ("\n", fp);

      /* bmi-name :| first-target */
      if (!d->is_legacy)
	{
	  column = 0;
	  column = write_name (d->bmi_name, fp, column, colmax);
	  fputs (":|", fp);
	  column++;
	  column = write_name (d->targets[0], fp, column, colmax);
	  fputs ("\n", fp);
	}
      column = fprintf (fp, "CXX_IMPORTS +=");
      write_vec (d->modules, fp, column, colmax);
      fputs ("\n", fp);
    }
}

void
deps_phony_targets (const struct mrules *d, FILE *fp)
{
  unsigned int i;

  for (i = 1; i < d->deps.size (); i++)
    fprintf (fp, "\n%s:\n", d->deps[i]);
}

/* Write out a deps buffer to a file, in a form that can be read back
   with deps_restore.  Returns nonzero on error, in which case the
   error number will be in errno.  */

int
deps_save (struct mrules *deps, FILE *f)
{
  unsigned int i;
  size_t size;

  /* The cppreader structure contains makefile dependences.  Write out this
     structure.  */

  /* The number of dependences.  */
  size = deps->deps.size ();
  if (fwrite (&size, sizeof (size), 1, f) != 1)
    return -1;

  /* The length of each dependence followed by the string.  */
  for (i = 0; i < deps->deps.size (); i++)
    {
      size = strlen (deps->deps[i]);
      if (fwrite (&size, sizeof (size), 1, f) != 1)
	return -1;
      if (fwrite (deps->deps[i], size, 1, f) != 1)
	return -1;
    }

  return 0;
}

/* Read back dependency information written with deps_save into
   the deps sizefer.  The third argument may be NULL, in which case
   the dependency information is just skipped, or it may be a filename,
   in which case that filename is skipped.  */

int
deps_restore (struct mrules *deps, FILE *fd, const char *self)
{
  size_t size;
  char *buf = NULL;
  size_t buf_size = 0;

  /* Number of dependences.  */
  if (fread (&size, sizeof (size), 1, fd) != 1)
    return -1;

  /* The length of each dependence string, followed by the string.  */
  for (unsigned i = size; i--;)
    {
      /* Read in # bytes in string.  */
      if (fread (&size, sizeof (size), 1, fd) != 1)
	return -1;

      if (size >= buf_size)
	{
	  buf_size = size + 512;
	  buf = XRESIZEVEC (char, buf, buf_size);
	}
      if (fread (buf, 1, size, fd) != size)
	{
	  XDELETEVEC (buf);
	  return -1;
	}
      buf[size] = 0;

      /* Generate makefile dependencies from .pch if -nopch-deps.  */
      // FIXME: This seems strange, the elements have already been
      // munged.
      if (self != NULL && filename_cmp (buf, self) != 0)
        deps_add_dep (deps, buf);
    }

  XDELETEVEC (buf);
  return 0;
}
