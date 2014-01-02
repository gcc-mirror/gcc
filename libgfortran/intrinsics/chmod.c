/* Implementation of the CHMOD intrinsic.
   Copyright (C) 2006-2014 Free Software Foundation, Inc.
   Contributed by François-Xavier Coudert <coudert@clipper.ens.fr>

This file is part of the GNU Fortran runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public
License as published by the Free Software Foundation; either
version 3 of the License, or (at your option) any later version.

Libgfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#include "libgfortran.h"

#if defined(HAVE_SYS_STAT_H)

#include <string.h>	/* For memcpy. */
#include <sys/stat.h>	/* For stat, chmod and umask.  */


/* INTEGER FUNCTION CHMOD (NAME, MODE)
   CHARACTER(len=*), INTENT(IN) :: NAME, MODE

   Sets the file permission "chmod" using a mode string.

   For MinGW, only _S_IWRITE and _S_IREAD are supported. To set those,
   only the user attributes are used.

   The mode string allows for the same arguments as POSIX's chmod utility.
   a) string containing an octal number.
   b) Comma separated list of clauses of the form:
      [<who-list>]<op>[<perm-list>|<permcopy>][<op>[<perm-list>|<permcopy>],...]
      <who> - 'u', 'g', 'o', 'a'
      <op>  - '+', '-', '='
      <perm> - 'r', 'w', 'x', 'X', 's', t'
   If <op> is not followed by a perm-list or permcopy, '-' and '+' do not
   change the mode while '=' clears all file mode bits. 'u' stands for the
   user permissions, 'g' for the group and 'o' for the permissions for others.
   'a' is equivalent to 'ugo'. '+' sets the given permission in addition to
   the ones of the file, '-' unsets the given permissions of the file, while
   '=' sets the file to that mode. 'r' sets the read, 'w' the write, and
   'x' the execute mode. 'X' sets the execute bit if the file is a directory
   or if the user, group or other executable bit is set. 't' sets the sticky
   bit, 's' (un)sets the and/or S_ISUID/S_ISGID bit.

   Note that if <who> is omitted, the permissions are filtered by the umask.

   A return value of 0 indicates success, -1 an error of chmod() while 1
   indicates a mode parsing error.  */

extern int chmod_func (char *, char *, gfc_charlen_type, gfc_charlen_type);
export_proto(chmod_func);

int
chmod_func (char *name, char *mode, gfc_charlen_type name_len,
	    gfc_charlen_type mode_len)
{
  char * file;
  int i;
  bool ugo[3];
  bool rwxXstugo[9];
  int set_mode, part;
  bool honor_umask, continue_clause = false;
#ifndef __MINGW32__
  bool is_dir;
#endif
  mode_t mode_mask, file_mode, new_mode;
  struct stat stat_buf;

  /* Trim trailing spaces of the file name.  */
  while (name_len > 0 && name[name_len - 1] == ' ')
    name_len--;

  /* Make a null terminated copy of the file name.  */
  file = gfc_alloca (name_len + 1);
  memcpy (file, name, name_len);
  file[name_len] = '\0';

  if (mode_len == 0)
    return 1;

  if (mode[0] >= '0' && mode[0] <= '9')
    {
#ifdef __MINGW32__
      unsigned fmode;
      if (sscanf (mode, "%o", &fmode) != 1)
	return 1;
      file_mode = (mode_t) fmode;
#else
      if (sscanf (mode, "%o", &file_mode) != 1)
	return 1;
#endif
      return chmod (file, file_mode);
    }

  /* Read the current file mode. */
  if (stat (file, &stat_buf))
    return 1;

  file_mode = stat_buf.st_mode & ~S_IFMT;
#ifndef __MINGW32__
  is_dir = stat_buf.st_mode & S_IFDIR;
#endif

#ifdef HAVE_UMASK
  /* Obtain the umask without distroying the setting.  */
  mode_mask = 0;
  mode_mask = umask (mode_mask);
  (void) umask (mode_mask);
#else
  honor_umask = false;
#endif

  for (i = 0; i < mode_len; i++)
    {
      if (!continue_clause)
	{
	  ugo[0] = false;
	  ugo[1] = false;
	  ugo[2] = false;
#ifdef HAVE_UMASK
	  honor_umask = true;
#endif
	}
      continue_clause = false; 
      rwxXstugo[0] = false;
      rwxXstugo[1] = false;
      rwxXstugo[2] = false;
      rwxXstugo[3] = false;
      rwxXstugo[4] = false;
      rwxXstugo[5] = false;
      rwxXstugo[6] = false;
      rwxXstugo[7] = false;
      rwxXstugo[8] = false;
      part = 0;
      set_mode = -1;
      for (; i < mode_len; i++)
	{
	  switch (mode[i])
	    {
	    /* User setting: a[ll]/u[ser]/g[roup]/o[ther].  */
	    case 'a':
	      if (part > 1)
		return 1;
	      ugo[0] = true;
	      ugo[1] = true;
	      ugo[2] = true;
	      part = 1;
#ifdef HAVE_UMASK
	      honor_umask = false;
#endif
	      break;
	    case 'u':
	      if (part == 2)
		{
		  rwxXstugo[6] = true; 
		  part = 4;
		  break; 
		}
	      if (part > 1)
		return 1;
	      ugo[0] = true;
	      part = 1;
#ifdef HAVE_UMASK
	      honor_umask = false;
#endif
	      break;
	    case 'g':
	      if (part == 2)
		{
		  rwxXstugo[7] = true; 
		  part = 4;
		  break; 
		}
	      if (part > 1)
		return 1;
       	      ugo[1] = true;
	      part = 1;
#ifdef HAVE_UMASK
	      honor_umask = false;
#endif
	      break;
	    case 'o':
	      if (part == 2)
		{
		  rwxXstugo[8] = true; 
		  part = 4;
		  break; 
		}
	      if (part > 1)
		return 1;
	      ugo[2] = true;
	      part = 1;
#ifdef HAVE_UMASK
	      honor_umask = false;
#endif
	      break;

	    /* Mode setting: =+-.  */
	    case '=':
	      if (part > 2)
		{
		  continue_clause = true;
		  i--;
		  part = 2;
		  goto clause_done;
		}
	      set_mode = 1;
	      part = 2;
	      break;

	    case '-':
	      if (part > 2)
		{
		  continue_clause = true;
		  i--;
		  part = 2;
		  goto clause_done;
		}
	      set_mode = 2;
	      part = 2;
	      break;

	    case '+':
	      if (part > 2)
		{
		  continue_clause = true;
		  i--;
		  part = 2;
		  goto clause_done;
		}
	      set_mode = 3;
	      part = 2;
	      break;

	    /* Permissions: rwxXst - for ugo see above.  */
	    case 'r':
	      if (part != 2 && part != 3)
		return 1;
	      rwxXstugo[0] = true;
	      part = 3;
	      break;

	    case 'w':
	      if (part != 2 && part != 3)
		return 1;
	      rwxXstugo[1] = true;
	      part = 3;
	      break;

	    case 'x':
	      if (part != 2 && part != 3)
		return 1;
	      rwxXstugo[2] = true;
	      part = 3;
	      break;

	    case 'X':
	      if (part != 2 && part != 3)
		return 1;
	      rwxXstugo[3] = true;
	      part = 3;
	      break;

	    case 's':
	      if (part != 2 && part != 3)
		return 1;
	      rwxXstugo[4] = true;
	      part = 3;
	      break;

	    case 't':
	      if (part != 2 && part != 3)
		return 1;
	      rwxXstugo[5] = true;
	      part = 3;
	      break;

	    /* Tailing blanks are valid in Fortran.  */
	    case ' ':
	      for (i++; i < mode_len; i++)
		if (mode[i] != ' ')
		  break;
	      if (i != mode_len)
		return 1;
	      goto clause_done;

	    case ',':
	      goto clause_done;

	    default:
	      return 1;
	    }
	}

clause_done:
      if (part < 2)
	return 1;

      new_mode = 0;

#ifdef __MINGW32__

      /* Read. */
      if (rwxXstugo[0] && (ugo[0] || honor_umask))
	new_mode |= _S_IREAD;

      /* Write. */
      if (rwxXstugo[1] && (ugo[0] || honor_umask))
	new_mode |= _S_IWRITE;

#else

      /* Read. */
      if (rwxXstugo[0])
	{
	  if (ugo[0] || honor_umask)
	    new_mode |= S_IRUSR;
	  if (ugo[1] || honor_umask)
	    new_mode |= S_IRGRP;
	  if (ugo[2] || honor_umask)
	    new_mode |= S_IROTH;
	}

      /* Write.  */
      if (rwxXstugo[1])
	{
	  if (ugo[0] || honor_umask)
	    new_mode |= S_IWUSR;
	  if (ugo[1] || honor_umask)
	    new_mode |= S_IWGRP;
	  if (ugo[2] || honor_umask)
	    new_mode |= S_IWOTH;
	}

      /* Execute. */
      if (rwxXstugo[2])
	{
	  if (ugo[0] || honor_umask)
	    new_mode |= S_IXUSR;
	  if (ugo[1] || honor_umask)
	    new_mode |= S_IXGRP;
	  if (ugo[2] || honor_umask)
	    new_mode |= S_IXOTH;
	}

      /* 'X' execute.  */
      if (rwxXstugo[3]
	  && (is_dir || (file_mode & (S_IXUSR | S_IXGRP | S_IXOTH))))
	new_mode |= (S_IXUSR | S_IXGRP | S_IXOTH);

      /* 's'.  */
      if (rwxXstugo[4])
	{
	  if (ugo[0] || honor_umask)
	    new_mode |= S_ISUID;
	  if (ugo[1] || honor_umask)
	    new_mode |= S_ISGID;
	}

      /* As original 'u'.  */
      if (rwxXstugo[6])
	{
	  if (ugo[1] || honor_umask)
	    {
	      if (file_mode & S_IRUSR)
		new_mode |= S_IRGRP;
	      if (file_mode & S_IWUSR)
		new_mode |= S_IWGRP;
	      if (file_mode & S_IXUSR)
		new_mode |= S_IXGRP;
	    }
	  if (ugo[2] || honor_umask)
	    {
	      if (file_mode & S_IRUSR)
		new_mode |= S_IROTH;
	      if (file_mode & S_IWUSR)
		new_mode |= S_IWOTH;
	      if (file_mode & S_IXUSR)
		new_mode |= S_IXOTH;
	    }
	}

      /* As original 'g'.  */
      if (rwxXstugo[7])
	{
	  if (ugo[0] || honor_umask)
	    {
	      if (file_mode & S_IRGRP)
		new_mode |= S_IRUSR;
	      if (file_mode & S_IWGRP)
		new_mode |= S_IWUSR;
	      if (file_mode & S_IXGRP)
		new_mode |= S_IXUSR;
	    }
	  if (ugo[2] || honor_umask)
	    {
	      if (file_mode & S_IRGRP)
		new_mode |= S_IROTH;
	      if (file_mode & S_IWGRP)
		new_mode |= S_IWOTH;
	      if (file_mode & S_IXGRP)
		new_mode |= S_IXOTH;
	    }
	}

      /* As original 'o'.  */
      if (rwxXstugo[8])
	{
	  if (ugo[0] || honor_umask)
	    {
	      if (file_mode & S_IROTH)
		new_mode |= S_IRUSR;
	      if (file_mode & S_IWOTH)
		new_mode |= S_IWUSR;
	      if (file_mode & S_IXOTH)
		new_mode |= S_IXUSR;
	    }
	  if (ugo[1] || honor_umask)
	    {
	      if (file_mode & S_IROTH)
		new_mode |= S_IRGRP;
	      if (file_mode & S_IWOTH)
		new_mode |= S_IWGRP;
	      if (file_mode & S_IXOTH)
		new_mode |= S_IXGRP;
	    }
	}
#endif  /* __MINGW32__ */

#ifdef HAVE_UMASK
    if (honor_umask)
      new_mode &= ~mode_mask;
#endif

    if (set_mode == 1)
      {
#ifdef __MINGW32__
	if (ugo[0] || honor_umask)
	  file_mode = (file_mode & ~(_S_IWRITE | _S_IREAD))
		      | (new_mode & (_S_IWRITE | _S_IREAD));
#else
	/* Set '='.  */
	if ((ugo[0] || honor_umask) && !rwxXstugo[6])
	  file_mode = (file_mode & ~(S_ISUID | S_IRUSR | S_IWUSR | S_IXUSR))
		      | (new_mode & (S_ISUID | S_IRUSR | S_IWUSR | S_IXUSR));
	if ((ugo[1] || honor_umask) && !rwxXstugo[7])
	  file_mode = (file_mode & ~(S_ISGID | S_IRGRP | S_IWGRP | S_IXGRP))
		      | (new_mode & (S_ISGID | S_IRGRP | S_IWGRP | S_IXGRP));
	if ((ugo[2] || honor_umask) && !rwxXstugo[8])
	  file_mode = (file_mode & ~(S_IROTH | S_IWOTH | S_IXOTH))
		      | (new_mode & (S_IROTH | S_IWOTH | S_IXOTH));
#ifndef __VXWORKS__
	if (is_dir && rwxXstugo[5])
	  file_mode |= S_ISVTX;
	else if (!is_dir)
	  file_mode &= ~S_ISVTX;
#endif
#endif
      }
    else if (set_mode == 2)
      {
	/* Clear '-'.  */
	file_mode &= ~new_mode;
#if !defined( __MINGW32__) && !defined (__VXWORKS__)
	if (rwxXstugo[5] || !is_dir)
	  file_mode &= ~S_ISVTX;
#endif
      }
    else if (set_mode == 3)
      {
	file_mode |= new_mode;
#if !defined (__MINGW32__) && !defined (__VXWORKS__)
	if (rwxXstugo[5] && is_dir)
	  file_mode |= S_ISVTX;
	else if (!is_dir)
	  file_mode &= ~S_ISVTX;
#endif
      }
  }

  return chmod (file, file_mode);
}


extern void chmod_i4_sub (char *, char *, GFC_INTEGER_4 *,
			  gfc_charlen_type, gfc_charlen_type);
export_proto(chmod_i4_sub);

void
chmod_i4_sub (char *name, char *mode, GFC_INTEGER_4 * status,
	      gfc_charlen_type name_len, gfc_charlen_type mode_len)
{
  int val;

  val = chmod_func (name, mode, name_len, mode_len);
  if (status)
    *status = val;
}


extern void chmod_i8_sub (char *, char *, GFC_INTEGER_8 *,
			  gfc_charlen_type, gfc_charlen_type);
export_proto(chmod_i8_sub);

void
chmod_i8_sub (char *name, char *mode, GFC_INTEGER_8 * status,
	      gfc_charlen_type name_len, gfc_charlen_type mode_len)
{
  int val;

  val = chmod_func (name, mode, name_len, mode_len);
  if (status)
    *status = val;
}

#endif
