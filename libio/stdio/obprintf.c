/* Print output of stream to given obstack.
   Copyright (C) 1996, 1997 Free Software Foundation, Inc.
   This file is part of the GNU C Library.
   Contributed by Ulrich Drepper <drepper@cygnus.com>, 1996.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with the GNU C Library; see the file COPYING.LIB.  If not,
   write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */


#ifdef __STDC__
#include <stdlib.h>
#endif
#include "libioP.h"
#include <string.h>
#include <errno.h>
#include <obstack.h>
#include <stdarg.h>


struct _IO_obstack_file
{
  struct _IO_FILE file;
  const void *vtable;
  struct obstack *obstack;
};


static int
_IO_obstack_overflow (_IO_FILE *fp, int c)
{
  struct obstack *obstack = ((struct _IO_obstack_file *) fp)->obstack;

  /* Make room for another character.  This might as well allocate a
     new chunk a memory and moves the old contents over.  */
  if (c != EOF)
    obstack_1grow (obstack, c);

  /* Setup the buffer pointers again.  */
  fp->_IO_write_base = obstack_base (obstack);
  fp->_IO_write_ptr = obstack_next_free (obstack);
  fp->_IO_write_end = fp->_IO_write_base + obstack_room (obstack);
  /* Now allocate the rest of the current chunk.  */
  obstack_blank_fast (obstack, fp->_IO_write_end - fp->_IO_write_ptr);

  return c;
}


static _IO_size_t
_IO_obstack_xsputn (_IO_FILE *fp, const void *data, _IO_size_t n)
{
  struct obstack *obstack = ((struct _IO_obstack_file *) fp)->obstack;

  if (fp->_IO_write_ptr + n > fp->_IO_write_end)
    {
      /* We need some more memory.  First shrink the buffer to the
	 space we really currently need.  */
      obstack_blank (obstack, fp->_IO_write_ptr - fp->_IO_write_end);

      /* Now grow for N bytes.  */
      obstack_blank (obstack, n);

      /* Setup the buffer pointers again.  */
      fp->_IO_write_base = obstack_base (obstack);
      fp->_IO_write_ptr = obstack_next_free (obstack);
      fp->_IO_write_end = (fp->_IO_write_base + obstack_room (obstack));
      /* Now allocate the rest of the current chunk.  */
      obstack_blank_fast (obstack, fp->_IO_write_end - fp->_IO_write_ptr);
    }
  else
    {
      memcpy (fp->_IO_write_ptr, data, n);
      fp->_IO_write_ptr += n;
    }

  return n;
}


/* the jump table.  */
static struct _IO_jump_t _IO_obstack_jumps =
{
  JUMP_INIT_DUMMY,
  JUMP_INIT(finish, NULL),
  JUMP_INIT(overflow, _IO_obstack_overflow),
  JUMP_INIT(underflow, NULL),
  JUMP_INIT(uflow, NULL),
  JUMP_INIT(pbackfail, NULL),
  JUMP_INIT(xsputn, _IO_obstack_xsputn),
  JUMP_INIT(xsgetn, NULL),
  JUMP_INIT(seekoff, NULL),
  JUMP_INIT(seekpos, NULL),
  JUMP_INIT(setbuf, NULL),
  JUMP_INIT(sync, NULL),
  JUMP_INIT(doallocate, NULL),
  JUMP_INIT(read, NULL),
  JUMP_INIT(write, NULL),
  JUMP_INIT(seek, NULL),
  JUMP_INIT(close, NULL),
  JUMP_INIT(stat, NULL)
};


int
_IO_obstack_vprintf (struct obstack *obstack, const char *format, va_list args)
{
  struct obstack_FILE
    {
      struct _IO_obstack_file ofile;
#ifdef _IO_MTSAFE_IO
      _IO_lock_t lock;
#endif
  } new_f;
  int result;

#ifdef _IO_MTSAFE_IO
  new_f.ofile.file._lock = &new_f.lock;
#endif

  _IO_init ((_IO_FILE *) &new_f.ofile, 0);
  _IO_JUMPS (&new_f.ofile.file) = &_IO_obstack_jumps;
  _IO_str_init_static (&new_f.ofile.file, obstack_base (obstack),
		       (obstack_object_size (obstack) +
			obstack_room (obstack)), obstack_next_free (obstack));
  /* Now allocate the rest of the current chunk.  */
  obstack_blank_fast (obstack,
		      (new_f.ofile.file._IO_write_end
		       - new_f.ofile.file._IO_write_ptr));
  new_f.ofile.obstack = obstack;

  result = _IO_vfprintf ((_IO_FILE *) &new_f, format, args);

  /* Shrink the buffer to the space we really currently need.  */
  obstack_blank (obstack, (new_f.ofile.file._IO_write_ptr
			   - new_f.ofile.file._IO_write_end));

  return result;
}
#ifdef weak_alias
weak_alias (_IO_obstack_vprintf, obstack_vprintf)
#endif


int
_IO_obstack_printf (struct obstack *obstack, const char *format, ...)
{
  int result;
  va_list ap;
  va_start (ap, format);
  result = _IO_obstack_vprintf (obstack, format, ap);
  va_end (ap);
  return result;
}
#ifdef weak_alias
weak_alias (_IO_obstack_printf, obstack_printf)
#endif
