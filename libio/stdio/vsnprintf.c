/* Copyright (C) 1994, 1997 Free Software Foundation, Inc.
   This file is part of the GNU IO Library.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2, or (at
   your option) any later version.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this library; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
   MA 02111-1307, USA.

   As a special exception, if you link this library with files
   compiled with a GNU compiler to produce an executable, this does
   not cause the resulting executable to be covered by the GNU General
   Public License.  This exception does not however invalidate any
   other reasons why the executable file might be covered by the GNU
   General Public License.  */

#include "libioP.h"
#include "strfile.h"


typedef struct
{
  _IO_strfile f;
  /* This is used for the characters which do not fit in the buffer
     provided by the user.  */
  char overflow_buf[64];
} _IO_strnfile;


static int _IO_strn_overflow __P ((_IO_FILE *fp, int c));

static int
_IO_strn_overflow (fp, c)
     _IO_FILE *fp;
     int c;
{
  /* When we come to here this means the user supplied buffer is
     filled.  But since we must return the number of characters which
     would have been written in total we must provide a buffer for
     further use.  We can do this by writing on and on in the overflow
     buffer in the _IO_strnfile structure.  */
  _IO_strnfile *snf = (_IO_strnfile *) fp;

  if (fp->_IO_buf_base != snf->overflow_buf)
    {
      /* Terminate the string.  We know that there is room for at
	 least one more character since we initialized the stream with
	 a size to make this possible.  */
      *fp->_IO_write_ptr = '\0';

      _IO_setb (fp, snf->overflow_buf,
		snf->overflow_buf + sizeof (snf->overflow_buf), 0);

      fp->_IO_write_base = snf->overflow_buf;
      fp->_IO_read_base = snf->overflow_buf;
      fp->_IO_read_ptr = snf->overflow_buf;
      fp->_IO_read_end = snf->overflow_buf + sizeof (snf->overflow_buf);
    }

  fp->_IO_write_ptr = snf->overflow_buf;
  fp->_IO_write_end = snf->overflow_buf;

  /* Since we are not really interested in storing the characters
     which do not fit in the buffer we simply ignore it.  */
  return c;
}


static struct _IO_jump_t _IO_strn_jumps =
{
  JUMP_INIT_DUMMY,
  JUMP_INIT(finish, _IO_str_finish),
  JUMP_INIT(overflow, _IO_strn_overflow),
  JUMP_INIT(underflow, _IO_str_underflow),
  JUMP_INIT(uflow, _IO_default_uflow),
  JUMP_INIT(pbackfail, _IO_str_pbackfail),
  JUMP_INIT(xsputn, _IO_default_xsputn),
  JUMP_INIT(xsgetn, _IO_default_xsgetn),
  JUMP_INIT(seekoff, _IO_str_seekoff),
  JUMP_INIT(seekpos, _IO_default_seekpos),
  JUMP_INIT(setbuf, _IO_default_setbuf),
  JUMP_INIT(sync, _IO_default_sync),
  JUMP_INIT(doallocate, _IO_default_doallocate),
  JUMP_INIT(read, _IO_default_read),
  JUMP_INIT(write, _IO_default_write),
  JUMP_INIT(seek, _IO_default_seek),
  JUMP_INIT(close, _IO_default_close),
  JUMP_INIT(stat, _IO_default_stat)
};


int
_IO_vsnprintf (string, maxlen, format, args)
     char *string;
     _IO_size_t maxlen;
     const char *format;
     _IO_va_list args;
{
  _IO_strnfile sf;
  int ret;
#ifdef _IO_MTSAFE_IO
  _IO_lock_t lock;
  sf.f._sbf._f._lock = &lock;
#endif

  /* We need to handle the special case where MAXLEN is 0.  Use the
     overflow buffer right from the start.  */
  if (maxlen == 0)
    {
      string = sf.overflow_buf;
      maxlen = sizeof (sf.overflow_buf);
    }

  _IO_init ((_IO_FILE *) &sf, 0);
  _IO_JUMPS ((_IO_FILE *) &sf) = &_IO_strn_jumps;
  _IO_str_init_static ((_IO_FILE *) &sf, string, maxlen - 1, string);
  ret = _IO_vfprintf ((_IO_FILE *) &sf, format, args);

  if (sf.f._sbf._f._IO_buf_base != sf.overflow_buf)
    *sf.f._sbf._f._IO_write_ptr = '\0';
  return ret;
}

#ifdef weak_alias
weak_alias (_IO_vsnprintf, __vsnprintf)
weak_alias (_IO_vsnprintf, vsnprintf)
#endif
