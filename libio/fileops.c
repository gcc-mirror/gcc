/* 
Copyright (C) 1993, 1995 Free Software Foundation

This file is part of the GNU IO Library.  This library is free
software; you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option)
any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this library; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

As a special exception, if you link this library with files
compiled with a GNU compiler to produce an executable, this does not cause
the resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why
the executable file might be covered by the GNU General Public License. */

/*  written by Per Bothner (bothner@cygnus.com) */

#define _POSIX_SOURCE
#include "libioP.h"
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>
#include <errno.h>
#ifndef errno
extern int errno;
#endif

/* An fstream can be in at most one of put mode, get mode, or putback mode.
   Putback mode is a variant of get mode.

   In a filebuf, there is only one current position, instead of two
   separate get and put pointers.  In get mode, the current posistion
   is that of gptr(); in put mode that of pptr().

   The position in the buffer that corresponds to the position
   in external file system is normally _IO_read_end, except in putback
   mode, when it is _IO_save_end.
   If the field _fb._offset is >= 0, it gives the offset in
   the file as a whole corresponding to eGptr(). (?)

   PUT MODE:
   If a filebuf is in put mode, then all of _IO_read_ptr, _IO_read_end,
   and _IO_read_base are equal to each other.  These are usually equal
   to _IO_buf_base, though not necessarily if we have switched from
   get mode to put mode.  (The reason is to maintain the invariant
   that _IO_read_end corresponds to the external file position.)
   _IO_write_base is non-NULL and usually equal to _IO_base_base.
   We also have _IO_write_end == _IO_buf_end, but only in fully buffered mode.
   The un-flushed character are those between _IO_write_base and _IO_write_ptr.

   GET MODE:
   If a filebuf is in get or putback mode, eback() != egptr().
   In get mode, the unread characters are between gptr() and egptr().
   The OS file position corresponds to that of egptr().

   PUTBACK MODE:
   Putback mode is used to remember "excess" characters that have
   been sputbackc'd in a separate putback buffer.
   In putback mode, the get buffer points to the special putback buffer.
   The unread characters are the characters between gptr() and egptr()
   in the putback buffer, as well as the area between save_gptr()
   and save_egptr(), which point into the original reserve buffer.
   (The pointers save_gptr() and save_egptr() are the values
   of gptr() and egptr() at the time putback mode was entered.)
   The OS position corresponds to that of save_egptr().
   
   LINE BUFFERED OUTPUT:
   During line buffered output, _IO_write_base==base() && epptr()==base().
   However, ptr() may be anywhere between base() and ebuf().
   This forces a call to filebuf::overflow(int C) on every put.
   If there is more space in the buffer, and C is not a '\n',
   then C is inserted, and pptr() incremented.
   
   UNBUFFERED STREAMS:
   If a filebuf is unbuffered(), the _shortbuf[1] is used as the buffer.
*/

#define CLOSED_FILEBUF_FLAGS \
  (_IO_IS_FILEBUF+_IO_NO_READS+_IO_NO_WRITES+_IO_TIED_PUT_GET)


void
DEFUN(_IO_file_init, (fp),
      register _IO_FILE *fp)
{
  /* POSIX.1 allows another file handle to be used to change the position
     of our file descriptor.  Hence we actually don't know the actual
     position before we do the first fseek (and until a following fflush). */
  fp->_offset = _IO_pos_BAD;
  fp->_IO_file_flags |= CLOSED_FILEBUF_FLAGS;

  _IO_link_in(fp);
  fp->_fileno = -1;
}

int
DEFUN(_IO_file_close_it, (fp),
      register _IO_FILE* fp)
{
  int write_status, close_status;
  if (!_IO_file_is_open(fp))
    return EOF;

  write_status = _IO_do_flush (fp);

  _IO_unsave_markers(fp);

  close_status = _IO_SYSCLOSE (fp);

  /* Free buffer. */
  _IO_setb(fp, NULL, NULL, 0);
  _IO_setg(fp, NULL, NULL, NULL);
  _IO_setp(fp, NULL, NULL);

  _IO_un_link(fp);
  fp->_flags = _IO_MAGIC|CLOSED_FILEBUF_FLAGS;
  fp->_fileno = EOF;
  fp->_offset = _IO_pos_BAD;

  return close_status ? close_status : write_status;
}

void
DEFUN(_IO_file_finish, (fp),
      register _IO_FILE* fp)
{
  if (_IO_file_is_open(fp))
    {
      _IO_do_flush (fp);
      if (!(fp->_flags & _IO_DELETE_DONT_CLOSE))
	_IO_SYSCLOSE (fp);
    }
  _IO_default_finish(fp);
}

_IO_FILE *
DEFUN(_IO_file_fopen, (fp, filename, mode),
      register _IO_FILE *fp AND const char *filename AND const char *mode)
{
  int oflags = 0, omode;
  int read_write, fdesc;
  int oprot = 0666;
  if (_IO_file_is_open (fp))
    return 0;
  switch (*mode++) {
  case 'r':
    omode = O_RDONLY;
    read_write = _IO_NO_WRITES;
    break;
  case 'w':
    omode = O_WRONLY;
    oflags = O_CREAT|O_TRUNC;
    read_write = _IO_NO_READS;
    break;
  case 'a':
    omode = O_WRONLY;
    oflags = O_CREAT|O_APPEND;
    read_write = _IO_NO_READS|_IO_IS_APPENDING;
    break;
  default:
    errno = EINVAL;
    return NULL;
  }
  if (mode[0] == '+' || (mode[0] == 'b' && mode[1] == '+')) {
    omode = O_RDWR;
    read_write &= _IO_IS_APPENDING;
  }
  fdesc = open(filename, omode|oflags, oprot);
  if (fdesc < 0)
    return NULL;
  fp->_fileno = fdesc;
  _IO_mask_flags(fp, read_write,_IO_NO_READS+_IO_NO_WRITES+_IO_IS_APPENDING);
  if (read_write & _IO_IS_APPENDING)
    if (_IO_SEEKOFF (fp, (_IO_off_t)0, _IO_seek_end, _IOS_INPUT|_IOS_OUTPUT)
	== _IO_pos_BAD)
      return NULL;
  _IO_link_in(fp);
  return fp;
}

_IO_FILE*
DEFUN(_IO_file_attach, (fp, fd),
      _IO_FILE *fp AND int fd)
{
  if (_IO_file_is_open(fp))
    return NULL;
  fp->_fileno = fd;
  fp->_flags &= ~(_IO_NO_READS+_IO_NO_WRITES);
  fp->_flags |= _IO_DELETE_DONT_CLOSE;
  fp->_offset = _IO_pos_BAD;
  return fp;
}

_IO_FILE*
DEFUN(_IO_file_setbuf, (fp, p, len),
      register _IO_FILE *fp AND char* p AND _IO_ssize_t len)
{
    if (_IO_default_setbuf(fp, p, len) == NULL)
	return NULL;

    fp->_IO_write_base = fp->_IO_write_ptr = fp->_IO_write_end
      = fp->_IO_buf_base;
    _IO_setg(fp, fp->_IO_buf_base, fp->_IO_buf_base, fp->_IO_buf_base);

    return fp;
}

/* Write TO_DO bytes from DATA to FP.
   Then mark FP as having empty buffers. */

int
DEFUN(_IO_do_write, (fp, data, to_do),
      register _IO_FILE *fp AND const char* data AND _IO_size_t to_do)
{
  _IO_size_t count;
  if (to_do == 0)
    return 0;
  else
    {
      if (fp->_flags & _IO_IS_APPENDING)
	/* On a system without a proper O_APPEND implementation,
	   you would need to sys_seek(0, SEEK_END) here, but is
	   is not needed nor desirable for Unix- or Posix-like systems.
	   Instead, just indicate that offset (before and after) is
	   unpredictable. */
	fp->_offset = _IO_pos_BAD;
      else if (fp->_IO_read_end != fp->_IO_write_base)
	{ 
	  _IO_pos_t new_pos
	    = _IO_SYSSEEK(fp, fp->_IO_write_base - fp->_IO_read_end, 1);
	  if (new_pos == _IO_pos_BAD)
	    return EOF;
	  fp->_offset = new_pos;
	}
      count = _IO_SYSWRITE (fp, data, to_do);
      if (fp->_cur_column)
	fp->_cur_column
	  = _IO_adjust_column (fp->_cur_column - 1, data, to_do) + 1;
    }
  _IO_setg(fp, fp->_IO_buf_base, fp->_IO_buf_base, fp->_IO_buf_base);
  fp->_IO_write_base = fp->_IO_write_ptr = fp->_IO_buf_base;
  fp->_IO_write_end = (fp->_flags & (_IO_LINE_BUF+_IO_UNBUFFERED)) ? fp->_IO_buf_base
    : fp->_IO_buf_end;
  return count != to_do ? EOF : 0;
}

int
DEFUN(_IO_file_underflow, (fp),
      register _IO_FILE *fp)
{
  _IO_ssize_t count;
#if 0
  /* SysV does not make this test; take it out for compatibility */
  if (fp->_flags & _IO_EOF_SEEN)
    return (EOF);
#endif

  if (fp->_flags & _IO_NO_READS)
    return EOF;
  if (fp->_IO_read_ptr < fp->_IO_read_end)
    return *(unsigned char*)fp->_IO_read_ptr;

  if (fp->_IO_buf_base == NULL)
    _IO_doallocbuf(fp);

  /* Flush all line buffered files before reading. */
  /* FIXME This can/should be moved to genops ?? */
  if (fp->_flags & (_IO_LINE_BUF|_IO_UNBUFFERED))
    _IO_flush_all_linebuffered();

  _IO_switch_to_get_mode(fp);

  count = _IO_SYSREAD (fp, fp->_IO_buf_base,
		       fp->_IO_buf_end - fp->_IO_buf_base);
  if (count <= 0)
    {
      if (count == 0)
	fp->_flags |= _IO_EOF_SEEN;
      else
	fp->_flags |= _IO_ERR_SEEN, count = 0;
  }
  fp->_IO_read_base = fp->_IO_read_ptr = fp->_IO_buf_base;
  fp->_IO_read_end = fp->_IO_buf_base + count;
  fp->_IO_write_base = fp->_IO_write_ptr = fp->_IO_write_end
    = fp->_IO_buf_base;
  if (count == 0)
    return EOF;
  if (fp->_offset != _IO_pos_BAD)
    _IO_pos_adjust(fp->_offset, count);
  return *(unsigned char*)fp->_IO_read_ptr;
}

int
DEFUN(_IO_file_overflow, (f, ch),
      register _IO_FILE* f AND int ch)
{
  if (f->_flags & _IO_NO_WRITES) /* SET ERROR */
    return EOF;
  /* If currently reading or no buffer allocated. */
  if ((f->_flags & _IO_CURRENTLY_PUTTING) == 0)
    {
      /* Allocate a buffer if needed. */
      if (f->_IO_write_base == 0)
	{
	  _IO_doallocbuf(f);
	  _IO_setg (f, f->_IO_buf_base, f->_IO_buf_base, f->_IO_buf_base);
	}
      /* Otherwise must be currently reading.
	 If _IO_read_ptr (and hence also _IO_read_end) is at the buffer end,
	 logically slide the buffer forwards one block (by setting the
	 read pointers to all point at the beginning of the block).  This
	 makes room for subsequent output.
	 Otherwise, set the read pointers to _IO_read_end (leaving that
	 alone, so it can continue to correspond to the external position). */
      if (f->_IO_read_ptr == f->_IO_buf_end)
	f->_IO_read_end = f->_IO_read_ptr = f->_IO_buf_base;
      f->_IO_write_ptr = f->_IO_read_ptr;
      f->_IO_write_base = f->_IO_write_ptr;
      f->_IO_write_end = f->_IO_buf_end;
      f->_IO_read_base = f->_IO_read_ptr = f->_IO_read_end;

      if (f->_flags & (_IO_LINE_BUF+_IO_UNBUFFERED))
	f->_IO_write_end = f->_IO_write_ptr;
      f->_flags |= _IO_CURRENTLY_PUTTING;
    }
  if (ch == EOF)
    return _IO_do_flush(f);
  if (f->_IO_write_ptr == f->_IO_buf_end ) /* Buffer is really full */
    if (_IO_do_flush(f) == EOF)
      return EOF;
  *f->_IO_write_ptr++ = ch;
  if ((f->_flags & _IO_UNBUFFERED)
      || ((f->_flags & _IO_LINE_BUF) && ch == '\n'))
    if (_IO_do_flush(f) == EOF)
      return EOF;
  return (unsigned char)ch;
}

int
DEFUN(_IO_file_sync, (fp),
      register _IO_FILE* fp)
{
  _IO_size_t delta;
  /*    char* ptr = cur_ptr(); */
  if (fp->_IO_write_ptr > fp->_IO_write_base)
    if (_IO_do_flush(fp)) return EOF;
  delta = fp->_IO_read_ptr - fp->_IO_read_end; 
  if (delta != 0)
    {
#ifdef TODO
      if (_IO_in_backup(fp))
	delta -= eGptr() - Gbase();
#endif
      _IO_off_t new_pos = _IO_SYSSEEK (fp, delta, 1);
      if (new_pos != (_IO_off_t)EOF)
	fp->_IO_read_end = fp->_IO_read_ptr;
#ifdef ESPIPE
      else if (errno == ESPIPE)
	; /* Ignore error from unseekable devices. */
#endif
      else
	return EOF;
    }
  fp->_offset = _IO_pos_BAD;
  /* FIXME: Cleanup - can this be shared? */
  /*    setg(base(), ptr, ptr); */
  return 0;
}

_IO_pos_t
DEFUN(_IO_file_seekoff, (fp, offset, dir, mode),
      register _IO_FILE *fp AND _IO_off_t offset AND int dir AND int mode)
{
  _IO_pos_t result;
  _IO_off_t delta, new_offset;
  long count;

  if (mode == 0)
    dir = _IO_seek_cur, offset = 0; /* Don't move any pointers. */

  /* Flush unwritten characters.
     (This may do an unneeded write if we seek within the buffer.
     But to be able to switch to reading, we would need to set
     egptr to ptr.  That can't be done in the current design,
     which assumes file_ptr() is eGptr.  Anyway, since we probably
     end up flushing when we close(), it doesn't make much difference.)
     FIXME: simulate mem-papped files. */

  if (fp->_IO_write_ptr > fp->_IO_write_base || _IO_in_put_mode(fp))
    if (_IO_switch_to_get_mode(fp)) return EOF;

  if (fp->_IO_buf_base == NULL)
    {
      _IO_doallocbuf(fp);
      _IO_setp(fp, fp->_IO_buf_base, fp->_IO_buf_base);
      _IO_setg(fp, fp->_IO_buf_base, fp->_IO_buf_base, fp->_IO_buf_base);
    }

  switch (dir)
    {
    case _IO_seek_cur:
      /* Adjust for read-ahead (bytes is buffer). */
      offset -= fp->_IO_read_end - fp->_IO_read_ptr;
      if (fp->_offset == _IO_pos_BAD)
	goto dumb;
      /* Make offset absolute, assuming current pointer is file_ptr(). */
      offset += _IO_pos_as_off(fp->_offset);

      dir = _IO_seek_set;
      break;
    case _IO_seek_set:
      break;
    case _IO_seek_end:
      {
	struct stat st;
	if (_IO_SYSSTAT (fp, &st) == 0 && S_ISREG(st.st_mode))
	  {
	    offset += st.st_size;
	    dir = _IO_seek_set;
	  }
	else
	  goto dumb;
      }
    }
  /* At this point, dir==_IO_seek_set. */

  /* If destination is within current buffer, optimize: */
  if (fp->_offset != _IO_pos_BAD && fp->_IO_read_base != NULL
      && !_IO_in_backup (fp))
    {
      /* Offset relative to start of main get area. */
      _IO_pos_t rel_offset = offset - fp->_offset
	+ (fp->_IO_read_end - fp->_IO_read_base);
      if (rel_offset >= 0)
	{
#if 0
	  if (_IO_in_backup(fp))
	    _IO_switch_to_main_get_area(fp);
#endif
	  if (rel_offset <= fp->_IO_read_end - fp->_IO_read_base)
	    {
	      _IO_setg(fp, fp->_IO_buf_base, fp->_IO_buf_base + rel_offset,
		       fp->_IO_read_end);
	      _IO_setp(fp, fp->_IO_buf_base, fp->_IO_buf_base);
	      return offset;
	    }
#ifdef TODO
	    /* If we have streammarkers, seek forward by reading ahead. */
	    if (_IO_have_markers(fp))
	      {
		int to_skip = rel_offset
		  - (fp->_IO_read_ptr - fp->_IO_read_base);
		if (ignore(to_skip) != to_skip)
		  goto dumb;
		return offset;
	      }
#endif
	}
#ifdef TODO
      if (rel_offset < 0 && rel_offset >= Bbase() - Bptr())
	{
	  if (!_IO_in_backup(fp))
	    _IO_switch_to_backup_area(fp);
	  gbump(fp->_IO_read_end + rel_offset - fp->_IO_read_ptr);
	  return offset;
	}
#endif
    }

#ifdef TODO
  _IO_unsave_markers(fp);
#endif

  if (fp->_flags & _IO_NO_READS)
    goto dumb;

  /* Try to seek to a block boundary, to improve kernel page management. */
  new_offset = offset & ~(fp->_IO_buf_end - fp->_IO_buf_base - 1);
  delta = offset - new_offset;
  if (delta > fp->_IO_buf_end - fp->_IO_buf_base)
    {
      new_offset = offset;
      delta = 0;
    }
  result = _IO_SYSSEEK (fp, new_offset, 0);
  if (result < 0)
    return EOF;
  if (delta == 0)
    count = 0;
  else
    {
      count = _IO_SYSREAD (fp, fp->_IO_buf_base,
			   fp->_IO_buf_end - fp->_IO_buf_base);
      if (count < delta)
	{
	  /* We weren't allowed to read, but try to seek the remainder. */
	  offset = count == EOF ? delta : delta-count;
	  dir = _IO_seek_cur;
	  goto dumb;
	}
    }
  _IO_setg(fp, fp->_IO_buf_base, fp->_IO_buf_base+delta, fp->_IO_buf_base+count);
  _IO_setp(fp, fp->_IO_buf_base, fp->_IO_buf_base);
  fp->_offset = result + count;
  _IO_mask_flags(fp, 0, _IO_EOF_SEEN);
  return offset;
 dumb:

  _IO_unsave_markers(fp);
  result = _IO_SYSSEEK (fp, offset, dir);
  if (result != EOF)
    _IO_mask_flags(fp, 0, _IO_EOF_SEEN);
  fp->_offset = result;
  _IO_setg(fp, fp->_IO_buf_base, fp->_IO_buf_base, fp->_IO_buf_base);
  _IO_setp(fp, fp->_IO_buf_base, fp->_IO_buf_base);
  return result;
}

_IO_ssize_t
DEFUN(_IO_file_read, (fp, buf, size),
      register _IO_FILE* fp AND void* buf AND _IO_ssize_t size)
{
  for (;;)
    {
      _IO_ssize_t count = _IO_read(fp->_fileno, buf, size);
#ifdef EINTR
      if (count == -1 && errno == EINTR)
	continue;
#endif
      return count;
    }
}

_IO_pos_t
DEFUN(_IO_file_seek, (fp, offset, dir),
      _IO_FILE *fp AND _IO_off_t offset AND int dir)
{
  return _IO_lseek(fp->_fileno, offset, dir);
}

int
DEFUN(_IO_file_stat, (fp, st),
      _IO_FILE *fp AND void* st)
{
  return _IO_fstat(fp->_fileno, (struct stat*)st);
}

int
DEFUN(_IO_file_close, (fp),
      _IO_FILE* fp)
{
  return _IO_close(fp->_fileno);
}

_IO_ssize_t
DEFUN(_IO_file_write, (f, data, n),
      register _IO_FILE* f AND const void* data AND _IO_ssize_t n)
{
  _IO_ssize_t to_do = n;
  while (to_do > 0)
    {
      _IO_ssize_t count = _IO_write(f->_fileno, data, to_do);
      if (count == EOF)
	{
#ifdef EINTR
	  if (errno == EINTR)
	    continue;
	  else
#endif
	    {
	      f->_flags |= _IO_ERR_SEEN;
	      break;
            }
        }
      to_do -= count;
      data = (void*)((char*)data + count);
    }
  n -= to_do;
  if (f->_offset >= 0)
    f->_offset += n;
  return n;
}

_IO_size_t
DEFUN(_IO_file_xsputn, (f, data, n),
      _IO_FILE *f AND const void *data AND _IO_size_t n)
{
  register const char *s = (char*) data;
  _IO_size_t to_do = n;
  int must_flush = 0;
  _IO_size_t count;

  if (n <= 0)
    return 0;
  /* This is an optimized implementation.
     If the amount to be written straddles a block boundary
     (or the filebuf is unbuffered), use sys_write directly. */

  /* First figure out how much space is available in the buffer. */
  count = f->_IO_write_end - f->_IO_write_ptr; /* Space available. */
  if ((f->_flags & _IO_LINE_BUF) && (f->_flags & _IO_CURRENTLY_PUTTING))
    {
      count = f->_IO_buf_end - f->_IO_write_ptr;
      if (count >= n)
	{ register const char *p;
	  for (p = s + n; p > s; )
	    {
	      if (*--p == '\n') {
		count = p - s + 1;
		must_flush = 1;
		break;
	      }
	    }
	}
    }
  /* Then fill the buffer. */
  if (count > 0)
    {
      if (count > to_do)
	count = to_do;
      if (count > 20) {
	memcpy(f->_IO_write_ptr, s, count);
	s += count;
      }
      else
	{
	  register char *p = f->_IO_write_ptr;
	  register int i = (int)count;
	  while (--i >= 0) *p++ = *s++;
	}
      f->_IO_write_ptr += count;
      to_do -= count;
    }
  if (to_do + must_flush > 0)
    { _IO_size_t block_size, dont_write;
      /* Next flush the (full) buffer. */
      if (__overflow(f, EOF) == EOF)
	return n - to_do;

      /* Try to maintain alignment: write a whole number of blocks.
	 dont_write is what gets left over. */
      block_size = f->_IO_buf_end - f->_IO_buf_base;
      dont_write = block_size >= 128 ? to_do % block_size : 0;

      count = to_do - dont_write;
      if (_IO_do_write(f, s, count) == EOF)
	return n - to_do;
      to_do = dont_write;
      
      /* Now write out the remainder.  Normally, this will fit in the
	 buffer, but it's somewhat messier for line-buffered files,
	 so we let _IO_default_xsputn handle the general case. */
      if (dont_write)
	to_do -= _IO_default_xsputn(f, s+count, dont_write);
    }
  return n - to_do;
}

#if 0
/* Work in progress */
_IO_size_t
DEFUN(_IO_file_xsgetn, (fp, data, n),
      _IO_FILE *fp AND void *data AND _IO_size_t n)
{
  register _IO_size_t more = n;
  register char *s = data;
  for (;;)
    {
      _IO_ssize_t count = fp->_IO_read_end - fp->_IO_read_ptr; /* Data available. */
      if (count > 0)
	{
	  if (count > more)
	    count = more;
	  if (count > 20)
	    {
	      memcpy(s, fp->_IO_read_ptr, count);
	      s += count;
	      fp->_IO_read_ptr += count;
	    }
	  else if (count <= 0)
	    count = 0;
	  else
	    {
	      register char *p = fp->_IO_read_ptr;
	      register int i = (int)count;
	      while (--i >= 0) *s++ = *p++;
	      fp->_IO_read_ptr = p;
            }
            more -= count;
        }
#if 0
      if (! _IO_in put_mode (fp)
	  && ! _IO_have_markers (fp) && ! IO_have_backup (fp))
	{
	  /* This is an optimization of _IO_file_underflow */
	  if (fp->_flags & _IO_NO_READS)
	    break;
	  /* If we're reading a lot of data, don't bother allocating
	     a buffer.  But if we're only reading a bit, perhaps we should ??*/
	  if (count <= 512 && fp->_IO_buf_base == NULL)
	    _IO_doallocbuf(fp);
	  if (fp->_flags & (_IO_LINE_BUF|_IO_UNBUFFERED))
	    _IO_flush_all_linebuffered();

	  _IO_switch_to_get_mode(fp); ???;
	  count = _IO_SYSREAD (fp, s, more);
	  if (count <= 0)
	     {
	       if (count == 0)
		 fp->_flags |= _IO_EOF_SEEN;
	       else
		 fp->_flags |= _IO_ERR_SEEN, count = 0;
	     }
	  
	  s += count;
	  more -= count;
	}
#endif
      if (more == 0 || __underflow(fp) == EOF)
	break;
    }
  return n - more;
}
#endif

struct _IO_jump_t _IO_file_jumps = {
  JUMP_INIT_DUMMY,
  JUMP_INIT(finish, _IO_file_finish),
  JUMP_INIT(overflow, _IO_file_overflow),
  JUMP_INIT(underflow, _IO_file_underflow),
  JUMP_INIT(uflow, _IO_default_uflow),
  JUMP_INIT(pbackfail, _IO_default_pbackfail),
  JUMP_INIT(xsputn, _IO_file_xsputn),
  JUMP_INIT(xsgetn, _IO_default_xsgetn),
  JUMP_INIT(seekoff, _IO_file_seekoff),
  JUMP_INIT(seekpos, _IO_default_seekpos),
  JUMP_INIT(setbuf, _IO_file_setbuf),
  JUMP_INIT(sync, _IO_file_sync),
  JUMP_INIT(doallocate, _IO_file_doallocate),
  JUMP_INIT(read, _IO_file_read),
  JUMP_INIT(write, _IO_file_write),
  JUMP_INIT(seek, _IO_file_seek),
  JUMP_INIT(close, _IO_file_close),
  JUMP_INIT(stat, _IO_file_stat)
};
