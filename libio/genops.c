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

/* Generic or default I/O operations. */

#include "libioP.h"
#ifdef __STDC__
#include <stdlib.h>
#endif
#include <string.h>

void
DEFUN(_IO_un_link, (fp),
      _IO_FILE *fp)
{
  if (fp->_flags & _IO_LINKED) {
    _IO_FILE **f;
    for (f = &_IO_list_all; *f != NULL; f = &(*f)->_chain) {
      if (*f == fp) {
	*f = fp->_chain;
	break;
      }
    }
    fp->_flags &= ~_IO_LINKED;
  }
}

void
DEFUN(_IO_link_in, (fp),
      _IO_FILE *fp)
{
    if ((fp->_flags & _IO_LINKED) == 0) {
	fp->_flags |= _IO_LINKED;
	fp->_chain = _IO_list_all;
	_IO_list_all = fp;
    }
}

/* Return minimum _pos markers
   Assumes the current get area is the main get area. */

_IO_size_t
DEFUN(_IO_least_marker, (fp),
      register _IO_FILE *fp)
{
  _IO_ssize_t least_so_far = fp->_IO_read_end - fp->_IO_read_base;
  register struct _IO_marker *mark;
  for (mark = fp->_markers; mark != NULL; mark = mark->_next)
    if (mark->_pos < least_so_far)
      least_so_far = mark->_pos;
  return least_so_far;
}

/* Switch current get area from backup buffer to (start of) main get area. */

void
DEFUN(_IO_switch_to_main_get_area, (fp),
      _IO_FILE *fp)
{
  char *tmp;
  fp->_flags &= ~_IO_IN_BACKUP;
  /* Swap _IO_read_end and _IO_save_end. */
  tmp = fp->_IO_read_end; fp->_IO_read_end= fp->_IO_save_end; fp->_IO_save_end= tmp;
  /* Swap _IO_read_base and _IO_save_base. */
  tmp = fp->_IO_read_base; fp->_IO_read_base = fp->_IO_save_base; fp->_IO_save_base = tmp;
  fp->_IO_read_ptr = fp->_IO_read_base;
}

/* Switch current get area from main get area to (end of) backup area. */

void
DEFUN(_IO_switch_to_backup_area, (fp),
     register _IO_FILE *fp)
{
  char *tmp;
  fp->_flags |= _IO_IN_BACKUP;
  /* Swap _IO_read_end and _IO_save_end. */
  tmp = fp->_IO_read_end; fp->_IO_read_end = fp->_IO_save_end; fp->_IO_save_end = tmp;
  /* Swap _gbase and _IO_save_base. */
  tmp = fp->_IO_read_base; fp->_IO_read_base = fp->_IO_save_base; fp->_IO_save_base = tmp;
  fp->_IO_read_ptr = fp->_IO_read_end;
}

int
DEFUN(_IO_switch_to_get_mode, (fp),
     register _IO_FILE *fp)
{
  if (fp->_IO_write_ptr > fp->_IO_write_base)
    if (_IO_OVERFLOW (fp, EOF) == EOF)
      return EOF;
  if (_IO_in_backup(fp))
    fp->_IO_read_base = fp->_IO_backup_base;
  else
    {
      fp->_IO_read_base = fp->_IO_buf_base;
      if (fp->_IO_write_ptr > fp->_IO_read_end)
	fp->_IO_read_end = fp->_IO_write_ptr;
    }
  fp->_IO_read_ptr = fp->_IO_write_ptr;

  fp->_IO_write_base = fp->_IO_write_ptr = fp->_IO_write_end = fp->_IO_read_ptr;

  fp->_flags &= ~_IO_CURRENTLY_PUTTING;
  return 0;
}

void
DEFUN(_IO_free_backup_area, (fp),
     register _IO_FILE *fp)
{
  if (_IO_in_backup (fp))
    _IO_switch_to_main_get_area(fp);  /* Just in case. */
  free (fp->_IO_save_base);
  fp->_IO_save_base = NULL;
  fp->_IO_save_end = NULL;
  fp->_IO_backup_base = NULL;
}

#if 0
int
DEFUN(_IO_switch_to_put_mode, (fp),
      register _IO_FILE *fp)
{
  fp->_IO_write_base = fp->_IO_read_ptr;
  fp->_IO_write_ptr = fp->_IO_read_ptr;
  /* Following is wrong if line- or un-buffered? */
  fp->_IO_write_end = fp->_flags & _IO_IN_BACKUP ? fp->_IO_read_end : fp->_IO_buf_end;

  fp->_IO_read_ptr = fp->_IO_read_end;
  fp->_IO_read_base = fp->_IO_read_end;

  fp->_flags |= _IO_CURRENTLY_PUTTING;
  return 0;
}
#endif

int
DEFUN(__overflow, (f, ch),
      _IO_FILE *f AND int ch)
{
  return _IO_OVERFLOW (f, ch);
}

static int
DEFUN(save_for_backup, (fp),
      _IO_FILE *fp)
{
  /* Append [_IO_read_base.._IO_read_end] to backup area. */
  int least_mark = _IO_least_marker(fp);
  /* needed_size is how much space we need in the backup area. */
  int needed_size = (fp->_IO_read_end - fp->_IO_read_base) - least_mark;
  int current_Bsize = fp->_IO_save_end - fp->_IO_save_base;
  int avail; /* Extra space available for future expansion. */
  int delta;
  struct _IO_marker *mark;
  if (needed_size > current_Bsize)
    {
      char *new_buffer;
      avail = 100;
      new_buffer = (char*)malloc(avail+needed_size);
      if (new_buffer == NULL)
	return EOF;		/* FIXME */
      if (least_mark < 0)
	{
	  memcpy(new_buffer + avail,
		 fp->_IO_save_end + least_mark,
		 -least_mark);
	  memcpy(new_buffer +avail - least_mark,
		 fp->_IO_read_base,
		 fp->_IO_read_end - fp->_IO_read_base);
	}
      else
	memcpy(new_buffer + avail,
	       fp->_IO_read_base + least_mark,
	       needed_size);
      if (fp->_IO_save_base)
	free (fp->_IO_save_base);
      fp->_IO_save_base = new_buffer;
      fp->_IO_save_end = new_buffer + avail + needed_size;
    }
  else
    {
      avail = current_Bsize - needed_size;
      if (least_mark < 0)
	{
	  memmove(fp->_IO_save_base + avail,
		  fp->_IO_save_end + least_mark,
		  -least_mark);
	  memcpy(fp->_IO_save_base + avail - least_mark,
		 fp->_IO_read_base,
		 fp->_IO_read_end - fp->_IO_read_base);
	}
      else if (needed_size > 0)
	memcpy(fp->_IO_save_base + avail,
	       fp->_IO_read_base + least_mark,
	       needed_size);
    }
  /* FIXME: Dubious arithmetic if pointers are NULL */
  fp->_IO_backup_base = fp->_IO_save_base + avail;
  /* Adjust all the streammarkers. */
  delta = fp->_IO_read_end - fp->_IO_read_base;
  for (mark = fp->_markers; mark != NULL; mark = mark->_next)
    mark->_pos -= delta;
  return 0;
}

int
DEFUN(__underflow, (fp),
      _IO_FILE *fp)
{
  if (_IO_in_put_mode(fp))
    if (_IO_switch_to_get_mode(fp) == EOF) return EOF;
  if (fp->_IO_read_ptr < fp->_IO_read_end)
    return *(unsigned char*)fp->_IO_read_ptr;
  if (_IO_in_backup(fp))
    {
      _IO_switch_to_main_get_area(fp);
      if (fp->_IO_read_ptr < fp->_IO_read_end)
	return *fp->_IO_read_ptr;
    }
  if (_IO_have_markers(fp))
    {
      if (save_for_backup (fp))
	return EOF;
    }
  else if (_IO_have_backup(fp))
    _IO_free_backup_area(fp);
  return _IO_UNDERFLOW (fp);
}

int
DEFUN(__uflow, (fp),
     _IO_FILE *fp)
{
  if (_IO_in_put_mode(fp))
    if (_IO_switch_to_get_mode(fp) == EOF) return EOF;
  if (fp->_IO_read_ptr < fp->_IO_read_end)
    return *(unsigned char*)fp->_IO_read_ptr++;
  if (_IO_in_backup(fp))
    {
      _IO_switch_to_main_get_area(fp);
      if (fp->_IO_read_ptr < fp->_IO_read_end)
	return *fp->_IO_read_ptr++;
    }
  if (_IO_have_markers(fp))
    {
      if (save_for_backup (fp))
	return EOF;
    }
  else if (_IO_have_backup(fp))
    _IO_free_backup_area(fp);
  return _IO_UFLOW (fp);
}

void
DEFUN(_IO_setb, (f, b, eb, a),
      _IO_FILE *f AND char *b AND char *eb AND int a)
{
  if (f->_IO_buf_base && !(f->_flags & _IO_USER_BUF))
    FREE_BUF(f->_IO_buf_base);
  f->_IO_buf_base = b;
  f->_IO_buf_end = eb;
  if (a)
    f->_flags &= ~_IO_USER_BUF;
  else
    f->_flags |= _IO_USER_BUF;
}

void
DEFUN(_IO_doallocbuf, (fp),
      register _IO_FILE *fp)
{
  if (fp->_IO_buf_base)
    return;
  if (!(fp->_flags & _IO_UNBUFFERED))
    if (_IO_DOALLOCATE (fp) != EOF)
      return;
  _IO_setb(fp, fp->_shortbuf, fp->_shortbuf+1, 0);
}

int
DEFUN(_IO_default_underflow, (fp),
      _IO_FILE *fp)
{
  return EOF;
}

int
DEFUN(_IO_default_uflow, (fp),
      _IO_FILE *fp)
{
  int ch = _IO_UNDERFLOW (fp);
  if (ch == EOF)
    return EOF;
  return *(unsigned char*)fp->_IO_read_ptr++;
}

_IO_size_t
DEFUN(_IO_default_xsputn, (f, data, n),
      register _IO_FILE *f AND const void *data AND _IO_size_t n)
{
  register const char *s = (char*) data;
  register _IO_size_t more = n;
  if (more <= 0)
    return 0;
  for (;;)
    {
      _IO_ssize_t count = f->_IO_write_end - f->_IO_write_ptr; /* Space available. */
      if (count > 0)
	{
	  if (count > more)
	    count = more;
	  if (count > 20)
	    {
	      memcpy(f->_IO_write_ptr, s, count);
	      s += count;
	      f->_IO_write_ptr += count;
            }
	  else if (count <= 0)
	    count = 0;
	  else
	    {
	      register char *p = f->_IO_write_ptr;
	      register _IO_ssize_t i;
	      for (i = count; --i >= 0; ) *p++ = *s++;
	      f->_IO_write_ptr = p;
            }
	  more -= count;
        }
      if (more == 0 || __overflow(f, (unsigned char)*s++) == EOF)
	break;
      more--;
    }
  return n - more;
}

_IO_size_t
DEFUN(_IO_sgetn, (fp, data, n),
      _IO_FILE *fp AND void *data AND _IO_size_t n)
{
  /* FIXME handle putback buffer here! */
  return _IO_XSGETN (fp, data, n);
}

_IO_size_t
DEFUN(_IO_default_xsgetn, (fp, data, n),
      _IO_FILE *fp AND void *data AND _IO_size_t n)
{
  register _IO_size_t more = n;
  register char *s = (char*) data;
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
      if (more == 0 || __underflow(fp) == EOF)
	break;
    }
  return n - more;
}

int
DEFUN(_IO_sync, (fp),
      register _IO_FILE *fp)
{
  return 0;
}

_IO_FILE*
DEFUN(_IO_default_setbuf, (fp, p, len),
      register _IO_FILE *fp AND char* p AND _IO_ssize_t len)
{
    if (_IO_SYNC (fp) == EOF)
	return NULL;
    if (p == NULL || len == 0)
      {
	fp->_flags |= _IO_UNBUFFERED;
	_IO_setb(fp, fp->_shortbuf, fp->_shortbuf+1, 0);
      }
    else
      {
	fp->_flags &= ~_IO_UNBUFFERED;
	_IO_setb(fp, p, p+len, 0);
      }
    fp->_IO_write_base = fp->_IO_write_ptr = fp->_IO_write_end = 0;
    fp->_IO_read_base = fp->_IO_read_ptr = fp->_IO_read_end = 0;
    return fp;
}

_IO_pos_t
DEFUN(_IO_default_seekpos, (fp, pos, mode),
      _IO_FILE *fp AND _IO_pos_t pos AND int mode)
{
  return _IO_SEEKOFF (fp, _IO_pos_as_off(pos), 0, mode);
}

int
DEFUN(_IO_default_doallocate, (fp),
      _IO_FILE *fp)
{
  char *buf = ALLOC_BUF(_IO_BUFSIZ);
  if (buf == NULL)
    return EOF;
  _IO_setb(fp, buf, buf+_IO_BUFSIZ, 1);
  return 1;
}

void
DEFUN(_IO_init, (fp, flags),
      register _IO_FILE *fp AND int flags)
{
  fp->_flags = _IO_MAGIC|flags;
  fp->_IO_buf_base = NULL;
  fp->_IO_buf_end = NULL;
  fp->_IO_read_base = NULL;
  fp->_IO_read_ptr = NULL;
  fp->_IO_read_end = NULL;
  fp->_IO_write_base = NULL;
  fp->_IO_write_ptr = NULL;
  fp->_IO_write_end = NULL;
  fp->_chain = NULL; /* Not necessary. */

  fp->_IO_save_base = NULL;
  fp->_IO_backup_base = NULL;
  fp->_IO_save_end = NULL;
  fp->_markers = NULL;
  fp->_cur_column = 0;
}

int
DEFUN(_IO_default_sync, (fp),
      _IO_FILE *fp)
{
  return 0;
}

/* The way the C++ classes are mapped into the C functions in the
   current implementation, this function can get called twice! */

void
DEFUN(_IO_default_finish, (fp),
      _IO_FILE *fp)
{
  struct _IO_marker *mark;
  if (fp->_IO_buf_base && !(fp->_flags & _IO_USER_BUF))
    {
      FREE_BUF(fp->_IO_buf_base);
      fp->_IO_buf_base = fp->_IO_buf_end = NULL;
    }

  for (mark = fp->_markers; mark != NULL; mark = mark->_next)
    mark->_sbuf = NULL;

  if (fp->_IO_save_base)
    {
      free (fp->_IO_save_base);
      fp->_IO_save_base = NULL;
    }

  _IO_un_link(fp);
}

_IO_pos_t
DEFUN(_IO_default_seekoff, (fp, offset, dir, mode),
      register _IO_FILE *fp AND _IO_off_t offset AND int dir AND int mode)
{
    return _IO_pos_BAD;
}

int
DEFUN(_IO_sputbackc, (fp, c),
      register _IO_FILE *fp AND int c)
{
  int result;
  
  if (fp->_IO_read_ptr > fp->_IO_read_base
      && (unsigned char)fp->_IO_read_ptr[-1] == (unsigned char)c)
    {
      fp->_IO_read_ptr--;
      result = (unsigned char)c;
    }
  else
    result = _IO_PBACKFAIL (fp, c);

  if (result != EOF)
    fp->_flags &= ~_IO_EOF_SEEN;

  return result;
}

int
DEFUN(_IO_sungetc, (fp),
      register _IO_FILE *fp)
{
  int result;
  
  if (fp->_IO_read_ptr > fp->_IO_read_base)
    {
      fp->_IO_read_ptr--;
      result = (unsigned char)*fp->_IO_read_ptr;
    }
  else
    result = _IO_PBACKFAIL (fp, EOF);

  if (result != EOF)
    fp->_flags &= ~_IO_EOF_SEEN;

  return result;
}

#if 0 /* Work in progress */
void
DEFUN(_IO_set_column, (fp, c),
      register _IO_FILE *fp AND int c)
{
  if (c == -1)
    fp->_column = -1;
  else
    fp->_column = c - (fp->_IO_write_ptr - fp->_IO_write_base);
}
#else
int
DEFUN(_IO_set_column, (fp, i),
      register _IO_FILE *fp AND int i)
{
  fp->_cur_column = i+1;
  return 0;
}
#endif


unsigned
DEFUN(_IO_adjust_column, (start, line, count),
      unsigned start AND const char *line AND int count)
{
  register const char *ptr = line + count;
  while (ptr > line)
    if (*--ptr == '\n')
      return line + count - ptr - 1;
  return start + count;
}

int
DEFUN(_IO_get_column, (fp),
      register _IO_FILE *fp)
{
  if (fp->_cur_column) 
    return _IO_adjust_column(fp->_cur_column - 1,
			      fp->_IO_write_base,
			      fp->_IO_write_ptr - fp->_IO_write_base);
  return -1;
}

int
DEFUN_VOID(_IO_flush_all)
{
  int result = 0;
  _IO_FILE *fp;
  for (fp = _IO_list_all; fp != NULL; fp = fp->_chain)
    if (fp->_IO_write_ptr > fp->_IO_write_base
	&& _IO_OVERFLOW (fp, EOF) == EOF)
      result = EOF;
  return result;
}

void
DEFUN_VOID(_IO_flush_all_linebuffered)
{
  _IO_FILE *fp;
  for (fp = _IO_list_all; fp != NULL; fp = fp->_chain)
    if (fp->_flags & _IO_LINE_BUF)
      _IO_OVERFLOW (fp, EOF);
}

void
DEFUN_VOID(_IO_unbuffer_all)
{
  _IO_FILE *fp;
  for (fp = _IO_list_all; fp != NULL; fp = fp->_chain)
    if (! (fp->_flags & _IO_UNBUFFERED))
      _IO_SETBUF (fp, NULL, 0);
}

void
DEFUN_VOID(_IO_cleanup)
{
  _IO_flush_all ();

  /* We currently don't have a reliable mechanism for making sure that
     C++ static destructors are executed in the correct order.
     So it is possible that other static destructord might want to
     write to cout - and they're supposed to be able to do so.

     The following will make the standard streambufs be unbuffered, 
     which forces any output from late destructors to be written out. */
  _IO_unbuffer_all ();
}

void
DEFUN(_IO_init_marker, (marker, fp),
      struct _IO_marker *marker AND _IO_FILE *fp)
{
  marker->_sbuf = fp;
  if (_IO_in_put_mode(fp))
    _IO_switch_to_get_mode(fp);
  if (_IO_in_backup(fp))
    marker->_pos = fp->_IO_read_ptr - fp->_IO_read_end;
  else
    marker->_pos = fp->_IO_read_ptr - fp->_IO_read_base;
  
  /* Should perhaps sort the chain? */
  marker->_next = fp->_markers;
  fp->_markers = marker;
}

void
DEFUN(_IO_remove_marker, (marker),
      register struct _IO_marker *marker)
{
  /* Unlink from sb's chain. */
  register struct _IO_marker **ptr = &marker->_sbuf->_markers;
  for (; ; ptr = &(*ptr)->_next)
    {
      if (*ptr == NULL)
	break;
      else if (*ptr == marker)
	{
	  *ptr = marker->_next;
	  return;
	}
    }
#if 0
    if _sbuf has a backup area that is no longer needed, should we delete
    it now, or wait until the next underflow?
#endif
}

#define BAD_DELTA EOF

int
DEFUN(_IO_marker_difference, (mark1, mark2),
      struct _IO_marker *mark1 AND struct _IO_marker *mark2)
{
  return mark1->_pos - mark2->_pos;
}

/* Return difference between MARK and current posistion of MARK's stream. */
int
DEFUN(_IO_marker_delta, (mark),
      struct _IO_marker *mark)
{
  int cur_pos;
  if (mark->_sbuf == NULL)
    return BAD_DELTA;
  if (_IO_in_backup(mark->_sbuf))
    cur_pos = mark->_sbuf->_IO_read_ptr - mark->_sbuf->_IO_read_end;
  else
    cur_pos = mark->_sbuf->_IO_read_ptr - mark->_sbuf->_IO_read_base;
  return mark->_pos - cur_pos;
}

int
DEFUN(_IO_seekmark, (fp, mark, delta),
      _IO_FILE *fp AND struct _IO_marker *mark AND int delta)
{
  if (mark->_sbuf != fp)
    return EOF;
 if (mark->_pos >= 0)
    {
      if (_IO_in_backup(fp))
	_IO_switch_to_main_get_area(fp);
      fp->_IO_read_ptr = fp->_IO_read_base + mark->_pos;
    }
  else
    {
      if (!_IO_in_backup(fp))
	_IO_switch_to_backup_area(fp);
      fp->_IO_read_ptr = fp->_IO_read_end + mark->_pos;
    }
  return 0;
}

void
DEFUN(_IO_unsave_markers, (fp),
     register _IO_FILE *fp)
{
  register struct _IO_marker *mark = fp->_markers;
  if (mark)
    {
#ifdef TODO
      streampos offset = seekoff(0, ios::cur, ios::in);
      if (offset != EOF)
	{
	  offset += eGptr() - Gbase();
	  for ( ; mark != NULL; mark = mark->_next)
	    mark->set_streampos(mark->_pos + offset);
	}
    else
      {
	for ( ; mark != NULL; mark = mark->_next)
	  mark->set_streampos(EOF);
      }
#endif
      fp->_markers = 0;
    }

  if (_IO_have_backup(fp))
    _IO_free_backup_area(fp);
}

int
DEFUN(_IO_nobackup_pbackfail, (fp, c),
     register _IO_FILE *fp AND int c)
{
  if (fp->_IO_read_ptr > fp->_IO_read_base)
	fp->_IO_read_ptr--;
  if (c != EOF && *fp->_IO_read_ptr != c)
      *fp->_IO_read_ptr = c;
  return (unsigned char)c;
}

int
DEFUN(_IO_default_pbackfail, (fp, c),
      register _IO_FILE *fp AND int c)
{
  if (fp->_IO_read_ptr <= fp->_IO_read_base)
      {
	/* Need to handle a filebuf in write mode (switch to read mode). FIXME!*/
	if (_IO_have_backup(fp) && !_IO_in_backup(fp))
	  _IO_switch_to_backup_area(fp);
	
	if (!_IO_have_backup(fp))
	  {
	    /* No backup buffer: allocate one. */
	    /* Use nshort buffer, if unused? (probably not)  FIXME */
	    int backup_size = 128;
	    char *bbuf = (char*)malloc(backup_size);
	    if (bbuf == NULL)
	      return EOF;
	    fp->_IO_save_base = bbuf;
	    fp->_IO_save_end = fp->_IO_save_base + backup_size;
	    fp->_IO_backup_base = fp->_IO_save_end;
	    _IO_switch_to_backup_area(fp);
	  }
	else if (fp->_IO_read_ptr <= fp->_IO_read_base)
	  {
	    /* Increase size of existing backup buffer. */
	    _IO_size_t new_size;
	    _IO_size_t old_size = fp->_IO_read_end - fp->_IO_read_base;
	    char *new_buf;
	    new_size = 2 * old_size;
	    new_buf = (char*)malloc(new_size);
	    if (new_buf == NULL)
	      return EOF;
	    memcpy(new_buf+(new_size-old_size), fp->_IO_read_base, old_size);
	    free (fp->_IO_read_base);
	    _IO_setg(fp,
		     new_buf, new_buf+(new_size-old_size), new_buf+new_size);
	    fp->_IO_backup_base = fp->_IO_read_ptr;
	  }
      }
  fp->_IO_read_ptr--;
  if (c != EOF && *fp->_IO_read_ptr != c)
    *fp->_IO_read_ptr = c;
  return (unsigned char)*fp->_IO_read_ptr;
}

_IO_pos_t
DEFUN(_IO_default_seek, (fp, offset, dir),
      _IO_FILE *fp AND _IO_off_t offset AND int dir)
{
  return _IO_pos_BAD;
}

int
DEFUN(_IO_default_stat, (fp, st),
      _IO_FILE *fp AND void* st)
{
  return EOF;
}

_IO_ssize_t
DEFUN(_IO_default_read, (fp, data, n),
      register _IO_FILE* fp AND void* data AND _IO_ssize_t n)
{
  return -1;
}

_IO_ssize_t
DEFUN(_IO_default_write, (fp, data, n),
      register _IO_FILE* fp AND const void* data AND _IO_ssize_t n)
{
  return 0;
}


#ifdef TODO
#if defined(linux)
#define IO_CLEANUP ;
#endif

#ifdef IO_CLEANUP
  IO_CLEANUP
#else
struct __io_defs {
    __io_defs() { }
    ~__io_defs() { _IO_cleanup(); }
};   
__io_defs io_defs__;
#endif

#endif /* TODO */
