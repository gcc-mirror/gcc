/* This is part of libio/iostream, providing -*- C++ -*- input/output.
Copyright (C) 1993 Free Software Foundation

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
the executable file might be covered by the GNU General Public License.

Written by Per Bothner (bothner@cygnus.com). */

#ifdef __GNUG__
#pragma implementation
#endif
#include "libioP.h"
#include "editbuf.h"
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

/* NOTE: Some of the code here is taken from GNU emacs */
/* Hence this file falls under the GNU License! */

// Invariants for edit_streambuf:
// An edit_streambuf is associated with a specific edit_string,
// which again is a sub-string of a specific edit_buffer.
// An edit_streambuf is always in either get mode or put mode, never both.
// In get mode, gptr() is the current position,
// and pbase(), pptr(), and epptr() are all NULL.
// In put mode, pptr() is the current position,
// and eback(), gptr(), and egptr() are all NULL.
// Any edit_streambuf that is actively doing insertion (as opposed to
// replacing) // must have its pptr() pointing to the start of the gap.
// Only one edit_streambuf can be actively inserting into a specific
// edit_buffer; the edit_buffer's _writer field points to that edit_streambuf.
// That edit_streambuf "owns" the gap, and the actual start of the
// gap is the pptr() of the edit_streambuf; the edit_buffer::_gap_start pointer
// will only be updated on an edit_streambuf::overflow().

int edit_streambuf::truncate()
{
    str->buffer->delete_range(str->buffer->tell((buf_char*)pptr()),
			      str->buffer->tell(str->end));
    return 0;
}

#ifdef OLD_STDIO
inline void  disconnect_gap_from_file(edit_buffer* buffer, FILE* fp)
{
    if (buffer->gap_start_ptr != &fp->__bufp)
	return;
    buffer->gap_start_normal = fp->__bufp;
    buffer->gap_start_ptr = &buffer->gap_start_normal;
}
#endif

void edit_streambuf::flush_to_buffer(edit_buffer* buffer)
{
    if (pptr() > buffer->_gap_start && pptr() < buffer->gap_end())
	buffer->_gap_start = pptr();
}

void edit_streambuf::disconnect_gap_from_file(edit_buffer* buffer)
{
    if (buffer->_writer != this) return;
    flush_to_buffer(buffer);
    setp(pptr(),pptr());
    buffer->_writer = NULL;    
}

buf_index edit_buffer::tell(buf_char *ptr)
{
    if (ptr <= gap_start())
	return ptr - data;
    else
	return ptr - gap_end() + size1();
}

#if 0
buf_index buf_cookie::tell()
{
    return str->buffer->tell(file->__bufp);
}
#endif

buf_index edit_buffer::tell(edit_mark*mark)
{
    return tell(data + mark->index_in_buffer(this));
}

// adjust the position of the gap

void edit_buffer::move_gap(buf_offset pos)
{
  if (pos < size1())
    gap_left (pos);
  else if (pos > size1())
    gap_right (pos);
}

void edit_buffer::gap_left (int pos)
{
  register buf_char *to, *from;
  register int i;
  int new_s1;

  i = size1();
  from = gap_start();
  to = from + gap_size();
  new_s1 = size1();

  /* Now copy the characters.  To move the gap down,
     copy characters up.  */

  for (;;)
    {
      /* I gets number of characters left to copy.  */
      i = new_s1 - pos;
      if (i == 0)
	break;
#if 0
      /* If a quit is requested, stop copying now.
	 Change POS to be where we have actually moved the gap to.  */
      if (QUITP)
	{
	  pos = new_s1;
	  break;
	}
#endif
      /* Move at most 32000 chars before checking again for a quit.  */
      if (i > 32000)
	i = 32000;
      new_s1 -= i;
      while (--i >= 0)
	*--to = *--from;
    }

  /* Adjust markers, and buffer data structure, to put the gap at POS.
     POS is where the loop above stopped, which may be what was specified
     or may be where a quit was detected.  */
  adjust_markers (pos << 1, size1() << 1, gap_size(), data);
#ifndef OLD_STDIO
  _gap_start = data + pos;
#else
  if (gap_start_ptr == &gap_start_normal)
	gap_start_normal = data + pos;
#endif
  __gap_end_pos = to - data;
/*  QUIT;*/
}

void edit_buffer::gap_right (int pos)
{
  register buf_char *to, *from;
  register int i;
  int new_s1;

  i = size1();
  to = gap_start();
  from = i + gap_end();
  new_s1 = i;

  /* Now copy the characters.  To move the gap up,
     copy characters down.  */

  while (1)
    {
      /* I gets number of characters left to copy.  */
      i = pos - new_s1;
      if (i == 0)
	break;
#if 0
      /* If a quit is requested, stop copying now.
	 Change POS to be where we have actually moved the gap to.  */
      if (QUITP)
	{
	  pos = new_s1;
	  break;
	}
#endif
      /* Move at most 32000 chars before checking again for a quit.  */
      if (i > 32000)
	i = 32000;
      new_s1 += i;
      while (--i >= 0)
	*to++ = *from++;
    }

  adjust_markers ((size1() + gap_size()) << 1, (pos + gap_size()) << 1,
	- gap_size(), data);
#ifndef OLD_STDIO
  _gap_start = data+pos;
#else
  if (gap_start_ptr == &gap_start_normal)
	gap_start_normal = data + pos;
#endif
  __gap_end_pos = from - data;
/*  QUIT;*/
}

/* make sure that the gap in the current buffer is at least k
   characters wide */

void edit_buffer::make_gap(buf_offset k)
{
  register buf_char *p1, *p2, *lim;
  buf_char *old_data = data;
  int s1 = size1();

  if (gap_size() >= k)
    return;

  /* Get more than just enough */
  if (buf_size > 1000) k += 2000;
  else k += /*200;*/ 20; // for testing!

  p1 = (buf_char *) realloc (data, s1 + size2() + k);
  if (p1 == 0)
    abort(); /*memory_full ();*/

  k -= gap_size();			/* Amount of increase.  */

  /* Record new location of text */
  data = p1;

  /* Transfer the new free space from the end to the gap
     by shifting the second segment upward */
  p2 = data + buf_size;
  p1 = p2 + k;
  lim = p2 - size2();
  while (lim < p2)
    *--p1 = *--p2;

  /* Finish updating text location data */
  __gap_end_pos += k;

#ifndef OLD_STDIO
  _gap_start = data + s1;
#else
  if (gap_start_ptr == &gap_start_normal)
	gap_start_normal = data + s1;
#endif

  /* adjust markers */
  adjust_markers (s1 << 1, (buf_size << 1) + 1, k, old_data);
  buf_size += k;
}

/* Add `amount' to the position of every marker in the current buffer
   whose current position is between `from' (exclusive) and `to' (inclusive).
   Also, any markers past the outside of that interval, in the direction
   of adjustment, are first moved back to the near end of the interval
   and then adjusted by `amount'.  */

void edit_buffer::adjust_markers(register mark_pointer low,
				 register mark_pointer high,
				 int amount, buf_char *old_data)
{
  register struct edit_mark *m;
  register mark_pointer mpos;
  /* convert to mark_pointer */
  amount <<= 1;

  if (_writer)
      _writer->disconnect_gap_from_file(this);

  for (m = mark_list(); m != NULL; m = m->chain)
    {
      mpos = m->_pos;
      if (amount > 0)
	{
	  if (mpos > high && mpos < high + amount)
	    mpos = high + amount;
	}
      else
	{
	  if (mpos > low + amount && mpos <= low)
	    mpos = low + amount;
	}
      if (mpos > low && mpos <= high)
	mpos += amount;
      m->_pos = mpos;
    }

    // Now adjust files
    edit_streambuf *file;

    for (file = files; file != NULL; file = file->next) {
	mpos = file->current() - old_data;
	if (amount > 0)
	{
	  if (mpos > high && mpos < high + amount)
	    mpos = high + amount;
	}
	else
	{
	  if (mpos > low + amount && mpos <= low)
	    mpos = low + amount;
	}
	if (mpos > low && mpos <= high)
	    mpos += amount;
	char* new_pos = data + mpos;
	file->set_current(new_pos, file->is_reading());
    }
}

#if 0
stdio_
   __off == index at start of buffer (need only be valid after seek ? )
   __buf ==

if read/read_delete/overwrite mode:
     __endp <= min(*gap_start_ptr, edit_string->end->ptr(buffer))

if inserting:
     must have *gap_start_ptr == __bufp && *gap_start_ptr+gap == __endp
     file->edit_string->end->ptr(buffer) == *gap_start_ptr+end
if write_mode:
     if before gap
#endif

int edit_streambuf::underflow()
{
    if (!(_mode & ios::in))
	return EOF;
    struct edit_buffer *buffer = str->buffer;
    if (!is_reading()) { // Must switch from put to get mode.
	disconnect_gap_from_file(buffer);
	set_current(pptr(), 1);
    }
    buf_char *str_end = str->end->ptr(buffer);
  retry:
    if (gptr() < egptr()) {
	return *gptr();
    }
    if ((buf_char*)gptr() == str_end)
	return EOF;
    if (str_end <= buffer->gap_start()) {
	setg(eback(), gptr(), str_end);
	goto retry;
    }
    if (gptr() < buffer->gap_start()) {
	setg(eback(), gptr(), buffer->gap_start());
	goto retry;
    }
    if (gptr() == buffer->gap_start()) {
	disconnect_gap_from_file(buffer);
//	fp->__offset += fp->__bufp - fp->__buffer;
	setg(buffer->gap_end(), buffer->gap_end(), str_end);
    }
    else
	setg(eback(), gptr(), str_end);
    goto retry;
}

int edit_streambuf::overflow(int ch)
{
    if (_mode == ios::in)
	return EOF;
    struct edit_buffer *buffer = str->buffer;
    flush_to_buffer(buffer);
    if (ch == EOF)
	return 0;
    if (is_reading()) { // Must switch from get to put mode.
	set_current(gptr(), 0);
    }
    buf_char *str_end = str->end->ptr(buffer);
  retry:
    if (pptr() < epptr()) {
	*pptr() = ch;
	pbump(1);
	return (unsigned char)ch;
    }
    if ((buf_char*)pptr() == str_end || inserting()) {
	/* insert instead */
	if (buffer->_writer)
	    buffer->_writer->flush_to_buffer(); // Redundant?
	buffer->_writer = NULL;
	if  (pptr() >= buffer->gap_end())
	    buffer->move_gap(pptr() - buffer->gap_size());
	else
	    buffer->move_gap(pptr());
	buffer->make_gap(1);
	setp(buffer->gap_start(), buffer->gap_end());
	buffer->_writer = this;
	*pptr() = ch;
	pbump(1);
	return (unsigned char)ch;
    }
    if (str_end <= buffer->gap_start()) {
	// Entire string is left of gap.
	setp(pptr(), str_end);
    }
    else if (pptr() < buffer->gap_start()) {
	// Current pos is left of gap.
	setp(pptr(), buffer->gap_start());
	goto retry;
    }
    else if (pptr() == buffer->gap_start()) {
	// Current pos is at start of gap; move to end of gap.
//	disconnect_gap_from_file(buffer);
	setp(buffer->gap_end(), str_end);
//	__offset += __bufp - __buffer;
    }
    else {
	// Otherwise, current pos is right of gap.
	setp(pptr(), str_end);
    }
    goto retry;
}

void edit_streambuf::set_current(char *new_pos, int reading)
{
    if (reading) {
	setg(new_pos, new_pos, new_pos);
	setp(NULL, NULL);
    }
    else {
	setg(NULL, NULL, NULL);
	setp(new_pos, new_pos);
    }
}

// Called by fseek(fp, pos, whence) if fp is bound to a edit_buffer.

streampos edit_streambuf::seekoff(streamoff offset, _seek_dir dir,
				  int /* =ios::in|ios::out*/)
{
    struct edit_buffer *buffer = str->buffer;
    disconnect_gap_from_file(buffer);
    buf_index cur_pos = buffer->tell((buf_char*)current());;
    buf_index start_pos = buffer->tell(str->start);
    buf_index end_pos = buffer->tell(str->end);
    switch (dir) {
      case ios::beg:
	offset += start_pos;
	break;
      case ios::cur:
	offset += cur_pos;
	break;
      case ios::end:
	offset += end_pos;
	break;
    }
    if (offset < start_pos || offset > end_pos)
	return EOF;
    buf_char *new_pos = buffer->data + offset;
    buf_char* gap_start = buffer->gap_start();
    if (new_pos > gap_start) {
	buf_char* gap_end = buffer->gap_end();
	new_pos += gap_end - gap_start;
	if (new_pos >= buffer->data + buffer->buf_size) abort(); // Paranoia.
    }
    set_current(new_pos, is_reading());
    return EOF;
}

#if 0
int buf_seek(void *arg_cookie, fpos_t * pos, int whence)
{
    struct buf_cookie *cookie = arg_cookie;
    FILE *file = cookie->file;
    struct edit_buffer *buffer = cookie->str->buffer;
    buf_char *str_start = cookie->str->start->ptr(buffer);
    disconnect_gap_from_file(buffer, cookie->file);
    fpos_t cur_pos, new_pos;
    if (file->__bufp <= *buffer->gap_start_ptr
	|| str_start >= buffer->__gap_end)
	cur_pos = str_start - file->__bufp;
    else
	cur_pos =
	    (*buffer->gap_start_ptr - str_start) + (file->__bufp - __gap_end);
    end_pos = ...;
    switch (whence) {
      case SEEK_SET:
	new_pos = *pos;
	break;
      case SEEK_CUR:
	new_pos = cur_pos + *pos;
	break;
      case SEEK_END:
	new_pos = end_pos + *pos;
	break;
    }
    if (new_pos > end_pos) {
	seek to end_pos;
	insert_nulls(new_pos - end_pos);
	return;
    }
    if (str_start + new_pos <= *gap_start_ptr &* *gap_start_ptr < end) {
	__buffer = str_start;
        __off = 0;
	__bufp = str_start + new_pos;
	file->__get_limit =
	    *buffer->gap_start_ptr; /* what if gap_start_ptr == &bufp ??? */
    } else if () {
	
    }
    *pos = new_pos;
}
#endif

/* Delete characters from `from' up to (but not incl) `to' */

void edit_buffer::delete_range (buf_index from, buf_index to)
{
  register int numdel;

  if ((numdel = to - from) <= 0)
    return;

  /* Make sure the gap is somewhere in or next to what we are deleting */
  if (from > size1())
    gap_right (from);
  if (to < size1())
    gap_left (to);

  /* Relocate all markers pointing into the new, larger gap
     to point at the end of the text before the gap.  */
  adjust_markers ((to + gap_size()) << 1, (to + gap_size()) << 1,
	- numdel - gap_size(), data);

   __gap_end_pos = to + gap_size();
  _gap_start = data + from;
}

void edit_buffer::delete_range(struct edit_mark *start, struct edit_mark *end)
{
    delete_range(tell(start), tell(end));
}

void buf_delete_chars(struct edit_buffer *, struct edit_mark *, size_t)
{
 abort();
}

edit_streambuf::edit_streambuf(edit_string* bstr, int mode)
{
    _mode = mode;
    str = bstr;
    edit_buffer* buffer = bstr->buffer;
    next = buffer->files;
    buffer->files = this;
    char* buf_ptr = bstr->start->ptr(buffer);
    _inserting = 0;
//    setb(buf_ptr, buf_ptr, 0);
    set_current(buf_ptr, !(mode & ios::out+ios::trunc+ios::app));
    if (_mode & ios::trunc)
	truncate();
    if (_mode & ios::ate)
	seekoff(0, ios::end);
}

// Called by fclose(fp) if fp is bound to a edit_buffer.

#if 0
static int buf_close(void *arg)
{
    register struct buf_cookie *cookie = arg;
    struct edit_buffer *buffer = cookie->str->buffer;
    struct buf_cookie **ptr;
    for (ptr = &buffer->files; *ptr != cookie; ptr = &(*ptr)->next) ;
    *ptr = cookie->next;
    disconnect_gap_from_file(buffer, cookie->file);
    free (cookie);
    return 0;
}
#endif

edit_streambuf::~edit_streambuf()
{
    if (_mode == ios::out)
	truncate();
    // Unlink this from list of files associated with bstr->buffer.
    edit_streambuf **ptr = &str->buffer->files;
    for (; *ptr != this; ptr = &(*ptr)->next) { }
    *ptr = next;

    disconnect_gap_from_file(str->buffer);
}

edit_buffer::edit_buffer()
{
    buf_size = /*200;*/ 15; /* for testing! */
    data = (buf_char*)malloc(buf_size);
    files = NULL;
#ifndef OLD_STDIO
    _gap_start = data;
    _writer = NULL;
#else
    gap_start_normal = data;
    gap_start_ptr = &gap_start_normal;
#endif
    __gap_end_pos = buf_size;
    start_mark.chain = &end_mark;
    start_mark._pos = 0;
    end_mark.chain = NULL;
    end_mark._pos = 2 * buf_size + 1;
}

// Allocate a new mark, which is adjusted by 'delta' bytes from 'this'.
// Restrict new mark to lie within 'str'.

edit_mark::edit_mark(struct edit_string *str, long delta)
{
    struct edit_buffer *buf = str->buffer;
    chain = buf->start_mark.chain;
    buf->start_mark.chain = this;
    mark_pointer size1 = buf->size1() << 1;
    int gap_size = buf->gap_size() << 1;
    delta <<= 1;

    // check if new and old marks are opposite sides of gap
    if (_pos <= size1 && _pos + delta > size1)
	delta += gap_size;
    else if (_pos >= size1 + gap_size && _pos + delta < size1 + gap_size)
	delta -= gap_size;

    _pos = _pos + delta;
    if (_pos < str->start->_pos & ~1)
	_pos = (str->start->_pos & ~ 1) + (_pos & 1);
    else if (_pos >= str->end->_pos)
	_pos = (str->end->_pos & ~ 1) + (_pos & 1);
}

// A (slow) way to find the buffer a mark belongs to.

edit_buffer * edit_mark::buffer()
{
    struct edit_mark *mark;
    for (mark = this; mark->chain != NULL; mark = mark->chain) ;
    // Assume that the last mark on the chain is the end_mark.
    return (edit_buffer *)((char*)mark - offsetof(edit_buffer, end_mark));
}

edit_mark::~edit_mark()
{
    // Must unlink mark from chain of owning buffer
    struct edit_buffer *buf = buffer();
    if (this == &buf->start_mark || this == &buf->end_mark) abort();
    edit_mark **ptr;
    for (ptr = &buf->start_mark.chain; *ptr != this; ptr = &(*ptr)->chain) ;
    *ptr = this->chain;
}

int edit_string::length() const
{
    ptrdiff_t delta = end->ptr(buffer) - start->ptr(buffer);
    if (end->ptr(buffer) <= buffer->gap_start() ||
	start->ptr(buffer) >= buffer->gap_end())
	return delta;
    return delta - buffer->gap_size();
}

buf_char * edit_string::copy_bytes(int *lenp) const
{
    char *new_str;
    int len1, len2;
    buf_char *start1, *start2;
    start1 = start->ptr(buffer);
    if (end->ptr(buffer) <= buffer->gap_start()
	|| start->ptr(buffer) >= buffer->gap_end()) {
	len1 = end->ptr(buffer) - start1;
	len2 = 0;
	start2 = NULL; // To avoid a warning from g++.
    }
    else {
	len1 = buffer->gap_start() - start1;
	start2 = buffer->gap_end();
	len2 = end->ptr(buffer) - start2;
    }
    new_str = (char*)malloc(len1 + len2 + 1);
    memcpy(new_str, start1, len1);
    if (len2 > 0) memcpy(new_str + len1, start2, len2);
    new_str[len1+len2] = '\0';
    *lenp = len1+len2;
    return new_str;
}

// Replace the buf_chars in 'this' with ones from 'src'.
// Equivalent to deleting this, then inserting src, except tries
// to leave marks in place: Marks whose offset from the start
// of 'this' is less than 'src->length()' will still have the
// same offset in 'this' when done.

void edit_string::assign(struct edit_string *src)
{
    edit_streambuf dst_file(this, ios::out);
    if (buffer == src->buffer /*&& ???*/) { /* overly conservative */
	int src_len;
	buf_char *new_str;
	new_str = src->copy_bytes(&src_len);
	dst_file.sputn(new_str, src_len);
	free (new_str);
    } else {
	edit_streambuf src_file(src, ios::in);
	for ( ; ; ) {
	    int ch = src_file.sbumpc();
	    if (ch == EOF) break;
	    dst_file.sputc(ch);
	}
    }
}
