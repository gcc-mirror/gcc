/* 
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
the executable file might be covered by the GNU General Public License. */

#ifndef _BUILTINBUF_H
#define _BUILTINBUF_H

#ifdef __GNUC__
#pragma interface
#endif

#include <streambuf.h>

#if !_IO_UNIFIED_JUMPTABLES
// A builtinbuf is a streambuf where all the virtual operations
// call the _IO_jump_t table.

extern "C++" {
class builtinbuf : public streambuf {
  friend ios;
  virtual int overflow(int);
  virtual int underflow();
  virtual streamsize xsgetn(char *, streamsize);
  virtual streamsize xsputn(const char *, streamsize);
  virtual streambuf* setbuf(char*, int);
  virtual int doallocate();
  virtual ~builtinbuf();
  virtual int sync();

  virtual streampos seekoff(streamoff, _seek_dir, int mode=ios::in|ios::out);
  virtual streampos seekpos(streampos pos, int mode = ios::in|ios::out);
  virtual int pbackfail(int c);
  virtual streamsize sys_read(char* buf, streamsize size);
  virtual streampos sys_seek(streamoff, _seek_dir);
  virtual streamsize sys_write(const char*, streamsize);
  virtual int sys_stat(void*); // Actually, a (struct stat*)
  virtual int sys_close();
#if 0
  virtual int get_column();
  virtual int set_column(int);
#endif
 private:
  builtinbuf() { }
};
} // extern "C++"
#endif

#endif /* _BUILTINBUF_H */
