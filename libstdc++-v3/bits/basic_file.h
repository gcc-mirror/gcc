// Wrapper of C-language FILE struct -*- C++ -*-

// Copyright (C) 1999, 2000 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

//
// ISO C++ 14882: 27.8  File-based streams
//

#ifndef _CPP_BASIC_FILE
#define _CPP_BASIC_FILE		1

#include <bits/c++config.h>
#include <bits/std_ios.h>

namespace std {
  
  // Some of these member functions are based on libio/filebuf.cc.
  // Also note that the order and number of virtual functions has to precisely
  // match the order and number in the _IO_jump_t struct defined in libioP.h.
#if _GLIBCPP_BASIC_FILE_INHERITANCE
  class __basic_file: public __c_file_type
#else
  class __basic_file
#endif
  {
#if _GLIBCPP_BASIC_FILE_ENCAPSULATION
    int 		_M_fileno;
    __c_file_type* 	_M_cfile;
#endif

  public:
    __basic_file(__c_lock* __lock = 0);

    // Eqivalent to the normal fopen function.
    __basic_file* 
    open(const char* __name, ios_base::openmode __mode, int __prot = 0664);

    // Used for opening the standard streams, cin, cout, cerr, clog,
    // and their wide-stream equivalents. Instead of calling open, it
    // just sets __c_file_type->_fileno and the respective _flags bits, and
    // returns.
    __basic_file*
    sys_open(int __fd, ios_base::openmode __mode);

    __basic_file* 
    close(); 

    bool 
    is_open();

    // Needed by ios_base::sync_with_stdio.
    int get_fileno(void);

    // NB: Must match FILE specific jump table starting here--this
    // means all virtual functions starting with the dtor must match,
    // slot by slot. For glibc-based dystems, this means the _IO_FILE
    // as the FILE struct and _IO_jump_t as the jump table.
    virtual 
    ~__basic_file(); // Takes the place of __finish.

    virtual int 
    overflow(int __c = EOF);

    virtual int 
    underflow();

    virtual int 
    uflow();

    virtual int 
    pbackfail(int __c);

    // A complex "write" function that sets all of __c_file_type's
    // ponters and associated data members correctly and manages it's
    // relation to the external byte sequence.
    virtual streamsize 
    xsputn(const char* __s, streamsize __n);

    // A complex "read" function that sets all of __c_file_type's
    // ponters and associated data members correctly and manages it's
    // relation to the external byte sequence.
    virtual streamsize 
    xsgetn(char* __s, streamsize __n);

    // A complex "seekoff" function that sets all of __c_file_type's
    // ponters and associated data members correctly and manages it's
    // relation to the external byte sequence.
    virtual streamoff
    seekoff(streamoff __off, ios_base::seekdir __way,
	    ios_base::openmode __mode = ios_base::in | ios_base::out);

    // A complex "seekpos" function that sets all of __c_file_type's
    // pointers and associated data members correctly and manages it's
    // relation to the external byte sequence.
    virtual streamoff
    seekpos(streamoff __pos, 
	    ios_base::openmode __mode = ios_base::in | ios_base::out);

    virtual streambuf* 
    setbuf(char* __b, int __len);

    virtual int 
    sync();

    virtual int 
    doallocate();

    // A simple read function for the external byte sequence, that
    // does no mucking around with or setting of the pointers or flags
    // in __c_file_type.
    virtual streamsize 
    sys_read(char* __s, streamsize __n);

    // A simple write function for the external byte sequence, that
    // does no mucking around with or setting of the pointers or flags
    // in __c_file_type.
    virtual streamsize 
    sys_write(const char* __s, streamsize __n);

    // A simple seek function for the external byte sequence, that
    // does no mucking around with or setting of the pointers or flags
    // in __c_file_type.
    virtual streamoff
    sys_seek(streamoff __off, ios_base::seekdir __way);

    virtual int 
    sys_close();

    virtual int 
    sys_stat(void* __v);

    virtual int 
    showmanyc();

    virtual void 
    imbue(void* __v);
    };

} // namespace std

#endif	/* _CPP_BASIC_FILE */








