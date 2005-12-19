// Explicit instantiation file.

// Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002, 2005
// Free Software Foundation, Inc.
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
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
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
// ISO C++ 14882:
//

#include <ostream>
#include <iomanip>

_GLIBCXX_BEGIN_NAMESPACE(std)

  // ostream
  template class basic_ostream<char>;
  template ostream& endl(ostream&);
  template ostream& ends(ostream&);
  template ostream& flush(ostream&);
  template ostream& operator<<(ostream&, char);
  template ostream& operator<<(ostream&, unsigned char);
  template ostream& operator<<(ostream&, signed char);
  template ostream& operator<<(ostream&, const char*);
  template ostream& operator<<(ostream&, const unsigned char*);
  template ostream& operator<<(ostream&, const signed char*);

  template ostream& operator<<(ostream&, _Setfill<char>);
  template ostream& operator<<(ostream&, _Setiosflags);
  template ostream& operator<<(ostream&, _Resetiosflags);
  template ostream& operator<<(ostream&, _Setbase);
  template ostream& operator<<(ostream&, _Setprecision);
  template ostream& operator<<(ostream&, _Setw);

#ifdef _GLIBCXX_USE_WCHAR_T
  template class basic_ostream<wchar_t>;
  template wostream& endl(wostream&);
  template wostream& ends(wostream&);
  template wostream& flush(wostream&);
  template wostream& operator<<(wostream&, wchar_t);
  template wostream& operator<<(wostream&, char);
  template wostream& operator<<(wostream&, const wchar_t*);
  template wostream& operator<<(wostream&, const char*);

  template wostream& operator<<(wostream&, _Setfill<wchar_t>);
  template wostream& operator<<(wostream&, _Setiosflags);
  template wostream& operator<<(wostream&, _Resetiosflags);
  template wostream& operator<<(wostream&, _Setbase);
  template wostream& operator<<(wostream&, _Setprecision);
  template wostream& operator<<(wostream&, _Setw);
#endif

_GLIBCXX_END_NAMESPACE
