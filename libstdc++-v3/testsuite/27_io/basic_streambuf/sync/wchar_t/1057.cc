// 1999-10-11 bkoz

// Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004
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

// 27.5.2 template class basic_streambuf

#include <streambuf>
#include <string>
#include <testsuite_hooks.h>

class setpbuf : public std::wstreambuf
{
  wchar_t	buffer[4];
  std::wstring 	result;

public:

  std::wstring&
  get_result()
  { return result; }

  setpbuf()
  {
    wchar_t foo[32];
    setp(foo, foo + 32);
    setp(buffer, buffer + 4);
  }

  ~setpbuf()
  { sync(); }

  virtual int_type 
  overflow(int_type n)
  {
    if (sync() != 0)
      return traits_type::eof();
    
    result += traits_type::to_char_type(n);
    
    return n;
  }
  
  virtual int 
  sync()
  {
    result.append(pbase(), pptr());
    setp(buffer, buffer + 4);
    return 0;
  }
};

// libstdc++/1057
void test04()
{
  bool test __attribute__((unused)) = true;
  std::wstring text = L"abcdefghijklmn";
  
  // 01
  setpbuf sp1;
  // Here xsputn writes over sp1.result
  sp1.sputn(text.c_str(), text.length());

  // This crashes when result is accessed
  sp1.pubsync();
  VERIFY( sp1.get_result() == text );
  
  // 02
  setpbuf sp2;
  for (std::wstring::size_type i = 0; i < text.length(); ++i)
    {
      // sputc also writes over result
      sp2.sputc(text[i]);
    }
  
  // Crash here
  sp2.pubsync();
  VERIFY( sp2.get_result() == text );
}

int main() 
{
  test04();
  return 0;
}
