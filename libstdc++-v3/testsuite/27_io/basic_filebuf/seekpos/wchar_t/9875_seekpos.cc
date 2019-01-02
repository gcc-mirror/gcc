// Copyright (C) 2003-2019 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// 27.8.1.4 Overridden virtual functions

#include <fstream>
#include <locale>
#include <algorithm>
#include <cstring>
#include <testsuite_hooks.h>

class Cvt : public std::codecvt<wchar_t, char, mbstate_t>
{
protected:
  virtual std::codecvt_base::result
  do_out(std::mbstate_t&, const wchar_t* from, const wchar_t* from_end,
	 const wchar_t*& from_next, char* to, char* to_end,
	 char*& to_next) const
  {
    std::size_t from_len = from_end - from;
    std::size_t to_len = (to_end - to) / sizeof(wchar_t);
    std::size_t len = std::min(from_len, to_len);
    std::memcpy(to, from, len * sizeof(wchar_t));
    from_next = from + len;
    to_next = to + len * sizeof(wchar_t);
    return from_next == from_end ? std::codecvt_base::ok :
           std::codecvt_base::partial;
  }

  virtual std::codecvt_base::result
  do_in(std::mbstate_t&, const char* from, const char* from_end,
	const char*& from_next, wchar_t* to, wchar_t* to_end,
	wchar_t*& to_next) const
  {
    std::size_t from_len =
      (from_end - from) / sizeof(wchar_t);
    std::size_t to_len = to_end - to;
    std::size_t len = std::min(from_len, to_len);
    std::memcpy(to, from, len * sizeof(wchar_t));
    from_next = from + len * sizeof(wchar_t);
    to_next = to + len;
    return from_next == from_end ? std::codecvt_base::ok :
           std::codecvt_base::partial;
  }
  
  virtual std::codecvt_base::result
  do_unshift(std::mbstate_t&, char*, char*, char*&) const
  { return std::codecvt_base::noconv; }
  
  virtual int do_encoding() const throw() { return sizeof(wchar_t); }
  virtual bool do_always_noconv() const throw() { return false; }
  
  virtual int
  do_length(std::mbstate_t&, const char* from, const char* end,
	    std::size_t max)
  {
    std::size_t len = (end - from) / sizeof(wchar_t);
    return std::min(len, max) * sizeof(wchar_t);
  }
  
  virtual int do_max_length() const throw() { return sizeof(wchar_t); }
};

void test01()
{
  using namespace std;

  // seekpos
  wfilebuf fb;
  fb.pubimbue(locale(locale::classic(), new Cvt));
  fb.open("tmp_9875_seekpos", ios_base::out | ios_base::in | ios_base::trunc);
  fb.sputn(L"0123456789", 10);
  
  streampos p1 = fb.pubseekoff(0, ios_base::cur);
  VERIFY( p1 != streampos(-1) );
  fb.sputc(L'a');
  
  streampos p2 = fb.pubseekpos(p1);
  VERIFY( p2 != streampos(-1) );
  VERIFY( p2 == p1 );
  VERIFY( fb.sgetc() == L'a' );
  
  fb.pubseekoff(0, ios_base::beg);
  wchar_t buf[11];
  streamsize s1 = fb.sgetn(buf, 11);
  VERIFY( s1 == 11 );
  VERIFY( !wmemcmp(buf, L"0123456789a", 11) );
  
  fb.close();
}

int main()
{
  test01();
  return 0;
}
