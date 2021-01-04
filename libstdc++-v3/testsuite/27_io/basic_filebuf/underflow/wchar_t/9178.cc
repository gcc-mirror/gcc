// Copyright (C) 2003-2021 Free Software Foundation, Inc.
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
#include <string>
#include <iterator>
#include <algorithm>
#include <locale>
#include <testsuite_hooks.h>

template <typename InternT, typename StateT = mbstate_t>
class checksumcvt : public std::codecvt<InternT, char, StateT>
{
  typedef std::codecvt<InternT, char, StateT> Base;
  static const size_t width = sizeof(InternT) + 1;

public:
  typedef InternT intern_type;
  typedef char extern_type;

  explicit checksumcvt(size_t refs = 0)
  : Base(refs)
  { }

protected:
  virtual typename std::codecvt<InternT, char, StateT>::result
  do_out(StateT&, const intern_type* from,
	 const intern_type* from_end, const intern_type*& from_next,
	 extern_type* to, extern_type* to_end,
	 extern_type*& to_next) const
  {
    size_t len = std::min(static_cast<size_t>(from_end - from),
			  static_cast<size_t>(to_end - to) / width);

    while (len--)
      {
	const char* p = reinterpret_cast<const char*>(from);
	unsigned char checksum = 0;
				
	for (size_t i = 0; i < sizeof(intern_type); ++i)
	  {
	    *to++ = p[i];
	    checksum ^= static_cast<unsigned char>(p[i]);
	  }

	*to++ = checksum;
	++from;
      }

    from_next = from;
    to_next = to;
    return from_next == from_end ? std::codecvt<InternT, char, StateT>::ok
           : std::codecvt<InternT, char, StateT>::partial;
  }

  virtual typename std::codecvt<InternT, char, StateT>::result
  do_unshift(StateT&, extern_type* to,
	     extern_type*, extern_type*& to_next) const
  {
    to_next = to;
    return std::codecvt<InternT, char, StateT>::ok;
  }

  virtual typename std::codecvt<InternT, char, StateT>::result
  do_in(StateT&, const extern_type* from,
	const extern_type* from_end, const extern_type*& from_next,
	intern_type* to, intern_type* to_end,
	intern_type*& to_next) const
  {
    size_t len = std::min(static_cast<size_t>(to_end - to),
			  static_cast<size_t>(from_end - from) / width);
			
    while (len)
      {
	const char* f = from;
	intern_type tmp;
	char* p = reinterpret_cast<char*>(&tmp);
	unsigned char checksum = 0;

	for (size_t i = 0; i < sizeof(intern_type); ++i)
	  {
	    p[i] = *f;
	    checksum ^= static_cast<unsigned char>(*f++);
	  }

	if (*f++ != checksum)
	  break;

	from = f;
	*to++ = tmp;
	len--;
      }

    from_next = from;
    to_next = to;
    return len ? std::codecvt<InternT, char, StateT>::error :
      (from_next == from_end ? std::codecvt<InternT, char, StateT>::ok
       : std::codecvt<InternT, char, StateT>::partial);
  }

  virtual int
  do_encoding() const throw()
  {
    return width;
  }

  virtual int
  do_length(StateT&, const extern_type* from,
	    const extern_type* end, size_t max) const
  {
    size_t len = std::min(max, static_cast<size_t>(end - from) / width);

    int ret = 0;
    while (len--)
      {
	unsigned char checksum = 0;

	for (size_t i = 0; i < sizeof(intern_type); ++i)
	  {
	    checksum ^= static_cast<unsigned char>(*from++);
	  }

	if (*from++ != checksum)
	  break;

	ret++;
      }

    return ret;
  }

  virtual int
  do_max_length() const throw()
  {
    return width;
  }

  virtual bool
  do_always_noconv() const throw()
  {
    return false;
  }
};

void test01()
{
  using namespace std;

  locale loc;
  loc = locale(loc, new checksumcvt<wchar_t>);

  wfilebuf fbuf1;
  fbuf1.pubimbue(loc);
  fbuf1.open("tmp_9178", ios_base::out | ios_base::trunc);
	
  string tmpstr = "abcdefghijklmnopqrstuvwxyz0123456789 \t\n";

  wifstream stream;
  wstring str1;

  while (str1.length() < 20000) 
    {
      transform(tmpstr.begin(), tmpstr.end(),
		back_inserter(str1),
		bind1st(std::mem_fun(&std::wios::widen), &stream));
    }
	
  fbuf1.sputn(str1.data(), str1.size());
  fbuf1.close();

  wfilebuf fbuf2;
  fbuf2.pubimbue(loc);
  fbuf2.open("tmp_9178", std::ios_base::in);
	
  wstring str2;
  copy(istreambuf_iterator<wchar_t>(&fbuf2),
       istreambuf_iterator<wchar_t>(),
       back_inserter(str2));

  VERIFY( str1 == str2 );
}

int main()
{
  test01();
  return 0;
}
