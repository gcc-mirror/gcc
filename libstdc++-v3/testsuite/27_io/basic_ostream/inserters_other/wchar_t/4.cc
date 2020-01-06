// Copyright (C) 2005-2020 Free Software Foundation, Inc.
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

// 27.6.2.5.4 basic_ostream character inserters

#include <ostream>
#include <sstream>
#include <testsuite_hooks.h>

class test_buffer_1 : public std::wstreambuf 
{
public:
  test_buffer_1(const std::wstring& s)
  : str(s), it(str.begin()) { }
  
protected:
  virtual int_type
  underflow()
  { return (it != str.end() ? *it : WEOF); }
  
  virtual int_type
  uflow()
  { return (it != str.end() ? *it++ : WEOF); }

private:
  const std::wstring str;
  std::wstring::const_iterator it;
};


class test_buffer_2 : public std::wstreambuf 
{
public:
  test_buffer_2(const std::wstring& s)
  : str(s), it(str.begin()) { }
  
protected:
  virtual int_type
  underflow()
  { return (it != str.end() ? *it : WEOF); }
  
  virtual int_type
  uflow()
  { return (it != str.end() ? *it++ : WEOF); }
  
  virtual std::streamsize
  showmanyc()
  { return std::distance(it, str.end()); }

private:
  const std::wstring str;
  std::wstring::const_iterator it;
};


class test_buffer_3 : public std::wstreambuf 
{
public:
  test_buffer_3(const std::wstring& s)
  : str(s), it(str.begin()) { }

protected:
  virtual int_type
  underflow()
  { return (it != str.end() ? *it : WEOF); }
  
  virtual int_type
  uflow()
  { return (it != str.end() ? *it++ : WEOF); }
  
  virtual std::streamsize
  showmanyc() 
  {
    std::streamsize ret = std::distance(it, str.end());
    return ret > 0 ? ret : -1;
  }

private:
  const std::wstring str;
  std::wstring::const_iterator it;
};

class test_buffer_4 : public std::wstreambuf
{
public:
  test_buffer_4(const std::wstring& s)
  : str(s), it(str.begin())
  {
    if (it != str.end())
      {
	buf[0] = *it++;
	setg(buf, buf, buf+1);
      }
  }

protected:
  virtual int_type
  underflow()
  { return (it != str.end() ? *it : WEOF); }
  
  virtual int_type
  uflow()
  { return (it != str.end() ? *it++ : WEOF); }
  
  virtual std::streamsize
  showmanyc()
  {
    std::streamsize ret = std::distance(it, str.end());
    return ret > 0 ? ret : -1;
  }

private:
  const std::wstring str;
  std::wstring::const_iterator it;
  wchar_t buf[1];
};

void test(const std::wstring& str, std::wstreambuf& buf)
{
  std::wostringstream out;
  std::wistream in(&buf);

  out << in.rdbuf();

  if (out.str() != str) 
    VERIFY( false );
}

// libstdc++/6745
// libstdc++/8071
// libstdc++/8127
// Jonathan Lennox  <lennox@cs.columbia.edu>
void test05()
{
  std::wstring string_a(L"Hello, world!");
  std::wstring string_b(L"");

  test_buffer_1 buf1a(string_a);
  test_buffer_1 buf1b(string_b);

  test_buffer_2 buf2a(string_a);
  test_buffer_2 buf2b(string_b);

  test_buffer_3 buf3a(string_a);
  test_buffer_3 buf3b(string_b);

  test_buffer_4 buf4a(string_a);
  test_buffer_4 buf4b(string_b);

  test(string_a, buf1a);
  test(string_b, buf1b);

  test(string_a, buf2a);
  test(string_b, buf2b);

  test(string_a, buf3a);
  test(string_b, buf3b);

  test(string_a, buf4a);
  test(string_b, buf4b);
}

int 
main()
{
  test05();
  return 0;
}
