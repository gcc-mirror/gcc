// 1999-08-16 bkoz
// 1999-11-01 bkoz

// Copyright (C) 1999-2019 Free Software Foundation, Inc.
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
#include <cstdio>
#include <testsuite_hooks.h>

class test_buffer_1 : public std::streambuf 
{
public:
  test_buffer_1(const std::string& s) : str(s), it(str.begin()) { }
  
protected:
  virtual int underflow() { return (it != str.end() ? *it : EOF); }
  virtual int uflow() { return (it != str.end() ? *it++ : EOF); }

private:
  const std::string str;
  std::string::const_iterator it;
};


class test_buffer_2 : public std::streambuf 
{
public:
  test_buffer_2(const std::string& s) : str(s), it(str.begin()) { }
  
protected:
  virtual int underflow() { return (it != str.end() ? *it : EOF); }
  virtual int uflow() { return (it != str.end() ? *it++ : EOF); }
  virtual std::streamsize showmanyc() { return std::distance(it, str.end()); }
private:
  const std::string str;
  std::string::const_iterator it;
};


class test_buffer_3 : public std::streambuf 
{
public:
  test_buffer_3(const std::string& s) : str(s), it(str.begin()) { }

protected:
  virtual int underflow() { return (it != str.end() ? *it : EOF); }
  virtual int uflow() { return (it != str.end() ? *it++ : EOF); }
  virtual std::streamsize showmanyc() 
  {
    std::streamsize ret = std::distance(it, str.end());
    return ret > 0 ? ret : -1;
  }
private:
  const std::string str;
  std::string::const_iterator it;
};

class test_buffer_4 : public std::streambuf {
public:
  test_buffer_4(const std::string& s) : str(s), it(str.begin())
  {
    if (it != str.end()) {
      buf[0] = *it++;
      setg(buf, buf, buf+1);
    }
  }

protected:
  virtual int underflow() { return (it != str.end() ? *it : EOF); }
  virtual int uflow() { return (it != str.end() ? *it++ : EOF); }
  virtual std::streamsize showmanyc() {
    std::streamsize ret = std::distance(it, str.end());
    return ret > 0 ? ret : -1;
  }
private:
  const std::string str;
  std::string::const_iterator it;
  char buf[1];
};

void test(const std::string& str, std::streambuf& buf)
{
  std::ostringstream out;
  std::istream in(&buf);

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
  std::string string_a("Hello, world!");
  std::string string_b("");

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
