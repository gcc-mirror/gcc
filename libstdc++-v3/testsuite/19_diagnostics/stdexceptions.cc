// 2001-02-26 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001 Free Software Foundation, Inc.
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

// 19.1 Exception classes

#include <string>
#include <stdexcept>
#include <debug_assert.h>

// libstdc++/1972
void test01()
{
  bool test = true;
  std::string s("lack of sunlight, no water error");
  // XXX work around long-standing, pathalogical, hostility-inducing parser bug
  // std::logic_error obj(std::string(strlit));

  // 1
  std::logic_error obj = std::logic_error(s);

  // 2
  // std::logic_error obj((std::string)strlit);

  VERIFY( strcmp(obj.what(), s.data()) );
}

void test02()
{
  bool test = true;
  std::string s("lack of sunlight error");
  std::domain_error x(s);
  
  VERIFY( strcmp(x.what(), s.data())  );
}

// libstdc++/2089
class fuzzy_logic : public std::logic_error
{
public:
  fuzzy_logic() : std::logic_error("whoa") { }
};

void test03()
{
  bool test = true;

  try
    { throw fuzzy_logic(); }
  catch(const fuzzy_logic& obj)
    { VERIFY( strcmp("whoa", obj.what()) ); }
  catch(...)
    { VERIFY( false ); }
}


int main(void)
{
  test01();
  test02();
  test03();
  
  return 0;
}
