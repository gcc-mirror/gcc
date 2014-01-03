// 2003-05-03  Petur Runolfsson  <peturr02@ru.is>

// Copyright (C) 2003-2014 Free Software Foundation, Inc.
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

#include <locale>
#include <fstream>
#include <cctype>
#include <testsuite_hooks.h>

class Cvt_to_upper : public std::codecvt<char, char, std::mbstate_t>
{
  typedef std::codecvt<char, char, std::mbstate_t> Base;

public:
  explicit Cvt_to_upper(std::size_t refs = 0)
  : Base(refs)
  { }

protected:
  virtual result
  do_in(state_type&,
	const extern_type* from, const extern_type* from_end,
	const extern_type*& from_next,
	intern_type* to, intern_type* to_end,
	intern_type*& to_next) const
  {
    while (from < from_end && to < to_end)
      *to++ = std::toupper(*from++);
    
    to_next = to;
    from_next = from;
    return from == from_end ? ok : partial;
  }

  virtual bool
  do_always_noconv() const throw()
  {
    return false;
  }
};

// libstdc++/9027
void test01()
{
  using namespace std;

  bool test __attribute__((unused)) = true;
  const char* name = "filebuf_virtuals-1.txt";
  locale loc (locale::classic(), new Cvt_to_upper);	

  filebuf fbin;
  fbin.pubimbue(loc);
  fbin.open(name, ios_base::in);

  int c;
  while ((c = fbin.sbumpc()) != EOF)
    {
      VERIFY( !islower(c) );
    }
  
  fbin.close();
}

int main()
{
  test01();
  return 0;
}
