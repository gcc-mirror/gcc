// 2004-02-09  Petur Runolfsson  <peturr02@ru.is>

// Copyright (C) 2004 Free Software Foundation, Inc.
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

// 27.4.4.2 basic_ios member functions

#include <sstream>
#include <locale>

#include <testsuite_hooks.h>
#include <testsuite_character.h>

namespace std
{
  template<>
    class ctype<__gnu_test::character>
    : public locale::facet, public ctype_base
    {
    public:
      typedef __gnu_test::character char_type;
      explicit ctype(size_t refs  = 0)
      : locale::facet(refs) { }

      bool
      is(mask m, char_type c) const
      { return this->do_is(m, c); }

      const char_type*
      is(const char_type* low, const char_type* high, mask* vec) const
      { return this->do_is(low, high, vec); }

      const char_type*
      scan_is(mask m, const char_type* low, const char_type* high) const
      { return this->do_scan_is(m, low, high); }

      const char_type*
      scan_not(mask m, const char_type* low, const char_type* high) const
      { return this->do_scan_not(m, low, high); }

      char_type
      toupper(char_type c) const
      { return this->do_toupper(c); }

      const char_type*
      toupper(char_type* low, const char_type* high) const
      { return this->do_toupper(low, high); }

      char_type
      tolower(char_type c) const
      { return this->do_tolower(c); }

      const char_type*
      tolower(char_type* low, const char_type* high) const
      { return this->do_tolower(low, high); }

      char_type
      widen(char c) const
      { return this->do_widen(c); }

      const char*
      widen(const char* low, const char* high, char_type* to) const
      { return this->do_widen(low, high, to); }

      char
      narrow(char_type c, char dfault) const
      { return this->do_narrow(c, dfault); }

      const char_type*
      narrow(const char_type* low, const char_type* high,
	     char dfault, char* to) const
      { return this->do_narrow(low, high, dfault, to); }

      static locale::id id;

    protected:
      ~ctype()
      { }

      virtual bool
      do_is(mask m, char_type c) const
      { return false; }

      virtual const char_type*
      do_is(const char_type* low, const char_type* high, mask* vec) const
      {
	fill_n(vec, high - low, mask());
	return high;
      }

      virtual const char_type*
      do_scan_is(mask m, const char_type* low, const char_type* high) const
      { return high; }

      virtual const char_type*
      do_scan_not(mask m, const char_type* low, const char_type* high) const
      { return low; }

      virtual char_type
      do_toupper(char_type c) const
      { return c; }

      virtual const char_type*
      do_toupper(char_type*  low, const char_type*  high) const
      { return high; }

      virtual char_type
      do_tolower(char_type c) const
      { return c; }

      virtual const char_type*
      do_tolower(char_type*  low, const char_type*  high) const
      { return high; }

      virtual char_type
      do_widen(char c) const
      { return __gnu_test::character::from_char(c); }

      virtual const char* 
      do_widen(const char* low, const char* high, char_type* dest) const
      {
	transform(low, high, dest, &__gnu_test::character::from_char);
	return high;
      }

      virtual char
      do_narrow(char_type, char dfault) const
      { return dfault; }

      virtual const char_type*
      do_narrow(const char_type* low, const char_type* high,
		char dfault, char*  dest) const
      {
	fill_n(dest, high - low, dfault);
	return high;
      }
    };

  locale::id ctype<__gnu_test::character>::id;
} // namespace std

// libstdc++/14072
void test01()
{
  bool test __attribute__((unused)) = true;
  using namespace std;

  locale loc;
  loc = locale(loc, new ctype<__gnu_test::character>());
  loc = locale(loc, new num_get<__gnu_test::character>());
  loc = locale(loc, new num_put<__gnu_test::character>());
	
  locale::global(loc);
  basic_stringstream<__gnu_test::character> s;
  s << "10\n";
  s.seekg(0, ios_base::beg);
  s.imbue(locale::classic());
  locale::global(locale::classic());
  loc = locale::classic();
	
  try
    {
      s.widen('\0');
    }
  catch (bad_cast&)
    {
    }
  
  s.clear();
  
  try
    {
      int i = 0;
      s << i;
    }
  catch (bad_cast&)
    {
    }

  s.clear();

  try
    {
      int i = 0;
      s >> i;
    }
  catch (bad_cast&)
    {
    }
}

int main()
{
  test01();
  return 0;
}
