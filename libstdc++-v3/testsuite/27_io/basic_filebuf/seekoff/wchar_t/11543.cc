// Copyright (C) 2003-2024 Free Software Foundation, Inc.
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

// { dg-require-fileio "" }

#include <fstream>
#include <locale>
#include <testsuite_hooks.h>

struct MyState
{
};

struct MyCharTraits : std::char_traits<wchar_t>
{
  typedef std::fpos<MyState> pos_type;
  typedef MyState state_type;
};

namespace std
{
  template <>
    class codecvt<wchar_t, char, MyState> :
      public locale::facet, public codecvt_base
    {
    public:
      typedef wchar_t intern_type;
      typedef char extern_type;
      typedef MyState state_type;
    
      explicit codecvt(size_t refs = 0)
      : locale::facet(refs) { }
    
      result out(state_type& state, const intern_type* from,
		 const intern_type* from_end,  const intern_type*& from_next,
		 extern_type* to, extern_type* to_limit,
		 extern_type*& to_next) const
      { return do_out(state, from, from_end, from_next,
		      to, to_limit, to_next); }

      result unshift(state_type& state, extern_type* to, extern_type* to_limit,
		     extern_type*& to_next) const
      { return do_unshift(state, to, to_limit, to_next); }

      result in(state_type& state, const extern_type* from,
		const extern_type* from_end, const extern_type*& from_next,
		intern_type* to, intern_type* to_limit,
		intern_type*& to_next) const
      { return do_in(state, from, from_end, from_next,
		     to, to_limit, to_next); }

      int encoding() const throw()
      { return do_encoding(); }
      
      bool always_noconv() const throw()
      { return do_always_noconv(); }

      int length(state_type& state, const extern_type* from,
		 const extern_type* end, size_t max) const
      { return do_length(state, from, end, max); }

      int max_length() const throw()
      { return do_max_length(); }
    
      static locale::id id;
    
    protected:
      virtual ~codecvt();

      virtual result do_out(state_type&, const intern_type*,
			    const intern_type*, const intern_type*&,
			    extern_type*, extern_type*, extern_type*&) const
      { return error; }

      virtual result do_in(state_type&, const extern_type*, const extern_type*,
			   const extern_type*&, intern_type*, intern_type*,
			   intern_type*&) const
      { return error; }

      virtual result do_unshift(state_type&, extern_type*, extern_type*,
				extern_type*&) const
      { return noconv; }

      virtual int do_encoding() const throw()
      { return 1; }

      virtual bool do_always_noconv() const throw()
      { return false; }

      virtual int do_length(state_type&, const extern_type* from,
			    const extern_type* end, size_t max) const
      {
	size_t len = end - from;
	return std::min(max, len);
      }

      virtual int do_max_length() const throw()
      { return 1; }
    };
  
  locale::id codecvt<wchar_t, char, MyState>::id;

  codecvt<wchar_t, char, MyState>::~codecvt()
  { }
}

void test01()
{
  std::locale loc(std::locale::classic(),
		  new std::codecvt<wchar_t, char, MyState>);
  std::basic_filebuf<wchar_t, MyCharTraits> fb;
  fb.pubimbue(loc);
  fb.open("tmp_11543", std::ios_base::out);
  VERIFY( fb.is_open() );
  MyCharTraits::pos_type pos = fb.pubseekoff(0, std::ios_base::beg);
  VERIFY( pos != MyCharTraits::pos_type(MyCharTraits::off_type(-1)) );
  fb.close();
}

int main()
{
  test01();
  return 0;
}
