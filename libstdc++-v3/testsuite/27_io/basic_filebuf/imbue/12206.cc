// Copyright (C) 2003 Free Software Foundation, Inc.
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

// 27.8.1.4 Overridden virtual functions

#include <fstream>

typedef unsigned char Char;

namespace std
{
  template <>
  class codecvt<Char, char, mbstate_t> :
    public locale::facet, public codecvt_base
  {
  public:
    typedef Char intern_type;
    typedef char extern_type;
    typedef mbstate_t state_type;
    
    explicit codecvt(size_t refs = 0)
      : locale::facet(refs) { }
    result out(mbstate_t& state, const Char* from,
	       const Char* from_end, const Char*& from_next,
	       char* to, char* to_limit, char*& to_next) const
    {
      return do_out(state, from, from_end, from_next,
		    to, to_limit, to_next);
    }
    result in(mbstate_t& state, const char* from,
	      const char* from_end, const char*& from_next,
	      Char* to, Char* to_limit, Char*& to_next) const
    {
      return do_in(state, from, from_end, from_next,
		   to, to_limit, to_next);
    }
    result unshift(mbstate_t& state, char* to, char* to_end,
		   char*& to_next) const
    { return do_unshift(state, to, to_end, to_next); }
    int length(mbstate_t& state, const char* from,
	       const char* from_end, size_t max) const
    { return do_length(state, from, from_end, max); }
    int encoding() const throw()
    { return do_encoding(); }
    bool always_noconv() const throw()
    { return do_always_noconv(); }
    int max_length() const throw()
    { return do_max_length(); }
    
    static locale::id id;
    
  protected:
    virtual result do_out(mbstate_t&, const Char*,
			  const Char*,
			  const Char*&, char*,
			  char*, char*&) const
    { return ok; }
    virtual result do_in(mbstate_t&, const char*,
			 const char*,
			 const char*&, Char*,
			 Char*, Char*&) const
    { return ok; }
    virtual result do_unshift(mbstate_t&, char*, char*,
			      char*&) const
    { return noconv; }
    virtual int do_length(mbstate_t&, const char*,
			  const char*, size_t) const
    { return 1; }
    virtual int do_encoding() const throw()
    { return 1; }
    virtual bool do_always_noconv() const throw()
    { return false; }
    virtual int do_max_length() const throw()
    { return 1; }
  };
  
  locale::id codecvt<Char, char, mbstate_t>::id;
}

// libstdc++/12206
void test01()
{
  using namespace std;
  bool test __attribute__((unused)) = true;
  
  locale loc(locale::classic(),
	     new codecvt<Char, char, std::mbstate_t>);
  locale::global(loc);
  
  basic_filebuf<Char, char_traits<Char> > fb;

  loc = locale::classic();
  locale::global(loc);
  fb.pubimbue(loc);

  fb.open("tmp_12206", ios_base::out);
  try
    {
      fb.pubseekoff(0, ios_base::cur);
    }
  catch (std::exception&)
    {
    }
  fb.close();
}

int main()
{
  test01();
  return 0;
}
