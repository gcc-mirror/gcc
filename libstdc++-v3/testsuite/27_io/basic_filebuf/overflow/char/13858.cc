// 2004-02-14  Petur Runolfsson  <peturr02@ru.is>

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
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// 27.8.1.4 Overridden virtual functions

#include <fstream>
#include <locale>

class Cvt : public std::codecvt<char, char, std::mbstate_t>
{
protected:
  virtual std::codecvt_base::result
  do_out(std::mbstate_t&, const char* from, const char*,
	 const char*& from_next, char* to, char*, char*& to_next) const
  {
    from_next = from;
    to_next = to;
    return std::codecvt_base::error;
  }

  virtual bool
  do_always_noconv() const throw()
  { return false; }
};

// libstdc++/13858
void test01()
{
  using namespace std;
  
  filebuf fb;
  fb.pubimbue(locale(locale::classic(), new Cvt));
  fb.open("tmp_13858_char", ios_base::out);
  
  try
    {
      fb.sputc('a');
      fb.sputc('b');
      fb.pubimbue(locale::classic());
      fb.sputc('c');
      fb.pubsync();
      fb.close();
    }
  catch (std::exception&)
    {
    }
}

int main()
{
  test01();
  return 0;
}
