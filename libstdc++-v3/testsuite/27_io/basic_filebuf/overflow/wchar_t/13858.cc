// 2004-02-14  Petur Runolfsson  <peturr02@ru.is>

// Copyright (C) 2004-2019 Free Software Foundation, Inc.
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

class Cvt : public std::codecvt<wchar_t, char, std::mbstate_t>
{
protected:
  virtual std::codecvt_base::result
  do_out(std::mbstate_t&, const wchar_t* from, const wchar_t*,
	 const wchar_t*& from_next, char* to, char*,
	 char*& to_next) const
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
  
  wfilebuf fb;
  fb.pubimbue(locale(locale::classic(), new Cvt));
  fb.open("tmp_13858_wchar_t", ios_base::out);
  
  try
    {
      fb.sputc(L'a');
      fb.sputc(L'b');
      fb.pubimbue(locale::classic());
      fb.sputc(L'c');
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
