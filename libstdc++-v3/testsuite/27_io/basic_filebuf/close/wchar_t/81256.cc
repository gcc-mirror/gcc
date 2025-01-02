// Copyright (C) 2018-2025 Free Software Foundation, Inc.
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

// { dg-do run { target c++11 } }

#include <fstream>
#include <locale>
#include <testsuite_hooks.h>

struct E : std::runtime_error
{
  E() : runtime_error("") { }
};

struct Cvt : std::codecvt<wchar_t, char, std::mbstate_t>
{
  explicit Cvt(size_t refs) : codecvt(refs) { }

  mutable int exceptions_thrown = 0;

private:
  int
  do_encoding() const noexcept override
  { return -1; }

  bool
  do_always_noconv() const noexcept override
  { return false; }

  result
  do_unshift(state_type&, char*, char*, char*&) const override
  {
    ++exceptions_thrown;
    throw E();
  }
};

struct filebuf : std::basic_filebuf<wchar_t>
{
  explicit filebuf(Cvt* c)
  {
    std::locale loc(std::locale::classic(), c);
    imbue(loc);
  }
};

void
test01()
{
  // This facet needs to still be valid when ~basic_filebuf runs:
  Cvt conv{1};
  {
    filebuf fb(&conv);
    fb.open("output.txt", std::wios::out);
    fb.sputn(L"x", 1);

    bool caught = false;
    try
    {
      /* [filebuf.members] p7: If one of these calls throws an exception,
       * the exception is caught and rethrown after closing the file.  */
      fb.close();
    }
    catch (const E&)
    {
      caught = true;
    }
    VERIFY( conv.exceptions_thrown == 1 );
    VERIFY( caught );
  }
  VERIFY( conv.exceptions_thrown == 1 );
}

void
test02()
{
  // This facet needs to still be valid when ~basic_filebuf runs:
  Cvt conv{1};
  {
    filebuf fb(&conv);
    fb.open("output.txt", std::wios::out);
    fb.sputn(L"x", 1);
    /* [filebuf.cons] p5: If an exception occurs during the destruction
     * of the object, including the call to close(), the exception is
     * caught but not rethrown.  */
  }
  VERIFY( conv.exceptions_thrown == 1 );
}

int
main()
{
  test01();
  test02();
}
