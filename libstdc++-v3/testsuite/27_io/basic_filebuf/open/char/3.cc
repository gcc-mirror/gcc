// Copyright (C) 2001-2018 Free Software Foundation, Inc.
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

// 27.8.1.3 filebuf member functions
// @require@ %-*.tst %-*.txt
// @diff@ %-*.tst %-*.txt

// various tests for filebuf::open() and filebuf::close() including
// the non-portable functionality in the libstdc++-v3 IO library

// { dg-require-fileio "" }

#include <fstream>
#include <testsuite_hooks.h>

// Charles Leggett <CGLeggett@lbl.gov>
void test_05()
{
  const char* name = "tmp_file5";

  std::fstream scratch_file;

  scratch_file.open(name, std::ios::out);
  scratch_file.close();

  scratch_file.open(name, std::ios::in);
  if (!scratch_file)
    VERIFY( false );
  scratch_file.close();
}

int
main()
{
  test_05();
  return 0;
}


