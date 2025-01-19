// { dg-options "-g -O0 -lstdc++fs" }
// { dg-do run { target c++11 } }
// { dg-require-filesystem-ts "" }

// Copyright (C) 2020-2025 Free Software Foundation, Inc.
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

#include <experimental/filesystem>
#include <iostream>

int
main()
{
  std::experimental::filesystem::path path0;
// { dg-final { note-test path0 {experimental::filesystem::path ""} } }
  std::experimental::filesystem::path path1("filename");
// { dg-final { note-test path1 {experimental::filesystem::path "filename"} } }
  std::experimental::filesystem::path path2("/dir/.");
// { dg-final { note-test path2 {experimental::filesystem::path "/dir/." = {[root-directory] = "/", [1] = "dir", [2] = "."}} } }

  std::cout << "\n";
  return 0;			// Mark SPOT
}

// { dg-final { gdb-test SPOT } }
