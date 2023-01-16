// Copyright (C) 2020-2023 Free Software Foundation, Inc.
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

// { dg-do run { target c++17 } }
// { dg-require-filesystem-ts "" }

#include <filesystem>
#include <cerrno>
#include <testsuite_hooks.h>

bool used_custom_readdir = false;

extern "C" void* readdir(void*)
{
  used_custom_readdir = true;
  errno = EIO;
  return nullptr;
}

void
test01()
{
  using std::filesystem::recursive_directory_iterator;
  std::error_code ec;
  recursive_directory_iterator it(".", ec);
  if (used_custom_readdir)
    VERIFY( ec.value() == EIO );
}

int
main()
{
  test01();
}
