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

// { dg-options "-DUSE_FILESYSTEM_TS -lstdc++fs" }
// { dg-do run { target { c++11 } } }
// { dg-require-filesystem-ts "" }

#include <experimental/filesystem>
#include <cerrno>
#include <stdio.h>
#include <stdlib.h>
#include <dirent.h>
#include <testsuite_hooks.h>
#include <testsuite_fs.h>

int choice;

extern "C" struct dirent* readdir(DIR*)
{
  // On some targets dirent::d_name is very small, but the OS allocates
  // a trailing char array after the dirent struct. Emulate that here.
  union State
  {
    struct dirent d;
    char buf[sizeof(struct dirent) + 16] = {};
  };

  static State state;
  char* d_name = state.buf + offsetof(struct dirent, d_name);

  switch (choice)
  {
  case 1:
    state.d.d_ino = 999;
#if defined _GLIBCXX_HAVE_STRUCT_DIRENT_D_TYPE && defined DT_REG
    state.d.d_type = DT_REG;
#endif
    state.d.d_reclen = 0;
    std::char_traits<char>::copy(d_name, "file", 5);
    choice = 0;
    return &state.d;
  case 2:
    state.d.d_ino = 111;
#if defined _GLIBCXX_HAVE_STRUCT_DIRENT_D_TYPE && defined DT_DIR
    state.d.d_type = DT_DIR;
#endif
    state.d.d_reclen = 60;
    std::char_traits<char>::copy(d_name, "subdir", 7);
    choice = 1;
    return &state.d;
  default:
    errno = EIO;
    return nullptr;
  }
  return &state.d;
}

void
test01()
{
  namespace fs = std::experimental::filesystem;
  std::error_code ec;
  choice = 1;
  fs::recursive_directory_iterator it(".", ec);
  if (choice == 0) // custom readdir was called
  {
    it.increment(ec);
    VERIFY( ec.value() == EIO );
    VERIFY( it == end(it) );
  }
  else
  {
    puts("Custom readdir not used, cannot test error handling");
    exit(0);
  }

#if __cpp_exceptions
  choice = 1;
  fs::recursive_directory_iterator it2(".", ec);
  if (choice == 0) // custom readdir was called
  {
    try {
      ++it2;
      VERIFY( false );
    } catch (const fs::filesystem_error& e) {
      VERIFY( e.code().value() == EIO );
      VERIFY( it2 == end(it2) );
    }
  }
#endif
}

void
test02()
{
  namespace fs = std::experimental::filesystem;
  const auto dir = __gnu_test::nonexistent_path();
  fs::create_directories(dir/"subdir");

  std::error_code ec;
  choice = 2;
  fs::recursive_directory_iterator it(dir, ec);
  if (choice == 1)
  {
    ++it;
    it.pop(ec);
    VERIFY( ec.value() == EIO );
    VERIFY( it == end(it) );
  }

#if __cpp_exceptions
  choice = 2;
  fs::recursive_directory_iterator it2(dir, ec);
  if (choice == 1)
  {
    ++it2;
    try {
      it2.pop();
      VERIFY( false );
    } catch (const fs::filesystem_error& e) {
      VERIFY( e.code().value() == EIO );
      VERIFY( it2 == end(it2) );
    }
  }
#endif

  // Cannot use fs::remove_all here because that depends on
  // recursive_directory_iterator which would use the fake readdir above.
#ifndef _GLIBCXX_FILESYSTEM_IS_WINDOWS
  ::rmdir((dir/"subdir").c_str());
  ::rmdir(dir.c_str());
#endif
}

int
main()
{
  test01();
  test02();
}
