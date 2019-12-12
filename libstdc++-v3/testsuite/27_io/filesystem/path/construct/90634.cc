// Copyright (C) 2019 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++17" }
// { dg-do run { target c++17 } }

#include <filesystem>
#include <cstdlib>
#include <testsuite_hooks.h>

std::size_t bytes_allocated = 0;

void* operator new(std::size_t n)
{
  bytes_allocated += n;
  return std::malloc(n);
}

void operator delete(void* p) noexcept { std::free(p); }
#if __cpp_sized_deallocation
void operator delete(void* p, std::size_t) noexcept { std::free(p); }
#endif

void
test01()
{
#ifdef _GLIBCXX_FILESYSTEM_IS_WINDOWS
  std::wstring s0;
  std::wstring s1 = L"/";
  std::wstring s2 = L"///";
  std::wstring s3 = L"file";
  std::wstring s4 = L"C:";
  std::wstring s5 = L"\\";
#else
  std::string s0;
  std::string s1 = "/";
  std::string s2 = "///";
  std::string s3 = "file";
  std::string s4 = "C:";
  std::string s5 = "\\";
#endif

  using std::filesystem::path;

  bytes_allocated = 0;
  path p0 = std::move(s0);
  VERIFY( bytes_allocated == 0 );
  path p1 = std::move(s1);
  VERIFY( bytes_allocated == 0 );
  path p2 = std::move(s2);
  VERIFY( bytes_allocated == 0 );
  path p3 = std::move(s3);
  VERIFY( bytes_allocated == 0 );
  path p4 = std::move(s4);
  VERIFY( bytes_allocated == 0 );
  path p5 = std::move(s5);
  VERIFY( bytes_allocated == 0 );
}

int
main()
{
  test01();
}
