// Copyright (C) 2016 Free Software Foundation, Inc.
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

// { dg-options "-lstdc++fs" }
// { dg-do run { target c++11 } }
// { dg-require-filesystem-ts "" }

// 15.25 Permissions [fs.op.last_write_time]

#include <experimental/filesystem>
#include <testsuite_fs.h>
#include <testsuite_hooks.h>

#ifdef _GLIBCXX_HAVE_FCNTL_H
# include <fcntl.h>
#endif
#if _GLIBCXX_HAVE_UTIME_H
# include <utime.h>
#endif

void
test01()
{
  using time_type = std::experimental::filesystem::file_time_type;

  auto p = __gnu_test::nonexistent_path();
  std::error_code ec;
  time_type mtime = last_write_time(p, ec);
  VERIFY( ec );
  VERIFY( ec == std::make_error_code(std::errc::no_such_file_or_directory) );
#if __cpp_exceptions
  bool caught = false;
  try {
    mtime = last_write_time(p);
  } catch (std::system_error const& e) {
    caught = true;
    ec = e.code();
  }
  VERIFY( caught );
  VERIFY( ec );
  VERIFY( ec == std::make_error_code(std::errc::no_such_file_or_directory) );
#endif

  __gnu_test::scoped_file file(p);
  VERIFY( exists(p) );
  mtime = last_write_time(p, ec);
  VERIFY( !ec );
  VERIFY( mtime <= time_type::clock::now() );
  VERIFY( mtime == last_write_time(p) );

  auto end_of_time = time_type::duration::max();
  auto last_second
    = std::chrono::duration_cast<std::chrono::seconds>(end_of_time).count();
  if (last_second > std::numeric_limits<std::time_t>::max())
    return; // can't test overflow

#if _GLIBCXX_USE_UTIMENSAT
  struct ::timespec ts[2];
  ts[0].tv_sec = 0;
  ts[0].tv_nsec = UTIME_NOW;
  ts[1].tv_sec = std::numeric_limits<std::time_t>::max() - 1;
  ts[1].tv_nsec = 0;
  VERIFY( !::utimensat(AT_FDCWD, p.c_str(), ts, 0) );
#elif _GLIBCXX_HAVE_UTIME_H
  ::utimbuf times;
  times.modtime = std::numeric_limits<std::time_t>::max() - 1;
  times.actime = std::numeric_limits<std::time_t>::max() - 1;
  VERIFY( !::utime(p.c_str(), &times) );
#else
  return;
#endif

  mtime = last_write_time(p, ec);
  VERIFY( ec );
  VERIFY( ec == std::make_error_code(std::errc::value_too_large) );
  VERIFY( mtime == time_type::min() );

#if __cpp_exceptions
  caught = false;
  try {
    mtime = last_write_time(p);
  } catch (std::system_error const& e) {
    caught = true;
    ec = e.code();
  }
  VERIFY( caught );
  VERIFY( ec );
  VERIFY( ec == std::make_error_code(std::errc::value_too_large) );
#endif
}

int
main()
{
  test01();
}
