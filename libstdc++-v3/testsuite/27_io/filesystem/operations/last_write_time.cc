// Copyright (C) 2016-2024 Free Software Foundation, Inc.
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
// { dg-require-target-fs-lwt "" }

// 15.25 Permissions [fs.op.last_write_time]

#include <filesystem>
#include <limits>
#include <testsuite_fs.h>
#include <testsuite_hooks.h>

#ifdef _GLIBCXX_HAVE_FCNTL_H
# include <fcntl.h>
#endif
#if _GLIBCXX_HAVE_UTIME_H
# include <utime.h>
#endif
#include <stdio.h>

using time_type = std::filesystem::file_time_type;

namespace chrono = std::chrono;

void
test01()
{
  // read times

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
    = chrono::duration_cast<chrono::seconds>(end_of_time).count();
  if (last_second > std::numeric_limits<std::time_t>::max())
  {
    puts("Range of time_t is smaller than range of chrono::file_clock, "
	 "can't test for overflow on this target.");
    return;
  }

  // Set mtime to a date past the maximum possible file_time_type:
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
  VERIFY( !::utime(p.string().c_str(), &times) );
#else
  puts("No utimensat or utime, giving up.");
  return;
#endif

  // Try to read back the impossibly-large mtime:
  mtime = last_write_time(p, ec);
  // Some filesystems (e.g. XFS) silently truncate distant times to
  // the time_t epochalypse, Jan 19 2038, so we won't get an error when
  // reading it back:
  if (ec)
  {
    VERIFY( ec == std::make_error_code(std::errc::value_too_large) );
    VERIFY( mtime == time_type::min() );
  }
  else
    puts("No overflow error, filesystem may not support 64-bit time_t.");

#if __cpp_exceptions
  // Once more, with exceptions:
  try {
    auto mtime2 = last_write_time(p);
    // If it didn't throw, expect to have read back the same value:
    VERIFY( mtime2 == mtime );
  } catch (std::filesystem::filesystem_error const& e) {
    // If it did throw, expect the error_code to be the same:
    VERIFY( e.code() == ec );
    VERIFY( e.path1() == p );
  }
#endif
}

bool approx_equal(time_type file_time, time_type expected)
{
  auto delta = expected - file_time;
  if (delta < delta.zero())
    delta = -delta;
  return delta < chrono::seconds(1);
}

void
test02()
{
  // write times

  const std::error_code bad_ec = make_error_code(std::errc::invalid_argument);
  __gnu_test::scoped_file f;
  std::error_code ec;
  time_type time;

  ec = bad_ec;
  time = last_write_time(f.path);
  last_write_time(f.path, time, ec);
  VERIFY( !ec );
  VERIFY( approx_equal(last_write_time(f.path), time) );

  ec = bad_ec;
  time -= chrono::milliseconds(1000 * 60 * 10 + 15);
  last_write_time(f.path, time, ec);
  VERIFY( !ec );
  VERIFY( approx_equal(last_write_time(f.path), time) );

  ec = bad_ec;
  time += chrono::milliseconds(1000 * 60 * 20 + 15);
  last_write_time(f.path, time, ec);
  VERIFY( !ec );
  VERIFY( approx_equal(last_write_time(f.path), time) );

  if (std::numeric_limits<std::time_t>::max()
      < std::numeric_limits<std::int64_t>::max())
    return; // file clock's epoch is out of range for 32-bit time_t

  using sys_time_32b
    = chrono::time_point<chrono::system_clock, chrono::duration<std::int32_t>>;
  auto duration_until_2038 = sys_time_32b::max() - sys_time_32b::clock::now();
  auto file_time_2038 = time_type::clock::now() + duration_until_2038;

  ec = bad_ec;
  time = file_time_2038 - chrono::seconds(1);
  // Assume all filesystems can store times that fit in 32-bit time_t
  // (i.e. up to Jan 19 2038)
  last_write_time(f.path, time, ec);
  VERIFY( !ec );
  VERIFY( approx_equal(last_write_time(f.path), time) );

  // Check whether the filesystem supports times larger than 32-bit time_t:
  time += chrono::seconds(60);
  last_write_time(f.path, time, ec);
  if (ec || !approx_equal(last_write_time(f.path), time))
  {
    puts("Filesystem seems to truncate times past Jan 19 2038, giving up.");
    return; // Tests below will fail on this filesystem
  }

  ec = bad_ec;
  // The file clock's epoch:
  time = time_type();
  last_write_time(f.path, time, ec);
  VERIFY( !ec );
  VERIFY( approx_equal(last_write_time(f.path), time) );

  ec = bad_ec;
  // A time after the epoch
  time += chrono::milliseconds(1000 * 60 * 10 + 15);
  last_write_time(f.path, time, ec);
  VERIFY( !ec );
  VERIFY( approx_equal(last_write_time(f.path), time) );

  ec = bad_ec;
  // A time before than the epoch
  time -= chrono::milliseconds(1000 * 60 * 20 + 15);
  last_write_time(f.path, time, ec);
  VERIFY( !ec );
  VERIFY( approx_equal(last_write_time(f.path), time) );
}

int
main()
{
  test01();
  test02();
}
