// { dg-do run { target c++11 } }

// 2011-04-27  Paolo Carlini  <paolo.carlini@oracle.com>
//
// Copyright (C) 2011-2020 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <utility>
#include <testsuite_hooks.h>

struct noexcept_move_copy
{
  noexcept_move_copy()
  : status(true)
  { };

  noexcept_move_copy(noexcept_move_copy&& r) noexcept
  { r.status = false; };

  noexcept_move_copy(const noexcept_move_copy&) = default;

  operator bool() { return status; }

private:
  bool status;
};

struct noexcept_move_no_copy
{
  noexcept_move_no_copy()
  : status(true)
  { };

  noexcept_move_no_copy(noexcept_move_no_copy&& r) noexcept
  { r.status = false; };

  noexcept_move_no_copy(const noexcept_move_no_copy&) = delete;

  operator bool() { return status; }

private:
  bool status;
};

struct except_move_copy
{
  except_move_copy()
  : status(true)
  { };

  except_move_copy(except_move_copy&& r) noexcept(false)
  { r.status = false; };

  except_move_copy(const except_move_copy&) = default;

  operator bool() { return status; }

private:
  bool status;
};

struct except_move_no_copy
{
  except_move_no_copy()
  : status(true)
  { };

  except_move_no_copy(except_move_no_copy&& r) noexcept(false)
  { r.status = false; };

  except_move_no_copy(const except_move_no_copy&) = delete;

  operator bool() { return status; }

private:
  bool status;
};

void
test01()
{
  noexcept_move_copy nemc1;
  auto nemc2 __attribute__((unused)) = std::move_if_noexcept(nemc1);
  VERIFY( nemc1 == false );

  noexcept_move_no_copy nemnc1;
  auto nemnc2 __attribute__((unused)) = std::move_if_noexcept(nemnc1);
  VERIFY( nemnc1 == false );

  except_move_copy emc1;
  auto emc2 __attribute__((unused)) = std::move_if_noexcept(emc1);
  VERIFY( emc1 == true );

  except_move_no_copy emnc1;
  auto emnc2 __attribute__((unused)) = std::move_if_noexcept(emnc1);
  VERIFY( emnc1 == false );
}

int main()
{
  test01();
  return 0;
}
