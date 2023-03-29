// Copyright (C) 2007-2023 Free Software Foundation, Inc.
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

// 23.2.5 class vector<bool> [lib.vector.bool]

// { dg-do run { xfail *-*-darwin8.[0-4].* } }
// { dg-skip-if "" { powerpc64-*-freebsd* } }

#include <vector>
#include <stdexcept>
#include <testsuite_hooks.h>

using std::_S_word_bit;

inline void
check_cap_ge_size(const std::vector<bool>& x)
{
  if (x.capacity() < x.size())
    throw std::logic_error("");
}

inline void
check_cap_eq_maxsize(const std::vector<bool>& x)
{
  if (x.capacity() != x.max_size())
    throw std::logic_error("");
}

// libstdc++/31370
void test01()
{
  int myexit = 0;

  try
    {
      std::vector<bool> x;
      x.reserve(x.max_size());
      check_cap_eq_maxsize(x);
    }
  catch(std::bad_alloc&)
    { }
  catch(std::exception&)
    { ++myexit; }

  // When doubling is too big, but smaller is sufficient, the resize
  // should do smaller and be happy.  It certainly shouldn't throw
  // other exceptions or crash.
  try
    {
      std::vector<bool> x;
      x.resize(x.max_size() / 2 + 1, false);
      for(int i = 0; i < _S_word_bit; ++i)
	x.push_back(false);
      check_cap_ge_size(x);
    }
  catch(std::bad_alloc&)
    { }
  catch(std::exception&)
    { ++myexit; }

  try
    {
      std::vector<bool> x;
      x.resize(x.max_size() / 2 + 1, false);
      x.insert(x.end(), _S_word_bit, false);
      check_cap_ge_size(x);
    }
  catch(std::bad_alloc&)
    { }
  catch(std::exception&)
    { ++myexit; }

  try
    {
      std::vector<bool> x;
      x.resize(x.max_size() / 2 + 1, false);
      std::vector<bool> y(_S_word_bit, false);
      x.insert(x.end(), y.begin(), y.end());
      check_cap_ge_size(x);
    }
  catch(std::bad_alloc&)
    { }
  catch(std::exception&)
    { ++myexit; }

  // These tests are currently only relevant to bool: don't get burned
  // by the attempt to round up when near the max size.
  try
    {
      std::vector<bool> x;
      x.resize(x.max_size() - _S_word_bit, false);
      for(int i = 0; i < _S_word_bit; ++i)
	x.push_back(false);
      check_cap_ge_size(x);
    }
  catch(std::bad_alloc&)
    { }
  catch(std::exception&)
    { ++myexit; }

  try
    {
      std::vector<bool> x;
      x.resize(x.max_size() - _S_word_bit, false);
      x.insert(x.end(), _S_word_bit, false);
      check_cap_ge_size(x);
    }
  catch(std::bad_alloc&)
    { }
  catch(std::exception&)
    { ++myexit; }

  try
    {
      std::vector<bool> x;
      x.resize(x.max_size() - _S_word_bit, false);
      std::vector<bool> y(_S_word_bit, false);
      x.insert(x.end(), y.begin(), y.end());
      check_cap_ge_size(x);
    }
  catch(std::bad_alloc&)
    { }
  catch(std::exception&)
    { ++myexit; }

  // Attempts to put in more than max_size() items should result in a
  // length error.
  try
    {
      std::vector<bool> x;
      x.resize(x.max_size() - _S_word_bit, false);
      for(int i = 0; i < _S_word_bit + 1; ++i)
	x.push_back(false);
      ++myexit;
    }
  catch(std::bad_alloc&)
    { }
  catch(std::length_error&)
    { }
  catch(std::exception&)
    { ++myexit; }

  try
    {
      std::vector<bool> x;
      x.resize(x.max_size() - _S_word_bit, false);
      x.insert(x.end(), _S_word_bit + 1, false);
      ++myexit;
    }
  catch(std::bad_alloc&)
    { }
  catch(std::length_error&)
    { }
  catch(std::exception&)
    { ++myexit; }

  try
    {
      std::vector<bool> x;
      x.resize(x.max_size() - _S_word_bit, false);
      std::vector<bool> y(_S_word_bit + 1, false);
      x.insert(x.end(), y.begin(), y.end());
      ++myexit;
    }
  catch(std::bad_alloc&)
    { }
  catch(std::length_error&)
    { }
  catch(std::exception&)
    { ++myexit; }

  VERIFY( !myexit );
}

int main()
{
  test01();
  return 0;
}
