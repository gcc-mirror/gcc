// Copyright (C) 2018-2021 Free Software Foundation, Inc.
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

#include <unordered_map>
#include <testsuite_hooks.h>

void
test01()
{
  struct X {
    mutable int conversions = 0;

    operator std::pair<const int, int>() const {
      if (++conversions > 1)
	throw 1;
      return {};
    }
  };

  std::unordered_map<int, int> m;
  m.insert(X());
  VERIFY( m.size() == 1 );
  m.insert(m.begin(), X());
  VERIFY( m.size() == 1 );

}
void
test02()
{
  struct Y {
    int conversions = 0;

    operator std::pair<const int, int>() && {
      if (++conversions > 1)
	throw 1;
      return {};
    }
  };

  std::unordered_map<int, int> m;
  m.insert(Y());
  VERIFY( m.size() == 1 );
  m.insert(m.begin(), Y());
  VERIFY( m.size() == 1 );
}

struct Key {
  int key;
  bool operator==(const Key& r) const { return key == r.key; }
};

namespace std {
  template<> struct hash<Key> {
    size_t operator()(const Key& k) const { return std::hash<int>()(k.key); }
  };
}

struct Z {
  operator std::pair<const Key, int>() const { return { { z }, 0 }; }
  int z;
};

template<typename T>
struct Alloc
{
  Alloc() = default;

  template<typename U>
    Alloc(const Alloc<U>&) { }

  using value_type = T;

  T* allocate(std::size_t n) { return std::allocator<T>().allocate(n); }

  void deallocate(T* p, std::size_t n) { std::allocator<T>().deallocate(p, n); }

  template<typename U>
    void construct(U* p, const Z& z) { ::new (p) U{ { z.z+1 }, 0}; }

  template<typename U>
    bool operator==(const Alloc<U>&) { return true; }

  template<typename U>
    bool operator!=(const Alloc<U>&) { return false; }
};

void
test03()
{
  std::unordered_map<Key, int, std::hash<Key>, std::equal_to<Key>,
		     Alloc<std::pair<const Key, int>>> m;
  m.insert(Z{});
  m.insert(Z{});
  VERIFY( m.size() == 1 );
  m.insert(Z{});
  m.insert(Z{1});
  VERIFY( m.size() == 2 );
}

int
main()
{
  test01();
  test02();
  test03();
}
