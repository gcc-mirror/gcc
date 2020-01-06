// Copyright (C) 2016-2020 Free Software Foundation, Inc.
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

// { dg-do compile { target c++11 } }

#include <vector>

struct Tag { };

template<typename T>
  struct TaggingAllocator
  {
    using value_type = T;

    TaggingAllocator() = default;

    template<typename U>
      TaggingAllocator(const TaggingAllocator<U>&) { }

    T*
    allocate(std::size_t n) { return std::allocator<T>{}.allocate(n); }

    void
    deallocate(T* p, std::size_t n) { std::allocator<T>{}.deallocate(p, n); }

    template<typename U, typename... Args>
      void
      construct(U* p, Args&&... args)
      { ::new((void*)p) U(Tag{}, std::forward<Args>(args)...); }

    template<typename U, typename... Args>
      void
      destroy(U* p)
      { p->~U(); }
  };

template<typename T, typename U>
  bool
  operator==(const TaggingAllocator<T>&, const TaggingAllocator<U>&)
  { return true; }

template<typename T, typename U>
  bool
  operator!=(const TaggingAllocator<T>&, const TaggingAllocator<U>&)
  { return false; }

struct X
{
  // All constructors must be passed the Tag type.

  // DefaultInsertable into vector<X, TaggingAllocator<X>>,
  X(Tag) { }
  // CopyInsertable into vector<X, TaggingAllocator<X>>,
  X(Tag, const X&) { }
  // MoveInsertable into vector<X, TaggingAllocator<X>>, and
  X(Tag, X&&) { }

  // EmplaceConstructible into vector<X, TaggingAllocator<X>> from args.
  template<typename... Args>
    X(Tag, Args&&...) { }

  // not DefaultConstructible, CopyConstructible or MoveConstructible.
  X() = delete;
  X(const X&) = delete;
  X(X&&) = delete;

  // CopyAssignable.
  X& operator=(const X&) { return *this; }

  // MoveAssignable.
  X& operator=(X&&) { return *this; }

private:
  // Not Destructible.
  ~X() { }

  // Erasable from vector<X, TaggingAllocator<X>>.
  friend class TaggingAllocator<X>;
};

template class std::vector<X, TaggingAllocator<X>>;

void test01()
{
  std::vector<X, TaggingAllocator<X>> v;
  v.reserve(3);
  v.emplace_back();
  v.emplace(v.begin());
  v.emplace(v.begin(), 1, 2, 3);
}
