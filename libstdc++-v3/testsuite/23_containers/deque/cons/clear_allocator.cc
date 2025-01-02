// Copyright (C) 2004-2025 Free Software Foundation, Inc.
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

#include <deque>
#include <ext/new_allocator.h>

using namespace std;
using __gnu_cxx::new_allocator;

template<typename T>
  class clear_alloc : public new_allocator<T>
  {
  public:

    template <typename T1>
      struct rebind
      { typedef clear_alloc<T1> other; };

    virtual void clear() throw()
    { }

    clear_alloc() throw()
    { }

    clear_alloc(clear_alloc const&) throw() : new_allocator<T>()
    { }

    template<typename T1>
    clear_alloc(clear_alloc<T1> const&) throw()
      { }

    virtual ~clear_alloc() throw()
    { this->clear(); }

    T* allocate(typename new_allocator<T>::size_type n, const void *hint = 0)
    {
      this->clear();
      return new_allocator<T>::allocate(n, hint);
    }

    void deallocate(T *ptr, typename new_allocator<T>::size_type n)
    {
      this->clear();
      new_allocator<T>::deallocate(ptr, n);
    }
  };

template<typename Container>
  void Check_Container()
  {
    Container* pic = new Container;
    int x = 230;

    while (x--)
      {
	pic->push_back(x);
      }

    (void) pic->get_allocator();

    // The following has led to infinite recursions or cores.
    pic->clear();

    delete pic;
  }


int main()
{
  Check_Container<std::deque<int, clear_alloc<int> > >();
  return 0;
}

