// Copyright (C) 2004 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.
 
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
 
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

#include <vector>
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
    
    clear_alloc(clear_alloc const& _wa) throw()
    { }
    
    template<typename T1>
      clear_alloc(clear_alloc<T1> const& _wa) throw()
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
    
    pic->get_allocator();
    
    // The following has led to infinite recursions or cores.
    pic->clear();

    delete pic;
  }


int main()
{
  Check_Container<std::vector<bool, clear_alloc<bool> > >();
  return 0;
}

