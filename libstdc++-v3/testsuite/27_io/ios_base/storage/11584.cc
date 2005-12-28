// 2004-01-25 jlquinn@gcc.gnu.org

// Copyright (C) 2004, 2005 Free Software Foundation
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

// 27.4.2.5 ios_base storage functions

#include <cstdlib>
#include <new>
#include <iostream>
#include <testsuite_hooks.h>

int new_fails;

void* operator new(std::size_t n) throw (std::bad_alloc)
{
  if (new_fails)
    throw std::bad_alloc();  
  return malloc(n);
}
void* operator new[] (std::size_t n) throw (std::bad_alloc)
{ return operator new(n); }

void operator delete (void *p) throw() { free(p); }
void operator delete[] (void *p) throw() { operator delete(p); }

int main ()
{
  bool test __attribute__((unused)) = true;
  const int i = std::ios::xalloc();
  VERIFY( i >= 0 );

  new_fails = 1;
  
  // Successive accesses to failure storage clears to zero.
  std::cout.iword(100) = 69;
  VERIFY( std::cout.iword(100) == 0 );
  
  // Access to pword failure storage shouldn't clear iword pword storage.
  long& lr = std::cout.iword(100);
  lr = 69;
  
  void* pv = std::cout.pword(100);
  VERIFY( pv == 0 );
  VERIFY( lr == 69 );
  
  return 0;
}

