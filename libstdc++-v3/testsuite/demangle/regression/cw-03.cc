// 2003-02-26  Carlo Wood  <carlo@alinoe.com>

// Copyright (C) 2003 Free Software Foundation, Inc.
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
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// IA 64 C++ ABI - 5.1 External Names (a.k.a. Mangling)

#include <testsuite_hooks.h>

// libcwd tests
int main()
{
  using namespace __gnu_test;

/*
namespace libcw {
  namespace _private_ {
    class GlobalObject { public: void dummy(std::ostream&) const; };
  }
  namespace debug {
    template<typename T>
      class cwprint_using_tct { };
    template<typename T>
      cwprint_using_tct<T> cwprint_using(T const&, void (T::*)(std::ostream&) const);
  }
}

void h(void)
{
  // Instantiation.
  libcw::_private_::GlobalObject dummy;
  (void)libcw::debug::cwprint_using(dummy, &libcw::_private_::GlobalObject::dummy);
}
*/
  // cplus-dem CORE
  verify_demangle("_ZN5libcw5debug13cwprint_usingINS_9_private_12GlobalObjectEEENS0_17cwprint_using_tctIT_EERKS5_MS5_KFvRSt7ostreamE", "libcw::debug::cwprint_using_tct<libcw::_private_::GlobalObject> libcw::debug::cwprint_using<libcw::_private_::GlobalObject>(libcw::_private_::GlobalObject const&, void (libcw::_private_::GlobalObject::*)(std::ostream&) const)");

  return 0;
}
