// 2003-02-26  Carlo Wood  <carlo@alinoe.com>

// Copyright (C) 2003-2017 Free Software Foundation, Inc.
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

// IA 64 C++ ABI - 5.1 External Names (a.k.a. Mangling)

#include <testsuite_hooks.h>

// libcwd tests
int main()
{
  using namespace __gnu_test;

/*
namespace libcw {
  namespace debug {
    class memblk_types_manipulator_data_ct { };
  }
  template<typename T>
    class omanip_id_tct { };
  namespace {
    template<typename T>
      class compiler_bug_workaround {
      public:
	static std::vector<int> ids;
      };
    template<typename T>
      std::vector<int> compiler_bug_workaround<T>::ids;
    typedef std::vector<libcw::omanip_id_tct<libcw::debug::memblk_types_manipulator_data_ct> > vector_t;
    compiler_bug_workaround<vector_t> dummy;
  }
}

void g(void)
{
  // Instantiation.
  libcw::dummy.ids.size();
}
*/
  verify_demangle("_ZGVN5libcw24_GLOBAL__N_cbll.cc0ZhUKa23compiler_bug_workaroundISt6vectorINS_13omanip_id_tctINS_5debug32memblk_types_manipulator_data_ctEEESaIS6_EEE3idsE", "guard variable for libcw::(anonymous namespace)::compiler_bug_workaround<std::vector<libcw::omanip_id_tct<libcw::debug::memblk_types_manipulator_data_ct>, std::allocator<libcw::omanip_id_tct<libcw::debug::memblk_types_manipulator_data_ct> > > >::ids");

  return 0;
}
