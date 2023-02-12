// { dg-do compile { target c++11 } }

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

// NB: parallel-mode uses TR1 bits...
// { dg-skip-if "" { *-*-* } { "-D_GLIBCXX_PARALLEL" } }

#include <algorithm>
#include <array>
#include <bitset>
#include <exception>
#include <functional>
#include <iterator>
#include <limits>
#include <memory>
#include <new>
#include <numeric>
#include <tuple>
#include <typeinfo>
#include <type_traits>
#include <utility>

#if __STDC_HOSTED__
#  include <complex>
#  include <deque>
#  include <fstream>
#  include <iomanip>
#  include <ios>
#  include <iosfwd>
#  include <iostream>
#  include <istream>
#  include <list>
#  include <locale>
#  include <map>
#  include <ostream>
#  include <queue>
#  include <random>
#  include <regex>
#  include <set>
#  include <sstream>
#  include <stack>
#  include <stdexcept>
#  include <streambuf>
#  include <string>
#  include <unordered_map>
#  include <unordered_set>
#  include <valarray>
#  include <vector>
#endif

namespace gnu
{
  using namespace std::tr1;  // { dg-error "is not a namespace-name" }
}
