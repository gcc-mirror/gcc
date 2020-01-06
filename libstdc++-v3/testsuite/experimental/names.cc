// Copyright (C) 2017-2020 Free Software Foundation, Inc.
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

// Define macros for some common variables names that we must not use for
// naming variables, parameters etc. in the library.

#include "../17_intro/names.cc"
// Filesystem
#if __has_include(<experimental/filesystem>)
# include <experimental/filesystem>
#endif

#if __cplusplus >= 201402L

// Library Fundamentals
#include <experimental/algorithm>
#include <experimental/any>
#include <experimental/array>
#include <experimental/chrono>
#include <experimental/deque>
#include <experimental/forward_list>
#include <experimental/functional>
#include <experimental/iterator>
#include <experimental/list>
#include <experimental/map>
#include <experimental/memory>
#include <experimental/memory_resource>
#include <experimental/numeric>
#include <experimental/optional>
#include <experimental/propagate_const>
#include <experimental/random>
#include <experimental/ratio>
#include <experimental/regex>
#include <experimental/set>
#include <experimental/source_location>
#include <experimental/string>
#include <experimental/string_view>
#include <experimental/system_error>
#include <experimental/tuple>
#include <experimental/type_traits>
#include <experimental/unordered_map>
#include <experimental/unordered_set>
#include <experimental/utility>
#include <experimental/vector>
// Networking
#ifdef _GLIBCXX_HAS_GTHREADS
# include <experimental/buffer>
# include <experimental/internet>
# include <experimental/io_context>
# include <experimental/net>
# include <experimental/netfwd>
# include <experimental/socket>
# include <experimental/timer>
# include <experimental/executor>
#endif

#endif // C++14
