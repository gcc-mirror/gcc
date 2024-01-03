// Copyright (C) 2019-2024 Free Software Foundation, Inc.
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

// { dg-do run { target c++20 } }
// { dg-add-options no_pch }

#include <functional>

// P1651R0 bind_front should not unwrap reference_wrapper

#ifndef __cpp_lib_bind_front
# error "Feature test macro for bind_front is missing in <functional>"
#elif __cpp_lib_bind_front < 201907L
# error "Feature test macro for bind_front has wrong value in <functional>"
#endif

#include <memory>
#include <string>
#include <testsuite_hooks.h>

void functionAcceptingStringView(std::string_view) { }

void
test01()
{
  std::string s;
  auto fs = std::bind_front(&functionAcceptingStringView, std::string_view(s));
  fs();
}

template <typename F>
struct PartialApply {
    PartialApply(F f) : f(f) {}
    F f;

    template <typename... A> decltype(auto) operator()(A const&... a) const {
        if constexpr (std::is_invocable<F const&, A const&...>::value) {
            return f(a...);
        } else {
            return bind_front(*this, a...);
        }
    }
};

void
test02()
{
  struct Thingy { };
  std::unique_ptr<Thingy> thingy;
  auto func = [](const std::unique_ptr<Thingy>&, int) {};
  PartialApply{func}(std::ref(thingy))(10);
}

void
test03()
{
  std::string str;
  auto func = [](const std::string& s, int) -> const std::string& { return s; };

  // sref refers to copy of str stored in bind_front result:
  const std::string& sref = PartialApply{func}(std::ref(str))(10);

  // pre-P1651R0 this is a use of a dangling reference:
  const char& c = sref[0];

  // post-P1651R0 the bind_front result stores a reference_wrapper by value,
  // and so sref is bound to str instead of dangling:
  VERIFY( &c == str.data() );
  VERIFY( &sref == &str );
}

int
main()
{
  test01();
  test02();
  test03();
}
