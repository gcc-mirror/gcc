// { dg-do compile { target c++11 } }

// Copyright (C) 2015-2018 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.


#include <tuple>
#include <utility>
#include <memory>

struct Explicit
{
  Explicit() = default;
  explicit Explicit(int) {}
};

struct ExplicitDefault
{
  explicit ExplicitDefault() {}
};

struct ExplicitDefaultDefault
{
  explicit ExplicitDefaultDefault() = default;
};

std::tuple<int> f1a() {return {1};}
std::tuple<int, int> f1b() {return {1,2};}
std::tuple<int, int, int> f1c() {return {1,2,3};}

std::tuple<Explicit> f2_a()
{return {1};} // { dg-error "explicit" }
std::tuple<Explicit, Explicit> f2_b()
{return {1,2};} // { dg-error "explicit" }
std::tuple<Explicit, Explicit, Explicit> f2_c()
{return {1,2,3};} // { dg-error "explicit" }

std::tuple<long> f3_a() {return std::tuple<int>{1};}
std::tuple<long, long> f3_b() {return std::tuple<int, int>{1,2};}
std::tuple<long, long, long> f3_c() {return std::tuple<int, int, int>{1,2,3};}

std::tuple<Explicit> f4_a()
{
  return std::tuple<int>{1};  // { dg-error "could not convert" }
}
std::tuple<Explicit, Explicit> f4_b()
{
  return std::tuple<int, int>{1,2};  // { dg-error "could not convert" }
}
std::tuple<Explicit, Explicit, Explicit> f4_c()
{
  return std::tuple<int, int,int>{1,2,3};  // { dg-error "could not convert" }
}

std::tuple<long> f5_a() {return {1};}
std::tuple<long, long> f5_b() {return {1,2};}
std::tuple<long, long, long> f5_c() {return {1,2,3};}

std::tuple<ExplicitDefault> f6_a()
{return {};} // { dg-error "explicit" }
std::tuple<ExplicitDefault, ExplicitDefault> f6_b()
{return {};} // { dg-error "explicit" }
std::tuple<ExplicitDefault, ExplicitDefault, ExplicitDefault> f6_c()
{return {};} // { dg-error "explicit" }
std::tuple<ExplicitDefault, int> f6_d()
{return {};} // { dg-error "explicit" }

std::tuple<ExplicitDefaultDefault> f7_a()
{return {};} // { dg-error "explicit" }
std::tuple<ExplicitDefaultDefault, ExplicitDefaultDefault> f7_b()
{return {};} // { dg-error "explicit" }
std::tuple<ExplicitDefaultDefault,
           ExplicitDefaultDefault,
           ExplicitDefaultDefault> f7_c()
{return {};} // { dg-error "explicit" }

std::tuple<int, int> fp1() {return std::pair<int, int>{1,2}; }
std::tuple<long, long> fp2() {return std::pair<int, int>{1,2}; }
std::tuple<Explicit, Explicit> fp3()
  {return std::pair<int, int>{1,2}; } // { dg-error "could not convert" }

std::tuple<int> v0_a{1};
std::tuple<int, int> v0_b{1,2};
std::tuple<int, int, int> v0_c{1,2,3};

std::tuple<Explicit> v1_a{1};
std::tuple<Explicit, Explicit> v1_b{1,2};
std::tuple<Explicit, Explicit, Explicit> v1_c{1,2,3};

std::tuple<Explicit> v2_a = {1}; // { dg-error "explicit" }
std::tuple<Explicit, Explicit> v2_b = {1,2}; // { dg-error "explicit" }
std::tuple<Explicit, Explicit, Explicit> v2_c = {1,2,3}; // { dg-error "explicit" }

std::tuple<Explicit> v3_a{std::tuple<int>{1}};
std::tuple<Explicit, Explicit> v3_b{std::tuple<int,int>{1,2}};
std::tuple<Explicit, Explicit, Explicit> v3_c{std::tuple<int,int,int>{1,2,3}};

std::tuple<Explicit, Explicit> v4_a =
  std::tuple<int>{1}; // { dg-error "conversion" }
std::tuple<Explicit, Explicit> v4_b =
  std::tuple<int,int>{1,2}; // { dg-error "conversion" }
std::tuple<Explicit, Explicit, Explicit> v4_c =
  std::tuple<int,int,int>{1,2,3}; // { dg-error "conversion" }

std::tuple<long> v6_a{1};
std::tuple<long, long> v6_b{1,2};
std::tuple<long, long, long> v6_c{1,2,3};

std::tuple<long> v7_a = {1};
std::tuple<long, long> v7_b = {1,2};
std::tuple<long, long, long> v7_c = {1,2,3};

std::tuple<long> v8_a{std::tuple<int>{1}};
std::tuple<long, long> v8_b{std::tuple<int,int>{1,2}};
std::tuple<long, long, long> v8_c{std::tuple<int,int,int>{1,2,3}};

std::tuple<long> v9_a = std::tuple<int>{1};
std::tuple<long, long> v9_b = std::tuple<int,int>{1,2};
std::tuple<long, long, long> v9_c = std::tuple<int,int,int>{1,2,3};

std::tuple<Explicit> v10_a{v0_a};
std::tuple<Explicit, Explicit> v10_b{v0_b};
std::tuple<Explicit, Explicit, Explicit> v10_c{v0_c};

std::tuple<Explicit> v11_a = v0_a; // { dg-error "conversion" }
std::tuple<Explicit, Explicit> v11_b = v0_b; // { dg-error "conversion" }
std::tuple<Explicit, Explicit, Explicit> v11_c
  = v0_c; // { dg-error "conversion" }

std::tuple<long> v12_a{v0_a};
std::tuple<long, long> v12_b{v0_b};
std::tuple<long, long, long> v12_c{v0_c};

std::tuple<long> v13_a = v0_a;
std::tuple<long, long> v13_b = v0_b;
std::tuple<long, long, long> v13_c = v0_c;

std::tuple<int, int> v14{std::pair<int, int>{1,2}};
std::tuple<long, long> v15{std::pair<int, int>{1,2}};
std::tuple<Explicit, Explicit> v16{std::pair<int, int>{1,2}};

std::tuple<int, int> v17 = std::pair<int, int>{1,2};
std::tuple<long, long> v18 = std::pair<int, int>{1,2};
std::tuple<Explicit, Explicit> v19
  = std::pair<int, int>{1,2}; // { dg-error "conversion" }

std::pair<int, int> v20;

std::tuple<int, int> v21{v20};
std::tuple<long, long> v22{v20};
std::tuple<Explicit, Explicit> v23{v20};

std::tuple<int, int> v24 = v20;
std::tuple<long, long> v25 = v20;
std::tuple<Explicit, Explicit> v26 = v20; // { dg-error "conversion" }

std::tuple<int> v27_a{std::allocator_arg, std::allocator<int>{}, 1};
std::tuple<int, int> v27_b{std::allocator_arg, std::allocator<int>{}, 1, 2};
std::tuple<int, int, int> v27_c{std::allocator_arg, std::allocator<int>{}, 1,2,3};

std::tuple<long> v28_a{std::allocator_arg, std::allocator<int>{}, 1};
std::tuple<long, long> v28_b{std::allocator_arg, std::allocator<int>{}, 1, 2};
std::tuple<long, long, long>
  v28_c{std::allocator_arg, std::allocator<int>{}, 1,2,3};

std::tuple<Explicit> v29_a{std::allocator_arg, std::allocator<int>{}, 1};
std::tuple<Explicit, Explicit>
  v29_b{std::allocator_arg, std::allocator<int>{}, 1, 2};
std::tuple<Explicit, Explicit, Explicit>
  v29_c{std::allocator_arg, std::allocator<int>{}, 1,2,3};

std::tuple<int> v30_a = {std::allocator_arg, std::allocator<int>{}, 1};
std::tuple<int, int> v30_b = {std::allocator_arg, std::allocator<int>{}, 1, 2};
std::tuple<int, int, int> v30_c
  = {std::allocator_arg, std::allocator<int>{}, 1,2,3};

std::tuple<long> v31_a = {std::allocator_arg, std::allocator<int>{}, 1};
std::tuple<long, long> v31_b = {std::allocator_arg, std::allocator<int>{}, 1, 2};
std::tuple<long, long, long>
  v31_c{std::allocator_arg, std::allocator<int>{}, 1,2,3};

std::tuple<Explicit> v32_a
  = {std::allocator_arg, std::allocator<int>{ }, 1}; // { dg-error "explicit" }
std::tuple<Explicit, Explicit> v32_b
  = {std::allocator_arg, std::allocator<int>{}, 1, 2}; // { dg-error "explicit" }
std::tuple<Explicit, Explicit, Explicit> v32_c
  = {std::allocator_arg, std::allocator<int>{}, 1,2,3}; // { dg-error "explicit" }

std::tuple<int, int> v33{std::allocator_arg, std::allocator<int>{},
  std::pair<int, int>{1, 2}};

std::tuple<long, long> v34{std::allocator_arg, std::allocator<int>{},
  std::pair<int, int>{1, 2}};

std::tuple<Explicit, Explicit>
  v35{std::allocator_arg, std::allocator<int>{}, std::pair<int, int>{1, 2}};

std::tuple<int, int> v36 = {std::allocator_arg, std::allocator<int>{},
  std::pair<int, int>{1, 2}};

std::tuple<long, long> v37 = {std::allocator_arg, std::allocator<int>{},
  std::pair<int, int>{1, 2}};

std::tuple<Explicit, Explicit> v38
= {std::allocator_arg, std::allocator<int>{}, std::pair<int, int>{1, 2}}; // { dg-error "explicit" }

std::tuple<int, int> v39{std::allocator_arg, std::allocator<int>{}, v20};

std::tuple<long, long> v40{std::allocator_arg, std::allocator<int>{}, v20};

std::tuple<Explicit, Explicit>
  v41{std::allocator_arg, std::allocator<int>{}, v20};

std::tuple<int, int> v42 = {std::allocator_arg, std::allocator<int>{}, v20};

std::tuple<long, long> v43 = {std::allocator_arg, std::allocator<int>{}, v20};

std::tuple<Explicit, Explicit> v44
= {std::allocator_arg, std::allocator<int>{ }, v20}; // { dg-error "explicit" }
std::tuple<ExplicitDefault> v45_a{};
std::tuple<ExplicitDefault, int> v45_b{};

std::tuple<ExplicitDefault> v46_a = {}; // { dg-error "explicit" }
std::tuple<ExplicitDefault, int> v46_b = {}; // { dg-error "explicit" }

std::tuple<ExplicitDefaultDefault> v47_a{};
std::tuple<ExplicitDefaultDefault, int> v47_b{};

std::tuple<ExplicitDefaultDefault> v48_a = {}; // { dg-error "explicit" }
std::tuple<ExplicitDefaultDefault, int> v48_b = { }; // { dg-error "explicit" }


struct DeletedCopy
{
  DeletedCopy(int);
  DeletedCopy(const DeletedCopy&) = delete;
};

std::tuple<DeletedCopy> v45{42};
std::tuple<DeletedCopy> v46{std::allocator_arg,
    std::allocator<DeletedCopy>{}, 42};

struct Sanity
{
  int v;
};

std::tuple<int, Sanity> v47(3, {42});
std::tuple<int, int, Sanity> v48(3, 4, {42});
std::tuple<int, Sanity> v49(std::allocator_arg,
                            std::allocator<Sanity>{},
                            3, {42});
std::tuple<int, int, Sanity> v50(std::allocator_arg,
                                 std::allocator<Sanity>{},
                                 3, 4, {42});

void f8_a(std::tuple<Explicit>) {}
void f8_b(std::tuple<Explicit, Explicit>) {}
void f8_c(std::tuple<Explicit, Explicit, Explicit>) {}

void f9_a(std::tuple<long>) {}
void f9_b(std::tuple<long, long>) {}
void f9_c(std::tuple<long, long, long>) {}

void f10_a(std::tuple<ExplicitDefault>) {}
void f10_b(std::tuple<ExplicitDefault, int>) {}

void f11_a(std::tuple<ExplicitDefaultDefault>) {}
void f11_b(std::tuple<ExplicitDefaultDefault, int>) {}

void test_arg_passing()
{
  f8_a(v0_a); // { dg-error "could not convert" }
  f8_b(v0_b); // { dg-error "could not convert" }
  f8_c(v0_c); // { dg-error "could not convert" }
  f8_b(v20); // { dg-error "could not convert" }

  f8_a(v1_a);
  f8_b(v1_b);
  f8_c(v1_c);

  f8_a({1}); // { dg-error "explicit" }
  f8_b({1,2}); // { dg-error "explicit" }
  f8_c({1,2,3}); // { dg-error "explicit" }

  f8_a(std::tuple<Explicit>{});
  f8_b(std::tuple<Explicit, Explicit>{});
  f8_c(std::tuple<Explicit, Explicit, Explicit>{});

  f8_a(std::tuple<int>{}); // { dg-error "could not convert" }
  f8_b(std::tuple<int, int>{}); // { dg-error "could not convert" }
  f8_c(std::tuple<int, int, int>{}); // { dg-error "could not convert" }
  f8_b(std::pair<int, int>{}); // { dg-error "could not convert" }

  f9_a(v0_a);
  f9_b(v0_b);
  f9_c(v0_c);
  f9_b(v20);

  f9_a(v6_a);
  f9_b(v6_b);
  f9_c(v6_c);

  f9_a({1});
  f9_b({1,2});
  f9_c({1,2,3});

  f9_a(std::tuple<int>{});
  f9_b(std::tuple<int, int>{});
  f9_c(std::tuple<int, int, int>{});
  f9_b(std::pair<int, int>{});

  f9_a(std::tuple<long>{});
  f9_b(std::tuple<long, long>{});
  f9_c(std::tuple<long, long, long>{});

  f10_a({}); // { dg-error "explicit" }
  f10_b({}); // { dg-error "explicit" }
  f11_a({}); // { dg-error "explicit" }
  f11_b({}); // { dg-error "explicit" }

  f10_a(std::tuple<ExplicitDefault>{});
  f10_b(std::tuple<ExplicitDefault, int>{});
  f11_a(std::tuple<ExplicitDefaultDefault>{});
  f11_b(std::tuple<ExplicitDefaultDefault, int>{});
}
