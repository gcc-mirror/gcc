// Copyright (C) 2020-2025 Free Software Foundation, Inc.
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
// { dg-require-effective-target cxx11_abi }
// { dg-require-gthreads "" }
// { dg-add-options libatomic }
// { dg-additional-options "-pthread" { target pthread } }

#include <algorithm>
#include <atomic>
#include <chrono>
#include <sstream>
#include <string>
#include <string_view>
#include <syncstream>
#include <thread>
#include <vector>
#include <unordered_map>
#include <utility>

#include <testsuite_hooks.h>

int
main()
{
  using namespace std::chrono_literals;

  std::stringbuf b;
  std::atomic<unsigned> running(0);

  auto const cstr = "This is a test";

  constexpr int ct = 1000;
  auto const body = [&]{
    ++running;
    auto tid = std::this_thread::get_id();
    std::syncbuf s(&b);
    for (auto i = 0; i < ct; ++i)
    {
      std::stringstream stm;
      stm << tid << ' ' << cstr << ' ' << i << std::endl;
      auto sv = stm.view();
      s.sputn(sv.data(), sv.size());
      VERIFY( s.emit() );
    }
  };

  const auto tct = 8;
  std::vector<std::thread> ts;
  ts.reserve(tct);

  for (auto i = 0; i < tct; ++i)
    ts.emplace_back(std::thread(body));

  do
  {
    std::this_thread::sleep_for(100ms);
  }
  while (running.load() < tct);

  std::unordered_map<std::string, int> tids;
  for (auto&& t : ts)
  {
    std::stringstream stm;
    stm << t.get_id();
    tids.emplace(std::make_pair(stm.str(), 0));
  };

  for (auto&& t : ts)
     t.join();

  std::vector<std::string_view> lines;
  const auto lct = ct * ts.size();
  lines.reserve(lct);

  std::size_t last = 0;
  auto sv = b.view();
  auto p = sv.find('\n');
  while (p != std::string_view::npos)
  {
    lines.emplace_back(sv.substr(last, p - last));
    last = p+1;
    p = sv.find('\n', last);
  }
  VERIFY( lines.size() == lct );

  auto sep = "";
  auto i = 0;
  sv = std::string_view(cstr);

  for (auto&& l : lines)
  {
    auto p = l.find(' ');
    VERIFY( p != std::string_view::npos );
    std::string tid(l.substr(0, p));
    ++p;

    VERIFY( l.substr(p, sv.size()) == sv );
    std::string s(l.substr(++p + sv.size()));
    std::stringstream stm(s);
    int n;
    stm >> n;
    VERIFY( stm.eof() );
    VERIFY( n >= 0 && n < ct );
    auto it = tids.find(tid);
    VERIFY( it != std::end(tids) );
    ++(it->second);
  }

  for (auto const& t : tids)
  {
    VERIFY( t.second == ct );
  }
  return 0;
}
