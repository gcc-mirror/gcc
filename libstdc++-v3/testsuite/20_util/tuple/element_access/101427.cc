// { dg-do compile { target c++14 } }
// PR libstdc++/101427

#include <tuple>

void test_pr101427()
{
  std::tuple<int, int> tup1;
  std::get<int>(tup1); // { dg-error "here" }

  const std::tuple<int, long, int, long> tup2;
  std::get<long>(tup2); // { dg-error "here" }

  std::tuple<char, short, float, short, int> tup3;
  std::get<short>(std::move(tup3)); // { dg-error "here" }

  const std::tuple<double, long, double, long> tup4;
  std::get<double>(std::move(tup4)); // { dg-error "here" }

  // { dg-error "must occur exactly once in the tuple" "" { target *-*-* } 0 }
}

// { dg-prune-output "use of deleted function" }
