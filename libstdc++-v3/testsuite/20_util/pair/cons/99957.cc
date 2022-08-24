// { dg-options "-Wdeprecated" }
// { dg-add-options using-deprecated }
// { dg-do compile { target { c++11 && { ! c++20 } } } }

#include <utility>

using std::pair;

struct MoveOnly
{
  MoveOnly() = default;
  MoveOnly(MoveOnly&&) {}
};

struct ExplicitMoveOnly
{
  ExplicitMoveOnly() = default;
  ExplicitMoveOnly(ExplicitMoveOnly&&) {}
  explicit ExplicitMoveOnly(MoveOnly&&) {}
};

// PR libstdc++/99957
// check non-standard constructors are deprecated

pair<int*, ExplicitMoveOnly> v14(0, MoveOnly{}); // { dg-warning "deprecated" }
pair<ExplicitMoveOnly, int*> v15(MoveOnly{}, 0); // { dg-warning "deprecated" }

pair<int*, MoveOnly> v16 = {0, MoveOnly{}}; // { dg-warning "deprecated" }
pair<MoveOnly, int*> v17 = {MoveOnly{}, 0}; // { dg-warning "deprecated" }

// PR libstdc++/101124
// check deprecated constructors don't cause unwanted ambiguities

std::pair<long*, int> p(0, 0); // { dg-bogus "ambiguous" }

struct X { } x;
std::pair<const X, void*> p2(x, 0); // { dg-bogus "ambiguous" }
