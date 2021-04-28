// { dg-options "-Wdeprecated" }
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

pair<int*, ExplicitMoveOnly> v14{0, MoveOnly{}}; // { dg-warning "deprecated" }
pair<ExplicitMoveOnly, int*> v15{MoveOnly{}, 0}; // { dg-warning "deprecated" }

pair<int*, MoveOnly> v16 = {0, MoveOnly{}}; // { dg-warning "deprecated" }
pair<MoveOnly, int*> v17 = {MoveOnly{}, 0}; // { dg-warning "deprecated" }
