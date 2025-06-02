// { dg-do compile { target c++26 } }

#include <functional>

using std::nontype;
using std::function_ref;

struct S
{
  int x;
  void foo();
};
S s;

constexpr int(*fp)(S) = nullptr;
constexpr int S::*mdp = nullptr;
constexpr int (S::*mfp)() = nullptr;

function_ref<int(S)> fd1(nontype<fp>);  // { dg-error "from here" }
function_ref<int(S)> fd2(nontype<mdp>); // { dg-error "from here" }
function_ref<int(S)> fd3(nontype<mfp>); // { dg-error "from here" }

function_ref<int()> br4(nontype<fp>, s);  // { dg-error "from here" }
function_ref<int()> br5(nontype<mdp>, s); // { dg-error "from here" }
function_ref<int()> br6(nontype<mfp>, s); // { dg-error "from here" }

function_ref<int()> bp7(nontype<mdp>, &s); // { dg-error "from here" }
function_ref<int()> bp8(nontype<mfp>, &s); // { dg-error "from here" }

// { dg-prune-output "static assertion failed" }
