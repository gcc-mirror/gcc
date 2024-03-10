// { dg-options "-fconcepts-ts" }
// { dg-do compile { target c++20 } }

// PR testsuite/101782
// attribute-specifier-seq cannot follow requires-clause with -fconcepts-ts

#include <algorithm>
#include <iterator>
#include <ranges>
