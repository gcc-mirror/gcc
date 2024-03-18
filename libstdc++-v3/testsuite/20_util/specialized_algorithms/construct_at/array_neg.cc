// { dg-do compile { target c++20 } }

// LWG 3436. std::construct_at should support arrays

#include <memory>

void
test_array_args()
{
  int arr[2];
  std::construct_at(&arr, 1, 2); // { dg-error "here" }
  // { dg-error "must not use any arguments" "" { target *-*-* } 0 }
}

void
test_unbounded_array(int (*p)[])
{
  std::construct_at(p); // { dg-error "no matching function" }
}
