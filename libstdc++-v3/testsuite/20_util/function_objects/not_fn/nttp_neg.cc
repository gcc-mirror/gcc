//  { dg-do compile { target c++26 } }

#include <functional>

bool f() { return {}; }
using fp = decltype(&f);
constexpr fp nfp = nullptr;

struct A { bool mf() const { return {}; } };
using mfp = decltype(&A::mf);
constexpr mfp nnmfp = &A::mf;
constexpr mfp nmfp = nullptr;
constexpr A a;

int main()
{
  (void) std::not_fn<f>()();

  // Verify not_fn<fn> with fn a null pointer fails.
  (void) std::not_fn<nfp>()();  // { dg-error "here" }
				//
  (void) std::not_fn<nnmfp>()(a);

  // Verify not_fn<mfn> with mfn a null member pointer fails.
  return std::not_fn<nmfp>()(a);  // { dg-error "here" }
}

// { dg-error "static assertion failed" "" { target *-*-* } 0 }
