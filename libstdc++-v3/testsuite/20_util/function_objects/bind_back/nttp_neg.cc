//  { dg-do compile { target c++26 } }

#include <functional>

void f() {};
using fp = decltype(&f);
constexpr fp nfp = nullptr;

struct A { void mf() const {} };
using mfp = decltype(&A::mf);
constexpr mfp nnmfp = &A::mf;
constexpr mfp nmfp = nullptr;

struct B { B() = default; B(B const&) = delete; };
void bf(B const&) {};

struct C { C() = default; C(C&&) = delete; };
void cf(C&&) {};

int main()
{
  std::bind_back<f>()();
  // Verify bind_back<fn> with fn a null pointer fails.
  std::bind_back<nfp>()();  // { dg-error "here" }

  std::bind_back<nnmfp>(A{})();
  // Verify bind_back<mfn> with mfn a null member pointer fails.
  std::bind_back<nmfp>(A{})(); // { dg-error "here" }

  // Verify passing uncopyable type fails.
  std::bind_back<bf>(B{}); // { dg-error "here" }
			   //
  // Verify passing unmovable type fails.
  std::bind_back<cf>(C{}); // { dg-error "here" }
}

// { dg-error "static assertion failed" "" { target *-*-* } 0 }
// { dg-error "use of deleted function" "" { target *-*-* } 0 }
