// { dg-do compile { target c++20 } }

#include <atomic>

template<size_t N>
struct NonTrivial
{
  NonTrivial() = default;
  NonTrivial(NonTrivial const&) { };
};

template<size_t N>
NonTrivial<N> ntv;

std::atomic_ref<NonTrivial<0>> nt(ntv<0>); // { dg-error "here" }
std::atomic_ref<const NonTrivial<1>> cnt(ntv<1>); // { dg-error "here" }
std::atomic_ref<volatile NonTrivial<2>> vnt(ntv<2>); // { dg-error "here" }
std::atomic_ref<const volatile NonTrivial<3>> cvnt(ntv<3>); // { dg-error "here" }

template<size_t N>
struct NonLockFree
{
  char c[1024 + N];
};

template<size_t N>
NonLockFree<N> nlfv;

std::atomic_ref<NonLockFree<0>> nlf(nlfv<0>);
std::atomic_ref<const NonLockFree<1>> cnlf(nlfv<1>);
std::atomic_ref<volatile NonLockFree<2>> vnlf(nlfv<2>); // { dg-error "here" }
std::atomic_ref<const volatile NonLockFree<3>> cvnlf(nlfv<3>); // { dg-error "here" }

// { dg-error "static assertion failed" "" { target *-*-* } 0 }
