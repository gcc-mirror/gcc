// { dg-do compile { target c++26 } }
// { dg-require-effective-target hosted }

// N5008 20.3.2.6 Struct owner_equal [util.smartptr.owner.equal]

#include <memory>

#ifndef __cpp_lib_smart_ptr_owner_equality
# error "Feature-test macro for smart ptr owner equality missing in <memory>"
#elif __cpp_lib_smart_ptr_owner_equality != 202306L
# error "Feature-test macro for smart ptr owner equality has wrong value in <memory>"
#endif

const std::owner_equal eq;
const std::shared_ptr<int> si;
const std::weak_ptr<int> wi;
static_assert( noexcept(!eq(si, si)) );
static_assert( noexcept(!eq(si, wi)) );
static_assert( noexcept(!eq(wi, si)) );
static_assert( noexcept(!eq(wi, wi)) );
static_assert( noexcept(!eq(si, wi)) );
static_assert( noexcept(!eq(wi, si)) );
const std::shared_ptr<long> sl;
const std::weak_ptr<char> wc;
static_assert( noexcept(!eq(si, si)) );
static_assert( noexcept(!eq(si, sl)) );
static_assert( noexcept(!eq(sl, si)) );
static_assert( noexcept(!eq(si, wc)) );
static_assert( noexcept(!eq(wc, si)) );
static_assert( noexcept(!eq(wc, wi)) );
