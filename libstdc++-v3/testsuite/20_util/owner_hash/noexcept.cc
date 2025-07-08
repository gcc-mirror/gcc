// { dg-do compile { target c++26 } }
// { dg-require-effective-target hosted }

// N5008 20.3.2.5 Struct owner_hash [util.smartptr.owner.hash]

#include <memory>

const std::owner_hash oh;
const std::shared_ptr<int> si;
const std::weak_ptr<int> wi;
static_assert( noexcept(!oh(si)) );
static_assert( noexcept(!oh(wi)) );
const std::shared_ptr<long> sl;
const std::weak_ptr<char> wc;
static_assert( noexcept(!oh(sl)) );
static_assert( noexcept(!oh(wc)) );
