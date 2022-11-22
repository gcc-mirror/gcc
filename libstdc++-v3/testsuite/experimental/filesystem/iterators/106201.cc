// { dg-options "-std=gnu++20" }
// { dg-do compile { target c++20 } }
// { dg-require-filesystem-ts "" }

// PR libstdc++/106201 constraint recursion in path(Source const&) constructor.

#include <experimental/filesystem>
#include <iterator>
#include <concepts>
namespace fs = std::experimental::filesystem;
using I = std::counted_iterator<fs::directory_iterator>;
static_assert( std::swappable<I> );
using R = std::counted_iterator<fs::recursive_directory_iterator>;
static_assert( std::swappable<R> );
