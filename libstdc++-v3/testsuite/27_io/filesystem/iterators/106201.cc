// { dg-options "-std=gnu++20" }
// { dg-do compile { target c++20 } }
// { dg-require-filesystem-ts "" }

// PR libstdc++/106201 constraint recursion in path(Source const&) constructor.

#include <filesystem>
#include <ranges>
using I = std::counted_iterator<std::filesystem::directory_iterator>;
static_assert( std::swappable<I> );
using R = std::counted_iterator<std::filesystem::recursive_directory_iterator>;
static_assert( std::swappable<R> );
