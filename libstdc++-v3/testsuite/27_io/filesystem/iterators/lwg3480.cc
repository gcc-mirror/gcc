// { dg-do compile { target c++20 } }
// { dg-require-filesystem-ts "" }

// LWG 3480
// directory_iterator and recursive_directory_iterator are not C++20 ranges

#include <filesystem>

namespace fs = std::filesystem;
namespace rg = std::ranges;

static_assert( rg::borrowed_range<fs::directory_iterator> );
static_assert( rg::borrowed_range<fs::recursive_directory_iterator> );

static_assert( rg::view<fs::directory_iterator> );
static_assert( rg::view<fs::recursive_directory_iterator> );
