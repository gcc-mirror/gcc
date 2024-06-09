// { dg-do compile { target c++20 } }

// PR libstdc++/113200
// char_traits::move is not constexpr when the argument is a string literal

#include <string_view>

template<std::size_t N> struct S
{
  char data_[ N ];

  constexpr S( char const* p ): data_{}
  {
    std::char_traits<char>::move( data_, p, N );
  }
};

template<std::size_t N> S( char const(&)[N] ) -> S<N>;

constexpr S s( "test" );
