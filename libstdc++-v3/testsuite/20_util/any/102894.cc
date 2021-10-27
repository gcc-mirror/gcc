// { dg-do compile { target c++17 } }
#include <any>

template<typename T, typename = void>
struct can_make_any
: std::false_type
{ };

template<typename T>
struct can_make_any<T, std::void_t<decltype(std::make_any<T>())>>
: std::true_type
{ };

struct move_only
{
  move_only() = default;
  move_only(move_only&&) = default;
};

static_assert( ! can_make_any<move_only>::value ); // PR libstdc++/102894
