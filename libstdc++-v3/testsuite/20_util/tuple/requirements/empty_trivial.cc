// { dg-do compile { target c++11 } }

#include <tuple>
#include <type_traits>

// Check that tuple<> has the expected trivial properties
static_assert(std::is_trivially_copyable<std::tuple<>>::value,
	      "tuple<> should be trivially copyable");
static_assert(std::is_trivially_copy_constructible<std::tuple<>>::value,
	      "tuple<> should be trivially copy constructible");
static_assert(std::is_trivially_move_constructible<std::tuple<>>::value,
	      "tuple<> should be trivially move constructible");
static_assert(std::is_trivially_copy_assignable<std::tuple<>>::value,
	      "tuple<> should be trivially copy assignable");
static_assert(std::is_trivially_move_assignable<std::tuple<>>::value,
	      "tuple<> should be trivially move assignable");

