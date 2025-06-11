// { dg-do compile { target c++20 } }

// Bug libstdc++/120625
// std::formatter<__disabled> specializations cause errors in user code

#include <format>

enum X { };

// A concept that cannot be used with incomplete types:
template<typename T>
concept is_X = !std::is_empty_v<T> && std::is_same_v<X, T>;

// A valid program-defined specialization:
template<typename T, typename C> requires is_X<T>
struct std::formatter<T, C> : std::formatter<int, C> { };

// Instantiate the program-defined formatter specialization:
auto s = sizeof(std::formatter<X, char>);
