// { dg-do compile { target c++20 } }

#include <semaphore>

// PR 110854 Constructor of std::counting_semaphore is not constexpr
constinit std::binary_semaphore b(0);
constinit std::counting_semaphore<5> c(2);
