// { dg-options "-O2 -Wstringop-overread" }
// { dg-do compile { target c++11 } }

// Bug 100516
// Unexpected -Wstringop-overread in deque<char> initialization from empty
// initializer_list

#include <deque>

void f()
{
    std::initializer_list<char> il{};
    std::deque<char>{il};
}
