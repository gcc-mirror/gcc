// PR libstdc++/91488 "inlining failed in call to always_inline"
// { dg-do run }
// { dg-additional-options "-O1" }

#include <string>

int main() {
    return std::char_traits<char>::length("");
}
