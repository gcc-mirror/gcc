// { dg-options "-std=c++03" }
// { dg-do compile }
// PR libstdc++/107037 bitset::_M_do_reset fails for strict -std=c++03 mode
#include <bitset>
template class std::bitset<0>;
template class std::bitset<1>;
template class std::bitset<100>;
