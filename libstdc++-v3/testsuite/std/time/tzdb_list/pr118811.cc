// { dg-do run { target c++20 } }
// { dg-require-effective-target tzdb }
// { dg-require-effective-target cxx11_abi }
// { dg-require-static-libstdcxx }
// { dg-additional-options "-static-libstdc++" }

#include <chrono>

struct Global
{
  Global()
  {
    (void) std::chrono::current_zone(); // initialize tzdb on first use
  }

  ~Global()
  {
    (void) std::chrono::current_zone(); // attempt to use it again on exit
  }

} global;

int main()
{
}
