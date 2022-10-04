// { dg-do compile { target c++11 } }
// { dg-require-effective-target hosted }

#include <memory>

// LWG 3548
// shared_ptr construction from unique_ptr should move (not copy) the deleter

struct D
{
  D() { }
  D(D&&) { }
  void operator()(int* p) const { delete p; }
};

std::unique_ptr<int, D> u;
std::shared_ptr<int> s1(std::move(u));
