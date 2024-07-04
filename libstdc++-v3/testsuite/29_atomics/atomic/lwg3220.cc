// { dg-options "-Wno-deprecated" }
// { dg-do compile { target c++11 } }

// std::shared_ptr is not freestanding.
// { dg-require-effective-target hosted }

// DR 3220. P0558 broke conforming C++14 uses of atomic shared_ptr

#include <atomic>
#include <memory>

struct Abstract { virtual void test() = 0; };
struct Concrete : Abstract { virtual void test() override {} };

int main() {
  std::shared_ptr<Abstract> ptr;
  std::atomic_store<Abstract>(&ptr, std::make_shared<Concrete>());
}
