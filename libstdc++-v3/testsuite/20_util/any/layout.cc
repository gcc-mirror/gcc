// { dg-options "-Wno-deprecated-declarations" }
// { dg-do compile { target c++17 } }

// Verify that r15-3419 did not change the layout of std::any

#include <any>

namespace test {
  class any {
    union Storage {
      constexpr Storage() : ptr(nullptr) { }
      void* ptr;
      std::aligned_storage<sizeof(ptr), alignof(void*)>::type buffer;
    };

    void (*manager)(int, const any*, void*);
    Storage storage;
  };
}

static_assert( sizeof(std::any) == sizeof(test::any) );
static_assert( alignof(std::any) == alignof(test::any) );
