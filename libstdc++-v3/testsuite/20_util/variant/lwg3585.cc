// { dg-do compile { target c++17 } }

// LWG 3585. Variant converting assignment with immovable alternative

#include <variant>
#include <string>

struct A {
  A() = default;
  A(A&&) = delete;
};

int main() {
  std::variant<A, std::string> v;
  v = "hello";
}
