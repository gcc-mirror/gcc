// { dg-do compile { target c++17 } }

#include <optional>

struct U {};

struct T {
  explicit T(const U&);
  T& operator=(const U&);
  T& operator=(U&&) = delete;
};

int main() {
  std::optional<U> opt1;
  std::optional<T> opt2;
  opt2 = opt1; // PR libstdc++/100982
}
