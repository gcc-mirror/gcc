// { dg-do compile { target c++17 } }

#include <vector>

void
test_observers(std::vector<int> v)
{
  v.size(); // { dg-warning "ignoring return value" }
  v.capacity(); // { dg-warning "ignoring return value" }
  v.empty(); // { dg-warning "ignoring return value" }
}

void
test_element_access(std::vector<float> v)
{
  v.front(); // { dg-warning "ignoring return value" }
  v.back();  // { dg-warning "ignoring return value" }
  v[1];      // { dg-warning "ignoring return value" }
  v.at(1);   // { dg-warning "ignoring return value" }
  v.data();  // { dg-warning "ignoring return value" }
  const auto& cv = v;
  cv[1];     // { dg-warning "ignoring return value" }
  cv.at(1);  // { dg-warning "ignoring return value" }
  cv.data(); // { dg-warning "ignoring return value" }
}

void
test_rel_ops(std::vector<char> v)
{
  v == v; // { dg-warning "ignoring return value" }
  v != v; // { dg-warning "ignoring return value" "PR c++/114104" { target c++17_down } }
  v < v;  // { dg-warning "ignoring return value" }
  v > v;  // { dg-warning "ignoring return value" }
  v <= v; // { dg-warning "ignoring return value" }
  v >= v; // { dg-warning "ignoring return value" }
}

struct S { };

void
test_iterators(std::vector<S> v)
{
  v.begin();  // { dg-warning "ignoring return value" }
  v.end();    // { dg-warning "ignoring return value" }
  v.rbegin(); // { dg-warning "ignoring return value" }
  v.rend();   // { dg-warning "ignoring return value" }
  const auto& cv = v;
  cv.begin();  // { dg-warning "ignoring return value" }
  cv.end();    // { dg-warning "ignoring return value" }
  cv.rbegin(); // { dg-warning "ignoring return value" }
  cv.rend();   // { dg-warning "ignoring return value" }

  v.cbegin();  // { dg-warning "ignoring return value" }
  v.cend();    // { dg-warning "ignoring return value" }
  v.crbegin(); // { dg-warning "ignoring return value" }
  v.crend();   // { dg-warning "ignoring return value" }

  auto i = v.begin(), j = v.end();
  i == j; // { dg-warning "ignoring return value" }
  i != j; // { dg-warning "ignoring return value" "PR c++/114104" { target c++17_down } }
  i < j;  // { dg-warning "ignoring return value" }
  i > j;  // { dg-warning "ignoring return value" }
  i <= j; // { dg-warning "ignoring return value" }
  i >= j; // { dg-warning "ignoring return value" }

  auto ci = cv.begin(), cj = cv.end();
  ci == cj; // { dg-warning "ignoring return value" }
  ci != cj; // { dg-warning "ignoring return value" "PR c++/114104" { target c++17_down } }
  ci < cj;  // { dg-warning "ignoring return value" }
  ci > cj;  // { dg-warning "ignoring return value" }
  ci <= cj; // { dg-warning "ignoring return value" }
  ci >= cj; // { dg-warning "ignoring return value" }

  ci == j; // { dg-warning "ignoring return value" }
  ci != j; // { dg-warning "ignoring return value" "PR c++/114104" { target c++17_down } }
  ci < j;  // { dg-warning "ignoring return value" }
  ci > j;  // { dg-warning "ignoring return value" }
  ci <= j; // { dg-warning "ignoring return value" }
  ci >= j; // { dg-warning "ignoring return value" }
}

void
test_observers(std::vector<bool> v)
{
  v.size(); // { dg-warning "ignoring return value" }
  v.capacity(); // { dg-warning "ignoring return value" }
  v.empty(); // { dg-warning "ignoring return value" }
}

void
test_element_access(std::vector<bool> v)
{
  v.front(); // { dg-warning "ignoring return value" }
  v.back();  // { dg-warning "ignoring return value" }
  v[1];      // { dg-warning "ignoring return value" }
  v.at(1);   // { dg-warning "ignoring return value" }
  const auto& cv = v;
  cv[1];     // { dg-warning "ignoring return value" }
  cv.at(1);  // { dg-warning "ignoring return value" }
}

void
test_rel_ops(std::vector<bool> v)
{
  v == v; // { dg-warning "ignoring return value" }
  v != v; // { dg-warning "ignoring return value" "PR c++/114104" { target c++17_down } }
  v < v;  // { dg-warning "ignoring return value" }
  v > v;  // { dg-warning "ignoring return value" }
  v <= v; // { dg-warning "ignoring return value" }
  v >= v; // { dg-warning "ignoring return value" }
}

void
test_iterators(std::vector<bool> v)
{
  v.begin();  // { dg-warning "ignoring return value" }
  v.end();    // { dg-warning "ignoring return value" }
  v.rbegin(); // { dg-warning "ignoring return value" }
  v.rend();   // { dg-warning "ignoring return value" }
  const auto& cv = v;
  cv.begin();  // { dg-warning "ignoring return value" }
  cv.end();    // { dg-warning "ignoring return value" }
  cv.rbegin(); // { dg-warning "ignoring return value" }
  cv.rend();   // { dg-warning "ignoring return value" }

  v.cbegin();  // { dg-warning "ignoring return value" }
  v.cend();    // { dg-warning "ignoring return value" }
  v.crbegin(); // { dg-warning "ignoring return value" }
  v.crend();   // { dg-warning "ignoring return value" }

  auto i = v.begin(), j = v.end();
  i == j; // { dg-warning "ignoring return value" }
  i != j; // { dg-warning "ignoring return value" "PR c++/114104" { target c++17_down } }
  i < j;  // { dg-warning "ignoring return value" }
  i > j;  // { dg-warning "ignoring return value" }
  i <= j; // { dg-warning "ignoring return value" }
  i >= j; // { dg-warning "ignoring return value" }

  auto ci = cv.begin(), cj = cv.end();
  ci == cj; // { dg-warning "ignoring return value" }
  ci != cj; // { dg-warning "ignoring return value" "PR c++/114104" { target c++17_down } }
  ci < cj;  // { dg-warning "ignoring return value" }
  ci > cj;  // { dg-warning "ignoring return value" }
  ci <= cj; // { dg-warning "ignoring return value" }
  ci >= cj; // { dg-warning "ignoring return value" }

  ci == j; // { dg-warning "ignoring return value" }
  ci != j; // { dg-warning "ignoring return value" "PR c++/114104" { target c++17_down } }
  ci < j;  // { dg-warning "ignoring return value" }
  ci > j;  // { dg-warning "ignoring return value" }
  ci <= j; // { dg-warning "ignoring return value" }
  ci >= j; // { dg-warning "ignoring return value" }
}
