// { dg-do compile { target c++20 } }

#include <format>

struct O {
  using difference_type = std::ranges::__detail::__max_diff_type;
  O& operator=(const char&);
  O& operator*();
  O& operator++();
  O& operator++(int);
};

auto str = std::format_to_n(O{}, 4, "{}", " "); // PR libstdc++/107871
