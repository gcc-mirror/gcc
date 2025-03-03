// { dg-do compile { target c++20 } }

// PR libstdc++/105609
// ranges::move should use ranges::iter_move instead of std::move

#include <algorithm>

namespace pr105609
{
  struct I {
    using value_type = int;
    using difference_type = std::ptrdiff_t;
    int operator*() const;
    I& operator++();
    I operator++(int);
    I& operator--();
    I operator--(int);
    bool operator==(I) const;
    friend int& iter_move(const I&);
  };
}

void
test(pr105609::I i)
{
  struct O {
    O(int&) { }
    O(int&&) = delete;
  };

  O* o = nullptr;
  std::ranges::move(i, i, o);
}
