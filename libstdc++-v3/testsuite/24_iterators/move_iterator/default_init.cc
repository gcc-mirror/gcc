// { dg-do compile { target c++20 } }

#include <iterator>

struct no_default_init_iterator
{
  using value_type = int;
  using difference_type = std::ptrdiff_t;
  using iterator_category = std::input_iterator_tag;
      
  no_default_init_iterator() = delete;
  no_default_init_iterator(no_default_init_iterator&&) = default;
  no_default_init_iterator& operator=(no_default_init_iterator&&) = default;

  no_default_init_iterator& operator++();
  no_default_init_iterator operator++(int);
  int& operator*() const;

  bool operator==(const no_default_init_iterator&) const;
};

static_assert( std::is_default_constructible_v<std::move_iterator<int*>> );
static_assert( !std::is_default_constructible_v<std::move_iterator<no_default_init_iterator>> );


