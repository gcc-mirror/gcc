// { dg-do compile { target c++26 } }

#include <algorithm>

#if !defined(__cpp_lib_algorithm_default_value_type)
#error "Feature test macro for default template type for algorithms' values is missing"
#elif __cpp_lib_algorithm_default_value_type < 202403L
#error "Feature test macro for default template type for algorithms' values is wrong"
#endif

#include <execution>
#include <ranges>
#include <iterator>
#include <vector>

// Conversions from Input to Output will be used in certain algorithms.
// Make Output have a different number of arguments to its constructor
// so we can check whether a braced-init-list is indeed matching Input
// or Output
struct Output
{
  Output(char, float, long);
};

#define OUTPUT_VAL {'x', 1.171f, 10L}

struct Input
{
  Input(int, double);
  friend auto operator<=>(const Input &, const Input &) = default;
  friend Input operator+(const Input &, const Input &);
  operator Output() const;
};

#define INPUT_VAL {1, 3.14}

void
test()
{
  extern std::vector<Input> in;
  extern std::vector<Output> out;

  const auto pred = [](auto &&) { return true; };

  // [alg.find]
  (void) std::find(in.begin(), in.end(), INPUT_VAL);
  (void) std::find(std::execution::seq, in.begin(), in.end(), INPUT_VAL);
  (void) std::ranges::find(in.begin(), in.end(), INPUT_VAL);
  (void) std::ranges::find(in, INPUT_VAL);

  // [alg.count]
  (void) std::count(in.begin(), in.end(), INPUT_VAL);
  (void) std::count(std::execution::seq, in.begin(), in.end(), INPUT_VAL);
  (void) std::ranges::count(in.begin(), in.end(), INPUT_VAL);
  (void) std::ranges::count(in, INPUT_VAL);

  // [alg.search]
  (void) std::search_n(in.begin(), in.end(), 10, INPUT_VAL);
  (void) std::search_n(in.begin(), in.end(), 10, INPUT_VAL, std::equal_to{});
  (void) std::search_n(std::execution::seq, in.begin(), in.end(), 10, INPUT_VAL);
  (void) std::search_n(std::execution::seq, in.begin(), in.end(), 10, INPUT_VAL, std::equal_to{});
  (void) std::ranges::search_n(in.begin(), in.end(), 10, INPUT_VAL);
  (void) std::ranges::search_n(in, 10, INPUT_VAL);

  // [alg.replace]
  (void) std::replace(in.begin(), in.end(), INPUT_VAL, INPUT_VAL);
  (void) std::replace(std::execution::seq, in.begin(), in.end(), INPUT_VAL, INPUT_VAL);
  (void) std::replace_if(in.begin(), in.end(), pred, INPUT_VAL);
  (void) std::replace_if(std::execution::seq, in.begin(), in.end(), pred, INPUT_VAL);

  (void) std::ranges::replace(in.begin(), in.end(), INPUT_VAL, INPUT_VAL);
  (void) std::ranges::replace(in, INPUT_VAL, INPUT_VAL);
  (void) std::ranges::replace_if(in.begin(), in.end(), pred, INPUT_VAL);
  (void) std::ranges::replace_if(in, pred, INPUT_VAL);

  (void) std::replace_copy_if(in.begin(), in.end(), out.begin(), pred, OUTPUT_VAL);
  (void) std::replace_copy_if(std::execution::seq, in.begin(), in.end(), out.begin(), pred, OUTPUT_VAL);
  (void) std::ranges::replace_copy_if(in.begin(), in.end(), out.begin(), pred, OUTPUT_VAL);
  (void) std::ranges::replace_copy_if(in, out.begin(), pred, OUTPUT_VAL);

  // Non-range replace_copy is deliberately skipped by P2248
  (void) std::ranges::replace_copy(in.begin(), in.end(), out.begin(), INPUT_VAL, OUTPUT_VAL);
  (void) std::ranges::replace_copy(in, out.begin(), INPUT_VAL, OUTPUT_VAL);

  // [alg.fill]
  (void) std::fill(in.begin(), in.end(), INPUT_VAL);
  (void) std::fill(std::execution::seq, in.begin(), in.end(), INPUT_VAL);
  (void) std::ranges::fill(in.begin(), in.end(), INPUT_VAL);
  (void) std::ranges::fill(in, INPUT_VAL);

  (void) std::fill_n(in.begin(), 10, INPUT_VAL);
  (void) std::fill_n(std::execution::seq, in.begin(), 10, INPUT_VAL);
  (void) std::ranges::fill_n(in.begin(), 10, INPUT_VAL);

  // [alg.remove]
  (void) std::remove(in.begin(), in.end(), INPUT_VAL);
  (void) std::remove(std::execution::seq, in.begin(), in.end(), INPUT_VAL);
  (void) std::ranges::remove(in.begin(), in.end(), INPUT_VAL);
  (void) std::ranges::remove(in, INPUT_VAL);

  (void) std::remove_copy(in.begin(), in.end(), out.begin(), INPUT_VAL);
  (void) std::remove_copy(std::execution::seq, in.begin(), in.end(), out.begin(), INPUT_VAL);
  (void) std::ranges::remove_copy(in.begin(), in.end(), out.begin(), INPUT_VAL);
  (void) std::ranges::remove_copy(in, out.begin(), INPUT_VAL);

  // [alg.binary.search]
  (void) std::lower_bound(in.begin(), in.end(), INPUT_VAL);
  (void) std::lower_bound(in.begin(), in.end(), INPUT_VAL, std::less{});
  (void) std::ranges::lower_bound(in.begin(), in.end(), INPUT_VAL);
  (void) std::ranges::lower_bound(in, INPUT_VAL);

  (void) std::upper_bound(in.begin(), in.end(), INPUT_VAL);
  (void) std::upper_bound(in.begin(), in.end(), INPUT_VAL, std::less{});
  (void) std::ranges::upper_bound(in.begin(), in.end(), INPUT_VAL);
  (void) std::ranges::upper_bound(in, INPUT_VAL);

  (void) std::equal_range(in.begin(), in.end(), INPUT_VAL);
  (void) std::equal_range(in.begin(), in.end(), INPUT_VAL, std::less{});
  (void) std::ranges::equal_range(in.begin(), in.end(), INPUT_VAL);
  (void) std::ranges::equal_range(in, INPUT_VAL);

  (void) std::binary_search(in.begin(), in.end(), INPUT_VAL);
  (void) std::binary_search(in.begin(), in.end(), INPUT_VAL, std::less{});
  (void) std::ranges::binary_search(in.begin(), in.end(), INPUT_VAL);
  (void) std::ranges::binary_search(in, INPUT_VAL);

  // [alg.fold]
  (void) std::ranges::fold_left(in.begin(), in.end(), INPUT_VAL, std::plus<>{});
  (void) std::ranges::fold_left(in, INPUT_VAL, std::plus<>{});
  (void) std::ranges::fold_right(in.begin(), in.end(), INPUT_VAL, std::plus<>{});
  (void) std::ranges::fold_right(in, INPUT_VAL, std::plus<>{});
  (void) std::ranges::fold_left_with_iter(in.begin(), in.end(), INPUT_VAL, std::plus<>{});
  (void) std::ranges::fold_left_with_iter(in, INPUT_VAL, std::plus<>{});

  // [alg.contains]
  (void) std::ranges::contains(in.begin(), in.end(), INPUT_VAL);
  (void) std::ranges::contains(in, INPUT_VAL);

  // [alg.find.last]
  (void) std::ranges::find_last(in.begin(), in.end(), INPUT_VAL);
  (void) std::ranges::find_last(in, INPUT_VAL);
}
