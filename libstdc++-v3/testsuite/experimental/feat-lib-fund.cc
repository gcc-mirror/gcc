// { dg-do preprocess }

// Include all the LFTS headers. This should work with any -std flag.

#include <experimental/algorithm>
#include <experimental/any>
#include <experimental/array>
#include <experimental/chrono>
#include <experimental/deque>
#include <experimental/forward_list>
#include <experimental/functional>
#if __has_include(<experimental/future>) // not supported as of GCC 11
# include <experimental/future>
#endif
#include <experimental/iterator>
#include <experimental/list>
#include <experimental/map>
#include <experimental/memory>
#include <experimental/memory_resource>
#include <experimental/numeric>
#include <experimental/optional>
#include <experimental/propagate_const>
#include <experimental/random>
#include <experimental/ratio>
#include <experimental/regex>
#include <experimental/set>
#include <experimental/source_location>
#include <experimental/string>
#include <experimental/string_view>
#include <experimental/system_error>
#include <experimental/tuple>
#include <experimental/type_traits>
#include <experimental/unordered_map>
#include <experimental/unordered_set>
#include <experimental/utility>
#include <experimental/vector>
