// { dg-do compile }

// Bug 119748
// string(InputIterator, InputIterator) rejects volatile charT* as iterator

#ifndef TEST_CHAR_TYPE
#define TEST_CHAR_TYPE char
#endif

#include <string>
#include <testsuite_iterators.h>

typedef TEST_CHAR_TYPE C;

volatile C vs[42] = {};
std::basic_string<C> s(vs+0, vs+42);
#ifdef __cpp_lib_containers_ranges
std::basic_string<C> s2(std::from_range, vs);
#endif

using namespace __gnu_test;

test_container<volatile C, input_iterator_wrapper> input_cont(vs);
std::basic_string<C> s3(input_cont.begin(), input_cont.end());

test_container<volatile C, forward_iterator_wrapper> fwd_cont(vs);
std::basic_string<C> s4(fwd_cont.begin(), fwd_cont.end());

#ifdef __cpp_lib_containers_ranges
test_input_range<volatile C> input_range(vs);
std::basic_string<C> s5(std::from_range, input_range);

test_forward_range<volatile C> fwd_range(vs);
std::basic_string<C> s6(std::from_range, fwd_range);
#endif
