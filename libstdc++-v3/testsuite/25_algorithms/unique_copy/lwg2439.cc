// { dg-do run }

#include <algorithm>
#include <testsuite_iterators.h>

using namespace __gnu_test;

int out[4];
short out_shrt[4];
short in[7] = { 1, 2, 2, 2, 3, 4, 4 };

template<typename T>
void
check_and_reset(T* p)
{
  VERIFY( p[0] == 1 );
  VERIFY( p[1] == 2 );
  VERIFY( p[2] == 3 );
  VERIFY( p[3] == 4 );
  std::fill_n(p, 4, 0);
}

struct Eq
{
  bool operator()(short i, short j) const { return i == j; }
  bool operator()(short, int) const { VERIFY(false); }
  bool operator()(int, short) const { VERIFY(false); }
};

struct Eq2
{
  bool operator()(const short& i, const short& j) const
  {
    // Both arguments should be elements of the 'in' array.
    VERIFY( in+0 <= &i && &i < in+7 );
    VERIFY( in+0 <= &j && &j < in+7 );
    VERIFY( &i < &j );
    return i == j;
  }
  bool operator()(short, int) const { VERIFY(false); }
  bool operator()(int, short) const { VERIFY(false); }
};

struct Eq3
{
  bool operator()(const short& i, const short& j) const
  {
    // First argument should be element of the 'out' array.
    // Second argument should be element of the 'in' array.
    VERIFY( out_shrt+0 <= &i && &i < out_shrt+4 );
    VERIFY( in+0 <= &j && &j < in+7 );
    return i == j;
  }
  bool operator()(short, int) const { VERIFY(false); }
  bool operator()(int, short) const { VERIFY(false); }
};

void
test_forward_source()
{
  // The input range uses forward iterators
  test_container<short, forward_iterator_wrapper> inc(in);
  test_container<int, output_iterator_wrapper> outc(out);
  std::unique_copy(inc.begin(), inc.end(), outc.begin());
  check_and_reset(out);

  test_container<short, forward_iterator_wrapper> inc2(in);
  test_container<int, output_iterator_wrapper> outc2(out);
  std::unique_copy(inc2.begin(), inc2.end(), outc2.begin(), Eq2());
  check_and_reset(out);
}

void
test_output_dest()
{
  // The input range uses input iterators.
  // The output range uses output iterators.
  test_container<short, input_iterator_wrapper> inc(in);
  test_container<int, output_iterator_wrapper> outc(out);
  std::unique_copy(inc.begin(), inc.end(), outc.begin());
  check_and_reset(out);

  test_container<short, input_iterator_wrapper> inc2(in);
  test_container<int, output_iterator_wrapper> outc2(out);
  std::unique_copy(inc2.begin(), inc2.end(), outc2.begin(), Eq());
  check_and_reset(out);
}

void
test_forward_dest_diff_type()
{
  // The input range uses input iterators.
  // The output range uses forward iterators, but with different value type.
  test_container<short, input_iterator_wrapper> inc(in);
  test_container<int, forward_iterator_wrapper> outc(out);
  std::unique_copy(inc.begin(), inc.end(), outc.begin());
  check_and_reset(out);

  test_container<short, input_iterator_wrapper> inc2(in);
  test_container<int, forward_iterator_wrapper> outc2(out);
  std::unique_copy(inc2.begin(), inc2.end(), outc2.begin(), Eq());
  check_and_reset(out);
}

void
test_forward_dest_same_type()
{
  // The input range uses input iterators.
  // The output range uses forward iterators, with same value type.
  test_container<short, input_iterator_wrapper> inc(in);
  test_container<short, forward_iterator_wrapper> outc(out_shrt);
  std::unique_copy(inc.begin(), inc.end(), outc.begin());
  check_and_reset(out_shrt);

  test_container<short, input_iterator_wrapper> inc2(in);
  test_container<short, forward_iterator_wrapper> outc2(out_shrt);
  std::unique_copy(inc2.begin(), inc2.end(), outc2.begin(), Eq3());
  check_and_reset(out_shrt);
}

int main()
{
  test_forward_source();
  test_output_dest();
  test_forward_dest_diff_type();
  test_forward_dest_same_type();
}
