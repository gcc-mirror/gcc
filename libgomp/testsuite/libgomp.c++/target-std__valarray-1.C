// { dg-additional-options -std=c++20 }
// { dg-output-file target-std__valarray-1.output }

#include <valarray>
#include <ostream>
#include <sstream>


/*TODO Work around PR118484 "ICE during IPA pass: cp, segfault in determine_versionability ipa-cp.cc:467".

We can't:

    #pragma omp declare target(std::basic_streambuf<char, std::char_traits<char>>::basic_streambuf)

... because:

    error: overloaded function name ‘std::basic_streambuf<char>::__ct ’ in clause ‘enter’

Therefore, use dummy classes in '#pragma omp declare target':
*/

#pragma omp declare target

// For 'std::basic_streambuf<char, std::char_traits<char> >::basic_streambuf':

class dummy_basic_streambuf__char
  : public std::basic_streambuf<char>
{
public:
  dummy_basic_streambuf__char() {}
};

// For 'std::basic_ios<char, std::char_traits<char> >::basic_ios()':

class dummy_basic_ios__char
  : public std::basic_ios<char>
{
public:
  dummy_basic_ios__char() {}
};

#pragma omp end declare target


int main()
{
  // Due to PR120021 "Offloading vs. C++ 'std::initializer_list'", we can't construct these on the device.
  std::initializer_list<int> v1_i = {10, 20, 30, 40, 50};
  const int *v1_i_data = std::data(v1_i);
  size_t v1_i_size = v1_i.size();
  std::initializer_list<int> v2_i = {5, 4, 3, 2, 1};
  const int *v2_i_data = std::data(v2_i);
  size_t v2_i_size = v2_i.size();
  std::initializer_list<int> shiftData_i = {1, 2, 3, 4, 5};
  const int *shiftData_i_data = std::data(shiftData_i);
  size_t shiftData_i_size = shiftData_i.size();
#pragma omp target \
  defaultmap(none) \
  map(to: v1_i_data[:v1_i_size], v1_i_size, \
          v2_i_data[:v2_i_size], v2_i_size, \
          shiftData_i_data[:shiftData_i_size], shiftData_i_size)
  {
    /* Manually set up a buffer we can stream into, similar to 'cout << [...]', and print it at the end of region.  */
    std::stringbuf out_b;
    std::ostream out(&out_b);

    std::valarray<int> v1(v1_i_data, v1_i_size);
    out << "\nv1:";
    for (auto val : v1)
      out << " " << val;

    std::valarray<int> v2(v2_i_data, v2_i_size);
    out << "\nv2:";
    for (auto val : v2)
      out << " " << val;

    std::valarray<int> sum = v1 + v2;
    out << "\nv1 + v2:";
    for (auto val : sum)
      out << " " << val;

    std::valarray<int> diff = v1 - v2;
    out << "\nv1 - v2:";
    for (auto val : diff)
      out << " " << val;

    std::valarray<int> product = v1 * v2;
    out << "\nv1 * v2:";
    for (auto val : product)
      out << " " << val;

    std::valarray<int> quotient = v1 / v2;
    out << "\nv1 / v2:";
    for (auto val : quotient)
      out << " " << val;

    std::valarray<int> squares = pow(v1, 2);
    out << "\npow(v1, 2):";
    for (auto val : squares)
      out << " " << val;

    std::valarray<int> sinhs = sinh(v2);
    out << "\nsinh(v2):";
    for (auto val : sinhs)
      out << " " << val;

    std::valarray<int> logs = log(v1 * v2);
    out << "\nlog(v1 * v2):";
    for (auto val : logs)
      out << " " << val;

    std::valarray<int> data(12);
    for (size_t i = 0; i < data.size(); ++i)
      data[i] = i;
    out << "\nOriginal array:";
    for (auto val : data)
      out << " " << val;

    std::slice slice1(2, 5, 1);
    std::valarray<int> sliced1 = data[slice1];
    out << "\nSlice(2, 5, 1):";
    for (auto val : sliced1)
      out << " " << val;

    std::slice slice2(1, 4, 3);
    std::valarray<int> sliced2 = data[slice2];
    out << "\nSlice(1, 4, 3):";
    for (auto val : sliced2)
      out << " " << val;

    data[slice1] = 99;
    out << "\nArray after slice modification:";
    for (auto val : data)
      out << " " << val;

    std::valarray<bool> mask = (v1 > 20);
    out << "\nElements of v1 > 20:";
    for (size_t i = 0; i < v1.size(); ++i)
      {
	if (mask[i])
	  out << " " << v1[i];
      }

    std::valarray<int> masked = v1[mask];
    out << "\nMasked array:";
    for (auto val : masked)
      out << " " << val;

    std::valarray<int> shiftData(shiftData_i_data, shiftData_i_size);
    out << "\nOriginal shiftData:";
    for (auto val : shiftData)
      out << " " << val;

    std::valarray<int> shifted = shiftData.shift(2);
    out << "\nshift(2):";
    for (auto val : shifted)
      out << " " << val;

    std::valarray<int> cshifted = shiftData.cshift(-1);
    out << "\ncshift(-1):";
    for (auto val : cshifted)
      out << " " << val;

    out << "\nSum(v1): " << v1.sum();
    out << "\nMin(v1): " << v1.min();
    out << "\nMax(v1): " << v1.max();

    out << "\n";

    /* Terminate with a NUL.  Otherwise, we'd have to use:
           __builtin_printf("%.*s", (int) out_b_sv.size(), out_b_sv.data());
       ... which nvptx 'printf', as implemented via PTX 'vprintf', doesn't support (TODO).  */
    out << '\0';
    std::string_view out_b_sv = out_b.view();
    __builtin_printf("%s", out_b_sv.data());
  }

  return 0;
}
