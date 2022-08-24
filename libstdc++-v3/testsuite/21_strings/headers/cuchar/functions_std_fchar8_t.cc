// { dg-options "-fchar8_t" }
// { dg-do compile { target c++11 } }

#include <cuchar>

namespace gnu
{
#if _GLIBCXX_USE_UCHAR_C8RTOMB_MBRTOC8_FCHAR8_T
  using std::mbrtoc8;
  using std::c8rtomb;
#endif // _GLIBCXX_USE_UCHAR_C8RTOMB_MBRTOC8_FCHAR8_T
}
