// { dg-options "-D_GLIBCXX_USE_NEW_PIECEWISE_DISTRIBUTIONS=0" }
// { dg-do compile { target c++11 } }
// { dg-require-cstdint "" }

#include <random>

template<typename RealType>
struct OnlyConv
{
  long double v;

  template<typename DestType>
  operator DestType() const
  {
    static_assert(std::is_same<DestType, RealType>::value);
    return v;
  }
};

template<typename RealType>
struct OnlyCall
{
  template<typename SourceType>
  OnlyConv<RealType> operator()(SourceType src)
  {
    static_assert(std::is_same<SourceType, RealType>::value);
    return OnlyConv<RealType>{src};
  }
};

template<typename RealType>
void
test()
{
  std::initializer_list<RealType> invs{0, 1, 2};
  OnlyConv<RealType> weights[3]{1, 2, 3};

  std::piecewise_linear_distribution<RealType>
    from_iter(invs.begin(), invs.end(), weights);

  std::piecewise_linear_distribution<RealType>
    from_init_list(invs, OnlyCall<RealType>());

  std::piecewise_linear_distribution<RealType>
    from_count(2, 0, 2, OnlyCall<RealType>());
}

int main()
{
  test<float>(); // { dg-error "here" }
  test<double>();
  test<long double>(); // { dg-error "here" }
}

// { dg-prune-output "static assertion failed" }
