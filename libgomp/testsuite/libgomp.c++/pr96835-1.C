// See also '../libgomp.oacc-c++/pr96835-1.C'.
#ifndef ALWAYS_INLINE
# define ALWAYS_INLINE
#endif

#pragma omp declare target

template<int sz>
struct vector {
  int values_[sz];
  vector();
  ALWAYS_INLINE
  vector(int const& init_val);
  ALWAYS_INLINE
  int dot(vector o) {
    int res = 0;
    for (int i = 0; i < sz; ++ i)
      res += values_[i] * o.values_[i];
    return res;
  }
};

template<int sz>
vector<sz>::vector(int const& init_val) {
  for (int i = 0; i < sz; ++ i) values_[i] = init_val;
}
template<int sz>
vector<sz>::vector() : vector(0) {
}

#pragma omp end declare target

int main() {
  int res = 0;
  #pragma omp target map(from:res)
  #pragma acc serial copyout(res)
  {
    vector<4> v1(1);
    vector<4> v2(2);
    res = v1.dot(v2);
  }
  if (res != 8)
    __builtin_abort();
  return 0;
}
