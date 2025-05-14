#include <vector>

int main()
{
#pragma omp target
  {
    {
      std::vector<int> v;
      if (!v.empty())
	__builtin_abort();
    }
    {
      std::vector<int> v(100);
      if (v.capacity() < 100)
	__builtin_abort();
    }
  }
}
