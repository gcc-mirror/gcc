/* { dg-additional-options "-std=c++20" } */

/* bit_cast and memcpy  */

#include <bit>
#include <cstring>

#include "target-flex-common.h"

struct S0
{
  int _v0;
  char _v1;
  long long _v2;
};

struct S1
{
  int _v0;
  char _v1;
  long long _v2;
};

bool test_bit_cast(int arg)
{
  bool ok;
  S1 s1_out;
  #pragma omp target map(from: ok, s1_out) map(to: arg)
    {
      bool inner_ok = true;
      {
	long long v = static_cast<long long>(arg + 42ll);
	S0 s = {arg, 'a', v};
	VERIFY (std::bit_cast<S1>(s)._v0 == arg);
	VERIFY (std::bit_cast<S1>(s)._v1 == 'a');
	VERIFY (std::bit_cast<S1>(s)._v2 == v);
	s1_out = std::bit_cast<S1>(s);
      }
      end:
      ok = inner_ok;
    }
  if (!ok)
    return false;
  long long v = static_cast<long long>(arg + 42ll);
  VERIFY_NON_TARGET (std::bit_cast<S0>(s1_out)._v0 == arg);
  VERIFY_NON_TARGET (std::bit_cast<S0>(s1_out)._v1 == 'a');
  VERIFY_NON_TARGET (std::bit_cast<S0>(s1_out)._v2 == v);
  return true;
}


struct OutStruct
{
  std::size_t _id;
  void *_next;
};

struct Extendable1
{
  std::size_t _id;
  void *_next;
  int _v;
};

struct Extendable2
{
  std::size_t _id;
  void *_next;
  char _str[256];
};

struct Extendable3
{
  std::size_t _id;
  void *_next;
  const int *_nums;
  std::size_t _size;
};

struct ExtendableUnknown
{
  std::size_t _id;
  void *_next;
};

template<typename To, std::size_t Id>
To *get_extendable(void *p)
{
  while (p != nullptr)
    {
      OutStruct out;
      std::memcpy(&out, p, sizeof(OutStruct));
      if (out._id == Id)
	return static_cast<To *>(p);
      p = out._next;
    }
  return nullptr;
}

bool test_memcpy(int arg, const int *nums, std::size_t nums_size)
{
  bool ok;
  Extendable2 e2_out;
  #pragma omp target map(from: ok, e2_out) map(to: arg, nums[:nums_size], nums_size)
    {
      bool inner_ok = true;
      {
	Extendable3 e3 = {3u, nullptr, nums, nums_size};
	ExtendableUnknown u1 = {100u, &e3};
	Extendable2 e2 = {2u, &u1, {'H', 'e', 'l', 'l', 'o', '!', '\000'}};
	ExtendableUnknown u2 = {101u, &e2};
	ExtendableUnknown u3 = {102u, &u2};
	ExtendableUnknown u4 = {142u, &u3};
	Extendable1 e1 = {1u, &u4, arg};

	void *p = &e1;
	while (p != nullptr)
	  {
	    /* You can always cast a pointer to a struct to a pointer to
	       the type of it's first member.  */
	    switch (*static_cast<std::size_t *>(p))
	      {
		case 1:
		  {
		    Extendable1 *e1_p = static_cast<Extendable1 *>(p);
		    p = e1_p->_next;
		    VERIFY (e1_p->_v == arg);
		    break;
		  }
		case 2:
		  {
		    Extendable2 *e2_p = static_cast<Extendable2 *>(p);
		    p = e2_p->_next;
		    VERIFY (std::strcmp(e2_p->_str, "Hello!") == 0);
		    break;
		  }
		case 3:
		  {
		    Extendable3 *e3_p = static_cast<Extendable3 *>(p);
		    p = e3_p->_next;
		    VERIFY (nums == e3_p->_nums);
		    VERIFY (nums_size == e3_p->_size);
		    break;
		  }
		default:
		  {
		    /* Casting to a pointer to OutStruct invokes undefined
		       behavior though, memcpy is required to extract the _next
		       member.  */
		    OutStruct out;
		    std::memcpy(&out, p, sizeof(OutStruct));
		    p = out._next;
		  }
	      }
	  }
	Extendable2 *e2_p = get_extendable<Extendable2, 2u>(&e1);
	VERIFY (e2_p != nullptr);
	e2_out = *e2_p;
      }
      end:
      ok = inner_ok;
    }
  if (!ok)
    return false;
  VERIFY_NON_TARGET (e2_out._id == 2u);
  VERIFY_NON_TARGET (std::strcmp(e2_out._str, "Hello!") == 0);
  return true;
}

int main()
{
  volatile int arg = 42;
  int arr[8] = {0, 1, 2, 3, 4, 5, 6, 7};
  return test_bit_cast(arg)
	 && test_memcpy(arg, arr, 8) ? 0 : 1;
}
