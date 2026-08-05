/* Container adaptors in target region.
   Does not test comparison operators other than equality to allow these tests
   to be generalized to arbitrary input data.  */

#include <algorithm>
#include <cstdio>
#include <deque>
#include <queue>
#include <stack>
#include <vector>

#include "target-flex-common.h"

template<typename T, std::size_t Size>
bool test_stack(T (&arr)[Size])
{
  bool ok;
  #pragma omp target map(from: ok) map(to: arr[:Size])
    {
      bool inner_ok = true;
      const std::size_t half_size = Size / 2;
      const T first_element = arr[0];
      const T middle_element = arr[half_size - 1];
      const T last_element = arr[Size - 1];
      typedef std::stack<T, std::vector<T> > stack_type;
      stack_type stack;
      VERIFY (stack.empty());
      VERIFY (stack.size() == 0);
      {
	/* Do half with push.  */
	std::size_t idx = 0;
	for (; idx < half_size; ++idx)
	  {
	    stack.push(arr[idx]);
	    VERIFY (stack.top() == arr[idx]);
	  }
	VERIFY (stack.size() == half_size);
	VERIFY (static_cast<const stack_type&>(stack).size() == half_size);
	for (; idx < Size; ++idx)
	  {
	    #if __cplusplus >= 201103L
	      /* Do the rest with emplace if C++11 or higher.  */
	      stack.emplace(arr[idx]);
	    #else
	      /* Otherwise just use push again.  */
	      stack.push(arr[idx]);
	    #endif
	    VERIFY (stack.top() == arr[idx]);
	  }
	VERIFY (stack.size() == Size);
	VERIFY (static_cast<const stack_type&>(stack).size() == Size);

	const stack_type stack_orig = stack_type(std::vector<T>(arr, arr + Size));
	VERIFY (stack == stack_orig);
	/* References are contained in their own scope so we don't accidently
	   add tests referencing them after they have been invalidated.  */
	{
	  const T& const_top = static_cast<const stack_type&>(stack).top();
	  VERIFY (const_top == last_element);
	  T& mutable_top = stack.top();
	  mutable_top = first_element;
	  VERIFY (const_top == first_element);
	}
	/* Will only compare inequal if the first and last elements are different.  */
	VERIFY (first_element != last_element || stack != stack_orig);
	for (std::size_t count = Size - half_size; count != 0; --count)
	  stack.pop();
	VERIFY (stack.top() == middle_element);
	const stack_type stack_half_orig = stack_type(std::vector<T>(arr, arr + half_size));
	VERIFY (stack == stack_half_orig);
      }
      end:
      ok = inner_ok;
    }
  return ok;
}

template<typename T, std::size_t Size>
bool test_queue(T (&arr)[Size])
{
  bool ok;
  #pragma omp target map(from: ok) map(to: arr[:Size])
    {
      bool inner_ok = true;
      const std::size_t half_size = Size / 2;
      const T first_element = arr[0];
      const T last_element = arr[Size - 1];
      typedef std::queue<T, std::deque<T> > queue_type;
      queue_type queue;
      VERIFY (queue.empty());
      VERIFY (queue.size() == 0);
      {
	/* Do half with push.  */
	std::size_t idx = 0;
	for (; idx < half_size; ++idx)
	  {
	    queue.push(arr[idx]);
	    VERIFY (queue.back() == arr[idx]);
	    VERIFY (queue.front() == first_element);
	  }
	VERIFY (queue.size() == half_size);
	VERIFY (static_cast<const queue_type&>(queue).size() == half_size);
	for (; idx < Size; ++idx)
	  {
	    #if __cplusplus >= 201103L
	      /* Do the rest with emplace if C++11 or higher.  */
	      queue.emplace(arr[idx]);
	    #else
	      /* Otherwise just use push again.  */
	      queue.push(arr[idx]);
	    #endif
	    VERIFY (queue.back() == arr[idx]);
	  }
	VERIFY (queue.size() == Size);
	VERIFY (static_cast<const queue_type&>(queue).size() == Size);

	const queue_type queue_orig = queue_type(std::deque<T>(arr, arr + Size));
	VERIFY (queue == queue_orig);

	/* References are contained in their own scope so we don't accidently
	   add tests referencing them after they have been invalidated.  */
	{
	  const T& const_front = static_cast<const queue_type&>(queue).front();
	  VERIFY (const_front == first_element);
	  T& mutable_front = queue.front();

	  const T& const_back = static_cast<const queue_type&>(queue).back();
	  VERIFY (const_back == last_element);
	  T& mutable_back = queue.back();
	  {
	    using std::swap;
	    swap(mutable_front, mutable_back);
	  }
	  VERIFY (const_front == last_element);
	  VERIFY (const_back == first_element);
	  /* Will only compare inequal if the first and last elements are different.  */
	  VERIFY (first_element != last_element || queue != queue_orig);
	  /* Return the last element to normal for the next comparison.  */
	  mutable_back = last_element;
	}

	const T middle_element = arr[half_size];
	for (std::size_t count = Size - half_size; count != 0; --count)
	  queue.pop();
	VERIFY (queue.front() == middle_element);
	const queue_type queue_upper_half = queue_type(std::deque<T>(arr + half_size, arr + Size));
	VERIFY (queue == queue_upper_half);
      }
      end:
      ok = inner_ok;
    }
  return ok;
}

template<typename T, std::size_t Size>
bool test_priority_queue(T (&arr)[Size], const T min_value, const T max_value)
{
  bool ok;
  #pragma omp target map(from: ok) map(to: arr[:Size])
    {
      bool inner_ok = true;
      typedef std::priority_queue<T, std::vector<T> > priority_queue_type;
      {
	priority_queue_type pqueue;
	VERIFY (pqueue.empty());
	VERIFY (pqueue.size() == 0);
      }
      {
	priority_queue_type pqueue(arr, arr + Size);
	VERIFY (!pqueue.empty());
	VERIFY (pqueue.size() == Size);
	VERIFY (static_cast<const priority_queue_type&>(pqueue).size() == Size);

	const T old_max = pqueue.top();

	#if __cplusplus >= 201103L
	  pqueue.emplace(max_value);
	#else
	  pqueue.push(max_value);
	#endif
	VERIFY (pqueue.top() == max_value);
	pqueue.pop();
	VERIFY (pqueue.top() == old_max);
	pqueue.push(min_value);
	VERIFY (pqueue.top() == old_max);
	pqueue.push(max_value);
	VERIFY (pqueue.top() == max_value);
	pqueue.pop();
	VERIFY (pqueue.top() == old_max);
	VERIFY (pqueue.size() == Size + 1);

	for (std::size_t count = Size; count != 0; --count)
	  pqueue.pop();
	VERIFY (pqueue.size() == 1);
	VERIFY (pqueue.top() == min_value);
      }
      end:
      ok = inner_ok;
    }
  return ok;
}

int main()
{
  int arr[10] = {0,1,2,3,4,5,6,7,8,9};

  return test_stack(arr)
	 && test_queue(arr)
	 && test_priority_queue(arr, 0, 1000) ? 0 : 1;
}
