/* Populated with mapped data, validate, mutate, validate again.
   The cases using sets do not mutate.
   Note: Some of the code in here really sucks due to being made to be
   compatible with c++98.  */

#include <vector>
#include <deque>
#include <list>
#include <set>
#include <map>
#if __cplusplus >= 201103L
#include <array>
#include <forward_list>
#include <unordered_set>
#include <unordered_map>
#endif

#include <limits>
#include <iterator>

#include "target-flex-common.h"

template<bool B, class T = void>
struct enable_if {};
 
template<class T>
struct enable_if<true, T> { typedef T type; };

struct identity_func
{
#if __cplusplus < 201103L
  template<typename T>
  T& operator()(T& arg) const BL_NOEXCEPT { return arg; }
  template<typename T>
  T const& operator()(T const& arg) const BL_NOEXCEPT { return arg; }
#else
  template<typename T>
  constexpr T&& operator()(T&& arg) const BL_NOEXCEPT { return std::forward<T>(arg); }
#endif
};

/* Applies projection to the second iterator.  */
template<typename It0, typename It1, typename Proj>
bool validate_sequential_elements(const It0 begin0, const It0 end0,
				  const It1 begin1, const It1 end1,
				  Proj proj) BL_NOEXCEPT
{
  It0 it0 = begin0;
  It1 it1 = begin1;
  for (; it0 != end0; ++it0, ++it1)
    {
      /* Sizes mismatch, don't bother aborting though just fail the test.  */
      if (it1 == end1)
	return false;
      if (*it0 != proj(*it1))
	return false;
    }
  /* Sizes mismatch, do as above.  */
  if (it1 != end1)
    return false;
  return true;
}

template<typename It0, typename It1>
bool validate_sequential_elements(const It0 begin0, const It0 end0,
				  const It1 begin1, const It1 end1) BL_NOEXCEPT
{
  return validate_sequential_elements(begin0, end0, begin1, end1, identity_func());
}

/* Inefficient, but simple.  */
template<typename It, typename OutIt>
void simple_copy(const It begin, const It end, OutIt out) BL_NOEXCEPT
{
  for (It it = begin; it != end; ++it, ++out)
    *out = *it;
}

template<typename It, typename MutateFn>
void simple_mutate(const It begin, const It end, MutateFn mut_fn) BL_NOEXCEPT
{
  for (It it = begin; it != end; ++it)
    *it = mut_fn(*it);
}

template<typename MutationFunc, typename T, std::size_t Size>
bool vector_test(const T (&arr)[Size])
{
  bool ok;
  T out_arr[Size];
  T out_mut_arr[Size];
  #pragma omp target map(from: ok, out_arr[:Size], out_mut_arr[:Size]) \
		     map(to: arr[:Size])
    {
      bool inner_ok = true;
      {
	std::vector<T> vector(arr, arr + Size);
	VERIFY (validate_sequential_elements(vector.begin(), vector.end(),
					     arr, arr + Size));
	simple_copy(vector.begin(), vector.end(), out_arr);
	simple_mutate(vector.begin(), vector.end(), MutationFunc());
	VERIFY (validate_sequential_elements(vector.begin(), vector.end(),
					     arr, arr + Size, MutationFunc()));
	simple_copy(vector.begin(), vector.end(), out_mut_arr);
      }
      end:
      ok = inner_ok;
    }
  if (!ok)
    return false;
  VERIFY_NON_TARGET (validate_sequential_elements(out_arr, out_arr + Size,
						  arr, arr + Size));
  VERIFY_NON_TARGET (validate_sequential_elements(out_mut_arr, out_mut_arr + Size,
						  arr, arr + Size, MutationFunc()));
  return true;
}

template<typename MutationFunc, typename T, std::size_t Size>
bool deque_test(const T (&arr)[Size])
{
  bool ok;
  T out_arr[Size];
  T out_mut_arr[Size];
  #pragma omp target map(from: ok, out_arr[:Size], out_mut_arr[:Size]) \
		     map(to: arr[:Size])
    {
      bool inner_ok = true;
      {
	std::deque<T> deque(arr, arr + Size);
	VERIFY (validate_sequential_elements(deque.begin(), deque.end(),
					     arr, arr + Size));
	simple_copy(deque.begin(), deque.end(), out_arr);
	simple_mutate(deque.begin(), deque.end(), MutationFunc());
	VERIFY (validate_sequential_elements(deque.begin(), deque.end(),
					     arr, arr + Size, MutationFunc()));
	simple_copy(deque.begin(), deque.end(), out_mut_arr);
      }
      end:
      ok = inner_ok;
    }
  if (!ok)
    return false;
  VERIFY_NON_TARGET (validate_sequential_elements(out_arr, out_arr + Size,
						  arr, arr + Size));
  VERIFY_NON_TARGET (validate_sequential_elements(out_mut_arr, out_mut_arr + Size,
						  arr, arr + Size, MutationFunc()));
  return true;
}

template<typename MutationFunc, typename T, std::size_t Size>
bool list_test(const T (&arr)[Size])
{
  bool ok;
  T out_arr[Size];
  T out_mut_arr[Size];
  #pragma omp target map(from: ok, out_arr[:Size], out_mut_arr[:Size]) \
		     map(to: arr[:Size])
    {
      bool inner_ok = true;
      {
	std::list<T> list(arr, arr + Size);
	VERIFY (validate_sequential_elements(list.begin(), list.end(),
					     arr, arr + Size));
	simple_copy(list.begin(), list.end(), out_arr);
	simple_mutate(list.begin(), list.end(), MutationFunc());
	VERIFY (validate_sequential_elements(list.begin(), list.end(),
					     arr, arr + Size, MutationFunc()));
	simple_copy(list.begin(), list.end(), out_mut_arr);
      }
      end:
      ok = inner_ok;
    }
  if (!ok)
    return false;
  VERIFY_NON_TARGET (validate_sequential_elements(out_arr, out_arr + Size,
						  arr, arr + Size));
  VERIFY_NON_TARGET (validate_sequential_elements(out_mut_arr, out_mut_arr + Size,
						  arr, arr + Size, MutationFunc()));
  return true;
}

template<typename T>
const T& get_key(const T& arg) BL_NOEXCEPT
  { return arg; }
template<typename K, typename V>
const K& get_key(const std::pair<K, V>& pair) BL_NOEXCEPT
  { return pair.first; }
template<typename T>
const T& get_value(const T& arg) BL_NOEXCEPT
  { return arg; }
template<typename K, typename V>
const K& get_value(const std::pair<K, V>& pair) BL_NOEXCEPT
  { return pair.second; }

template<typename T>
struct key_type { typedef T type; };
template<typename K, typename V>
struct key_type<std::pair<K, V> > { typedef K type; };

template<typename Proj, typename Container, typename It>
bool validate_associative(const Container& container,
			  const It compare_begin,
			  const It compare_end,
			  Proj proj) BL_NOEXCEPT
{
  const typename Container::const_iterator elem_end = container.end();
  for (It compare_it = compare_begin; compare_it != compare_end; ++compare_it)
    {
      const typename Container::const_iterator elem_it = container.find(get_key(*compare_it));
      VERIFY_NON_TARGET (elem_it != elem_end);
      VERIFY_NON_TARGET (proj(get_value(*compare_it)) == get_value(*elem_it));
    }
  return true;
}

template<typename Container, typename It>
bool validate_associative(const Container& container,
			  const It compare_begin,
			  const It compare_end) BL_NOEXCEPT
{
  return validate_associative(container, compare_begin, compare_end, identity_func());
}

template<typename It, typename MutateFn>
void simple_mutate_map(const It begin, const It end, MutateFn mut_fn) BL_NOEXCEPT
{
  for (It it = begin; it != end; ++it)
    it->second = mut_fn(it->second);
}

template<typename It, typename OutIter>
void simple_copy_unique(const It begin, const It end, OutIter out) BL_NOEXCEPT
{
  /* In case anyone reads this, I want it to be known that I hate c++98.  */
  typedef typename key_type<typename std::iterator_traits<It>::value_type>::type key_t;
  std::set<key_t> already_seen;
  for (It it = begin; it != end; ++it, ++out)
    {
      key_t key = get_key(*it);
      if (already_seen.find(key) != already_seen.end())
	continue;
      already_seen.insert(key);
      *out = *it;
    }
}

template<typename MutationFunc, typename K, typename V, std::size_t Size>
bool map_test(const std::pair<K, V> (&arr)[Size])
{
  std::map<K, V> reference_map(arr, arr + Size);
  bool ok;
  /* Both sizes should be the same.  */
  std::pair<K, V> out_pairs[Size];
  std::size_t out_size;
  std::pair<K, V> out_pairs_mut[Size];
  std::size_t out_size_mut;
  #pragma omp target map(from: ok, out_pairs[:Size], out_size, \
			       out_pairs_mut[:Size], out_size_mut) \
		     map(to: arr[:Size])
    {
      bool inner_ok = true;
      {
	std::vector<std::pair<K, V> > unique_elems;
	simple_copy_unique(arr, arr + Size,
			   std::back_insert_iterator<std::vector<std::pair<K, V> > >(unique_elems));

	std::map<K, V> map(arr, arr + Size);
	VERIFY (validate_associative(map, unique_elems.begin(), unique_elems.end()));
	simple_copy(map.begin(), map.end(), out_pairs);
	out_size = map.size();
	simple_mutate_map(map.begin(), map.end(), MutationFunc());
	VERIFY (validate_associative(map, unique_elems.begin(), unique_elems.end(),
				     MutationFunc()));
	simple_copy(map.begin(), map.end(), out_pairs_mut);
	out_size_mut = map.size();
      }
      end:
      ok = inner_ok;
    }
  if (!ok)
    return false;
  VERIFY_NON_TARGET (out_size == out_size_mut);
  VERIFY_NON_TARGET (validate_associative(reference_map,
					  out_pairs, out_pairs + out_size));
  simple_mutate_map(reference_map.begin(), reference_map.end(), MutationFunc());
  VERIFY_NON_TARGET (validate_associative(reference_map,
					  out_pairs_mut, out_pairs_mut + out_size_mut));
  return true;
}

template<typename T, std::size_t Size>
bool set_test(const T (&arr)[Size])
{
  std::set<T> reference_set(arr, arr + Size);
  bool ok;
  /* Both sizes should be the same.  */
  T out_arr[Size];
  std::size_t out_size;
  #pragma omp target map(from: ok, out_arr[:Size], out_size) \
		     map(to: arr[:Size])
    {
      bool inner_ok = true;
      {
	std::vector<T> unique_elems;
	simple_copy_unique(arr, arr + Size,
			   std::back_insert_iterator<std::vector<T> >(unique_elems));

	std::set<T> set(arr, arr + Size);
	VERIFY (validate_associative(set, unique_elems.begin(), unique_elems.end()));
	simple_copy(set.begin(), set.end(), out_arr);
	out_size = set.size();
	/* Sets can't be mutated, we could create another set with mutated
	   but it gets a little annoying and probably isn't an interesting test.  */
      }
      end:
      ok = inner_ok;
    }
  if (!ok)
    return false;
  VERIFY_NON_TARGET (validate_associative(reference_set,
					  out_arr, out_arr + out_size));
  return true;
}

template<typename Proj, typename Container, typename It>
bool validate_multi_associative(const Container& container,
				const It compare_begin,
				const It compare_end,
				Proj proj) BL_NOEXCEPT
{
  /* Once again, for the poor soul reviewing these, I hate c++98.  */
  typedef typename key_type<typename std::iterator_traits<It>::value_type>::type key_t;
  typedef std::map<key_t, std::size_t> counter_map; 
  counter_map key_count_map;
  for (It it = compare_begin; it != compare_end; ++it)
    {
      const key_t& key = get_key(*it);
      typename counter_map::iterator counter_it
	= key_count_map.find(key);
      if (counter_it != key_count_map.end())
	++counter_it->second;
      else
	key_count_map.insert(std::pair<const key_t, std::size_t>(key, std::size_t(1)));
    }
  const typename Container::const_iterator elem_end = container.end();
  for (It compare_it = compare_begin; compare_it != compare_end; ++compare_it)
    {
      const key_t& key = get_key(*compare_it);
      typename counter_map::iterator count_it = key_count_map.find(key);
      std::size_t key_count = count_it != key_count_map.end() ? count_it->second
							      : std::size_t(0);
      VERIFY_NON_TARGET (key_count > std::size_t(0) && "this will never happen");
      /* This gets tested multiple times but that should be fine.  */
      VERIFY_NON_TARGET (key_count == container.count(key));
      typename Container::const_iterator elem_it = container.find(key);
      /* This will never happen if the previous case passed.  */
      VERIFY_NON_TARGET (elem_it != elem_end);
      bool found_element = false;
      for (; elem_it != elem_end; ++elem_it)
	if (proj(get_value(*compare_it)) == get_value(*elem_it))
	  {
	    found_element = true;
	    break;
	  }
      VERIFY_NON_TARGET (found_element);
    }
  return true;
}

template<typename Container, typename It>
bool validate_multi_associative(const Container& container,
				const It compare_begin,
				const It compare_end) BL_NOEXCEPT
{
  return validate_multi_associative(container, compare_begin, compare_end, identity_func());
}

template<typename MutationFunc, typename K, typename V, std::size_t Size>
bool multimap_test(const std::pair<K, V> (&arr)[Size])
{
  std::multimap<K, V> reference_multimap(arr, arr + Size);
  bool ok;
  std::pair<K, V> out_pairs[Size];
  std::pair<K, V> out_pairs_mut[Size];
  #pragma omp target map(from: ok, out_pairs[:Size], out_pairs_mut[:Size]) \
		     map(to: arr[:Size])
    {
      bool inner_ok = true;
      {
	std::multimap<K, V> multimap(arr, arr + Size);
	VERIFY (validate_multi_associative(multimap, arr, arr + Size));
	simple_copy(multimap.begin(), multimap.end(), out_pairs);
	simple_mutate_map(multimap.begin(), multimap.end(), MutationFunc());
	VERIFY (validate_multi_associative(multimap, arr, arr + Size, MutationFunc()));
	simple_copy(multimap.begin(), multimap.end(), out_pairs_mut);
      }
      end:
      ok = inner_ok;
    }
  if (!ok)
    return false;
  VERIFY_NON_TARGET (validate_multi_associative(reference_multimap,
						out_pairs, out_pairs + Size));
  simple_mutate_map(reference_multimap.begin(), reference_multimap.end(), MutationFunc());
  VERIFY_NON_TARGET (validate_multi_associative(reference_multimap,
						out_pairs_mut, out_pairs_mut + Size));
  return true;
}

template<typename T, std::size_t Size>
bool multiset_test(const T (&arr)[Size])
{
  std::multiset<T> reference_multiset(arr, arr + Size);
  bool ok;
  T out_arr[Size];
  #pragma omp target map(from: ok, out_arr[:Size]) \
		     map(to: arr[:Size])
    {
      bool inner_ok = true;
      {
	std::multiset<T> set(arr, arr + Size);
	VERIFY (validate_multi_associative(set, arr, arr + Size));
	simple_copy(set.begin(), set.end(), out_arr);
	/* Sets can't be mutated, we could create another set with mutated
	   but it gets a little annoying and probably isn't an interesting test.  */
      }
      end:
      ok = inner_ok;
    }
  if (!ok)
    return false;
  VERIFY_NON_TARGET (validate_multi_associative(reference_multiset,
						out_arr, out_arr + Size));
  return true;
}

#if __cplusplus >= 201103L

template<typename MutationFunc, typename T, std::size_t Size>
bool array_test(const T (&arr)[Size])
{
  bool ok;
  T out_arr[Size];
  T out_mut_arr[Size];
  #pragma omp target map(from: ok, out_arr[:Size], out_mut_arr[:Size]) \
		     map(to: arr[:Size])
    {
      bool inner_ok = true;
      {
	std::array<T, Size> std_array{};
	/* Special case for std::array since it can't be initialized
	   with iterators.  */
	{
	  T zero_val = T{};
	  for (auto it = std_array.begin(); it != std_array.end(); ++it)
	    VERIFY (*it == zero_val);
	}
	simple_copy(arr, arr + Size, std_array.begin());
	VERIFY (validate_sequential_elements(std_array.begin(), std_array.end(),
					     arr, arr + Size));
	simple_copy(std_array.begin(), std_array.end(), out_arr);
	simple_mutate(std_array.begin(), std_array.end(), MutationFunc());
	VERIFY (validate_sequential_elements(std_array.begin(), std_array.end(),
					     arr, arr + Size, MutationFunc()));
	simple_copy(std_array.begin(), std_array.end(), out_mut_arr);
      }
      end:
      ok = inner_ok;
    }
  if (!ok)
    return false;
  VERIFY_NON_TARGET (validate_sequential_elements(out_arr, out_arr + Size,
						  arr, arr + Size));
  VERIFY_NON_TARGET (validate_sequential_elements(out_mut_arr, out_mut_arr + Size,
						  arr, arr + Size, MutationFunc()));
  return true;
}

template<typename MutationFunc, typename T, std::size_t Size>
bool forward_list_test(const T (&arr)[Size])
{
  bool ok;
  T out_arr[Size];
  T out_mut_arr[Size];
  #pragma omp target map(from: ok, out_arr[:Size], out_mut_arr[:Size]) \
		     map(to: arr[:Size])
    {
      bool inner_ok = true;
      {
	std::forward_list<T> fwd_list(arr, arr + Size);
	VERIFY (validate_sequential_elements(fwd_list.begin(), fwd_list.end(),
					     arr, arr + Size));
	simple_copy(fwd_list.begin(), fwd_list.end(), out_arr);
	simple_mutate(fwd_list.begin(), fwd_list.end(), MutationFunc());
	VERIFY (validate_sequential_elements(fwd_list.begin(), fwd_list.end(),
					     arr, arr + Size, MutationFunc()));
	simple_copy(fwd_list.begin(), fwd_list.end(), out_mut_arr);
      }
      end:
      ok = inner_ok;
    }
  if (!ok)
    return false;
  VERIFY_NON_TARGET (validate_sequential_elements(out_arr, out_arr + Size,
						  arr, arr + Size));
  VERIFY_NON_TARGET (validate_sequential_elements(out_mut_arr, out_mut_arr + Size,
						  arr, arr + Size, MutationFunc()));
  return true;
}

template<typename MutationFunc, typename K, typename V, std::size_t Size>
bool unordered_map_test(const std::pair<K, V> (&arr)[Size])
{
  std::unordered_map<K, V> reference_map(arr, arr + Size);
  bool ok;
  /* Both sizes should be the same.  */
  std::pair<K, V> out_pairs[Size];
  std::size_t out_size;
  std::pair<K, V> out_pairs_mut[Size];
  std::size_t out_size_mut;
  #pragma omp target map(from: ok, out_pairs[:Size], out_size, \
			       out_pairs_mut[:Size], out_size_mut) \
		     map(to: arr[:Size])
    {
      bool inner_ok = true;
      {
	std::vector<std::pair<K, V> > unique_elems;
	simple_copy_unique(arr, arr + Size,
			   std::back_insert_iterator<std::vector<std::pair<K, V> > >(unique_elems));

	std::unordered_map<K, V> map(arr, arr + Size);
	VERIFY (validate_associative(map, unique_elems.begin(), unique_elems.end()));
	simple_copy(map.begin(), map.end(), out_pairs);
	out_size = map.size();
	simple_mutate_map(map.begin(), map.end(), MutationFunc());
	VERIFY (validate_associative(map, unique_elems.begin(), unique_elems.end(),
				     MutationFunc()));
	simple_copy(map.begin(), map.end(), out_pairs_mut);
	out_size_mut = map.size();
      }
      end:
      ok = inner_ok;
    }
  if (!ok)
    return false;
  VERIFY_NON_TARGET (out_size == out_size_mut);
  VERIFY_NON_TARGET (validate_associative(reference_map,
					  out_pairs, out_pairs + out_size));
  simple_mutate_map(reference_map.begin(), reference_map.end(), MutationFunc());
  VERIFY_NON_TARGET (validate_associative(reference_map,
					  out_pairs_mut, out_pairs_mut + out_size_mut));
  return true;
}

template<typename T, std::size_t Size>
bool unordered_set_test(const T (&arr)[Size])
{
  std::unordered_set<T> reference_set(arr, arr + Size);
  bool ok;
  /* Both sizes should be the same.  */
  T out_arr[Size];
  std::size_t out_size;
  #pragma omp target map(from: ok, out_arr[:Size], out_size) \
		     map(to: arr[:Size])
    {
      bool inner_ok = true;
      {
	std::vector<T> unique_elems;
	simple_copy_unique(arr, arr + Size,
			   std::back_insert_iterator<std::vector<T> >(unique_elems));

	std::unordered_set<T> set(arr, arr + Size);
	VERIFY (validate_associative(set, unique_elems.begin(), unique_elems.end()));
	simple_copy(set.begin(), set.end(), out_arr);
	out_size = set.size();
	/* Sets can't be mutated, we could create another set with mutated
	   but it gets a little annoying and probably isn't an interesting test.  */
      }
      end:
      ok = inner_ok;
    }
  if (!ok)
    return false;
  VERIFY_NON_TARGET (validate_associative(reference_set,
					  out_arr, out_arr + out_size));
  return true;
}

template<typename MutationFunc, typename K, typename V, std::size_t Size>
bool unordered_multimap_test(const std::pair<K, V> (&arr)[Size])
{
  std::unordered_multimap<K, V> reference_multimap(arr, arr + Size);
  bool ok;
  std::pair<K, V> out_pairs[Size];
  std::pair<K, V> out_pairs_mut[Size];
  #pragma omp target map(from: ok, out_pairs[:Size], out_pairs_mut[:Size]) \
		     map(to: arr[:Size])
    {
      bool inner_ok = true;
      {
	std::unordered_multimap<K, V> multimap(arr, arr + Size);
	VERIFY (validate_multi_associative(multimap, arr, arr + Size));
	simple_copy(multimap.begin(), multimap.end(), out_pairs);
	simple_mutate_map(multimap.begin(), multimap.end(), MutationFunc());
	VERIFY (validate_multi_associative(multimap, arr, arr + Size, MutationFunc()));
	simple_copy(multimap.begin(), multimap.end(), out_pairs_mut);
      }
      end:
      ok = inner_ok;
    }
  if (!ok)
    return false;
  VERIFY_NON_TARGET (validate_multi_associative(reference_multimap,
						out_pairs, out_pairs + Size));
  simple_mutate_map(reference_multimap.begin(), reference_multimap.end(), MutationFunc());
  VERIFY_NON_TARGET (validate_multi_associative(reference_multimap,
						out_pairs_mut, out_pairs_mut + Size));
  return true;
}

template<typename T, std::size_t Size>
bool unordered_multiset_test(const T (&arr)[Size])
{
  std::unordered_multiset<T> reference_multiset(arr, arr + Size);
  bool ok;
  T out_arr[Size];
  #pragma omp target map(from: ok, out_arr[:Size]) \
		     map(to: arr[:Size])
    {
      bool inner_ok = true;
      {
	std::unordered_multiset<T> set(arr, arr + Size);
	VERIFY (validate_multi_associative(set, arr, arr + Size));
	simple_copy(set.begin(), set.end(), out_arr);
	/* Sets can't be mutated, we could create another set with mutated
	   but it gets a little annoying and probably isn't an interesting test.  */
      }
      end:
      ok = inner_ok;
    }
  if (!ok)
    return false;
  VERIFY_NON_TARGET (validate_multi_associative(reference_multiset,
						out_arr, out_arr + Size));
  return true;
}

#else
template<typename, typename T, std::size_t Size> bool array_test(const T (&arr)[Size]) { return true; }
template<typename, typename T, std::size_t Size> bool forward_list_test(const T (&arr)[Size]) { return true; }
template<typename, typename T, std::size_t Size> bool unordered_map_test(const T (&arr)[Size]) { return true; }
template<typename T, std::size_t Size> bool unordered_set_test(const T (&arr)[Size]) { return true; }
template<typename, typename T, std::size_t Size> bool unordered_multimap_test(const T (&arr)[Size]) { return true; }
template<typename T, std::size_t Size> bool unordered_multiset_test(const T (&arr)[Size]) { return true; }
#endif

/* This clamps to the maximum value to guard against overflowing,
   assuming std::numeric_limits is specialized for T.  */
struct multiply_by_2
{
  template<typename T>
  typename enable_if<std::numeric_limits<T>::is_specialized, T>::type
  operator()(T arg) const BL_NOEXCEPT {
    if (arg < static_cast<T>(0))
      {
	if (std::numeric_limits<T>::min() / static_cast<T>(2) >= arg)
	  return std::numeric_limits<T>::min();
      }
    else
      {
	if (std::numeric_limits<T>::max() / static_cast<T>(2) <= arg)
	  return std::numeric_limits<T>::max();
      }
    return arg * 2;
  }
  template<typename T>
  typename enable_if<!std::numeric_limits<T>::is_specialized, T>::type
  operator()(T arg) const BL_NOEXCEPT {
    return arg * 2;
  }
};

int main()
{
  int data[8] = {0, 1, 2, 3, 4, 5, 6, 7};
  std::pair<int, int> pairs[10] = {std::pair<int, int>( 1,  2),
				   std::pair<int, int>( 2,  4),
				   std::pair<int, int>( 3,  6),
				   std::pair<int, int>( 4,  8),
				   std::pair<int, int>( 5, 10),
				   std::pair<int, int>( 6, 12),
				   std::pair<int, int>( 7, 14),
				   std::pair<int, int>( 8, 16),
				   std::pair<int, int>( 9, 18),
				   std::pair<int, int>(10, 20)};
  const bool vec_res                = vector_test<multiply_by_2>(data);
  const bool deque_res              = deque_test<multiply_by_2>(data);
  const bool list_res               = list_test<multiply_by_2>(data);
  const bool map_res                = map_test<multiply_by_2>(pairs);
  const bool set_res                = set_test(data);
  const bool multimap_res           = multimap_test<multiply_by_2>(pairs);
  const bool multiset_res           = multiset_test(data);
  const bool array_res              = array_test<multiply_by_2>(data);
  const bool forward_list_res       = forward_list_test<multiply_by_2>(data);
  const bool unordered_map_res      = unordered_map_test<multiply_by_2>(pairs);
  const bool unordered_set_res      = unordered_set_test(data);
  const bool unordered_multimap_res = unordered_multimap_test<multiply_by_2>(pairs);
  const bool unordered_multiset_res = unordered_multiset_test(data);
  std::printf("vector            : %s\n", vec_res                ? "PASS" : "FAIL");
  std::printf("deque             : %s\n", deque_res              ? "PASS" : "FAIL");
  std::printf("list              : %s\n", list_res               ? "PASS" : "FAIL");
  std::printf("map               : %s\n", map_res                ? "PASS" : "FAIL");
  std::printf("set               : %s\n", set_res                ? "PASS" : "FAIL");
  std::printf("multimap          : %s\n", multimap_res           ? "PASS" : "FAIL");
  std::printf("multiset          : %s\n", multiset_res           ? "PASS" : "FAIL");
  std::printf("array             : %s\n", array_res              ? "PASS" : "FAIL");
  std::printf("forward_list      : %s\n", forward_list_res       ? "PASS" : "FAIL");
  std::printf("unordered_map     : %s\n", unordered_map_res      ? "PASS" : "FAIL");
  std::printf("unordered_set     : %s\n", unordered_set_res      ? "PASS" : "FAIL");
  std::printf("unordered_multimap: %s\n", unordered_multimap_res ? "PASS" : "FAIL");
  std::printf("unordered_multiset: %s\n", unordered_multiset_res ? "PASS" : "FAIL");
  const bool ok = vec_res
		  && deque_res
		  && list_res
		  && map_res
		  && set_res
		  && multimap_res
		  && multiset_res
		  && array_res
		  && forward_list_res
		  && unordered_map_res
		  && unordered_set_res
		  && unordered_multimap_res
		  && unordered_multiset_res;
  return ok ? 0 : 1;
}
