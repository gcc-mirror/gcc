// Copyright (C) 2003 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

#include <cmath>
#include <cstdlib>

#include <vector>
#include <algorithm>
#include <list>
#include <deque>
#include <set>

#include <sstream>
#include <testsuite_performance.h>

using namespace std;

typedef double element_t;
typedef void(*test)(element_t*, element_t*);

void array_test(element_t* first, element_t* last)
{
  element_t* array = new element_t[last - first];
  copy(first, last, array);
  sort(array, array + (last - first));
  unique(array, array + (last - first));
  delete [] array;	  
}

void vector_pointer_test(element_t* first, element_t* last)
{ 
  vector<element_t> container(first, last);
  sort(&*container.begin(), &*container.end());
  unique(&*container.begin(), &*container.end());
}

void vector_iterator_test(element_t* first, element_t* last)
{
  vector<element_t> container(first, last);
  sort(container.begin(), container.end());
  unique(container.begin(), container.end());
}

void deque_test(element_t* first, element_t* last)
{  
  deque<element_t> container(first, last);
  copy(first, last, container.begin()); 
  sort(container.begin(), container.end());
  unique(container.begin(), container.end());
}
    
void list_test(element_t* first, element_t* last)
{ 
  list<element_t> container(first, last);
  container.sort();
  container.unique();
}
 
void set_test(element_t* first, element_t* last)
{ set<element_t> container(first, last); }

void multiset_test(element_t* first, element_t* last)
{
  multiset<element_t> container(first, last);
  typedef multiset<element_t>::iterator iterator;
  {
    iterator first = container.begin();
    iterator last = container.end();
    
    while (first != last)
      {
	iterator next = first;
	if (++next == last) break;
	if (*first == *next)
	  container.erase(next);
	else
	  ++first;
      }
  }
}

double logtwo(double x)
{ return log(x)/log(2.0); }

int number_of_tests(int size)
{
  const double n = size;
  const double largest_n = 1000000;
  return int(floor((largest_n * logtwo(largest_n))
		   / (n * logtwo(n))));
}

void initialize(element_t* first, element_t* last)
{
  element_t value = 0.0;
  while (first != last)
    {
      *first++ = value;
      value += 1.;
    }
}

void run_tests(int size, const test* tests, const char** names,
	       int ntests)
{
  using namespace __gnu_test;
  time_counter time;
  resource_counter resource;

  const int n = number_of_tests(size);
  const size_t length = 2 * size;

  // make a random test set of the chosen size:
  vector<element_t> buf(length);
  element_t* buffer = &buf[0];
  element_t* buffer_end = &buf[length];
  initialize(buffer, buffer + size);	 // elements
  initialize(buffer + size, buffer_end); // duplicate elements
  random_shuffle(buffer, buffer_end);

  // test the containers:
  ostringstream oss;
  oss << "size = " << size << " :";
  report_header(__FILE__, oss.str());
  for (int i = 0; i < ntests; ++i)
    {
      start_counters(time, resource);
      for (int j = 0; j < n; ++j)
	tests[i](buffer, buffer_end);
      stop_counters(time, resource);
      report_performance(__FILE__, names[i], time, resource);
      clear_counters(time, resource);
    }
}

int main()
{
  const test tests[] = { &array_test, &vector_pointer_test,
			 &vector_iterator_test, &deque_test,
			 &list_test, &set_test, &multiset_test };
  const int ntests = sizeof(tests) / sizeof(test);
  const char* names[ntests] = { "array", "vector (pointer)",
				"vector (iterator)", "deque",
				"list", "set", "multiset" };

  const int sizes[] = {100, 1000, 10000, 100000};
  for (int i = 0; i < sizeof(sizes) / sizeof(int); ++i)
    run_tests(sizes[i], tests, names, ntests);

  return 0;
}
