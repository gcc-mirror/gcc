// 2009-12-23  Paolo Carlini  <paolo.carlini@oracle.com>
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <algorithm>
#include <deque>
#include <testsuite_hooks.h>

void test01()
{
  bool test __attribute__((unused)) = true;
  using namespace std;

  deque<long> data(200);
  for (unsigned i = 0; i < data.size(); ++i)
    data[i] = i;

  const deque<long> data_1(data.size(), -1);

  for (unsigned i = 0; i < data.size(); i += 2)
    for (unsigned j = i; j <= data.size(); j += 3)
      for (unsigned k = 0; k + (j - i) <= data.size(); k += 5)
	{
	  deque<long> d(data.size(), -1);
	  copy(data.begin() + i, data.begin() + j, d.begin() + k);

	  VERIFY( equal(data.begin() + i, data.begin() + j,
			d.begin() + k) );
	  VERIFY( equal(d.begin(), d.begin() + k, data_1.begin()) );
	  VERIFY( equal(d.begin() + k + (j - i), d.end(), data_1.begin()) );
	}
}

int main()
{
  test01();
  return 0;
}
