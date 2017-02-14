// 2007-01-11  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2007-2017 Free Software Foundation, Inc.
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


#include <valarray>
#include <testsuite_hooks.h>

bool
comp_vala(const std::valarray<int>& v1, const std::valarray<int>& v2)
{
  if (v1.size() == v2.size())
    {
      for (size_t i = 0; i < v1.size(); ++i)
	if (v1[i] != v2[i])
	  return false;
      return true;
    }
  return false;
}

void
init_vala(std::valarray<int>& v, size_t first, size_t last, int val)
{
  for (size_t i = first; i <= last; ++i)
    v[i] = val++;
}

// libstdc++/30416
void test01()
{
  using namespace std;

  // shift
  valarray<int> v1;
  valarray<int> v1_ris(v1.shift(0));
  VERIFY( comp_vala(v1, v1_ris) );
  
  valarray<int> v2;
  valarray<int> v2_ris(v2.shift(10));
  VERIFY( comp_vala(v2, v2_ris) );
  
  valarray<int> v3;
  valarray<int> v3_ris(v3.shift(-10));
  VERIFY( comp_vala(v3, v3_ris) );
  
  valarray<int> v4(10);
  valarray<int> v4_ris(v4.shift(0));
  VERIFY( comp_vala(v4, v4_ris) );
  
  valarray<int> v5(10);
  init_vala(v5, 0, 9, 1);
  valarray<int> v5_ref(10);  
  
  valarray<int> v5_ris(v5.shift(16));
  VERIFY( comp_vala(v5_ris, v5_ref) );
  
  valarray<int> v6(10);
  init_vala(v6, 0, 9, 1);
  valarray<int> v6_ref(10);
  
  valarray<int> v6_ris(v6.shift(-16));
  VERIFY( comp_vala(v6_ris, v6_ref) );

  valarray<int> v7(10);
  init_vala(v7, 0, 9, 1);
  valarray<int> v7_ref(10);  
  
  valarray<int> v7_ris(v7.shift(10));
  VERIFY( comp_vala(v7_ris, v7_ref) );
  
  valarray<int> v8(10);
  init_vala(v8, 0, 9, 1);
  valarray<int> v8_ref(10);
  
  valarray<int> v8_ris(v8.shift(-10));
  VERIFY( comp_vala(v8_ris, v8_ref) );

  valarray<int> v9(10);
  init_vala(v9, 0, 9, 1);
  valarray<int> v9_ref(10);  
  init_vala(v9_ref, 0, 3, 7);
  
  valarray<int> v9_ris(v9.shift(6));
  VERIFY( comp_vala(v9_ris, v9_ref) );
  
  valarray<int> v10(10);
  init_vala(v10, 0, 9, 1);
  valarray<int> v10_ref(10);
  init_vala(v10_ref, 6, 9, 1);

  valarray<int> v10_ris(v10.shift(-6));
  VERIFY( comp_vala(v10_ris, v10_ref) );

  // cshift
  valarray<int> v11;
  valarray<int> v11_ris(v11.cshift(0));
  VERIFY( comp_vala(v11, v11_ris) );
  
  valarray<int> v12;
  valarray<int> v12_ris(v12.cshift(10));
  VERIFY( comp_vala(v12, v12_ris) );
  
  valarray<int> v13;
  valarray<int> v13_ris(v13.cshift(-10));
  VERIFY( comp_vala(v13, v13_ris) );
  
  valarray<int> v14(10);
  valarray<int> v14_ris(v14.cshift(0));
  VERIFY( comp_vala(v14, v14_ris) );
  
  valarray<int> v15(10);
  init_vala(v15, 0, 9, 1);
  valarray<int> v15_ref(10);
  init_vala(v15_ref, 0, 3, 7);
  init_vala(v15_ref, 4, 9, 1);
  
  valarray<int> v15_ris(v15.cshift(16));
  VERIFY( comp_vala(v15_ris, v15_ref) );

  valarray<int> v16(10);
  init_vala(v16, 0, 9, 1);
  valarray<int> v16_ref(10);
  init_vala(v16_ref, 0, 5, 5);
  init_vala(v16_ref, 6, 9, 1);
  
  valarray<int> v16_ris(v16.cshift(-16));
  VERIFY( comp_vala(v16_ris, v16_ref) );

  valarray<int> v17(10);
  init_vala(v17, 0, 9, 1);
  
  valarray<int> v17_ris(v15.cshift(10));
  VERIFY( comp_vala(v17, v17_ris) );
  
  valarray<int> v18(10);
  init_vala(v18, 0, 9, 1);
  
  valarray<int> v18_ris(v18.cshift(-10));
  VERIFY( comp_vala(v18, v18_ris) );

  valarray<int> v19(10);
  init_vala(v19, 0, 9, 1);
  valarray<int> v19_ref(10);
  init_vala(v19_ref, 0, 3, 7);
  init_vala(v19_ref, 4, 9, 1);
  
  valarray<int> v19_ris(v15.cshift(6));
  VERIFY( comp_vala(v19_ris, v19_ref) );
  
  valarray<int> v20(10);
  init_vala(v20, 0, 9, 1);
  valarray<int> v20_ref(10);
  init_vala(v20_ref, 0, 5, 5);
  init_vala(v20_ref, 6, 9, 1);
  
  valarray<int> v20_ris(v20.cshift(-6));
  VERIFY( comp_vala(v20_ris, v20_ref) );
}

int main()
{
  test01();
  return 0;
} 
