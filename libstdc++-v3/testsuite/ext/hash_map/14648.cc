// Copyright (C) 2004 Free Software Foundation
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
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

#include <iostream>
#include <ext/hash_map> 
#include <ext/rope> 

// libstdc++/14648
void test01() 
{ 
  using namespace std; 
  using namespace __gnu_cxx;

  typedef hash_map<char, crope, hash<char>, equal_to<char> > maptype; 
  maptype m; 
  m['l'] = "50";
  m['x'] = "10";
  cout << "m['x'] = " << m['x'] << endl; 
}  

int main()
{
  test01();
  return 0;
}
