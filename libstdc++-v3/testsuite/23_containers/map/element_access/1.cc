// 2005-08-29  Paolo Carlini  <pcarlini@suse.de>
//
// Copyright (C) 2005 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

#include <map>
#include <stdexcept>
#include <testsuite_hooks.h>

// libstdc++/23578
void test01() 
{ 
  bool test __attribute__((unused)) = true;
  typedef std::map<int, double> map_type;

  {
    map_type m;
    m[0] = 1.5;

    double& rd = m.at(0);
    VERIFY( rd == 1.5 );
    try
      {
	m.at(1);
      }
    catch(std::out_of_range& obj)
      {
	// Expected.
      }
    catch(...)
      {
	// Failed.
	throw;
      }    
  }

  {
    map_type m;
    m[1] = 2.5;
    const map_type cm(m);

    const double& crd = cm.at(1);
    VERIFY( crd == 2.5 );
    try
      {
	cm.at(0);
      }
    catch(std::out_of_range& obj)
      {
	// Expected.
      }
    catch(...)
      {
	// Failed.
	throw;
      }    
  }
}

int main()
{
  test01();
  return 0;
}
