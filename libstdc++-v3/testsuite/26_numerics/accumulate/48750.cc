// Copyright (C) 2011-2013 Free Software Foundation, Inc.
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

#include <vector> 
#include <numeric>

class NaturalParameters
{
public:

  NaturalParameters()
  : m_data(2)
  {  }

  std::vector<double>::const_iterator
  begin() const
  { return m_data.begin(); }

  std::vector<double>::const_iterator
  end() const
  { return m_data.begin(); }

  NaturalParameters& 
  operator+=(const NaturalParameters&)
  { return *this; }

private:
  std::vector<double> m_data;
};

inline
NaturalParameters
operator+(const NaturalParameters& a, const NaturalParameters& b)
{
  NaturalParameters tmp = a;
  return tmp += b;
}

// libstdc++/48750
void test01()
{
  // Used to fail in parallel-mode with a segfault.
  for (std::size_t i = 0; i < 1000; ++i)
    {
      std::vector<NaturalParameters> ChildrenNP(1000);
      NaturalParameters init;
      NaturalParameters NP = std::accumulate(ChildrenNP.begin(),
					     ChildrenNP.end(), init); 
    }
}

int main()
{
  test01();
  return 0;    
}
