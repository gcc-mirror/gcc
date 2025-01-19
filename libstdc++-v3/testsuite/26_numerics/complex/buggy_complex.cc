// 2000-02-09
// Gabriel Dos Reis <dosreis@cmla.ens-cachan.fr>

// Copyright (C) 1999-2025 Free Software Foundation, Inc.
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


// Test buggy builtin GNU __complex__ support.  This used to cause
// an ICE on some 64-bits plateforms.
// Origin: petter@matfys.lth.se

#include <complex>

int main()
{
    std::complex<double> a(9), b(0, 8), c;

    c = a * b;

    return 0;
}
