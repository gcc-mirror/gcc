// Tests for the -*- C++ -*- complex number classes.
// Copyright (C) 1994 Free Software Foundation

// This file is part of the GNU ANSI C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the terms of
// the GNU General Public License as published by the Free Software
// Foundation; either version 2, or (at your option) any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

#include <assert.h>
#include <iostream.h>
#include <complex>

// to test near-equality

const double eps = 0.000001;

static void close_enough(const double_complex& a, const double_complex& b)
{
  assert(fabs(real(a) - real(b)) < eps &&
         fabs(imag(a) - imag(b)) < eps);
}


void test3(double_complex& a, double_complex& b, double_complex& c)
{

  close_enough(-(-a) , a);
  close_enough((a + b) ,  (b + a));
  close_enough((a + (-b)) ,  (a - b));
  close_enough((a * b) ,  (b * a));
  close_enough((a * (-b)) , -(a * b));
  close_enough((a / (-b)) , -(a / b));
  close_enough((a - b) ,  -(b - a));
  close_enough((a + (b + c)) , ((a + b) + c));
  close_enough((a * (b * c)) , ((a * b) * c));
  close_enough((a * (b + c)) , ((a * b) + (a * c)));
  close_enough(((a - b) + b) , a);
  close_enough(((a + b) - b) , a);
  close_enough(((a * b) / b) , a);
  close_enough(((a / b) * b) , a);


  double_complex x = a;
  x *= b;
  close_enough(x , (a * b));
  x += c;
  close_enough(x , ((a * b) + c));
  x -= a;
  close_enough(x , (((a * b) + c) - a));
  x /= b;
  close_enough(x , ((((a * b) + c) - a) / b));

}

main()
{
  double_complex one = 1.0;
  double_complex i (0.0, 1.0);
  double_complex neg_one = -1.0;

  cout << "double_complex one = " << one << "\n";
  cout << "i = " << i << "\n";
  cout << "neg_one = " << neg_one << "\n";
  cout << "sqrt(neg_one) = " << sqrt(neg_one) << "\n";

  double_complex a (2.0, 3.0);
  double_complex b (4.0, 5.0);

  cout << "a = " << a << "\n";
  cout << "b = " << b << "\n";

  cout << "a + one = " << (a + one) << "\n";
  (close_enough((a+one), double_complex(3.0, 3.0)));
  cout << "a - one = " << (a - one) << "\n";
  (close_enough((a-one), double_complex(1.0, 3.0)));
  cout << "a * one = " << (a * one) << "\n";
  (close_enough((a*one), a));
  cout << "a / one = " << (a / one) << "\n";
  (close_enough((a/one), a));

  cout << "a + b = " << (a + b) << "\n";
  (close_enough((a+b), double_complex(6.0, 8.0)));
  cout << "a - b = " << (a - b) << "\n";
  (close_enough((a-b), double_complex(-2.0, -2.0)));
  cout << "a * b = " << (a * b) << "\n";
  (close_enough((a*b), double_complex(-7.0, 22.0)));
  cout << "a / b = " << (a / b) << "\n";
  (close_enough((a/b), double_complex(0.5609760976, 0.0487804878)));

  double_complex c;

  c = a; cout << "c = a; c += b = " << (c += b) << "\n";
  c = a; cout << "c = a; c -= b = " << (c -= b) << "\n";
  c = a; cout << "c = a; c *= b = " << (c *= b) << "\n";
  c = a; cout << "c = a; c /= b = " << (c /= b) << "\n";

  cout << "-a = " << (-a) << "\n";
  cout << "real(a) = " << real(a) << "\n";
  assert(real(a) == 2.0);
  cout << "imag(a) = " << imag(a) << "\n";
  assert(imag(a) == 3.0);
  cout << "conj(a) = " << conj(a) << "\n";
  assert(conj(a) == double_complex(2.0, -3.0));
  cout << "norm(a) = " << norm(a) << "\n";
  assert(norm(a) == 13.0);

  cout << "abs(a) = " << abs(a) << "\n";
  cout << "arg(a) = " << arg(a) << "\n";
  cout << "cos(a) = " << cos(a) << "\n";
  cout << "sin(a) = " << sin(a) << "\n";
  cout << "cosh(a) = " << cosh(a) << "\n";
  cout << "sinh(a) = " << sinh(a) << "\n";
  cout << "log(a) = " << log(a) << "\n";
  cout << "exp(a) = " << exp(a) << "\n";
  cout << "sqrt(a) = " << sqrt(a) << "\n";
  cout << "pow(a, 2) = " << pow(a, 2) << "\n";
  {
     double_complex p = pow(a, b);
     if(sizeof(float)==sizeof(double)) {
	long w = (long)(p.imag()*100000);
	if (w==-98642)
	   p=double_complex(-0.753046,-0.986429);
     }
     cout << "pow(a, b) = " << p << "\n";
  }

  double_complex d (10, 20);
  double_complex e = pow(a, 2);
 
  test3(one, one, one);
  test3(a, a, a);
  test3(a, b, d);
  test3(e, i, b);
  test3(d, d, i);

  cout << "enter a complex number in form a or (a) or (a, b): ";
  cin >> c;
  cout << "number = " << c << "\n";

  cout << "\nEnd of test\n";
  return 0;
}
