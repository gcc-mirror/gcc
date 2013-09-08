// { dg-do run }

/* This test script is part of GDB, the GNU debugger.

   Copyright 1993, 1994, 1997, 1998, 1999, 2003, 2004,
   Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.
   */

// Pls try the following program on virtual functions and try to do print on
//  most of the code in main().  Almost none of them works !

//
// The inheritance structure is:
//
// V : VA VB
// A : (V)
// B : A
// D : AD (V)
// C : (V)
// E : B (V) D C
//

class VA 
{
public:
    int va;
};

class VB
{
public:
    int vb;
    int fvb();
    virtual int vvb();
};

class V : public VA, public VB
{
public:
    int f();
    virtual int vv();
    int w;
};

class A : virtual public V
{
public:
    virtual int f();
private:
    int a;
};

class B : public A
{
public:
    int f();
private:
    int b;
};

class C : public virtual V
{
public:
    int c;
};

class AD
{
public:
    virtual int vg() = 0;
};

class D : public AD, virtual public V
{
public:
    static void s();
    virtual int vg();
    virtual int vd();
    int fd();
    int d;
};

class E : public B, virtual public V, public D, public C
{
public:
    int f();
    int vg();
    int vv();
    int e;
};

D   dd;
D*  ppd = &dd;
AD* pAd = &dd;

A   a;
B   b;
C   c;
D   d;
E   e;
V   v;
VB  vb;


A* 	pAa	= 	&a;
A*	pAe	=	&e;

B*	pBe	=	&e;

D*	pDd	=	&d;
D*	pDe	=	&e;

V*	pVa	=	&a;
V*	pVv	=	&v;
V*	pVe	=	&e;
V*     pVd	=	&d;

AD*	pADe	=	&e;

E*	pEe	=	&e;

VB*     pVB	=	&vb;

void init()
{
	a.vb = 1;
	b.vb = 2;
	c.vb = 3;
	d.vb = 4;
	e.vb = 5;
	v.vb = 6;
	vb.vb = 7;

	d.d	= 1;
	e.d	=  2;
}

extern "C" int printf(const char *, ...);

int all_count = 0;
int failed_count = 0;

#define TEST(EXPR, EXPECTED) \
   ret = EXPR; \
   if (ret != EXPECTED) {\
      printf("Failed %s is %d, should be %d!\n", #EXPR, ret, EXPECTED); \
      failed_count++; } \
   all_count++;

int ret;

void test_calls()
{
	TEST(pAe->f(), 20);
	TEST(pAa->f(), 1);

	TEST(pDe->vg(), 202);
	TEST(pADe->vg(), 202);
	TEST(pDd->vg(), 101);

	TEST(pEe->vvb(), 411);

	TEST(pVB->vvb(), 407);

	TEST(pBe->vvb(), 411);
	TEST(pDe->vvb(), 411);

        TEST(pEe->vd(), 282);
        TEST(pEe->fvb(), 311);
    
        TEST(pEe->D::vg(), 102);
	printf("Did %d tests, of which %d failed.\n", all_count, failed_count);
}
#ifdef usestubs
extern "C" {
  void set_debug_traps();
  void breakpoint();
};
#endif

int main()
{
#ifdef usestubs
   set_debug_traps();
   breakpoint();
#endif
    init();

    e.w = 7;
    e.vb = 11;

    test_calls();
    return 0;
    
}

int A::f() {return 1;}
int B::f() {return 2;}
void D::s() {}
int E::f() {return 20;}
int D::vg() {return 100+d;}
int E::vg() {return 200+d;}
int V::f() {return 600+w;}
int V::vv() {return 400+w;}
int E::vv() {return 450+w;}
int D::fd() {return 250+d;}
int D::vd() {return 280+d;}
int VB::fvb() {return 300+vb;}
int VB::vvb() {return 400+vb;}
