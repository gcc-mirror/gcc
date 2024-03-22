// Copyright (C) 2018-2024 Free Software Foundation, Inc.
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

// { dg-do run }
// { dg-xfail-run-if "AIX operator new" { powerpc-ibm-aix* } }
// { dg-require-effective-target hosted }

#include <new>
#include <stdlib.h>
#include <testsuite_hooks.h>

// PR libstdc++/68210

struct MyBadAlloc: std::bad_alloc { };

static bool new_fail;
static bool bad_alloc_thrown;
static unsigned new_called;
static unsigned delete_called;
static unsigned new_vec_called;
static unsigned delete_vec_called;
static unsigned new_handler_called;

static void new_handler ()
{
    if (new_handler_called++)
        throw MyBadAlloc ();
}

void* operator new (size_t n)
{
    static size_t cntr;

    ++new_called;

    for ( ; ; ) {
        if (void *p = new_fail ? 0 : malloc (n + sizeof n)) {
            *static_cast<size_t*>(p) = ++cntr;
            return static_cast<size_t*>(p) + 1;
        }

        if (std::new_handler h = std::set_new_handler (0)) {
            std::set_new_handler (h);
            h ();
        }
        else {
            bad_alloc_thrown = true;
            throw MyBadAlloc ();
        }
    }
}

#if __cplusplus >= 201103L
#define NOEXCEPT noexcept
#else
#define NOEXCEPT
#endif

void operator delete (void *p) NOEXCEPT
{
    ++delete_called;
    if (p)
        free (static_cast<size_t*>(p) - 1);
}

void* operator new[] (size_t n)
{
    ++new_vec_called;
    return operator new(n);
}

void operator delete[] (void *p) NOEXCEPT
{
    ++delete_vec_called;
    operator delete(p);
}

#if __cplusplus >= 201402L
void operator delete (void *p, std::size_t) noexcept
{
  ::operator delete(p);
}
void operator delete[] (void *p, std::size_t) noexcept
{
  ::operator delete[](p);
}
#endif

void init()
{
    new_fail = false;
    new_called = 0;
    delete_called = 0;
    new_vec_called = 0;
    delete_vec_called = 0;
    new_handler_called = 0;
    std::set_new_handler (0);
}

void
test01()
{
    init ();

    void *p = operator new (1, std::nothrow);

    VERIFY (p != 0);
    VERIFY (1 == new_called);
    VERIFY (0 == new_handler_called);
    VERIFY (!bad_alloc_thrown);

    operator delete (p, std::nothrow);
    VERIFY( 1 == delete_called );

    new_fail = true;
    p = operator new (1, std::nothrow);

    VERIFY (0 == p);
    VERIFY (2 == new_called);
    VERIFY (0 == new_handler_called);
    VERIFY (bad_alloc_thrown);

    new_fail = true;
    bad_alloc_thrown = false;
    std::set_new_handler (new_handler);
    p = operator new (1, std::nothrow);

    VERIFY (0 == p);
    VERIFY (3 == new_called);
    VERIFY (2 == new_handler_called);
    VERIFY (!bad_alloc_thrown);
}

void
test02()
{
    init ();

    void *p = operator new[] (1, std::nothrow);

    VERIFY (p != 0);
    VERIFY (1 == new_called);
    VERIFY (1 == new_vec_called);
    VERIFY (0 == new_handler_called);
    VERIFY (!bad_alloc_thrown);

    operator delete[] (p, std::nothrow);
    VERIFY( 1 == delete_called );
    VERIFY( 1 == delete_vec_called );

    new_fail = true;
    p = operator new[] (1, std::nothrow);

    VERIFY (0 == p);
    VERIFY (2 == new_called);
    VERIFY (2 == new_vec_called);
    VERIFY (0 == new_handler_called);
    VERIFY (bad_alloc_thrown);

    new_fail = true;
    bad_alloc_thrown = false;
    std::set_new_handler (new_handler);
    p = operator new[] (1, std::nothrow);

    VERIFY (0 == p);
    VERIFY (3 == new_called);
    VERIFY (3 == new_vec_called);
    VERIFY (2 == new_handler_called);
    VERIFY (!bad_alloc_thrown);
}


int main()
{
  test01();
  test02();
}
