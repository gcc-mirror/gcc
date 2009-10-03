// -*- C++ -*-

// Copyright (C) 2006, 2007, 2009 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// { dg-options "-D_GLIBCXX_PROFILE" }
// { dg-do compile }

#include <stdio.h>
#include <malloc.h>
#include <vector>

using std::vector;

static void my_init_hook (void);
static void *my_malloc_hook (size_t, const void *);
typedef void* (*malloc_hook) (size_t, const void *);

malloc_hook old_malloc_hook;
     
void (*__malloc_initialize_hook) (void) = my_init_hook;

static void
my_init_hook (void)
{
  old_malloc_hook = __malloc_hook;
  __malloc_hook = my_malloc_hook;
}

static void *
my_malloc_hook (size_t size, const void *caller)
{
  void *result;
  __malloc_hook = old_malloc_hook;
  result = malloc (size);
  old_malloc_hook = __malloc_hook;

  // With _GLIBCXX_PROFILE, the instrumentation of the vector constructor
  // will call back into malloc.
  vector<int> v;

  __malloc_hook = my_malloc_hook;
  return result;
}
     

int main() {
  int* test = (int*) malloc(sizeof(int));
  *test = 1;
  return *test;
}
