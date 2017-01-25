// { dg-require-sharedlib "" }
// { dg-skip-if "invalid options for C" { *-*-* } { "-std=c++??" "-std=gnu++??" } }
// { dg-options "-g -O2 -pthread -ldl -x c -fvtable-verify=none -Wno-pedantic" { target *-*-linux* *-*-gnu* *-*-solaris2.1[2-9]* } }

// Copyright (C) 2005-2017 Free Software Foundation, Inc.
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

#include <pthread.h>
#include <dlfcn.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>

// NB: This must be compiled and linked as a "C" executable.
static void* run(void* arg)
{
  typedef void (*function_type) (void);
  void* lib;
  void (*cb)();

  lib = dlopen("./testsuite_shared.so", RTLD_NOW);
  if (!lib)
    {
      printf("dlopen failed: %s\n", strerror(errno));
      return 0;
    }
  cb = (function_type) dlsym(lib, "try_throw_exception");
  if (!cb)
    {
      printf("dlsym failed: %s\n", strerror(errno));
      return 0;
    }
  cb();
  dlclose(lib);
  return 0;
}

// libstdc++/23591
int main(void)
{
  pthread_t pt;

  if (pthread_create(&pt, 0, &run, 0) != 0)
    return 1;
  if (pthread_join(pt, 0) != 0)
    return 1;

  return 0;
}
