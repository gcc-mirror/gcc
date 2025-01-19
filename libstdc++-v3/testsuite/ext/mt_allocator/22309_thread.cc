// { dg-require-sharedlib "" }
// { dg-options "-g -O2 -pthread -ldl" { target *-*-linux* *-*-gnu* } }

// Copyright (C) 2004-2025 Free Software Foundation, Inc.
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

#include <dlfcn.h>
#include <pthread.h>
#include <cstdlib>
#include <stdexcept>
#include <string>

void
check_dlopen(void*& h)
{
  dlerror();
  void* tmp = dlopen("./testsuite_shared.so", RTLD_LAZY);
  if (!tmp) 
    {
      try 
	{
	  // Throws std::logic_error on NULL string.
	  std::string error(dlerror());
	  throw std::runtime_error(error);
	}
      catch (const std::logic_error&)
	{ }
    }
  h = tmp;
}

void
check_dlsym(void*& h)
{
  dlerror();

  typedef void (*function_type) (void);
  function_type fn;
  fn = reinterpret_cast<function_type>(dlsym(h, "try_allocation"));

  try 
    {
      std::string error(dlerror());
      throw std::runtime_error(error);
    }
  catch (const std::logic_error&)
    { }

  fn();
}

void
check_dlclose(void*& h)
{
  dlerror();
  if (dlclose(h) != 0)
    {
      try 
	{
	  std::string error(dlerror());
	  throw std::runtime_error(error);
	}
      catch (const std::logic_error&)
	{ }
    }
}

void*
tf(void* arg)
{
  void* h;
  check_dlopen(h);
  check_dlsym(h);
  check_dlclose(h);
  return 0;
}

// libstdc++/22309
int
main (void)
{
  pthread_t th;
  pthread_create(&th, 0, tf, 0);
  pthread_join(th, 0);
  return 0;
}
