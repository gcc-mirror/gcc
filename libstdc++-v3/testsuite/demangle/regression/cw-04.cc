// 2003-02-26  Carlo Wood  <carlo@alinoe.com>

// Copyright (C) 2003 Free Software Foundation, Inc.
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
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// IA 64 C++ ABI - 5.1 External Names (a.k.a. Mangling)

#include <testsuite_hooks.h>

// libcwd tests
int main()
{
  using namespace __gnu_test;


/*
class timer_event_request_base_ct { };
struct timer_greater { };

void i(void)
{
  // Instantiation.
  std::priority_queue<timer_event_request_base_ct*, std::deque<timer_event_request_base_ct*>, timer_greater> dummy;
  dummy.top();
}
*/
  verify_demangle("_ZNKSt14priority_queueIP27timer_event_request_base_ctSt5dequeIS1_SaIS1_EE13timer_greaterE3topEv", "std::priority_queue<timer_event_request_base_ct*, std::deque<timer_event_request_base_ct*, std::allocator<timer_event_request_base_ct*> >, timer_greater>::top() const");

  return 0;
}
