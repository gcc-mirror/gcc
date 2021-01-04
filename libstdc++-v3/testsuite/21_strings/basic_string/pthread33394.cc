// Copyright (C) 2015-2021 Free Software Foundation, Inc.
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
// { dg-options "-pthread"  }
// { dg-require-effective-target pthread }

// { dg-additional-options "-DITERATIONS=1000" { target simulator } }
#ifndef ITERATIONS
#define ITERATIONS 50000
#endif

// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=32261

#include <pthread.h>
#include <string>

extern "C" void* thread_function(void*) {
    for (int k = 0; k < ITERATIONS; k++) {
        std::string my_str;
        my_str += "foo";
    }
    return 0;
}

int main()
{
    pthread_t thread1, thread2;

    pthread_create(&thread1, NULL, thread_function, NULL);
    pthread_create(&thread2, NULL, thread_function, NULL);

    void* exitcode;
    pthread_join(thread1, &exitcode);
    pthread_join(thread2, &exitcode);
}
