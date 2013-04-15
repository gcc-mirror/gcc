// { dg-do compile }
// { dg-options " -std=gnu++0x -pthread" { target *-*-freebsd* *-*-netbsd* *-*-linux* alpha*-*-osf* mips-sgi-irix6* powerpc-ibm-aix* hppa*-hp-hpux11* } }
// { dg-options " -std=gnu++0x -pthreads" { target *-*-solaris* } }
// { dg-options " -std=gnu++0x " { target *-*-cygwin *-*-darwin* } }
// { dg-require-cstdint "" }
// { dg-require-gthreads "" }

// Copyright (C) 2012 Free Software Foundation, Inc.
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

// PR libstdc++/53841

#include <chrono>
#include <mutex>
#include <condition_variable>

namespace ch = std::chrono;

struct FPClock : ch::system_clock
{
    typedef double rep;
    typedef std::ratio<1> period;
    typedef ch::duration<rep, period> duration;
    typedef ch::time_point<FPClock> time_point;

    static time_point now()
    { return time_point(duration(system_clock::now().time_since_epoch())); }
};

void f()
{
    std::mutex mx;
    std::unique_lock<std::mutex> l(mx);
    std::condition_variable cv;
    cv.wait_until(l, FPClock::now());
}
