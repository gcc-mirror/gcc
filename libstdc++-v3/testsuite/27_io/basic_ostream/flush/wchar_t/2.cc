// Copyright (C) 2005 Free Software Foundation, Inc.
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

// 27.6.2.6 Unformatted output functions
//
// _GLIBCXX_RESOLVE_LIB_DEFECTS
// DR 60. What is a formatted input function?
// basic_ostream::flush() does not behave as an unformatted output function.

#include <ostream>
#include <testsuite_hooks.h>
#include <testsuite_io.h>

void test02()
{
  bool test __attribute__((unused)) = true;

  __gnu_test::sync_wstreambuf buf;
  std::wostream os(&buf);
  
  __gnu_test::sync_wstreambuf buf_tie;
  std::wostream os_tie(&buf_tie);

  // No sentry should be constructed so os.tie()->flush() should not be
  // called.
  os.tie(&os_tie);
  
  os.flush();

  VERIFY( os.good() );
  VERIFY( buf.sync_called() );
  VERIFY( !buf_tie.sync_called() );

  // os.rdbuf()->pubsync() should be called even if !os.good().
  os.setstate(std::ios_base::eofbit);

  os.flush();

  VERIFY( os.rdstate() == std::ios_base::eofbit );
  VERIFY( buf.sync_called() );
  VERIFY( !buf_tie.sync_called() );
}

int main()
{
  test02();
  return 0;
}

