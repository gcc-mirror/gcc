// Contains various utility functions used by the runtime implementation.
// Copyright (C) 2019-2025 Free Software Foundation, Inc.

// GCC is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 3, or (at your option) any later
// version.

// GCC is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

module gcc.sections.common;

/**
 * Asserts that the given condition is `true`.
 *
 * The assertion is independent from -release, by abort()ing. Regular assertions
 * throw an AssertError and thus require an initialized GC, which might not be
 * the case (yet or anymore) for the startup/shutdown code in this package
 * (called by CRT ctors/dtors etc.).
 */
package(gcc) void safeAssert(
    bool condition, scope string msg, scope string file = __FILE__, size_t line = __LINE__
) nothrow @nogc @safe
{
    import core.internal.abort;
    condition || abort(msg, file, line);
}
