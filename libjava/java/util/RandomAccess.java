/* RandomAccess.java -- A tagging interface that lists can use to tailor
   operations to the correct algorithm
   Copyright (C) 2001 Free Software Foundation, Inc.

This file is part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */


package java.util;

/**
 * Marker interface used to inform <code>List</code> implementations that
 * they support fast (usually constant time) random access. This allows
 * generic list algorithms to tailor their behavior based on the list
 * type.
 * <p>
 *
 * For example, some sorts are n*log(n) on an array, but decay to quadratic
 * time on a linked list.  As a rule of thumb, this interface should be
 * used is this loop:<br>
 * <code>for (int i = 0, n = list.size(); i &lt; n; i++) list.get(i);</code>
 * <br>runs faster than this loop:<br>
 * <code>for (Iterator i = list.iterator(); i.hasNext(); ) i.next();</code>
 *
 * @author Eric Blake <ebb9@email.byu.edu>
 * @see List
 * @since 1.4
 * @status updated to 1.4
 */
public interface RandomAccess
{
  // Tagging interface only.
}
