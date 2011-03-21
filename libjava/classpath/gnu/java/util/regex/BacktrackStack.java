/* gnu/regexp/BacktrackStack.java
   Copyright (C) 2006 Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

Linking this library statically or dynamically with other modules is
making a combined work based on this library.  Thus, the terms and
conditions of the GNU General Public License cover the whole
combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent
modules, and to copy and distribute the resulting executable under
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */


package gnu.java.util.regex;

/**
 * An instance of this class represents a stack
 * used for backtracking.
 *
 * @author Ito Kazumitsu</A>
 */
final class BacktrackStack
{

    /** A set of data to be used for backtracking. */
  static class Backtrack
  {
        /** REToken to which to go back */
    REToken token;
        /** CharIndexed on which matches are being searched for. */
    CharIndexed input;
        /** REMatch to be used by the REToken token. */
    REMatch match;
        /** Some parameter used by the token's backtrack method. */
    Object param;
      Backtrack (REToken token, CharIndexed input, REMatch match,
                 Object param)
    {
      this.token = token;
      this.input = input;
      //  REMatch may change before backtracking is needed. So we
      //  keep a clone of it.
      this.match = (REMatch) match.clone ();
      this.param = param;
    }
  }

  Backtrack[] stack;
  private int size;
  private int capacity;
  private static final int INITIAL_CAPACITY = 32;
  private static final int CAPACITY_INCREMENT = 16;

  BacktrackStack ()
  {
    stack = new Backtrack[INITIAL_CAPACITY];
    size = 0;
    capacity = INITIAL_CAPACITY;
  }

  boolean empty ()
  {
    return size == 0;
  }

  Backtrack peek ()
  {
    return stack[size - 1];
  }

  Backtrack pop ()
  {
    Backtrack bt = stack[--size];
    stack[size] = null;
    return bt;
  }

  void clear ()
  {
    for (int i = 0; i < size; i++)
      {
        stack[i] = null;
      }
    size = 0;
  }

  void push (Backtrack bt)
  {
    if (size >= capacity)
      {
        capacity += CAPACITY_INCREMENT;
        Backtrack[]newStack = new Backtrack[capacity];
        System.arraycopy (stack, 0, newStack, 0, size);
        stack = newStack;
      }
    stack[size++] = bt;
  }

}
