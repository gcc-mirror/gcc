/* Sequence.java -- a sequence of integers.
   Copyright (C) 2004, 2006 Free Software Foundation, Inc.

This file is a part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301
USA

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
exception statement from your version.  */


package gnu.java.security.util;

import java.util.AbstractList;
import java.util.LinkedList;

/**
 * A monotonic sequence of integers in the finite field 2<sup>32</sup>.
 */
public final class Sequence
    extends AbstractList
{
  private final Integer[] sequence;

  /**
   * Create a sequence of integers from 0 to <i>end</i>, with an increment of
   * 1. If <i>end</i> is less than 0, then the sequence will wrap around
   * through all positive integers then negative integers until the end value is
   * reached. Naturally, this will result in an enormous object, so don't do
   * this.
   *
   * @param end The ending value.
   */
  public Sequence(int end)
  {
    this(0, end, 1);
  }

  /**
   * Create a sequence of integers from <i>start</i> to <i>end</i>, with an
   * increment of 1. If <i>end</i> is less than <i>start</i>, then the
   * sequence will wrap around until the end value is reached. Naturally, this
   * will result in an enormous object, so don't do this.
   *
   * @param start The starting value.
   * @param end The ending value.
   */
  public Sequence(int start, int end)
  {
    this(start, end, 1);
  }

  /**
   * Create a sequence of integers from <i>start</i> to <i>end</i>, with an
   * increment of <i>span</i>. If <i>end</i> is less than <i>start</i>, then
   * the sequence will wrap around until the end value is reached. Naturally,
   * this will result in an enormous object, so don't do this.
   * <p>
   * <i>span</i> can be negative, resulting in a decresing sequence.
   * <p>
   * If <i>span</i> is 0, then the sequence will contain {<i>start</i>,
   * <i>end</i>} if <i>start</i> != <i>end</i>, or just the singleton
   * <i>start</i> if <i>start</i> == <i>end</i>.
   *
   * @param start The starting value.
   * @param end The ending value.
   * @param span The increment value.
   */
  public Sequence(int start, int end, int span)
  {
    if (span == 0)
      {
        if (start != end)
          sequence = new Integer[] { Integer.valueOf(start),
                                     Integer.valueOf(end) };
        else
          sequence = new Integer[] { Integer.valueOf(start) };
      }
    else
      {
        LinkedList l = new LinkedList();
        for (int i = start; i != end; i += span)
          l.add(Integer.valueOf(i));

        l.add(Integer.valueOf(end));
        sequence = (Integer[]) l.toArray(new Integer[l.size()]);
      }
  }

  public Object get(int index)
  {
    if (index < 0 || index >= size())
      throw new IndexOutOfBoundsException("index=" + index + ", size=" + size());
    return sequence[index];
  }

  public int size()
  {
    return sequence.length;
  }

  public Object[] toArray()
  {
    return (Object[]) sequence.clone();
  }
}
