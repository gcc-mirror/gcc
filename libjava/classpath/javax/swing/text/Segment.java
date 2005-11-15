/* Segment.java --
   Copyright (C) 2002, 2004 Free Software Foundation, Inc.

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

package javax.swing.text;

import java.text.CharacterIterator;

public class Segment implements Cloneable, CharacterIterator
{
  private boolean partialReturn;
  private int current;
  
  public char[] array;
  public int count;
  public int offset;

  public Segment()
  {
    // Nothing to do here.
  }

  public Segment(char[] array, int offset, int count)
  {
    this.array = array;
    this.offset = offset;
    this.count = count;
  }
  
  public Object clone()
  {
    try
      {
	return super.clone();
      }
    catch (CloneNotSupportedException e)
      {
	return null;
      }
  }

  public char current()
  {
    if (count == 0
	|| current >= getEndIndex())
      return DONE;
    
    return array[current];
  }

  public char first()
  {
    if (count == 0)
      return DONE;

    current = getBeginIndex();
    return array[current];
  }

  public int getBeginIndex()
  {
    return offset;
  }

  public int getEndIndex()
  {
    return offset + count;
  }

  public int getIndex()
  {
    return current;
  }

  public char last()
  {
    if (count == 0)
      return DONE;
    
    current = getEndIndex() - 1;
    return array[current];
  }

  public char next()
  {
    if (count == 0)
      return DONE;

    if ((current + 1) >= getEndIndex())
      {
	current = getEndIndex();
	return DONE;
      }
    
    current++;
    return array[current];
  }

  public char previous()
  {
    if (count == 0
	|| current == getBeginIndex())
      return DONE;
    
    current--;
    return array[current];
  }

  public char setIndex(int position)
  {
    if (position < getBeginIndex()
	|| position > getEndIndex())
      throw new IllegalArgumentException();

    current = position;

    if (position == getEndIndex())
      return DONE;
    
    return array[current];
  }

  public String toString()
  {
    return new String(array, offset, count);
  }

  /**
   * @since 1.4
   */
  public void setPartialReturn(boolean p)
  {
    partialReturn = p;
  }
  
  /**
   * @since 1.4
   */
  public boolean isPartialReturn()
  {
    return partialReturn;
  }
}
