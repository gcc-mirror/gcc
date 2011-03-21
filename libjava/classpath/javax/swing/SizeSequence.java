/* SizeSequence.java --
   Copyright (C) 2002, 2006, Free Software Foundation, Inc.

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

package javax.swing;

import java.util.Arrays;

/**
 * A sequence of values that represent the dimensions (widths or heights) of
 * some collection of items (for example, the widths of the columns in a table).
 *
 * @author      Andrew Selkirk
 */
public class SizeSequence
{
  // TODO: Sun's API specification for this class contains an implementation
  // note regarding the encoding for the element sizes.  We currently use the
  // simple size encoding but we should look at improving this.

  /** Storage for the element sizes. */
  private int[] sizes;

  /**
   * Creates a new empty <code>SizeSequence</code> instance.
   */
  public SizeSequence()
  {
    sizes = new int[0];
  }

  /**
   * Creates a new <code>SizeSequence</code> instance with the specified number
   * of elements, each having a size of 0.
   *
   * @param numEntries  the number of elements.
   */
  public SizeSequence(int numEntries)
  {
    this(numEntries, 0);
  }

  /**
   * Creates a new <code>SizeSequence</code> instance with the specified number
   * of elements all having the same size (<code>value</code>).
   *
   * @param numEntries  the number of elements.
   * @param value  the value for each element.
   */
  public SizeSequence(int numEntries, int value)
  {
    sizes = new int[numEntries];
    Arrays.fill(sizes, value);
  }

  /**
   * Creates a new <code>SizeSequence</code> instance using the specified
   * element sizes.
   *
   * @param sizes  the element sizes (<code>null</code> not permitted).
   */
  public SizeSequence(int[] sizes)
  {
    this.sizes = (int[]) sizes.clone();
  }

  /**
   * Sets the size of the element at the specified index.
   *
   * @param index  the index.
   * @param size  the size.
   */
  public void setSize(int index, int size)
  {
    if (index >= 0 && index < sizes.length)
      sizes[index] = size;
  }

  /**
   * Returns the index of the element that contains the specified position.
   *
   * @param position  the position.
   *
   * @return The index of the element that contains the specified position.
   */
  public int getIndex(int position)
  {
    int i = 0;
    int runningTotal = 0;
    while (i < sizes.length && position >= runningTotal + sizes[i])
      {
        runningTotal += sizes[i];
        i++;
      }
    return i;
  }

  /**
   * Returns the size of the specified element, or 0 if the element index is
   * outside the defined range.
   *
   * @param index  the element index.
   *
   * @return The size of the specified element, or 0 if the element index is
   *     outside the defined range.
   */
  public int getSize(int index)
  {
    if (index < 0 || index >= sizes.length)
      return 0;
    return sizes[index];
  }

  /**
   * Sets the sizes for the elements in the sequence.
   *
   * @param sizes  the element sizes (<code>null</code> not permitted).
   */
  public void setSizes(int[] sizes)
  {
    this.sizes = (int[]) sizes.clone();
  }

  /**
   * Returns an array containing the sizes for all the elements in the sequence.
   *
   * @return The element sizes.
   */
  public int[] getSizes()
  {
    return (int[]) sizes.clone();
  }

  /**
   * Returns the position of the specified element.
   *
   * @param index  the element index.
   *
   * @return The position.
   */
  public int getPosition(int index)
  {
    int position;
    int loop;
    position = 0;
    for (loop = 0; loop < index; loop++)
      position += sizes[loop];
    return position;

  }

  /**
   * Inserts new entries into the sequence at the <code>start</code> position.
   * There are <code>length</code> new entries each having the specified
   * <code>value</code>.
   *
   * @param start  the start element.
   * @param length  the number of elements to insert.
   * @param value  the size for each of the new elements.
   */
  public void insertEntries(int start, int length, int value)
  {
    int[] newSizes = new int[sizes.length + length];
    System.arraycopy(sizes, 0, newSizes, 0, start);
    for (int i = start; i < start + length; i++)
      newSizes[i] = value;
    System.arraycopy(sizes, start, newSizes, start + length,
                     sizes.length - start);
    sizes = newSizes;
  }

  /**
   * Removes the element(s) at index <code>start</code> (the number of elements
   * removed is <code>length</code>).
   *
   * @param start  the index of the first element to remove.
   * @param length  the number of elements to remove.
   */
  public void removeEntries(int start, int length)
  {
    // Sanity check.
    if ((start + length) > sizes.length)
      throw new IllegalArgumentException("Specified start/length that "
                                         + "is greater than available sizes");

    int[] newSizes = new int[sizes.length - length];
    System.arraycopy(sizes, 0, newSizes, 0, start);
    System.arraycopy(sizes, start + length, newSizes, start,
                     sizes.length - start - length);
    sizes = newSizes;
  }
}
