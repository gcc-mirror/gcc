/* SizeSequence.java --
   Copyright (C) 2002 Free Software Foundation, Inc.

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

/**
 * SizeSequence
 * @author	Andrew Selkirk
 * @version	1.0
 */
public class SizeSequence
{
  
  /**
    * sizes
    */
  private int[] sizes = new int[0];

  /**
   * Constructor SizeSequence
   */
  public SizeSequence()
  {
    sizes = new int[0];
  }

  /**
   * Constructor SizeSequence
   * @param numEntries TODO
   */
  public SizeSequence(int numEntries)
  {
    this(numEntries, 0);
  }

  /**
   * Constructor SizeSequence
   * @param numEntries TODO
   * @param value TODO
   */
  public SizeSequence(int numEntries, int value)
  {
    insertEntries(0, numEntries, value);
  }

  /**
   * Constructor SizeSequence
   * @param sizes TODO
   */
  public SizeSequence(int[] sizes)
  {
    setSizes(sizes);
  }

  /**
   * setSize
   * @param index TODO
   * @param size TODO
   */
  public void setSize(int index, int size)
  {
    sizes[index] = size;
  }

  /**
   * getIndex
   * @param position TODO
   * @returns int
   */
  public int getIndex(int position)
  {
    return 0; // TODO
  }

  /**
   * getSize
   * @param index TODO
   * @returns int
   */
  public int getSize(int index)
  {
    return sizes[index];
  }

  /**
   * setSizes
   * @param sizes TODO
   */
  public void setSizes(int[] sizes)
  {
    int index;
    // Initialize sizes.
    this.sizes = new int[sizes.length];
    for (index = 0; index < sizes.length; index++)
      this.sizes[index] = sizes[index];

  }

  /**
   * getSizes
   * @returns int[]
   */
  public int[] getSizes()
  {
    int[] array;
    int index;

    // Create new array.
    array = new int[sizes.length];
    for (index = 0; index < sizes.length; index++)
      array[index] = sizes[index];

    // Return newly created array.
    return array;

  }

  /**
   * getPosition
   * @param index TODO
   * @returns int
   */
  public int getPosition(int index)
  {
    int position;
    int loop;

    // Process sizes.
    position = 0;
    for (loop = 0; loop < index; loop++)
      position += sizes[loop];

    // Return position.
    return position;

  }

  /**
   * insertEntries
   * @param start TODO
   * @param length TODO
   * @param value TODO
   */
  public void insertEntries(int start, int length, int value)
  {
    int[] array;
    int index;
    int arrayIndex;
    int loop;

    // Create new array.
    array = new int[sizes.length + length];
    arrayIndex = 0;
    for (index = 0; index < sizes.length; index++)
      {
        if (index == start)
          {
            for (loop = 0; loop < length; loop++)
              {
                array[arrayIndex] = value;
                arrayIndex++;
              }
          }
        else
          {
            array[arrayIndex] = sizes[index];
            arrayIndex++;
          }
      }

	}

  /**
   * removeEntries
   * @param start TODO
   * @param length TODO
   */
  public void removeEntries(int start, int length)
  {
    int[] array;
    int index;
    int arrayIndex;

    // Sanity check.
    if ((start + length) > sizes.length)
      throw new IllegalArgumentException("Specified start/length that "
                                         + "is greater than available sizes");

    // Create new array.
    array = new int[sizes.length - length];
    arrayIndex = 0;
    for (index = 0; index < sizes.length; index++)
      {
        if (index == start)
          index += length - 1;
        else
          {
            array[arrayIndex] = sizes[index];
            arrayIndex++;
          }
      }
  }

}
