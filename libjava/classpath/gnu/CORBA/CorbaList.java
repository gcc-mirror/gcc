/* CorbaList.java --
   Copyright (C) 2005 Free Software Foundation, Inc.

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


package gnu.CORBA;

import java.io.Serializable;

import java.util.ArrayList;

import org.omg.CORBA.Bounds;

/**
 * This class is used to store array lists. Differently from
 * the java.util lists,
 * it throws {@link org.omg.CORBA.Bounds} rather than
 * {@link IndexOutOfBoundsException}.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class CorbaList
  extends ArrayList
  implements Serializable
{
  /**
   * Use serialVersionUID for interoperability.
   */
  private static final long serialVersionUID = 1;

  /**
   * Creates the list with the given initial size.
   */
  public CorbaList(int initial_size)
  {
    super(initial_size);
  }

  /**
   * Creates the list with the default size.
   */
  public CorbaList()
  {
  }

  /**
   * Remove the item at the given index.
   * @param at the index
   * @throws org.omg.CORBA.Bounds if the index is out of bounds.
   */
  public void drop(int at)
            throws Bounds
  {
    try
      {
        super.remove(at);
      }
    catch (IndexOutOfBoundsException ex)
      {
        throw new Bounds("[" + at + "], valid [0.." + size() + "]");
      }
  }

  /**
   * Get the item at the given index.
   * @param at the index
   * @return the item at the index
   * @throws org.omg.CORBA.Bounds if the index is out of bounds.
   */
  public Object item(int at)
              throws Bounds
  {
    try
      {
        return super.get(at);
      }
    catch (IndexOutOfBoundsException ex)
      {
        throw new Bounds("[" + at + "], valid [0.." + size() + "]");
      }
  }
}
