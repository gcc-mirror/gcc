/* ListUI.java
   Copyright (C) 2002, 2003 Free Software Foundation, Inc.

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


package javax.swing.plaf;

import java.awt.Point;
import java.awt.Rectangle;
import javax.swing.JList;


/**
 * An abstract base class for delegates that implement the pluggable
 * look and feel for a <code>JList</code>.
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public abstract class ListUI
  extends ComponentUI
{
  /**
   * Constructs a new <code>ListUI</code>.
   */
  public ListUI()
  {
  }


  /**
   * Determines the cell index which is the closest to the specified
   * location. The find out whether the returned cell actually
   * contains the location, the caller should also use {@link
   * #getCellBounds}.
   *
   * @param list the <code>JList</code> for which this delegate object
   *        provides the pluggable user interface.
   *
   * @param location a point in the <code>JList</code> coordinate
   *        system.
   *
   * @return the index of the closest cell, or -1 if the list model
   *         is empty.
   */
  public abstract int locationToIndex(JList index, Point location);


  /**
   * Determines the location of the specified cell.
   *
   * @param list the <code>JList</code> for which this delegate object
   *        provides the pluggable user interface.
   *
   * @param index the zero-based index of the cell whose location shall be
   *        determined.
   *
   * @return the position of the top left corner of the cell in the
   *         <code>JList</code> coordinate system, or <code>null</code>
   *         if <code>cell</code> does not designate a valid cell.
   */
  public abstract Point indexToLocation(JList list, int index);


  /**
   * Determines the bounding box of the rectangle spanned by
   * two list indices.
   *
   * @param list the <code>JList</code> for which this delegate object
   *        provides the pluggable user interface.
   *
   * @param index1 the zero-based index of the first cell.
   *
   * @param index2 the zero-based index of the second cell.
   *
   * @return the spanned rectangle, or <code>null</code> if either
   *         <code>index1</code> or <code>index2</code> does not
   *         designate a valid cell.
   */
  public abstract Rectangle getCellBounds(JList list,
                                          int index1, int index2);
}
