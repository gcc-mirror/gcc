/* Scrollable.java -- 
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

package javax.swing;

import java.awt.Dimension;
import java.awt.Rectangle;

/**
 * Defines the method that a component should implement to behave nicely
 * in {@link JScrollPane}s. Note that this is not required for a component
 * to be used in a <code>JScrollPane</code>, but can highly improve the
 * user experience when scrolling the component.
 */
public interface Scrollable
{
  Dimension getPreferredScrollableViewportSize();

  /**
   * Return the preferred scrolling amount (in pixels) for the given
   * scrolling direction and orientation when scrolling in small amounts
   * like table lines.
   * 
   * @param visibleRect the currently visible part of the component. 
   * @param orientation the scrolling orientation
   * @param direction the scrolling direction (negative - up, positive -down).
   * The values greater than one means that more mouse wheel or similar 
   * events were generated, and hence it is better to scroll the longer
   * distance.
   * 
   * @return the preferred scrolling distance, negative if up or left.
   */
  int getScrollableUnitIncrement(Rectangle visibleRect, int orientation,
                                 int direction);

  /**
   * Return the preferred scrolling amount (in pixels) for the given
   * scrolling direction and orientation when scrolling in large amounts
   * (pages).
   * 
   * @param visibleRect the currently visible part of the component. 
   * @param orientation the scrolling orientation
   * @param direction the scrolling direction (negative - up, positive -down).
   * The values greater than one means that more mouse wheel or similar 
   * events were generated, and hence it is better to scroll the longer
   * distance.
   * 
   * @return the preferred scrolling distance, negative if up or left. 
   */
  int getScrollableBlockIncrement(Rectangle visibleRect, int orientation,
                                  int direction);

  /**
   * Return true if the width of the scrollable is always equal to the
   * view, where it is displayed, width (for instance, the text area with
   * the word wrap). In such case, the horizontal scrolling should not be
   * performed.
   * 
   * @return true is no horizontal scrolling is assumed, faster otherwise.
   */
  boolean getScrollableTracksViewportWidth();

  /**
   * Return true if the height of the scrollable is always equal to the view,
   * where it is displayed, height.In such case, the vertical scrolling should
   * not be performed.
   * 
   * @return true is no horizontal scrolling is assumed, faster otherwise.
   */
  boolean getScrollableTracksViewportHeight();

}
