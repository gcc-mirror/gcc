/* ListView.java --
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

package javax.swing.text.html;

import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.Shape;

import javax.swing.text.Element;

/**
 * A View to render HTML lists, like the <code>&lt;ul&gt;</code> and
 * <code>&lt;ol&gt;</code> tags.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class ListView
  extends BlockView
{

  /**
   * The painter used to paint the list items.
   */
  private StyleSheet.ListPainter painter;

  /**
   * Creates a new <code>ListView</code> for the specified element.
   *
   * @param el the element to create a list view for
   */
  public ListView(Element el)
  {
    super(el, Y_AXIS);
  }

  /**
   * Returns the alignment of this view along the specified axis.
   *
   * This returns <code>0.5</code> unconditionally.
   *
   * @param axis the axis
   *
   * @return the alignment of this view along the specified axis
   */
  public float getAlignment(int axis)
  {
    if (axis != X_AXIS && axis != Y_AXIS)
      throw new IllegalArgumentException("Illegal axis parameter: " + axis);

    return 0.5F;
  }

  /**
   * Paints the <code>ListView</code>.
   *
   * @param g the graphics context to use for painting
   * @param allocation the allocation given to this view
   */
  public void paint(Graphics g, Shape allocation)
  {
    super.paint(g, allocation);
  }

  /**
   * Paints the child with the specified index into the specified allocation.
   *
   * This implementation forwards to the list painter fetched from the
   * {@link StyleSheet} and then calls
   * <code>super.paintChild(g, a, index)</code>.
   *
   * @param g the graphics context to use
   * @param a the allocation for the child
   * @param index the child index
   */
  protected void paintChild(Graphics g, Rectangle a, int index)
  {
    painter.paint(g, a.x, a.y, a.width, a.height, this, index);
    super.paintChild(g, a, index);
  }

  /**
   * Fetches this view's properties from the style attributes of this view's
   * element.
   *
   * This forwards to super and then fetches a {@link StyleSheet.ListPainter}
   * from the stylesheet suitable for painting the list.
   */
  protected void setPropertiesFromAttributes()
  {
    super.setPropertiesFromAttributes();
    painter = getStyleSheet().getListPainter(getAttributes());
  }
}
