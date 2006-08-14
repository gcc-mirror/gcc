/* HRuleView.java -- Horizontal dash in HTML documents.
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

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.Shape;

import javax.swing.text.Element;
import javax.swing.text.View;

/**
 * Represents the long horizontal separating dash that can be inserted into the
 * HTML documents with HR tag.
 */
class HRuleView extends InlineView
{
  /**
   * The null view, indicating, that nothing should be painted ahead the
   * breaking point. 
   */
  View nullView;
  
  /**
   * The height of the horizontal dash area.
   */
  static int HEIGHT = 4;
  
  /**
   * The imaginary invisible view that stays after end of line after the
   * breaking procedure. It occupies on character.
   */
  class Beginning extends NullView
  {
    /**
     * The break offset that becomes the views start offset.
     */
    int breakOffset;
    
    /**
     * Return the end offset that is always one char after the break offset.
     */
    public int getEndOffset()
    {
      return breakOffset + 1;
    }
    
    /**
     * Return the start offset that has been passed in a constructor.
     */
    public int getStartOffset()
    {
      return breakOffset;
    }
     
    /**
     * Create the new instance of this view.
     * 
     * @param element the element (inherited from the HR view)
     * @param offset the position where the HR view has been broken
     */
    public Beginning(Element element, int offset)
    {
      super(element);
      breakOffset = offset;
    }
  }
  
  /**
   * Creates the new HR view.
   */
  public HRuleView(Element element)
  {
    super(element);
  }

  /**
   * Returns the ForcedBreakWeight for the vertical axis, indicating, the the
   * view must be broken to be displayed correctly. The horizontal dash is
   * not breakeable along the Y axis.
   */
  public int getBreakWeight(int axis, float pos, float len)
  {
    if (axis == X_AXIS && ((getEndOffset() - getStartOffset()) > 1))
      return ForcedBreakWeight;      
    else
      return BadBreakWeight;      
  }
  
  /**
   * Draws the double line, upped black and the lower light gray.
   */
  public void paint(Graphics g, Shape a)
  {
    Rectangle bounds = a.getBounds();

    int x = bounds.x;
    int y = bounds.y;

    int w = bounds.x + bounds.width;

    // We move "half pixel up" from the actual horizontal position -
    // this will be rounded to the closest actual int co-ordinate.
    int h = bounds.y + (int) Math.round(bounds.height * 0.5 - 0.5);

    g.setColor(Color.black);
    g.drawLine(x, y++, w, h++);
    g.setColor(Color.lightGray);    
    g.drawLine(x, y, w, h);
  }

  /**
   * Break the view into this view and the invisible imaginary view that
   * stays on the end of line that is broken by HR dash. The view is broken
   * only if its length is longer than one (the two characters are expected
   * in the initial length).
   */
  public View breakView(int axis, int offset, float pos, float len)
  {
    if (getEndOffset() - getStartOffset() > 1)
      return new Beginning(getElement(), offset);
    else
      return this;
  }
  
  /**
   * Returns the width of the container for the horizontal axis and the
   * thickness of the dash area for the vertical axis.
   */
  public float getMaximumSpan(int axis)
  {
    if (axis == X_AXIS)
      {
        Component container = getContainer();
        if (container != null)
          return getContainer().getWidth();
        else
          return 640;
      }
    else
      return HEIGHT;     
  }

  /**
   * Returns the same values as {@link #getMaximumSpan(int)}
   */
  public float getPreferredSpan(int axis)
  {
    return getMaximumSpan(axis);
  }
}
