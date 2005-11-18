/* ParagraphView.java -- A composite View
   Copyright (C) 2005  Free Software Foundation, Inc.

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

import java.awt.Shape;

import javax.swing.event.DocumentEvent;

/**
 * A {@link FlowView} that flows it's children horizontally and boxes the rows
 * vertically.
 *
 * @author Roman Kennke (roman@kennke.org)
 */
public class ParagraphView extends FlowView implements TabExpander
{
  /**
   * A specialized horizontal <code>BoxView</code> that represents exactly
   * one row in a <code>ParagraphView</code>.
   */
  class Row extends BoxView
  {
    /**
     * Creates a new instance of <code>Row</code>.
     */
    Row(Element el)
    {
      super(el, X_AXIS);
    }
    public float getAlignment(int axis)
    {
      // FIXME: This is very likely not 100% correct. Work this out.
      return 0.0F;
    }
  }

  /**
   * The indentation of the first line of the paragraph.
   */
  protected int firstLineIndent;

  /**
   * The justification of the paragraph.
   */
  private int justification;

  /**
   * The line spacing of this paragraph.
   */
  private float lineSpacing;

  /**
   * The TabSet of this paragraph.
   */
  private TabSet tabSet;

  /**
   * Creates a new <code>ParagraphView</code> for the given
   * <code>Element</code>.
   *
   * @param element the element that is rendered by this ParagraphView
   */
  public ParagraphView(Element element)
  {
    super(element, Y_AXIS);
  }

  public float nextTabStop(float x, int tabOffset)
  {
    throw new InternalError("Not implemented yet");
  }

  /**
   * Creates a new view that represents a row within a flow.
   *
   * @return a view for a new row
   */
  protected View createRow()
  {
    return new Row(getElement());
  }

  /**
   * Returns the alignment for this paragraph view for the specified axis.
   * For the X_AXIS the paragraph view will be aligned at it's left edge
   * (0.0F). For the Y_AXIS the paragraph view will be aligned at the
   * center of it's first row.
   *
   * @param axis the axis which is examined
   *
   * @return the alignment for this paragraph view for the specified axis
   */
  public float getAlignment(int axis)
  {
    if (axis == X_AXIS)
      return 0.0F;
    else if (getViewCount() > 0)
      {

        float prefHeight = getPreferredSpan(Y_AXIS);
        float firstRowHeight = getView(0).getPreferredSpan(Y_AXIS);
        return (firstRowHeight / 2.F) / prefHeight;
      }
    else
      return 0.0F;
  }

  /**
   * Receives notification when some attributes of the displayed element
   * changes. This triggers a refresh of the cached attributes of this
   * paragraph.
   *
   * @param ev the document event
   * @param a the allocation of this view
   * @param fv the view factory to use for creating new child views
   */
  public void changedUpdate(DocumentEvent ev, Shape a, ViewFactory fv)
  {
    setPropertiesFromAttributes();
  }

  /**
   * Fetches the cached properties from the element's attributes.
   */
  protected void setPropertiesFromAttributes()
  {
    Element el = getElement();
    AttributeSet atts = el.getAttributes();
    setFirstLineIndent(StyleConstants.getFirstLineIndent(atts));
    setLineSpacing(StyleConstants.getLineSpacing(atts));
    setJustification(StyleConstants.getAlignment(atts));
    tabSet = StyleConstants.getTabSet(atts);
  }

  /**
   * Sets the indentation of the first line of the paragraph.
   *
   * @param i the indentation to set
   */
  protected void setFirstLineIndent(float i)
  {
    firstLineIndent = (int) i;
  }

  /**
   * Sets the justification of the paragraph.
   *
   * @param j the justification to set 
   */
  protected void setJustification(int j)
  {
    justification = j;
  }

  /**
   * Sets the line spacing for this paragraph.
   *
   * @param s the line spacing to set
   */
  protected void setLineSpacing(float s)
  {
    lineSpacing = s;
  }

  /**
   * Returns the i-th view from the logical views, before breaking into rows.
   *
   * @param i the index of the logical view to return
   *
   * @return the i-th view from the logical views, before breaking into rows
   */
  protected View getLayoutView(int i)
  {
    return layoutPool.getView(i);
  }

  /**
   * Returns the number of logical child views.
   *
   * @return the number of logical child views
   */
  protected int getLayoutViewCount()
  {
    return layoutPool.getViewCount();
  }

  /**
   * Returns the TabSet used by this ParagraphView.
   *
   * @return the TabSet used by this ParagraphView
   */
  protected TabSet getTabSet()
  {
    return tabSet;
  }
}
