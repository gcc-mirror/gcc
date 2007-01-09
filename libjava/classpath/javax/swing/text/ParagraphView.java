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

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.Shape;

import javax.swing.SizeRequirements;
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

    /**
     * Overridden to adjust when we are the first line, and firstLineIndent
     * is not 0.
     */
    public short getLeftInset()
    {
      short leftInset = super.getLeftInset();
      View parent = getParent();
      if (parent != null)
        {
          if (parent.getView(0) == this)
            leftInset += firstLineIndent;
        }
      return leftInset;
    }

    public float getAlignment(int axis)
    {
      float align;
      if (axis == X_AXIS)
        switch (justification)
          {
          case StyleConstants.ALIGN_RIGHT:
            align = 1.0F;
            break;
          case StyleConstants.ALIGN_CENTER:
          case StyleConstants.ALIGN_JUSTIFIED:
            align = 0.5F;
            break;
          case StyleConstants.ALIGN_LEFT:
          default:
            align = 0.0F;
          }
      else
        align = super.getAlignment(axis);
      return align;
    }

    /**
     * Overridden because child views are not necessarily laid out in model
     * order.
     */
    protected int getViewIndexAtPosition(int pos)
    {
      int index = -1;
      if (pos >= getStartOffset() && pos < getEndOffset())
        {
          int nviews = getViewCount();
          for (int i = 0; i < nviews && index == -1; i++)
            {
              View child = getView(i);
              if (pos >= child.getStartOffset() && pos < child.getEndOffset())
                index = i;
            }
        }
      return index;
    }


    /**
     * Overridden to perform a baseline layout. The normal BoxView layout
     * isn't completely suitable for rows.
     */
    protected void layoutMinorAxis(int targetSpan, int axis, int[] offsets,
                                   int[] spans)
    {
      baselineLayout(targetSpan, axis, offsets, spans);
    }

    /**
     * Overridden to perform a baseline layout. The normal BoxView layout
     * isn't completely suitable for rows.
     */
    protected SizeRequirements calculateMinorAxisRequirements(int axis,
                                                            SizeRequirements r)
    {
      return baselineRequirements(axis, r);
    }

    protected void loadChildren(ViewFactory vf)
    {
      // Do nothing here. The children are added while layouting.
    }

    /**
     * Overridden to determine the minimum start offset of the row's children.
     */
    public int getStartOffset()
    {
      // Determine minimum start offset of the children.
      int offset = Integer.MAX_VALUE;
      int n = getViewCount();
      for (int i = 0; i < n; i++)
        {
          View v = getView(i);
          offset = Math.min(offset, v.getStartOffset());
        }
      return offset;
    }

    /**
     * Overridden to determine the maximum end offset of the row's children.
     */
    public int getEndOffset()
    {
      // Determine minimum start offset of the children.
      int offset = 0;
      int n = getViewCount();
      for (int i = 0; i < n; i++)
        {
          View v = getView(i);
          offset = Math.max(offset, v.getEndOffset());
        }
      return offset;
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
    float align;
    if (axis == X_AXIS)
      align = 0.5F;
    else if (getViewCount() > 0)
      {
        float prefHeight = getPreferredSpan(Y_AXIS);
        float firstRowHeight = getView(0).getPreferredSpan(Y_AXIS);
        align = (firstRowHeight / 2.F) / prefHeight;
      }
    else
      align = 0.5F;
    return align;
  }

  /**
   * Receives notification when some attributes of the displayed element
   * changes. This triggers a refresh of the cached attributes of this
   * paragraph.
   *
   * @param ev the document event
   * @param a the allocation of this view
   * @param vf the view factory to use for creating new child views
   */
  public void changedUpdate(DocumentEvent ev, Shape a, ViewFactory vf)
  {
    setPropertiesFromAttributes();
    layoutChanged(X_AXIS);
    layoutChanged(Y_AXIS);
    super.changedUpdate(ev, a, vf);
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

  /**
   * Finds the next offset in the document that has one of the characters
   * specified in <code>string</code>. If there is no such character found,
   * this returns -1.
   *
   * @param string the characters to search for
   * @param start the start offset
   *
   * @return the next offset in the document that has one of the characters
   *         specified in <code>string</code>
   */
  protected int findOffsetToCharactersInString(char[] string, int start)
  {
    int offset = -1;
    Document doc = getDocument();
    Segment text = new Segment();
    try
      {
        doc.getText(start, doc.getLength() - start, text);
        int index = start;

        searchLoop:
        while (true)
          {
            char ch = text.next();
            if (ch == Segment.DONE)
              break;

            for (int j = 0; j < string.length; ++j)
              {
                if (string[j] == ch)
                  {
                    offset = index;
                    break searchLoop;
                  }
              }
            index++;
          }
      }
    catch (BadLocationException ex)
      {
        // Ignore this and return -1.
      }
    return offset;
  }

  protected int getClosestPositionTo(int pos, Position.Bias bias, Shape a,
                                     int direction, Position.Bias[] biasRet,
                                     int rowIndex, int x)
    throws BadLocationException
  {
    // FIXME: Implement this properly. However, this looks like it might
    // have been replaced by viewToModel.
    return pos;
  }

  /**
   * Returns the size that is used by this view (or it's child views) between
   * <code>startOffset</code> and <code>endOffset</code>. If the child views
   * implement the {@link TabableView} interface, then this is used to
   * determine the span, otherwise we use the preferred span of the child
   * views.
   *
   * @param startOffset the start offset
   * @param endOffset the end offset
   *
   * @return the span used by the view between <code>startOffset</code> and
   *         <code>endOffset</cod>
   */
  protected float getPartialSize(int startOffset, int endOffset)
  {
    int startIndex = getViewIndex(startOffset, Position.Bias.Backward);
    int endIndex = getViewIndex(endOffset, Position.Bias.Forward);
    float span;
    if (startIndex == endIndex)
      {
        View child = getView(startIndex);
        if (child instanceof TabableView)
          {
            TabableView tabable = (TabableView) child;
            span = tabable.getPartialSpan(startOffset, endOffset);
          }
        else
          span = child.getPreferredSpan(X_AXIS);
      }
    else if (endIndex - startIndex == 1)
      {
        View child1 = getView(startIndex);
        if (child1 instanceof TabableView)
          {
            TabableView tabable = (TabableView) child1;
            span = tabable.getPartialSpan(startOffset, child1.getEndOffset());
          }
        else
          span = child1.getPreferredSpan(X_AXIS);
        View child2 = getView(endIndex);
        if (child2 instanceof TabableView)
          {
            TabableView tabable = (TabableView) child2;
            span += tabable.getPartialSpan(child2.getStartOffset(), endOffset);
          }
        else
          span += child2.getPreferredSpan(X_AXIS);
      }
    else
      {
        // Start with the first view.
        View child1 = getView(startIndex);
        if (child1 instanceof TabableView)
          {
            TabableView tabable = (TabableView) child1;
            span = tabable.getPartialSpan(startOffset, child1.getEndOffset());
          }
        else
          span = child1.getPreferredSpan(X_AXIS);

        // Add up the view spans between the start and the end view.
        for (int i = startIndex + 1; i < endIndex; i++)
          {
            View child = getView(i);
            span += child.getPreferredSpan(X_AXIS);
          }

        // Add the span of the last view.
        View child2 = getView(endIndex);
        if (child2 instanceof TabableView)
          {
            TabableView tabable = (TabableView) child2;
            span += tabable.getPartialSpan(child2.getStartOffset(), endOffset);
          }
        else
          span += child2.getPreferredSpan(X_AXIS);
      }
    return span;
  }

  /**
   * Returns the location where the tabs are calculated from. This returns
   * <code>0.0F</code> by default.
   *
   * @return the location where the tabs are calculated from
   */
  protected float getTabBase()
  {
    return 0.0F;
  }

  /**
   * @specnote This method is specified to take a Row parameter, which is a
   *           private inner class of that class, which makes it unusable from
   *           application code. Also, this method seems to be replaced by
   *           {@link FlowStrategy#adjustRow(FlowView, int, int, int)}.
   *
   */
  protected void adjustRow(Row r, int desiredSpan, int x)
  {
  }

  /**
   * @specnote This method's signature differs from the one defined in
   *           {@link View} and is therefore never called. It is probably there
   *           for historical reasons.
   */
  public View breakView(int axis, float len, Shape a)
  {
    // This method is not used.
    return null;
  }

  /**
   * @specnote This method's signature differs from the one defined in
   *           {@link View} and is therefore never called. It is probably there
   *           for historical reasons.
   */
  public int getBreakWeight(int axis, float len)
  {
    // This method is not used.
    return 0;
  }
}
