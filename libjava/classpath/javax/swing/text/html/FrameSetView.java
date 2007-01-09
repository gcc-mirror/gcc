/* FrameSetView.java -- Implements HTML frameset
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

import java.util.StringTokenizer;

import javax.swing.text.AttributeSet;
import javax.swing.text.BoxView;
import javax.swing.text.Element;
import javax.swing.text.View;
import javax.swing.text.ViewFactory;

/**
 * Implements HTML framesets. This is implemented as a vertical box that
 * holds the rows of the frameset. Each row is again a horizontal box that
 * holds the actual columns.
 */
public class FrameSetView
  extends BoxView
{

  /**
   * A row of a frameset.
   */
  private class FrameSetRow
    extends BoxView
  {
    private int row;
    FrameSetRow(Element el, int r)
    {
      super(el, X_AXIS);
      row = r;
    }

    protected void loadChildren(ViewFactory f)
    {
      // Load the columns here.
      Element el = getElement();
      View[] columns = new View[numViews[X_AXIS]];
      int offset = row * numViews[X_AXIS];
      for (int c = 0; c < numViews[X_AXIS]; c++)
        {
          Element child = el.getElement(offset + c);
          columns[c] = f.create(child);
        }
      replace(0, 0, columns);
    }

    protected void layoutMajorAxis(int targetSpan, int axis, int[] offsets,
                                   int[] spans)
    {
      int numRows = numViews[X_AXIS];
      int[] abs = absolute[X_AXIS];
      int[] rel = relative[X_AXIS];
      int[] perc = percent[X_AXIS];
      layoutViews(targetSpan, axis, offsets, spans, numRows, abs, rel, perc);
    }
  }

  /**
   * Holds the absolute layout information for the views along one axis. The
   * indices are absolute[axis][index], where axis is either X_AXIS (columns)
   * or Y_AXIS (rows). Rows or columns that don't have absolute layout have
   * a -1 in this array.
   */
  int[][] absolute;

  /**
   * Holds the relative (*) layout information for the views along one axis.
   * The indices are relative[axis][index], where axis is either X_AXIS
   * (columns) or Y_AXIS (rows). Rows or columns that don't have relative
   * layout have a Float.NaN in this array.
   */
  int[][] relative;

  /**
   * Holds the relative (%) layout information for the views along one axis.
   * The indices are relative[axis][index], where axis is either X_AXIS
   * (columns) or Y_AXIS (rows). Rows or columns that don't have relative
   * layout have a Float.NaN in this array.
   *
   * The percentage is divided by 100 so that we hold the actual fraction here.
   */
  int[][] percent;

  /**
   * The number of children in each direction.
   */
  int[] numViews;

  FrameSetView(Element el)
  {
    super(el, Y_AXIS);
    numViews = new int[2];
    absolute = new int[2][];
    relative = new int[2][];
    percent = new int[2][];
  }

  /**
   * Loads the children and places them inside the grid.
   */
  protected void loadChildren(ViewFactory f)
  {
    parseRowsCols();
    // Set up the rows.
    View[] rows = new View[numViews[Y_AXIS]];
    for (int r = 0; r < numViews[Y_AXIS]; r++)
      {
        rows[r] = new FrameSetRow(getElement(), r);
      }
    replace(0, 0, rows);
  }

  /**
   * Parses the rows and cols attributes and sets up the layout info.
   */
  private void parseRowsCols()
  {
    Element el = getElement();
    AttributeSet atts = el.getAttributes();
    String cols = (String) atts.getAttribute(HTML.Attribute.COLS);
    if (cols == null) // Defaults to '100%' when not specified.
      cols = "100%";
    parseLayout(cols, X_AXIS);
    String rows = (String) atts.getAttribute(HTML.Attribute.ROWS);
    if (rows == null) // Defaults to '100%' when not specified.
      rows = "100%";
    parseLayout(rows, Y_AXIS);
  }

  /**
   * Parses the cols or rows attribute and places the layout info in the
   * appropriate arrays.
   *
   * @param att the attributes to parse
   * @param axis the axis
   */
  private void parseLayout(String att, int axis)
  {
    StringTokenizer tokens = new StringTokenizer(att, ",");
    numViews[axis] = tokens.countTokens();
    absolute[axis] = new int[numViews[axis]];
    relative[axis] = new int[numViews[axis]];
    percent[axis] = new int[numViews[axis]];
    for (int index = 0; tokens.hasMoreTokens(); index++)
      {
        String token = tokens.nextToken();
        int p = token.indexOf('%');
        int s = token.indexOf('*');
        if (p != -1)
          {
            // Percent value.
            String number = token.substring(0, p);
            try
              {
                percent[axis][index] = Integer.parseInt(number);
              }
            catch (NumberFormatException ex)
              {
                // Leave value as 0 then.
              }
          }
        else if (s != -1)
          {
            // Star relative value.
            String number = token.substring(0, s);
            try
              {
                relative[axis][index] = Integer.parseInt(number);
              }
            catch (NumberFormatException ex)
              {
                // Leave value as 0 then.
              }
          }
        else
          {
            // Absolute value.
            try
              {
                absolute[axis][index] = Integer.parseInt(token);
              }
            catch (NumberFormatException ex)
              {
                // Leave value as 0 then.
              }
          }
      }
  }

  protected void layoutMajorAxis(int targetSpan, int axis, int[] offsets,
                                 int[] spans)
  {
    int numRows = numViews[Y_AXIS];
    int[] abs = absolute[Y_AXIS];
    int[] rel = relative[Y_AXIS];
    int[] perc = percent[Y_AXIS];
    layoutViews(targetSpan, axis, offsets, spans, numRows, abs, rel, perc);
  }

  void layoutViews(int targetSpan, int axis, int[] offsets, int[] spans,
                   int numViews, int[] abs, int[] rel, int[] perc)
  {
    // We need two passes. In the first pass we layout the absolute and
    // percent values and accumulate the needed space. In the second pass
    // the relative values are distributed and the offsets are set.
    int total = 0;
    int relTotal = 0;
    for (int i = 0; i < numViews; i++)
      {
        if (abs[i] > 0)
          {
            spans[i] = abs[i];
            total += spans[i];
          }
        else if (perc[i] > 0)
          {
            spans[i] = (targetSpan * perc[i]) / 100;
            total += spans[i];
          }
        else if (rel[i] > 0)
          {
            relTotal += rel[i];
          }
      }
    int offs = 0;
    for (int i = 0; i < numViews; i++)
      {
        if (relTotal > 0 && rel[i] > 0)
          {
            spans[i] = targetSpan * (rel[i] / relTotal);
          }
        offsets[i] = offs;
        offs += spans[i];
      }
  }
}
