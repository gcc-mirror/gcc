/* TableView.java -- A view impl for tables inside styled text
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


package javax.swing.text;

import java.awt.Rectangle;
import java.awt.Shape;

import javax.swing.SizeRequirements;
import javax.swing.event.DocumentEvent;

/**
 * A {@link View} implementation for rendering tables inside styled text.
 * Tables are rendered as vertical boxes (see {@link BoxView}). These boxes
 * have a number of child views, which are the rows of the table. These are
 * horizontal boxes containing the actuall cells of the table. These cells
 * can be arbitrary view implementations and are fetched via the
 * {@link ViewFactory} returned by {@link View#getViewFactory}.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public abstract class TableView
  extends BoxView
{

  /**
   * A view implementation that renders a row of a <code>TableView</code>.
   * This is implemented as a horizontal box that contains the actual cells
   * of the table.
   *
   * @author Roman Kennke (kennke@aicas.com)
   */
  public class TableRow
    extends BoxView
  {
    /**
     * Creates a new instance of <code>TableRow</code>.
     *
     * @param el the element for which to create a row view
     */
    public TableRow(Element el)
    {
      super(el, X_AXIS);
    }

    /**
     * Replaces some child views with a new set of child views. This is
     * implemented to call the superclass behaviour and invalidates the row
     * grid so that rows and columns will be recalculated.
     *
     * @param offset the start offset at which to replace views
     * @param length the number of views to remove
     * @param views the new set of views
     */
    public void replace(int offset, int length, View[] views)
    {
      super.replace(offset, length, views);
      int viewCount = getViewCount();
      if (columnRequirements == null
          || viewCount > columnRequirements.length)
        {
          columnRequirements = new SizeRequirements[viewCount];
          for (int i = 0; i < columnRequirements.length; i++)
            columnRequirements[i] = new SizeRequirements();
        }
      if (columnOffsets == null || columnOffsets.length < viewCount)
        columnOffsets = new int[viewCount];
      if (columnSpans == null || columnSpans.length < viewCount)
        columnSpans = new int[viewCount];
      layoutChanged(X_AXIS);
    }

    /**
     * Lays out the box's child views along the major axis. This is
     * reimplemented so that the child views all have the width of their
     * column.
     *
     * @param targetSpan the total span of the view
     * @param axis the axis that is laid out
     * @param offsets an array that holds the offsets of the child views after
     *        this method returned
     * @param spans an array that holds the spans of the child views after this
     *        method returned
     */
    protected void layoutMajorAxis(int targetSpan, int axis, int[] offsets,
                                   int[] spans)
    {
      // Some sanity checks. If these preconditions are not met, then the
      // following code will not work. Also, there must be something
      // seriously wrong then.
      assert(offsets.length == columnOffsets.length);
      assert(spans.length == columnSpans.length);
      assert(offsets.length == spans.length);
      for (int i = 0; i < offsets.length; ++i)
        {
          offsets[i] = columnOffsets[i];
          spans[i] = columnSpans[i];
        }
    }

    /**
     * Lays out the box's child views along the minor axis (the orthogonal axis
     * to the major axis). This is reimplemented to call the super behaviour
     * and then adjust the span of the child views that span multiple rows.
     *
     * @param targetSpan the total span of the view
     * @param axis the axis that is laid out
     * @param offsets an array that holds the offsets of the child views after
     *        this method returned
     * @param spans an array that holds the spans of the child views after this
     *        method returned
     */
    protected void layoutMinorAxis(int targetSpan, int axis, int[] offsets,
                                   int[] spans)
    {
      // FIXME: Figure out how to fetch the row heights from the TableView's
      // element.
      super.layoutMinorAxis(targetSpan, axis, offsets, spans);
    }

    /**
     * Determines the resizeability of this view along the specified axis.
     *
     * @param axis the axis of which to fetch the resizability
     *
     * @return the resize weight or &lt;= 0 if this view is not resizable
     *
     * @throws IllegalArgumentException when an illegal axis is specified
     */
    public int getResizeWeight(int axis)
    {
      // TODO: Figure out if this is ok. I would think so, but better test
      // this.
      return 0;
    }

    /**
     * Returns the child view that represents the specified position in the
     * model. This is reimplemented because in this view we do not necessarily
     * have a one to one mapping of child elements to child views.
     *
     * @param pos the model position for which to query the view
     * @param a the allocation of this view
     *
     * @return the view that corresponds to the specified model position or
     *         <code>null</code> if there is none
     */
    protected View getViewAtPosition(int pos, Rectangle a)
    {
      // FIXME: Do not call super here. Instead walk through the child views
      // and look for a range that contains the given position.
      return super.getViewAtPosition(pos, a);
    }
  }

  /**
   * This class is deprecated and not used anymore. Table cells are
   * rendered by an arbitrary <code>View</code> implementation.
   *
   * @author Roman Kennke (kennke@aicas.com)
   *
   * @deprecated Table cells are now rendered by an arbitrary <code>View</code>
   *             implementation.
   */
  public class TableCell
    extends BoxView
  {

    /**
     * The row number of this cell.
     */
    private int row;

    /**
     * The column number of this cell.
     */
    private int column;

    /**
     * Creates a new instance.
     *
     * @param el the element
     *
     * @deprecated Table cells are now rendered by an arbitrary
     *             <code>View</code> implementation.
     */
    public TableCell(Element el)
    {
      super(el, X_AXIS);
    }

    /**
     * Returns the number of columns that this cell spans.
     *
     * @return the number of columns that this cell spans
     *
     * @deprecated Table cells are now rendered by an arbitrary
     *             <code>View</code> implementation.
     */
    public int getColumnCount()
    {
      // TODO: Figure out if this is right. However, this is not so important
      // since this class isn't used anyway (except maybe be application code
      // that still uses this deprecated class).
      return 1;
    }

    /**
     * Returns the number of rows that this cell spans.
     *
     * @return the number of rows that this cell spans
     *
     * @deprecated Table cells are now rendered by an arbitrary
     *             <code>View</code> implementation.
     */
    public int getRowCount()
    {
      // TODO: Figure out if this is right. However, this is not so important
      // since this class isn't used anyway (except maybe be application code
      // that still uses this deprecated class).
      return 1;
    }

    /**
     * Sets the grid location of this table cell.
     *
     * @param r the row of this cell
     * @param c the column of this cell
     *
     * @deprecated Table cells are now rendered by an arbitrary
     *             <code>View</code> implementation.
     */
    public void setGridLocation(int r, int c)
    {
      row = r;
      column = c;
    }

    /**
     * Returns the row number of this cell.
     *
     * @return the row number of this cell
     *
     * @deprecated Table cells are now rendered by an arbitrary
     *             <code>View</code> implementation.
     */
    public int getGridRow()
    {
      return row;
    }

    /**
     * Returns the column number of this cell.
     *
     * @return the column number of this cell
     *
     * @deprecated Table cells are now rendered by an arbitrary
     *             <code>View</code> implementation.
     */
    public int getGridColumn()
    {
      return column;
    }
  }

  /**
   * The offsets of the columns of this table. Package private to avoid
   * synthetic accessor methods.
   */
  int[] columnOffsets;

  /**
   * The spans of the columns of this table. Package private to avoid
   * synthetic accessor methods.
   */
  int[] columnSpans;

  /**
   * The size requirements of the columns.
   */
  SizeRequirements[] columnRequirements = new SizeRequirements[0];

  /**
   * Creates a new instance of <code>TableView</code>.
   *
   * @param el the element for which to create a table view
   */
  public TableView(Element el)
  {
    super(el, Y_AXIS);
  }

  /**
   * Replaces a number of child views with a set of new child views. This is
   * implemented to call the superclass behaviour and invalidate the layout.
   *
   * @param offset the offset at which to replace child views
   * @param length the number of child views to remove
   * @param views the new set of views
   */
  public void replace(int offset, int length, View[] views)
  {
    super.replace(offset, length, views);
    layoutChanged(Y_AXIS);
  }

  /**
   * Creates a view for a table row.
   *
   * @param el the element that represents the table row
   *
   * @return a view for rendering the table row
   */
  protected TableRow createTableRow(Element el)
  {
    return new TableRow(el);
  }

  /**
   * Creates a view for a table cell. This method is deprecated and not used
   * anymore.
   *
   * @param el the element that represents the table cell
   *
   * @return a view for rendering the table cell
   *
   * @deprecated Table cells are now rendered by an arbitrary
   *             <code>View</code> implementation.
   */
  protected TableCell createTableCell(Element el)
  {
    return new TableCell(el);
  }

  protected void forwardUpdate(DocumentEvent.ElementChange ec, DocumentEvent e,
                               Shape a, ViewFactory vf)
  {
    // TODO: Figure out what to do here.
  }

  /**
   * Lays out the columns to fit within the specified target span.
   *
   * @param targetSpan the total span for the columns
   * @param offsets an array that holds the offsets of the columns when this
   *        method returns
   * @param spans an array that holds the spans of the columns when this method
   *        returns
   * @param reqs the size requirements for each column
   */
  protected void layoutColumns(int targetSpan, int[] offsets, int spans[],
                               SizeRequirements[] reqs)
  {
    updateColumnRequirements();
    SizeRequirements r = calculateMinorAxisRequirements(X_AXIS, null);
    SizeRequirements.calculateTiledPositions(targetSpan, r, columnRequirements,
                                             offsets, spans);
  }

  /**
   * Lays out the child views along the minor axis of the table (that is the
   * horizontal axis). This is implemented to call {@link #layoutColumns} to
   * layout the column layout of this table, and then forward to the superclass
   * to actually lay out the rows.
   *
   * @param targetSpan the available span along the minor (horizontal) axis
   * @param axis the axis
   * @param offsets an array that holds the offsets of the columns when this
   *        method returns
   * @param spans an array that holds the spans of the columns when this method
   *        returns
   */
  protected void layoutMinorAxis(int targetSpan, int axis, int[] offsets,
                                 int[] spans)
  {
    // TODO: Prepare size requirements for the columns.
    layoutColumns(targetSpan, columnOffsets, columnSpans, columnRequirements);
    super.layoutMinorAxis(targetSpan, axis, offsets, spans);
  }

  /**
   * Calculates the requirements of this view for the minor (== horizontal)
   * axis.
   *
   * This is reimplemented to calculate the requirements as the sum of the
   * size requirements of the columns.
   *
   * @param axis the axis
   * @param req the size requirements object to use, if <code>null</code> a new
   *        one will be created
   */
  protected SizeRequirements calculateMinorAxisRequirements(int axis,
                                                            SizeRequirements req)
  {
    // TODO: Maybe prepare columnRequirements.
    SizeRequirements res = req;
    if (res == null)
      res = new SizeRequirements();
    else
      {
        res.alignment = 0.5f;
        res.maximum = 0;
        res.minimum = 0;
        res.preferred = 0;
      }

    for (int i = 0; i < columnRequirements.length; ++i)
      {
        res.minimum += columnRequirements[i].minimum;
        res.preferred += columnRequirements[i].preferred;
        res.maximum += columnRequirements[i].maximum;
        // TODO: Do we have to handle alignment somehow?
      }
    return res;
  }

  /**
   * Returns the child view that represents the specified position in the
   * model. This is reimplemented because in this view we do not necessarily
   * have a one to one mapping of child elements to child views.
   *
   * @param pos the model position for which to query the view
   * @param a the allocation of this view
   *
   * @return the view that corresponds to the specified model position or
   *         <code>null</code> if there is none
   */
  protected View getViewAtPosition(int pos, Rectangle a)
  {
    // FIXME: Do not call super here. Instead walk through the child views
    // and look for a range that contains the given position.
    return super.getViewAtPosition(pos, a);
  }

  /**
   * Updates the column requirements.
   */
  private void updateColumnRequirements()
  {
    int rowCount = getViewCount();
    for (int r = 0; r < rowCount; ++r)
      {
        TableRow row = (TableRow) getView(r);
        int columnCount = row.getViewCount();
        for (int c = 0; c < columnCount; ++c)
          {
            View cell = row.getView(c);
            SizeRequirements cr = columnRequirements[c];
            cr.minimum = Math.max(cr.minimum, (int) cell.getMinimumSpan(X_AXIS));
            cr.preferred = Math.max(cr.preferred,
                                    (int) cell.getPreferredSpan(X_AXIS));
            cr.maximum = Math.max(cr.maximum, (int) cell.getMaximumSpan(X_AXIS));
          }
      }
  }
}
