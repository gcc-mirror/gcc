/* TableColumn.java --
   Copyright (C) 2002, 2004, 2005, 2006, Free Software Foundation, Inc.

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


package javax.swing.table;

import java.awt.Component;
import java.awt.Dimension;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.Serializable;

import javax.swing.event.SwingPropertyChangeSupport;

/**
 * Represents the attributes of a column in a table, including the column index,
 * width, minimum width, preferred width and maximum width.
 * 
 * @author	Andrew Selkirk
 */
public class TableColumn
  implements Serializable
{
  static final long serialVersionUID = -6113660025878112608L;

  /**
   * The name for the <code>columnWidth</code> property (this field is
   * obsolete and no longer used).  Note also that the typo in the value 
   * string is deliberate, to match the specification.
   */
  public static final String COLUMN_WIDTH_PROPERTY = "columWidth";

  /**
   * The name for the <code>headerValue</code> property.
   */
  public static final String HEADER_VALUE_PROPERTY = "headerValue";

  /**
   * The name for the <code>headerRenderer</code> property.
   */
  public static final String HEADER_RENDERER_PROPERTY = "headerRenderer";

  /**
   * The name for the <code>cellRenderer</code> property.
   */
  public static final String CELL_RENDERER_PROPERTY = "cellRenderer";

  /**
   * The index of the corresponding column in the table model.
   */
  protected int modelIndex;

  /**
   * The identifier for the column.
   */
  protected Object identifier;

  /**
   * The current width for the column.
   */
  protected int width;

  /**
   * The minimum width for the column.
   */
  protected int minWidth = 15;

  /**
   * The preferred width for the column.
   */
  private int preferredWidth;

  /**
   * The maximum width for the column.
   */
  protected int maxWidth = Integer.MAX_VALUE;

  /**
   * The renderer for the column header.
   */
  protected TableCellRenderer headerRenderer;

  /**
   * The value for the column header.
   */
  protected Object headerValue;

  /**
   * The renderer for the regular cells in this column.
   */
  protected TableCellRenderer cellRenderer;

  /**
   * An editor for the regular cells in this column.
   */
  protected TableCellEditor cellEditor;

  /**
   * A flag that determines whether or not the column is resizable (the default
   * is <code>true</code>).
   */
  protected boolean isResizable = true;

  /**
   * resizedPostingDisableCount
   *
   * @deprecated 1.3
   */
  protected transient int resizedPostingDisableCount;

  /**
   * A storage and notification mechanism for property change listeners.
   */
  private SwingPropertyChangeSupport changeSupport =
    new SwingPropertyChangeSupport(this);

  /**
   * Creates a new <code>TableColumn</code> that maps to column 0 in the
   * related table model.  The default width is <code>75</code> units.
   */
  public TableColumn()
  {
    this(0, 75, null, null);
  }

  /**
   * Creates a new <code>TableColumn</code> that maps to the specified column 
   * in the related table model.  The default width is <code>75</code> units.
   * 
   * @param modelIndex the index of the column in the model
   */
  public TableColumn(int modelIndex)
  {
    this(modelIndex, 75, null, null);
  }

  /**
   * Creates a new <code>TableColumn</code> that maps to the specified column 
   * in the related table model, and has the specified <code>width</code>.
   * 
   * @param modelIndex the index of the column in the model
   * @param width the width
   */
  public TableColumn(int modelIndex, int width)
  {
    this(modelIndex, width, null, null);
  }

  /**
   * Creates a new <code>TableColumn</code> that maps to the specified column 
   * in the related table model, and has the specified <code>width</code>,
   * <code>cellRenderer</code> and <code>cellEditor</code>.
   * 
   * @param modelIndex the index of the column in the model
   * @param width the width
   * @param cellRenderer the cell renderer (<code>null</code> permitted).
   * @param cellEditor the cell editor (<code>null</code> permitted).
   */
  public TableColumn(int modelIndex, int width,
                     TableCellRenderer cellRenderer, TableCellEditor cellEditor)
  {
    this.modelIndex = modelIndex;
    this.width = width;
    this.preferredWidth = width;
    this.cellRenderer = cellRenderer;
    this.cellEditor = cellEditor;
    this.headerValue = null;
    this.identifier = null;
  }

  /**
   * Sets the index of the column in the related {@link TableModel} that this
   * <code>TableColumn</code> maps to, and sends a {@link PropertyChangeEvent}
   * (with the property name 'modelIndex') to all registered listeners.
   * 
   * @param modelIndex the column index in the model.
   * 
   * @see #getModelIndex()
   */
  public void setModelIndex(int modelIndex)
  {
    if (this.modelIndex != modelIndex)
      {
        int oldValue = this.modelIndex;
        this.modelIndex = modelIndex;
        changeSupport.firePropertyChange("modelIndex", oldValue, modelIndex);
      }
  }

  /**
   * Returns the index of the column in the related {@link TableModel} that
   * this <code>TableColumn</code> maps to.
   * 
   * @return the model index.
   * 
   * @see #setModelIndex(int)
   */
  public int getModelIndex()
  {
    return modelIndex;
  }

  /**
   * Sets the identifier for the column and sends a {@link PropertyChangeEvent}
   * (with the property name 'identifier') to all registered listeners.
   * 
   * @param identifier the identifier (<code>null</code> permitted).
   * 
   * @see #getIdentifier()
   */
  public void setIdentifier(Object identifier)
  {
    if (this.identifier != identifier)
      {       
        Object oldValue = this.identifier;
        this.identifier = identifier;
        changeSupport.firePropertyChange("identifier", oldValue, identifier);
      }
  }

  /**
   * Returns the identifier for the column, or {@link #getHeaderValue()} if the 
   * identifier is <code>null</code>.
   * 
   * @return The identifier (or {@link #getHeaderValue()} if the identifier is 
   *         <code>null</code>).
   */
  public Object getIdentifier()
  {
    if (identifier == null)
      return getHeaderValue();
    return identifier;
  }

  /**
   * Sets the header value and sends a {@link PropertyChangeEvent} (with the 
   * property name {@link #HEADER_VALUE_PROPERTY}) to all registered listeners.
   * 
   * @param headerValue the value of the header (<code>null</code> permitted).
   * 
   * @see #getHeaderValue()
   */
  public void setHeaderValue(Object headerValue)
  {
    if (this.headerValue == headerValue)
      return;
    
    Object oldValue = this.headerValue;
    this.headerValue = headerValue;
    changeSupport.firePropertyChange(HEADER_VALUE_PROPERTY, oldValue, 
                                     headerValue);
  }

  /**
   * Returns the header value.
   * 
   * @return the value of the header.
   * 
   * @see #getHeaderValue()
   */
  public Object getHeaderValue()
  {
    return headerValue;
  }

  /**
   * Sets the renderer for the column header and sends a 
   * {@link PropertyChangeEvent} (with the property name 
   * {@link #HEADER_RENDERER_PROPERTY}) to all registered listeners.
   * 
   * @param renderer the header renderer (<code>null</code> permitted).
   * 
   * @see #getHeaderRenderer()
   */
  public void setHeaderRenderer(TableCellRenderer renderer)
  {
    if (headerRenderer == renderer)
      return;
    
    TableCellRenderer oldRenderer = headerRenderer;
    headerRenderer = renderer;
    changeSupport.firePropertyChange(HEADER_RENDERER_PROPERTY, oldRenderer, 
                                     headerRenderer);
  }

  /**
   * Returns the renderer for the column header.
   * 
   * @return The renderer for the column header (possibly <code>null</code>).
   * 
   * @see #setHeaderRenderer(TableCellRenderer)
   */
  public TableCellRenderer getHeaderRenderer()
  {
    return headerRenderer;
  }

  /**
   * Sets the renderer for cells in this column and sends a 
   * {@link PropertyChangeEvent} (with the property name 
   * {@link #CELL_RENDERER_PROPERTY}) to all registered listeners.
   * 
   * @param renderer the cell renderer (<code>null</code> permitted).
   * 
   * @see #getCellRenderer()
   */
  public void setCellRenderer(TableCellRenderer renderer)
  {
    if (cellRenderer == renderer)
      return;
    
    TableCellRenderer oldRenderer = cellRenderer;
    cellRenderer = renderer;
    changeSupport.firePropertyChange(CELL_RENDERER_PROPERTY, oldRenderer, 
                                     cellRenderer);
  }

  /**
   * Returns the renderer for the table cells in this column.
   * 
   * @return The cell renderer (possibly <code>null</code>).
   * 
   * @see #setCellRenderer(TableCellRenderer)
   */
  public TableCellRenderer getCellRenderer()
  {
    return cellRenderer;
  }

  /**
   * Sets the cell editor for the column and sends a {@link PropertyChangeEvent}
   * (with the property name 'cellEditor') to all registered listeners.
   * 
   * @param cellEditor the cell editor (<code>null</code> permitted).
   * 
   * @see #getCellEditor()
   */
  public void setCellEditor(TableCellEditor cellEditor)
  {
    if (this.cellEditor != cellEditor)
      {
        TableCellEditor oldValue = this.cellEditor;
        this.cellEditor = cellEditor;
        changeSupport.firePropertyChange("cellEditor", oldValue, cellEditor);
      }
  }

  /**
   * Returns the cell editor for the column (the default value is 
   * <code>null</code>).
   * 
   * @return The cell editor (possibly <code>null</code>).
   * 
   * @see #setCellEditor(TableCellEditor)
   */
  public TableCellEditor getCellEditor()
  {
    return cellEditor;
  }

  /**
   * Sets the width for the column and sends a {@link PropertyChangeEvent} 
   * (with the property name 'width') to all registered listeners.  If the new
   * width falls outside the range getMinWidth() to getMaxWidth() it is 
   * adjusted to the appropriate boundary value.
   * 
   * @param newWidth the width.
   * 
   * @see #getWidth()
   */
  public void setWidth(int newWidth)
  {
    int	oldWidth = width;

    if (newWidth < minWidth)
      width = minWidth;
    else if (newWidth > maxWidth)
      width = maxWidth;
    else
      width = newWidth;

    if (width == oldWidth)
      return;

    // We do have a constant field COLUMN_WIDTH_PROPERTY,
    // however, tests show that the actual fired property name is 'width'
    // and even Sun's API docs say that this constant field is obsolete and
    // not used.
    changeSupport.firePropertyChange("width", oldWidth, width);
  }

  /**
   * Returns the width for the column (the default value is <code>75</code>).
   * 
   * @return The width.
   *
   * @see #setWidth(int)
   */
  public int getWidth()
  {
    return width;
  }

  /**
   * Sets the preferred width for the column and sends a 
   * {@link PropertyChangeEvent} (with the property name 'preferredWidth') to 
   * all registered listeners.  If necessary, the supplied value will be 
   * adjusted to fit in the range {@link #getMinWidth()} to 
   * {@link #getMaxWidth()}.
   * 
   * @param preferredWidth  the preferred width.
   * 
   * @see #getPreferredWidth()
   */
  public void setPreferredWidth(int preferredWidth)
  {
    int oldPrefWidth = this.preferredWidth;

    if (preferredWidth < minWidth)
      this.preferredWidth = minWidth;
    else if (preferredWidth > maxWidth)
      this.preferredWidth = maxWidth;
    else
      this.preferredWidth = preferredWidth;

    changeSupport.firePropertyChange("preferredWidth", oldPrefWidth, 
                                     this.preferredWidth);
  }

  /**
   * Returns the preferred width for the column (the default value is 
   * <code>75</code>).
   * 
   * @return The preferred width.
   * 
   * @see #setPreferredWidth(int)
   */
  public int getPreferredWidth()
  {
    return preferredWidth;
  }

  /**
   * Sets the minimum width for the column and sends a 
   * {@link PropertyChangeEvent} (with the property name 'minWidth') to all
   * registered listeners.  If the current <code>width</code> and/or 
   * <code>preferredWidth</code> are less than the new minimum width, they are
   * adjusted accordingly.
   * 
   * @param minWidth  the minimum width (negative values are treated as 0).
   * 
   * @see #getMinWidth()
   */
  public void setMinWidth(int minWidth)
  {
    if (minWidth < 0)
      minWidth = 0;
    if (this.minWidth != minWidth)
      {
        if (width < minWidth)
          setWidth(minWidth);
        if (preferredWidth < minWidth)
          setPreferredWidth(minWidth);
        int oldValue = this.minWidth;
        this.minWidth = minWidth;
        changeSupport.firePropertyChange("minWidth", oldValue, minWidth);
      }
  }

  /**
   * Returns the <code>TableColumn</code>'s minimum width (the default value
   * is <code>15</code>).
   * 
   * @return The minimum width.
   * 
   * @see #setMinWidth(int)
   */
  public int getMinWidth()
  {
    return minWidth;
  }

  /**
   * Sets the maximum width for the column and sends a 
   * {@link PropertyChangeEvent} (with the property name 'maxWidth') to all
   * registered listeners.  If the current <code>width</code> and/or 
   * <code>preferredWidth</code> are greater than the new maximum width, they 
   * are adjusted accordingly.
   * 
   * @param maxWidth the maximum width.
   * 
   * @see #getMaxWidth()
   */
  public void setMaxWidth(int maxWidth)
  {
    if (this.maxWidth != maxWidth)
      {
        if (width > maxWidth)
          setWidth(maxWidth);
        if (preferredWidth > maxWidth)
          setPreferredWidth(maxWidth);
        int oldValue = this.maxWidth;
        this.maxWidth = maxWidth;
        changeSupport.firePropertyChange("maxWidth", oldValue, maxWidth);
       }
  }

  /**
   * Returns the maximum width for the column (the default value is
   * {@link Integer#MAX_VALUE}).
   * 
   * @return The maximum width for the column.
   * 
   * @see #setMaxWidth(int)
   */
  public int getMaxWidth()
  {
    return maxWidth;
  }

  /**
   * Sets the flag that controls whether or not the column is resizable, and
   * sends a {@link PropertyChangeEvent} (with the property name 'isResizable')
   * to all registered listeners.
   * 
   * @param isResizable <code>true</code> if this column is resizable,
   * <code>false</code> otherwise.
   * 
   * @see #getResizable()
   */
  public void setResizable(boolean isResizable)
  {
    if (this.isResizable != isResizable)
      {
        this.isResizable = isResizable;
        changeSupport.firePropertyChange("isResizable", !this.isResizable, 
            isResizable);
      }
  }

  /**
   * Returns the flag that controls whether or not the column is resizable.
   * 
   * @return <code>true</code> if this column is resizable,
   * <code>false</code> otherwise.
   * 
   * @see #setResizable(boolean)
   */
  public boolean getResizable()
  {
    return isResizable;
  }

  /**
   * Sets the minimum, maximum, preferred and current width to match the
   * minimum, maximum and preferred width of the header renderer component.
   * If there is no header renderer component, this method does nothing.
   */
  public void sizeWidthToFit()
  {
    if (headerRenderer == null)
      return;
    Component c = headerRenderer.getTableCellRendererComponent(null, 
        getHeaderValue(), false, false, 0, 0);
    Dimension min = c.getMinimumSize();
    Dimension max = c.getMaximumSize();
    Dimension pref = c.getPreferredSize();
    setMinWidth(min.width);
    setMaxWidth(max.width);
    setPreferredWidth(pref.width);
    setWidth(pref.width);
  }

  /**
   * This method is empty, unused and deprecated.
   * @deprecated 1.3
   */
  public void disableResizedPosting()
  {
    // Does nothing
  }

  /**
   * This method is empty, unused and deprecated.
   * @deprecated 1.3
   */
  public void enableResizedPosting()
  {
    // Does nothing
  }

  /**
   * Adds a listener so that it receives {@link PropertyChangeEvent} 
   * notifications from this column.  The properties defined by the column are:
   * <ul>
   * <li><code>width</code> - see {@link #setWidth(int)};</li>
   * <li><code>preferredWidth</code> - see {@link #setPreferredWidth(int)};</li>
   * <li><code>minWidth</code> - see {@link #setMinWidth(int)};</li> 
   * <li><code>maxWidth</code> - see {@link #setMaxWidth(int)};</li>
   * <li><code>modelIndex</code> - see {@link #setModelIndex(int)};</li>
   * <li><code>isResizable</code> - see {@link #setResizable(boolean)};</li>
   * <li><code>cellRenderer</code> - see 
   *   {@link #setCellRenderer(TableCellRenderer)};</li>
   * <li><code>cellEditor</code> - see 
   *   {@link #setCellEditor(TableCellEditor)};</li>
   * <li><code>headerRenderer</code> - see 
   *   {@link #setHeaderRenderer(TableCellRenderer)};</li>
   * <li><code>headerValue</code> - see {@link #setHeaderValue(Object)};</li>
   * <li><code>identifier</code> - see {@link #setIdentifier(Object)}.</li>
   * </ul>
   * 
   * @param listener the listener to add (<code>null</code> is ignored).
   * 
   * @see #removePropertyChangeListener(PropertyChangeListener)
   */
  public synchronized void addPropertyChangeListener(
      PropertyChangeListener listener)
  {
    changeSupport.addPropertyChangeListener(listener);
  }

  /**
   * Removes a listener so that it no longer receives 
   * {@link PropertyChangeEvent} notifications from this column.  If 
   * <code>listener</code> is not registered with the column, or is 
   * <code>null</code>, this method does nothing.
   * 
   * @param listener the listener to remove (<code>null</code> is ignored).
   */
  public synchronized void removePropertyChangeListener(
      PropertyChangeListener listener)
  {
    changeSupport.removePropertyChangeListener(listener);
  }

  /**
   * Returns the property change listeners for this <code>TableColumn</code>.
   * An empty array is returned if there are currently no listeners registered.
   * 
   * @return The property change listeners registered with this column.
   * 
   * @since 1.4
   */
  public PropertyChangeListener[] getPropertyChangeListeners()
  {
    return changeSupport.getPropertyChangeListeners();
  }

  /**
   * Creates and returns a default renderer for the column header (in this case,
   * a new instance of {@link DefaultTableCellRenderer}).
   * 
   * @return A default renderer for the column header.
   */
  protected TableCellRenderer createDefaultHeaderRenderer()
  {
    return new DefaultTableCellRenderer();
  }
}
