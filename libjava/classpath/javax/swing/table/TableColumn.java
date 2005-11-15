/* TableColumn.java --
   Copyright (C) 2002, 2004, 2005 Free Software Foundation, Inc.

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

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.Serializable;

import javax.swing.event.SwingPropertyChangeSupport;

/**
 * Represents the attributes of a column in a table, including the column index,
 * width, minimum width, preferred width and maximum width.
 * 
 * @author	Andrew Selkirk
 * @version	1.0
 */
public class TableColumn
  implements Serializable
{
  static final long serialVersionUID = -6113660025878112608L;

  /**
   * The name for the <code>columnWidth</code> property.  Note that the typo
   * in the name value is deliberate, to match the specification.
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
   * The width.
   */
  protected int width;

  /**
   * The minimum width.
   */
  protected int minWidth = 15;

  /**
   * The preferred width.
   */
  private int preferredWidth;

  /**
   * The maximum width.
   */
  protected int maxWidth = Integer.MAX_VALUE;

  /**
   * headerRenderer
   */
  protected TableCellRenderer headerRenderer;

  /**
   * The header value.
   */
  protected Object headerValue;

  /**
   * cellRenderer
   */
  protected TableCellRenderer cellRenderer;

  /**
   * cellEditor
   */
  protected TableCellEditor cellEditor;

  /**
   * isResizable
   */
  protected boolean isResizable = true;

  /**
   * resizedPostingDisableCount
   *
   * @deprecated 1.3
   */
  protected transient int resizedPostingDisableCount;

  /**
   * changeSupport
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
   * firePropertyChange
   * 
   * @param property the name of the property
   * @param oldValue the old value
   * @param newValue the new value
   */
  private void firePropertyChange(String property, Object oldValue,
                                  Object newValue)
  {
    changeSupport.firePropertyChange(property, oldValue, newValue);
  }

  /**
   * firePropertyChange
   * 
   * @param property the name of the property
   * @param oldValue the old value
   * @param newValue the new value
   */
  private void firePropertyChange(String property, int oldValue, int newValue)
  {
    firePropertyChange(property, new Integer(oldValue), new Integer(newValue));
  }

  /**
   * firePropertyChange
   * 
   * @param property the name of the property 
   * @param oldValue the old value
   * @param newValue the new value
   */
  private void firePropertyChange(String property, boolean oldValue,
                                  boolean newValue)
  {
    firePropertyChange(property, Boolean.valueOf(oldValue),
		       Boolean.valueOf(newValue));
  }

  /**
   * Sets the index of the column in the related {@link TableModel} that this
   * <code>TableColumn</code> maps to.
   * 
   * @param modelIndex the column index in the model.
   */
  public void setModelIndex(int modelIndex)
  {
    this.modelIndex = modelIndex;
  }

  /**
   * Returns the index of the column in the related {@link TableModel} that
   * this <code>TableColumn</code> maps to.
   * 
   * @return the model index
   */
  public int getModelIndex()
  {
    return modelIndex;
  }

  /**
   * Sets the identifier for the column.
   * 
   * @param identifier the identifier
   */
  public void setIdentifier(Object identifier)
  {
    this.identifier = identifier;
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
   * Sets the header value and sends a {@link PropertyChangeEvent} to all
   * registered listeners.  The header value property uses the name
   * {@link #HEADER_VALUE_PROPERTY}.
   * 
   * @param headerValue the value of the header
   */
  public void setHeaderValue(Object headerValue)
  {
    if (this.headerValue == headerValue)
      return;
    
    Object oldValue = this.headerValue;
    this.headerValue = headerValue;
    firePropertyChange(HEADER_VALUE_PROPERTY, oldValue, headerValue);
  }

  /**
   * Returns the header value.
   * 
   * @return the value of the header
   */
  public Object getHeaderValue()
  {
    return headerValue;
  }

  /**
   * setHeaderRenderer
   * 
   * @param renderer the renderer to use
   */
  public void setHeaderRenderer(TableCellRenderer renderer)
  {
    if (headerRenderer == renderer)
      return;
    
    TableCellRenderer oldRenderer = headerRenderer;
    headerRenderer = renderer;
    firePropertyChange(HEADER_RENDERER_PROPERTY, 
		       oldRenderer, headerRenderer);
  }

  /**
   * getHeaderRenderer
   * @return TableCellRenderer
   */
  public TableCellRenderer getHeaderRenderer()
  {
    return headerRenderer;
  }

  /**
   * Sets the renderer for cells in this column and sends a 
   * {@link PropertyChangeEvent} to all registered listeners.
   * 
   * @param renderer the cell renderer (<code>null</code> permitted).
   */
  public void setCellRenderer(TableCellRenderer renderer)
  {
    if (cellRenderer == renderer)
      return;
    
    TableCellRenderer oldRenderer = cellRenderer;
    cellRenderer = renderer;
    firePropertyChange(CELL_RENDERER_PROPERTY, 
		       oldRenderer, cellRenderer);
  }

  /**
   * Returns the renderer for the table cells in this column.
   * 
   * @return The cell renderer.
   */
  public TableCellRenderer getCellRenderer()
  {
    return cellRenderer;
  }

  /**
   * setCellEditor
   * 
   * @param cellEditor the cell editor
   */
  public void setCellEditor(TableCellEditor cellEditor)
  {
    this.cellEditor = cellEditor;
  }

  /**
   * getCellEditor
   * 
   * @return the cell editor
   */
  public TableCellEditor getCellEditor()
  {
    return cellEditor;
  }

  /**
   * setWidth
   * 
   * @param newWidth the width
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
    firePropertyChange("width", oldWidth, width);
  }

  /**
   * getWidth
   * 
   * @return int
   */
  public int getWidth()
  {
    return width;
  }

  /**
   * setPreferredWidth
   * 
   * @param preferredWidth the preferred width
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

    firePropertyChange("preferredWidth", oldPrefWidth, this.preferredWidth);
  }

  /**
   * getPreferredWidth
   * 
   * @return the preferred width
   */
  public int getPreferredWidth()
  {
    return preferredWidth;
  }

  /**
   * Sets the minimum width for the column and, if necessary, updates the
   * <code>width</code> and <code>preferredWidth</code>.
   * 
   * @param minWidth the minimum width
   */
  public void setMinWidth(int minWidth)
  {
    this.minWidth = minWidth;
    setWidth(getWidth());
    setPreferredWidth(getPreferredWidth());
  }

  /**
   * Returns the <code>TableColumn</code>'s minimum width.
   * 
   * @return The minimum width.
   */
  public int getMinWidth()
  {
    return minWidth;
  }

  /**
   * Sets the maximum width and, if necessary, updates the <code>width</code>
   * and <code>preferredWidth</code>.
   * 
   * @param maxWidth the maximum width
   */
  public void setMaxWidth(int maxWidth)
  {
    this.maxWidth = maxWidth;
    setWidth(getWidth());
    setPreferredWidth(getPreferredWidth());
  }

  /**
   * Returns the maximum width.
   * 
   * @return The maximum width.
   */
  public int getMaxWidth()
  {
    return maxWidth;
  }

  /**
   * setResizable
   * 
   * @param isResizable <code>true</code> if this column is resizable,
   * <code>false</code> otherwise
   */
  public void setResizable(boolean isResizable)
  {
    this.isResizable = isResizable;
  }

  /**
   * getResizable
   * 
   * @return <code>true</code> if this column is resizable,
   * <code>false</code> otherwise
   */
  public boolean getResizable()
  {
    return isResizable;
  }

  /**
   * sizeWidthToFit
   */
  public void sizeWidthToFit()
  {
    // TODO
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
   * Adds a property change listener.
   * 
   * @param listener the listener to add
   */
  public synchronized void addPropertyChangeListener(PropertyChangeListener listener)
  {
    changeSupport.addPropertyChangeListener(listener);
  }

  /**
   * removePropertyChangeListener
   * @param listener the listener to remove
   */
  public synchronized void removePropertyChangeListener(PropertyChangeListener listener)
  {
    changeSupport.removePropertyChangeListener(listener);
  }

  /**
   * Returns the property change listeners for this <code>TableColumn</code>.
   * @since 1.4
   */
  public PropertyChangeListener[] getPropertyChangeListeners()
  {
    return changeSupport.getPropertyChangeListeners();
  }

  /**
   * createDefaultHeaderRenderer
   * @return TableCellRenderer
   */
  protected TableCellRenderer createDefaultHeaderRenderer()
  {
    return new DefaultTableCellRenderer();
  }
}
