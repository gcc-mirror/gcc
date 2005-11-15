/* JTableHeader.java --
   Copyright (C) 2003, 2004, 2005  Free Software Foundation, Inc.

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

import java.awt.Color;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.FocusListener;
import java.beans.PropertyChangeListener;
import java.util.Locale;

import javax.accessibility.Accessible;
import javax.accessibility.AccessibleAction;
import javax.accessibility.AccessibleComponent;
import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleRole;
import javax.accessibility.AccessibleSelection;
import javax.accessibility.AccessibleStateSet;
import javax.accessibility.AccessibleText;
import javax.accessibility.AccessibleValue;
import javax.swing.JComponent;
import javax.swing.JTable;
import javax.swing.UIManager;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.TableColumnModelEvent;
import javax.swing.event.TableColumnModelListener;
import javax.swing.plaf.TableHeaderUI;

public class JTableHeader extends JComponent
  implements TableColumnModelListener, Accessible
{
  protected class AccessibleJTableHeader extends AccessibleJComponent
  {
    protected class AccessibleJTableHeaderEntry extends AccessibleContext
      implements Accessible, AccessibleComponent
    {
      public AccessibleJTableHeaderEntry(int c, JTableHeader p, JTable t) 
      {
        throw new Error("not implemented");
      }
      
      public void addFocusListener(FocusListener l)
      {
        throw new Error("not implemented");
      }
      
      public void addPropertyChangeListener(PropertyChangeListener l)
      {
        throw new Error("not implemented");
      }
      
      public boolean contains(Point p)
      {
        throw new Error("not implemented");
      }
      
      public AccessibleAction getAccessibleAction()
      {
        throw new Error("not implemented");
      }
      
      public Accessible getAccessibleAt(Point p)
      {
        throw new Error("not implemented");
      }
      
      public Accessible getAccessibleChild(int i)
      {
        throw new Error("not implemented");
      }
      
      public int getAccessibleChildrenCount()
      {
        throw new Error("not implemented");
      }
      
      public AccessibleComponent getAccessibleComponent()
      {
        throw new Error("not implemented");
      }
      
      public AccessibleContext getAccessibleContext()
      {
        throw new Error("not implemented");
      }
      
      public String getAccessibleDescription()
      {
        throw new Error("not implemented");
      }
      
      public int getAccessibleIndexInParent()
      {
        throw new Error("not implemented");
      }
      
      public String getAccessibleName()
      {
        throw new Error("not implemented");
      }
      
      public AccessibleRole getAccessibleRole()
      {
        throw new Error("not implemented");
      }
      
      public AccessibleSelection getAccessibleSelection()
      {
        throw new Error("not implemented");
      }
      
      public AccessibleStateSet getAccessibleStateSet()
      {
        throw new Error("not implemented");
      }
      
      public AccessibleText getAccessibleText()
      {
        throw new Error("not implemented");
      }
      
      public AccessibleValue getAccessibleValue()
      {
        throw new Error("not implemented");
      }
      
      public Color getBackground()
      {
        throw new Error("not implemented");
      }
      
      public Rectangle getBounds()
      {
        throw new Error("not implemented");
      }
      
      public Cursor getCursor()
      {
        throw new Error("not implemented");
      }
      
      public Font getFont()
      {
        throw new Error("not implemented");
      }
      
      public FontMetrics getFontMetrics(Font f)
      {
        throw new Error("not implemented");
      }
      
      public Color getForeground()
      {
        throw new Error("not implemented");
      }
      
      public Locale getLocale()
      {
        throw new Error("not implemented");
      }
      
      public Point getLocation()
      {
        throw new Error("not implemented");
      }
      
      public Point getLocationOnScreen()
      {
        throw new Error("not implemented");
      }
      
      public Dimension getSize()
      {
        throw new Error("not implemented");
      }
      
      public boolean isEnabled()
      {
        throw new Error("not implemented");
      }
      
      public boolean isFocusTraversable()
      {
        throw new Error("not implemented");
      }
      
      public boolean isShowing()
      {
        throw new Error("not implemented");
      }
      
      public boolean isVisible()
      {
        throw new Error("not implemented");
      }
      
      public void removeFocusListener(FocusListener l)
      {
        throw new Error("not implemented");
      }
      
      public void removePropertyChangeListener(PropertyChangeListener l)
      {
        throw new Error("not implemented");
      }
      
      public void requestFocus()
      {
        throw new Error("not implemented");
      }
      
      public void setAccessibleDescription(String s)
      {
        throw new Error("not implemented");
      }
      
      public void setAccessibleName(String s)
      {
        throw new Error("not implemented");
      }
      
      public void setBackground(Color c)
      {
        throw new Error("not implemented");
      }
      
      public void setBounds(Rectangle r)
      {
        throw new Error("not implemented");
      }
      
      public void setCursor(Cursor c)
      {
        throw new Error("not implemented");
      }
      
      public void setEnabled(boolean b)
      {
        throw new Error("not implemented");
      }
      
      public void setFont(Font f)
      {
        throw new Error("not implemented");
      }
      
      public void setForeground(Color c)
      {
        throw new Error("not implemented");
      }
      
      public void setLocation(Point p)
      {
        throw new Error("not implemented");
      }
      
      public void setSize(Dimension d)
      {
        throw new Error("not implemented");
      }
      
      public void setVisible(boolean b)
      {
        throw new Error("not implemented");
      }
    };
  }

  private static final long serialVersionUID = 5144633983372967710L;

  /**
   * The columnModel property.
   */
  protected TableColumnModel columnModel;

  /**
   * The draggedColumn property.
   */
  protected TableColumn draggedColumn;

  /**
   * The draggedDistance property.
   */
  protected int draggedDistance;

  /**
   * The opaque property.
   */
  boolean opaque;

  /**
   * The reorderingAllowed property.
   */
  protected boolean reorderingAllowed;

  /**
   * The resizingAllowed property.
   */
  protected boolean resizingAllowed = true;

  /**
   * The resizingColumn property.
   */
  protected TableColumn resizingColumn;

  /**
   * The table property.
   */
  protected JTable table;

  /**
   * The updateTableInRealTime property.
   */
  protected boolean updateTableInRealTime;

  TableCellRenderer cellRenderer; 

  /**
   * Creates a new default instance.
   */
  public JTableHeader()
  {
    this(null);
  }

  /**
   * Creates a new header.  If <code>cm</code> is <code>null</code>, a new
   * table column model is created by calling 
   * {@link #createDefaultColumnModel()}.
   * 
   * @param cm  the table column model (<code>null</code> permitted).
   */
  public JTableHeader(TableColumnModel cm)
  {
    columnModel = cm == null ? createDefaultColumnModel() : cm; 
    initializeLocalVars();
    updateUI();
  }

  /**
   * Creates a default table column model.
   * 
   * @return A default table column model.
   */
  protected TableColumnModel createDefaultColumnModel()
  {
    return new DefaultTableColumnModel();
  }

  /**
   * Get the value of the {@link #accessibleContext} property.
   *
   * @return The current value of the property
   */
  public AccessibleContext getAccessibleContext()
  {
    return accessibleContext;
  }

  /**
   * Get the value of the {@link #columnModel} property.
   *
   * @return The current value of the property
   */
  public TableColumnModel getColumnModel()
  {
    return columnModel;
  }

  /**
   * Get the value of the {@link #draggedColumn} property.
   *
   * @return The current value of the property
   */
  public TableColumn getDraggedColumn()
  {
    return draggedColumn;
  }

  /**
   * Get the value of the {@link #draggedDistance} property.
   *
   * @return The current value of the property
   */
  public int getDraggedDistance()
  {
    return draggedDistance;
  }

  /**
   * Get the value of the {@link #reorderingAllowed} property.
   *
   * @return The current value of the property
   */
  public boolean getReorderingAllowed()
  {
    return reorderingAllowed;
  }

  /**
   * Get the value of the {@link #resizingAllowed} property.
   *
   * @return The current value of the property
   */
  public boolean getResizingAllowed()
  {
    return resizingAllowed;
  }

  /**
   * Get the value of the {@link #resizingColumn} property.
   *
   * @return The current value of the property
   */
  public TableColumn getResizingColumn()
  {
    return resizingColumn;
  }

  /**
   * Get the value of the {@link #table} property.
   *
   * @return The current value of the property
   */
  public JTable getTable()
  {
    return table;
  }

  /**
   * Get the value of the {@link #updateTableInRealTime} property.
   *
   * @return The current value of the property
   */
  public boolean getUpdateTableInRealTime()
  {
    return updateTableInRealTime;
  }

  /**
   * Get the value of the {@link #opaque} property.
   *
   * @return The current value of the property
   */
  public boolean isOpaque()
  {
    return opaque;
  }

  /**
   * Set the value of the {@link #columnModel} property.
   *
   * @param c The new value of the property
   */ 
  public void setColumnModel(TableColumnModel c)
  {
    columnModel.removeColumnModelListener(this);
    columnModel = c;
    columnModel.addColumnModelListener(this);
  }

  /**
   * Set the value of the {@link #draggedColumn} property.
   *
   * @param d The new value of the property
   */ 
  public void setDraggedColumn(TableColumn d)
  {
    draggedColumn = d;
  }

  /**
   * Set the value of the {@link #draggedDistance} property.
   *
   * @param d The new value of the property
   */ 
  public void setDraggedDistance(int d)
  {
    draggedDistance = d;
  }

  /**
   * Set the value of the {@link #opaque} property.
   *
   * @param o The new value of the property
   */ 
  public void setOpaque(boolean o)
  {
    opaque = o;
  }

  /**
   * Set the value of the {@link #reorderingAllowed} property.
   *
   * @param r The new value of the property
   */ 
  public void setReorderingAllowed(boolean r)
  {
    reorderingAllowed = r;
  }

  /**
   * Set the value of the {@link #resizingAllowed} property.
   *
   * @param r The new value of the property
   */ 
  public void setResizingAllowed(boolean r)
  {
    resizingAllowed = r;
  }

  /**
   * Set the value of the {@link #resizingColumn} property.
   *
   * @param r The new value of the property
   */ 
  public void setResizingColumn(TableColumn r)
  {
    resizingColumn = r;
  }

  /**
   * Set the value of the {@link #table} property.
   *
   * @param t The new value of the property
   */ 
  public void setTable(JTable t)
  {
    table = t;
  }

  /**
   * Set the value of the {@link #updateTableInRealTime} property.
   *
   * @param u The new value of the property
   */ 
  public void setUpdateTableInRealTime(boolean u)
  {
    updateTableInRealTime = u;
  }

  /**
   * Creates a default renderer.
   * 
   * @return A default renderer.
   */
  protected TableCellRenderer createDefaultRenderer()
  {
    return new DefaultTableCellRenderer();
  }
  
  /**
   * Returns the default table cell renderer.
   * 
   * @return The default table cell renderer.
   */
  public TableCellRenderer getDefaultRenderer()
  {
    return cellRenderer;
  }

  /**
   * Sets the default table cell renderer.
   * 
   * @param cellRenderer  the renderer.
   */
  public void setDefaultRenderer(TableCellRenderer cellRenderer)
  {
    this.cellRenderer = cellRenderer;
  }

  public Rectangle getHeaderRect(int column)
  {
    Rectangle r = getTable().getCellRect(-1, column, false);
    r.height = getHeight();
    return r;
  }

  protected String paramString()
  {
    return "JTableHeader";
  }

  // UI support

  public String getUIClassID()
  {
    return "TableHeaderUI";
  }

  public TableHeaderUI getUI()
  {
    return (TableHeaderUI) ui;
  }

  public void setUI(TableHeaderUI u)
  {
    super.setUI(u);
  }

  public void updateUI()
  {
    setUI((TableHeaderUI) UIManager.getUI(this));
  }

  /**
   * Returns the index of the column at the specified point.
   * 
   * @param point  the point.
   * 
   * @return The column index, or -1.
   */
  public int columnAtPoint(Point point)
  {
    if (getBounds().contains(point))
      return columnModel.getColumnIndexAtX(point.x);
    
    return -1;
  }

  /**
   * Receives notification when a column is added to the column model.
   *
   * @param event the table column model event
   */
  public void columnAdded(TableColumnModelEvent event)
  {
    // TODO: What else to do here (if anything)?
    resizeAndRepaint();
  }

  /**
   * Receives notification when a column margin changes in the column model.
   *
   * @param event the table column model event
   */
  public void columnMarginChanged(ChangeEvent event)
  {
    // TODO: What else to do here (if anything)?
    resizeAndRepaint();
  }

  /**
   * Receives notification when a column is moved within the column model.
   *
   * @param event the table column model event
   */
  public void columnMoved(TableColumnModelEvent event)
  {
    // TODO: What else to do here (if anything)?
    resizeAndRepaint();
  }

  /**
   * Receives notification when a column is removed from the column model.
   *
   * @param event the table column model event
   */
  public void columnRemoved(TableColumnModelEvent event)
  {
    // TODO: What else to do here (if anything)?
    resizeAndRepaint();
  }

  /**
   * Receives notification when the column selection has changed.
   *
   * @param event the table column model event
   */
  public void columnSelectionChanged(ListSelectionEvent event)
  {
    // TODO: What else to do here (if anything)?
    resizeAndRepaint();
  }

  /**
   * Validates the layout of this table header and repaints it. This is
   * equivalent to <code>revalidate()</code> followed by
   * <code>repaint()</code>.
   */
  public void resizeAndRepaint()
  {
    revalidate();
    repaint();
  }

  /**
   * Initializes the fields and properties of this class with default values.
   * This is called by the constructors.
   */
  protected void initializeLocalVars()
  {
    accessibleContext = new AccessibleJTableHeader();
    draggedColumn = null;
    draggedDistance = 0;
    opaque = true;
    reorderingAllowed = true;
    resizingAllowed = true;
    resizingColumn = null;
    table = null;
    updateTableInRealTime = true;
    cellRenderer = createDefaultRenderer();
  }
}
