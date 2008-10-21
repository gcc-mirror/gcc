/* JList.java --
   Copyright (C) 2002, 2003, 2004, 2005, 2006,  Free Software Foundation, Inc.

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

import gnu.java.lang.CPStringBuilder;

import java.awt.Color;
import java.awt.Component;
import java.awt.ComponentOrientation;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.FocusListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.Locale;
import java.util.Vector;

import javax.accessibility.Accessible;
import javax.accessibility.AccessibleComponent;
import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleRole;
import javax.accessibility.AccessibleSelection;
import javax.accessibility.AccessibleState;
import javax.accessibility.AccessibleStateSet;
import javax.swing.event.ListDataEvent;
import javax.swing.event.ListDataListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.plaf.ListUI;
import javax.swing.text.Position;

/**
 * <p>This class is a facade over three separate objects: {@link
 * javax.swing.ListModel}, {@link javax.swing.ListSelectionModel} and
 * {@link javax.swing.plaf.ListUI}. The facade represents a unified "list"
 * concept, with independently replacable (possibly client-provided) models
 * for its contents and its current selection. In addition, each element in
 * the list is rendered via a strategy class {@link
 * javax.swing.ListCellRenderer}.</p>
 *
 * <p>Lists have many properties, some of which are stored in this class
 * while others are delegated to the list's model or selection. The
 * following properties are available:</p>
 *
 * <table>
 * <tr><th>Property                       </th><th>Stored in</th><th>Bound?</th></tr>
 * <tr><td>accessibleContext              </td><td>list     </td><td>no    </td></tr>
 * <tr><td>anchorSelectionIndex           </td><td>selection</td><td>no    </td></tr>
 * <tr><td>cellRenderer                   </td><td>list     </td><td>yes   </td></tr>
 * <tr><td>dragEnabled                    </td><td>list     </td><td>no    </td></tr>
 * <tr><td>firstVisibleIndex              </td><td>list     </td><td>no    </td></tr>
 * <tr><td>fixedCellHeight                </td><td>list     </td><td>yes   </td></tr>
 * <tr><td>fixedCellWidth                 </td><td>list     </td><td>yes   </td></tr>
 * <tr><td>lastVisibleIndex               </td><td>list     </td><td>no    </td></tr>
 * <tr><td>layoutOrientation              </td><td>list     </td><td>yes   </td></tr>
 * <tr><td>leadSelectionIndex             </td><td>selection</td><td>no    </td></tr>
 * <tr><td>maxSelectionIndex              </td><td>selection</td><td>no    </td></tr>
 * <tr><td>minSelectionIndex              </td><td>selection</td><td>no    </td></tr>
 * <tr><td>model                          </td><td>list     </td><td>yes   </td></tr>
 * <tr><td>opaque                         </td><td>list     </td><td>no    </td></tr>
 * <tr><td>preferredScrollableViewportSize</td><td>list     </td><td>no    </td></tr>
 * <tr><td>prototypeCellValue             </td><td>list     </td><td>yes   </td></tr>
 * <tr><td>scrollableTracksViewportHeight </td><td>list     </td><td>no    </td></tr>
 * <tr><td>scrollableTracksViewportWidth  </td><td>list     </td><td>no    </td></tr>
 * <tr><td>selectedIndex                  </td><td>selection</td><td>no    </td></tr>
 * <tr><td>selectedIndices                </td><td>selection</td><td>no    </td></tr>
 * <tr><td>selectedValue                  </td><td>model    </td><td>no    </td></tr>
 * <tr><td>selectedValues                 </td><td>model    </td><td>no    </td></tr>
 * <tr><td>selectionBackground            </td><td>list     </td><td>yes   </td></tr>
 * <tr><td>selectionEmpty                 </td><td>selection</td><td>no    </td></tr>
 * <tr><td>selectionForeground            </td><td>list     </td><td>yes   </td></tr>
 * <tr><td>selectionMode                  </td><td>selection</td><td>no    </td></tr>
 * <tr><td>selectionModel                 </td><td>list     </td><td>yes   </td></tr>
 * <tr><td>UI                             </td><td>list     </td><td>yes   </td></tr>
 * <tr><td>UIClassID                      </td><td>list     </td><td>no    </td></tr>
 * <tr><td>valueIsAdjusting               </td><td>list     </td><td>no    </td></tr>
 * <tr><td>visibleRowCount                </td><td>list     </td><td>no    </td></tr>
 * </table> 
 *
 * @author Graydon Hoare (graydon@redhat.com)
 */

public class JList extends JComponent implements Accessible, Scrollable
{

  /**
   * Provides accessibility support for <code>JList</code>.
   */
  protected class AccessibleJList extends AccessibleJComponent
    implements AccessibleSelection, PropertyChangeListener,
               ListSelectionListener, ListDataListener
  {

    /**
     * Provides accessibility support for list elements in <code>JList</code>s.
     */
    protected class AccessibleJListChild extends AccessibleContext
      implements Accessible, AccessibleComponent
    {

      /**
       * The parent list.
       */
      JList parent;

      /**
       * The index in the list for that child.
       */
      int listIndex;

      /**
       * The cursor for this list child.
       */
      // TODO: Testcases show that this class somehow stores state about the
      // cursor. I cannot make up though how that could affect
      // the actual list.
      Cursor cursor = Cursor.getDefaultCursor();

      /**
       * Creates a new instance of <code>AccessibleJListChild</code>.
       *
       * @param list the list of which this is an accessible child
       * @param index the list index for this child
       */
      public AccessibleJListChild(JList list, int index)
      {
        parent = list;
        listIndex = index;
      }

      /**
       * Returns the accessible context of this object. Returns
       * <code>this</code> since <code>AccessibleJListChild</code>s are their
       * own accessible contexts.
       *
       * @return the accessible context of this object, <code>this</code>
       */
      public AccessibleContext getAccessibleContext()
      {
        return this;
      }

      /**
       * Returns the background color for this list child. This returns the
       * background of the <code>JList</code> itself since the background
       * cannot be set on list children individually
       *
       * @return the background color for this list child
       */
      public Color getBackground()
      {
        return parent.getBackground();
      }

      /**
       * Calling this method has no effect, since the background color cannot be
       * set on list children individually.
       *
       * @param color not used here.
       */
      public void setBackground(Color color)
      {
        // Calling this method has no effect, since the background color cannot
        // be set on list children individually.
      }

      /**
       * Returns the foreground color for this list child. This returns the
       * background of the <code>JList</code> itself since the foreground
       * cannot be set on list children individually.
       *
       * @return the background color for this list child
       */
      public Color getForeground()
      {
        return parent.getForeground();
      }

      /**
       * Calling this method has no effect, since the foreground color cannot be
       * set on list children individually.
       *
       * @param color not used here.
       */
      public void setForeground(Color color)
      {
        // Calling this method has no effect, since the foreground color cannot
        // be set on list children individually.
      }

      /**
       * Returns the cursor for this list child.
       *
       * @return the cursor for this list child
       */
      public Cursor getCursor()
      {
        // TODO: Testcases show that this method returns the cursor that has
        // been set by setCursor. I cannot make up though how that could affect
        // the actual list.
        return cursor;
      }

      /**
       * Sets the cursor for this list child.
       */
      public void setCursor(Cursor cursor)
      {
        this.cursor = cursor;
        // TODO: Testcases show that this method returns the cursor that has
        // been set by setCursor. I cannot make up though how that could affect
        // the actual list.
      }

      /**
       * Returns the font of the <code>JList</code> since it is not possible to
       * set fonts for list children individually.
       *
       * @return the font of the <code>JList</code>
       */
      public Font getFont()
      {
        return parent.getFont();
      }

      /**
       * Does nothing since it is not possible to set the font on list children
       * individually.
       *
       * @param font not used here
       */
      public void setFont(Font font)
      {
        // Does nothing since it is not possible to set the font on list
        // children individually.
      }

      /**
       * Returns the font metrics for the specified font. This method forwards
       * to the parent <code>JList</code>. 
       *
       * @param font the font for which the font metrics is queried
       *
       * @return the font metrics for the specified font
       */
      public FontMetrics getFontMetrics(Font font)
      {
        return parent.getFontMetrics(font);
      }

      /**
       * Returns <code>true</code> if the parent <code>JList</code> is enabled,
       * <code>false</code> otherwise. The list children cannot have an enabled
       * flag set individually.
       *
       * @return <code>true</code> if the parent <code>JList</code> is enabled,
       *         <code>false</code> otherwise
       */
      public boolean isEnabled()
      {
        return parent.isEnabled();
      }

      /**
       * Does nothing since the enabled flag cannot be set for list children
       * individually.
       *
       * @param b not used here
       */
      public void setEnabled(boolean b)
      {
        // Does nothing since the enabled flag cannot be set for list children
        // individually.
      }

      /**
       * Returns <code>true</code> if this list child is visible,
       * <code>false</code> otherwise. The value of this property depends
       * on {@link JList#getFirstVisibleIndex()} and
       * {@link JList#getLastVisibleIndex()}.
       *
       * @return <code>true</code> if this list child is visible,
       *         <code>false</code> otherwise
       */
      public boolean isVisible()
      {
        return listIndex >= parent.getFirstVisibleIndex()
               && listIndex <= parent.getLastVisibleIndex();
      }

      /**
       * The value of the visible property cannot be modified, so this method
       * does nothing.
       *
       * @param b not used here
       */
      public void setVisible(boolean b)
      {
        // The value of the visible property cannot be modified, so this method
        // does nothing.
      }

      /**
       * Returns <code>true</code> if this list child is currently showing on
       * screen and <code>false</code> otherwise. The list child is showing if
       * it is visible and if it's parent JList is currently showing.
       *
       * @return <code>true</code> if this list child is currently showing on
       *         screen and <code>false</code> otherwise
       */
      public boolean isShowing()
      {
        return isVisible() && parent.isShowing();
      }

      /**
       * Returns <code>true</code> if this list child covers the screen location
       * <code>point</code> (relative to the <code>JList</code> coordinate
       * system, <code>false</code> otherwise.
       *
       * @return <code>true</code> if this list child covers the screen location
       *         <code>point</code> , <code>false</code> otherwise
       */
      public boolean contains(Point point)
      {
        return getBounds().contains(point);
      }

      /**
       * Returns the absolute screen location of this list child.
       *
       * @return the absolute screen location of this list child
       */
      public Point getLocationOnScreen()
      {
        Point loc = getLocation();
        SwingUtilities.convertPointToScreen(loc, parent);
        return loc;
      }

      /**
       * Returns the screen location of this list child relative to it's parent.
       *
       * @return the location of this list child relative to it's parent
       *
       * @see JList#indexToLocation(int)
       */
      public Point getLocation()
      {
        return parent.indexToLocation(listIndex);
      }

      /**
       * Does nothing since the screen location cannot be set on list children
       * explictitly.
       *
       * @param point not used here
       */
      public void setLocation(Point point)
      {
        // Does nothing since the screen location cannot be set on list children
        // explictitly.
      }

      /**
       * Returns the bounds of this list child.
       *
       * @return the bounds of this list child
       *
       * @see JList#getCellBounds(int, int)
       */
      public Rectangle getBounds()
      {
        return parent.getCellBounds(listIndex, listIndex);
      }

      /**
       * Does nothing since the bounds cannot be set on list children
       * individually.
       *
       * @param rectangle not used here
       */
      public void setBounds(Rectangle rectangle)
      {
        // Does nothing since the bounds cannot be set on list children
        // individually.
      }

      /**
       * Returns the size of this list child.
       *
       * @return the size of this list child
       */
      public Dimension getSize()
      {
        Rectangle b = getBounds();
        return b.getSize();
      }

      /**
       * Does nothing since the size cannot be set on list children
       * individually.
       *
       * @param dimension not used here
       */
      public void setSize(Dimension dimension)
      {
        // Does nothing since the size cannot be set on list children
        // individually.
      }

      /**
       * Returns <code>null</code> because list children do not have children
       * themselves
       *
       * @return <code>null</code>
       */
      public Accessible getAccessibleAt(Point point)
      {
        return null;
      }

      /**
       * Returns <code>true</code> since list children are focus traversable.
       *
       * @return true
       */
      public boolean isFocusTraversable()
      {
        // TODO: Is this 100% ok?
        return true;
      }

      /**
       * Requests focus on the parent list. List children cannot request focus
       * individually.
       */
      public void requestFocus()
      {
        // TODO: Is this 100% ok?
        parent.requestFocus();
      }

      /**
       * Adds a focus listener to the parent list. List children do not have
       * their own focus management.
       *
       * @param listener the focus listener to add
       */
      public void addFocusListener(FocusListener listener)
      {
        // TODO: Is this 100% ok?
        parent.addFocusListener(listener);
      }

      /**
       * Removes a focus listener from the parent list. List children do not
       * have their own focus management.
       *
       * @param listener the focus listener to remove
       */
      public void removeFocusListener(FocusListener listener)
      {
        // TODO: Is this 100%
        parent.removeFocusListener(listener);
      }

      /**
       * Returns the accessible role of this list item, which is
       * {@link AccessibleRole#LABEL}.
       *
       * @return {@link AccessibleRole#LABEL}
       */
      public AccessibleRole getAccessibleRole()
      {
        return AccessibleRole.LABEL;
      }

      /**
       * Returns the accessible state set of this list item.
       *
       * @return the accessible state set of this list item
       */
      public AccessibleStateSet getAccessibleStateSet()
      {
        AccessibleStateSet states = new AccessibleStateSet();
        if (isVisible())
          states.add(AccessibleState.VISIBLE);
        if (isShowing())
          states.add(AccessibleState.SHOWING);
        if (isFocusTraversable())
          states.add(AccessibleState.FOCUSABLE);
        // TODO: How should the active state be handled? The API docs
        // suggest that this state is set on the activated list child,
        // that is the one that is drawn with a box. However, I don't know how
        // to implement this.

        // TODO: We set the selectable state here because list children are
        // selectable. Is there a way to disable single children?
        if (parent.isEnabled())
          states.add(AccessibleState.SELECTABLE);
 
        if (parent.isSelectedIndex(listIndex))
          states.add(AccessibleState.SELECTED);

        // TODO: Handle more states here?
        return states;
      }

      /**
       * Returns the index of this list child within it's parent list.
       *
       * @return the index of this list child within it's parent list
       */
      public int getAccessibleIndexInParent()
      {
        return listIndex;
      }

      /**
       * Returns <code>0</code> since list children don't have children
       * themselves.
       *
       * @return <code>0</code>
       */
      public int getAccessibleChildrenCount()
      {
        return 0;
      }

      /**
       * Returns <code>null</code> since list children don't have children
       * themselves.
       *
       * @return <code>null</code>
       */
      public Accessible getAccessibleChild(int i)
      {
        return null;
      }

      /**
       * Returns the locale of this component. This call is forwarded to the
       * parent list since list children don't have a separate locale setting.
       *
       * @return the locale of this component
       */
      public Locale getLocale()
      {
        return parent.getLocale();
      }

      /**
       * This method does
       * nothing, list children are transient accessible objects which means
       * that they don't fire property change events.
       *
       * @param l not used here
       */
      public void addPropertyChangeListener(PropertyChangeListener l)
      {
        // Do nothing here.
      }

      /**
       * This method does
       * nothing, list children are transient accessible objects which means
       * that they don't fire property change events.
       *
       * @param l not used here
       */
      public void removePropertyChangeListener(PropertyChangeListener l)
      {
        // Do nothing here.
      }
      
      // TODO: Implement the remaining methods of this class.
    }
    
    /**
     * Create a new AccessibleJList.
     */
    public AccessibleJList()
    {
      // Nothing to do here.
    }

    /**
     * Returns the number of selected accessible children.
     *
     * @return the number of selected accessible children
     */
    public int getAccessibleSelectionCount()
    {
      return getSelectedIndices().length;
    }

    /**
     * Returns the n-th selected accessible child.
     *
     * @param n the index of the selected child to return
     *
     * @return the n-th selected accessible child
     */
    public Accessible getAccessibleSelection(int n)
    {
      return new AccessibleJListChild(JList.this, getSelectedIndices()[n]);
    }

    /**
     * Returns <code>true</code> if the n-th child is selected,
     * <code>false</code> otherwise.
     *
     * @param n the index of the child of which the selected state is queried
     *
     * @return <code>true</code> if the n-th child is selected,
     *         <code>false</code> otherwise
     */
    public boolean isAccessibleChildSelected(int n)
    {
      return isSelectedIndex(n);
    }

    /**
     * Adds the accessible item with the specified index to the selected items.
     * If multiple selections are supported, the item is added to the selection,
     * otherwise the item replaces the current selection.
     *
     * @param i the index of the item to add to the selection
     */
    public void addAccessibleSelection(int i)
    {
      addSelectionInterval(i, i);
    }

    /**
     * Removes the accessible item with the specified index to the selection.
     *
     * @param i the index of the item to be removed from the selection
     */
    public void removeAccessibleSelection(int i)
    {
      removeSelectionInterval(i, i);
    }

    /**
     * Remove all selection items from the selection.
     */
    public void clearAccessibleSelection()
    {
      clearSelection();
    }

    /**
     * Selects all items if multiple selections are supported.
     * Otherwise do nothing.
     */
    public void selectAllAccessibleSelection()
    {
      addSelectionInterval(0, getModel().getSize());
    }

    /**
     * Receices notification when the list selection is changed. This method
     * fires two property change events, the first with
     * {@link AccessibleContext#ACCESSIBLE_VISIBLE_DATA_PROPERTY} and the second
     * with {@link AccessibleContext#ACCESSIBLE_SELECTION_PROPERTY}.
     *
     * @param event the list selection event
     */
    public void valueChanged(ListSelectionEvent event)
    {
      firePropertyChange(ACCESSIBLE_VISIBLE_DATA_PROPERTY, Boolean.FALSE,
                         Boolean.TRUE);
      firePropertyChange(ACCESSIBLE_SELECTION_PROPERTY, Boolean.FALSE,
                         Boolean.TRUE);
    }

    /**
     * Receives notification when items have changed in the
     * <code>JList</code>. This method fires a property change event with
     * {@link AccessibleContext#ACCESSIBLE_VISIBLE_DATA_PROPERTY}.
     *
     * @param event the list data event
     */
    public void contentsChanged(ListDataEvent event)
    {
      firePropertyChange(ACCESSIBLE_VISIBLE_DATA_PROPERTY, Boolean.FALSE,
                         Boolean.TRUE);
    }

    /**
     * Receives notification when items are inserted into the
     * <code>JList</code>. This method fires a property change event with
     * {@link AccessibleContext#ACCESSIBLE_VISIBLE_DATA_PROPERTY}.
     *
     * @param event the list data event
     */
    public void intervalAdded(ListDataEvent event)
    {
      firePropertyChange(ACCESSIBLE_VISIBLE_DATA_PROPERTY, Boolean.FALSE,
                         Boolean.TRUE);
    }

    /**
     * Receives notification when items are removed from the
     * <code>JList</code>. This method fires a property change event with
     * {@link AccessibleContext#ACCESSIBLE_VISIBLE_DATA_PROPERTY}.
     *
     * @param event the list data event
     */
    public void intervalRemoved(ListDataEvent event)
    {
      firePropertyChange(ACCESSIBLE_VISIBLE_DATA_PROPERTY, Boolean.FALSE,
                         Boolean.TRUE);
    }


    /**
     * Receives notification about changes of the <code>JList</code>'s
     * properties. This is used to re-register this object as listener to
     * the data model and selection model when the data model or selection model
     * changes.
     *
     * @param e the property change event
     */
    public void propertyChange(PropertyChangeEvent e)
    {
      String propertyName = e.getPropertyName();
      if (propertyName.equals("model"))
        {
          ListModel oldModel = (ListModel) e.getOldValue();
          oldModel.removeListDataListener(this);
          ListModel newModel = (ListModel) e.getNewValue();
          newModel.addListDataListener(this);
        }
      else if (propertyName.equals("selectionModel"))
        {
          ListSelectionModel oldModel = (ListSelectionModel) e.getOldValue();
          oldModel.removeListSelectionListener(this);
          ListSelectionModel newModel = (ListSelectionModel) e.getNewValue();
          oldModel.addListSelectionListener(this);
        }
    }

    /**
     * Return the state set of the <code>JList</code>.
     *
     * @return the state set of the <code>JList</code>
     */
    public AccessibleStateSet getAccessibleStateSet()
    {
      // TODO: Figure out if there is possibly more state that must be
      // handled here.
      AccessibleStateSet s = super.getAccessibleStateSet();
      if (getSelectionMode() != ListSelectionModel.SINGLE_SELECTION)
        s.add(AccessibleState.MULTISELECTABLE);
      return s;
    }

    /**
     * Returns the accessible role for <code>JList</code>,
     * {@link AccessibleRole#LIST}.
     *
     * @return the accessible role for <code>JList</code>
     */
    public AccessibleRole getAccessibleRole()
    {
      return AccessibleRole.LIST;
    }

    /**
     * Returns the accessible child at the visual location <code>p</code>
     * (relative to the upper left corner of the <code>JList</code>). If there
     * is no child at that location, this returns <code>null</code>.
     *
     * @param p the screen location for which to return the accessible child
     *
     * @return the accessible child at the specified location, or
     *         <code>null</code> if there is no child at that location
     */
    public Accessible getAccessibleAt(Point p)
    {
      int childIndex = locationToIndex(p);
      return getAccessibleChild(childIndex);
    }

    /**
     * Returns the number of accessible children in the <code>JList</code>.
     *
     * @return the number of accessible children in the <code>JList</code>
     */
    public int getAccessibleChildrenCount()
    {
      return getModel().getSize();
    }

    /**
     * Returns the n-th accessible child of this <code>JList</code>. This will
     * be an instance of {@link AccessibleJListChild}. If there is no child
     * at that index, <code>null</code> is returned.
     *
     * @param n the index of the child to return
     *
     * @return the n-th accessible child of this <code>JList</code>
     */
    public Accessible getAccessibleChild(int n)
    {
      if (getModel().getSize() <= n)
        return null;
      return new AccessibleJListChild(JList.this, n);
    }
  }

  private static final long serialVersionUID = 4406629526391098046L;

  /** 
   * Constant value used in "layoutOrientation" property. This value means
   * that cells are laid out in a single vertical column. This is the default. 
   */
  public static final int VERTICAL = 0;

  /** 
   * Constant value used in "layoutOrientation" property. This value means
   * that cells are laid out in multiple columns "newspaper style", filling
   * vertically first, then horizontally. 
   */
  public static final int VERTICAL_WRAP = 1;
  
  /** 
   * Constant value used in "layoutOrientation" property. This value means
   * that cells are laid out in multiple columns "newspaper style",
   * filling horizontally first, then vertically. 
   */
  public static final int HORIZONTAL_WRAP = 2;

  /**
   * This property indicates whether "drag and drop" functions are enabled
   * on the list.
   */
  boolean dragEnabled;

  /** This property provides a strategy for rendering cells in the list. */
  ListCellRenderer cellRenderer;

  /**
   * This property indicates an fixed width to assign to all cells in the
   * list. If its value is <code>-1</code>, no width has been
   * assigned. This value can be set explicitly, or implicitly by setting
   * the {@link #prototypeCellValue} property.
   */
  int fixedCellWidth;
  
  /**
   * This property indicates an fixed height to assign to all cells in the
   * list. If its value is <code>-1</code>, no height has been
   * assigned. This value can be set explicitly, or implicitly by setting
   * the {@link #prototypeCellValue} property.
   */
  int fixedCellHeight;

  /** 
   * This property holds the current layout orientation of the list, which
   * is one of the integer constants {@link #VERTICAL}, {@link
   * #VERTICAL_WRAP}, or {@link #HORIZONTAL_WRAP}. 
   */
  int layoutOrientation;
  
  /** This property holds the data elements displayed by the list. */
  ListModel model;

  /**
   * <p>This property holds a reference to a "prototype" data value --
   * typically a String -- which is used to calculate the {@link
   * #fixedCellWidth} and {@link #fixedCellHeight} properties, using the
   * {@link #cellRenderer} property to acquire a component to render the
   * prototype.</p>
   *
   * <p>It is important that you <em>not</em> set this value to a
   * component. It has to be a <em>data value</em> such as the objects you
   * would find in the list's model. Setting it to a component will have
   * undefined (and undesirable) affects. </p>
   */
  Object prototypeCellValue;

  /** 
   * This property specifies a foreground color for the selected cells in
   * the list. When {@link ListCellRenderer#getListCellRendererComponent}
   * is called with a selected cell object, the component returned will
   * have its "foreground" set to this color.
   */
  Color selectionBackground;

  /** 
   * This property specifies a background color for the selected cells in
   * the list. When {@link ListCellRenderer#getListCellRendererComponent}
   * is called with a selected cell object, the component returned will
   * have its "background" property set to this color.
   */
  Color selectionForeground;

  /** 
   * This property holds a description of which data elements in the {@link
   * #model} property should be considered "selected", when displaying and
   * interacting with the list.
   */
  ListSelectionModel selectionModel;

  /** 
   * This property indicates a <em>preference</em> for the number of rows
   * displayed in the list, and will scale the
   * {@link #getPreferredScrollableViewportSize} property accordingly. The actual
   * number of displayed rows, when the list is placed in a real {@link
   * JViewport} or other component, may be greater or less than this number.
   */
  int visibleRowCount;

  /**
   * Fire a {@link ListSelectionEvent} to all the registered 
   * ListSelectionListeners.
   * 
   * @param firstIndex  the lowest index covering the selection change.
   * @param lastIndex  the highest index covering the selection change.
   * @param isAdjusting  a flag indicating if this event is one in a series
   *     of events updating the selection.
   */
  protected void fireSelectionValueChanged(int firstIndex, int lastIndex, 
                                           boolean isAdjusting) 
  {
    ListSelectionEvent evt = new ListSelectionEvent(this, firstIndex, 
                                                    lastIndex, isAdjusting);
    ListSelectionListener listeners[] = getListSelectionListeners();
    for (int i = 0; i < listeners.length; ++i)
      {
        listeners[i].valueChanged(evt);
      }
  }

  /**
   * This private listener propagates {@link ListSelectionEvent} events
   * from the list's "selectionModel" property to the list's {@link
   * ListSelectionListener} listeners. It also listens to {@link
   * ListDataEvent} events from the list's {@link #model} property. If this
   * class receives either type of event, it triggers repainting of the
   * list.
   */
  private class ListListener 
    implements ListSelectionListener, ListDataListener
  {
    // ListDataListener events
    public void contentsChanged(ListDataEvent event)
    {
      JList.this.revalidate();
      JList.this.repaint();
    }
    public void intervalAdded(ListDataEvent event)
    {
      JList.this.revalidate();
      JList.this.repaint();
    }
    public void intervalRemoved(ListDataEvent event)
    {
      JList.this.revalidate();
      JList.this.repaint();
    }
    // ListSelectionListener events
    public void valueChanged(ListSelectionEvent event)
    {
      JList.this.fireSelectionValueChanged(event.getFirstIndex(),
                                           event.getLastIndex(),
                                           event.getValueIsAdjusting());
      JList.this.repaint();
    }
  }

  /** 
   * Shared ListListener instance, subscribed to both the current {@link
   * #model} and {@link #selectionModel} properties of the list.
   */
  ListListener listListener;


  /**
   * Creates a new <code>JList</code> object.
   */
  public JList()
  {
    init(new DefaultListModel());
  }

  /**
   * Creates a new <code>JList</code> object.
   *
   * @param items  the initial list items.
   */
  public JList(Object[] items)
  {
    init(createListModel(items));
  }

  /**
   * Creates a new <code>JList</code> object.
   *
   * @param items  the initial list items.
   */
  public JList(Vector<?> items)
  {
    init(createListModel(items));
  }

  /**
   * Creates a new <code>JList</code> object.
   *
   * @param model  a model containing the list items (<code>null</code> not
   *     permitted).
   *     
   * @throws IllegalArgumentException if <code>model</code> is 
   *     <code>null</code>.
   */
  public JList(ListModel model)
  {
    init(model);
  }

  /**
   * Initializes the list.
   *
   * @param m  the list model (<code>null</code> not permitted).
   */
  private void init(ListModel m)
  {
    if (m == null)
      throw new IllegalArgumentException("Null model not permitted.");
    dragEnabled = false;
    fixedCellHeight = -1;
    fixedCellWidth = -1;
    layoutOrientation = VERTICAL;
    opaque = true;
    visibleRowCount = 8;

    cellRenderer = new DefaultListCellRenderer();
    listListener = new ListListener();

    model = m;
    if (model != null)
      model.addListDataListener(listListener);

    selectionModel = createSelectionModel();
    if (selectionModel != null)
      {
        selectionModel.addListSelectionListener(listListener);
        selectionModel.setSelectionMode
                              (ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
      }
    setLayout(null);
    
    updateUI();
  }

  /**
   * Creates the default <code>ListSelectionModel</code>.
   *
   * @return the <code>ListSelectionModel</code>
   */
  protected ListSelectionModel createSelectionModel()
  {
    return new DefaultListSelectionModel();
  }
  
  /**
   * Gets the value of the {@link #fixedCellHeight} property. This property
   * may be <code>-1</code> to indicate that no cell height has been
   * set. This property is also set implicitly when the
   * {@link #prototypeCellValue} property is set.
   *
   * @return The current value of the property 
   * 
   * @see #fixedCellHeight
   * @see #setFixedCellHeight
   * @see #setPrototypeCellValue
   */
  public int getFixedCellHeight()
  {
    return fixedCellHeight;
  }

  /**
   * Sets the value of the {@link #fixedCellHeight} property. This property
   * may be <code>-1</code> to indicate that no cell height has been
   * set. This property is also set implicitly when the {@link
   * #prototypeCellValue} property is set, but setting it explicitly
   * overrides the height computed from {@link #prototypeCellValue}.
   *
   * @param h  the height.
   * 
   * @see #getFixedCellHeight
   * @see #getPrototypeCellValue
   */
  public void setFixedCellHeight(int h)
  {
    if (fixedCellHeight == h)
      return;

    int old = fixedCellHeight;
    fixedCellHeight = h;
    firePropertyChange("fixedCellHeight", old, h);
  }


  /**
   * Gets the value of the {@link #fixedCellWidth} property. This property
   * may be <code>-1</code> to indicate that no cell width has been
   * set. This property is also set implicitly when the {@link
   * #prototypeCellValue} property is set.
   *
   * @return The current value of the property 
   * 
   * @see #setFixedCellWidth
   * @see #setPrototypeCellValue
   */
  public int getFixedCellWidth()
  {
    return fixedCellWidth;
  }

  /**
   * Sets the value of the {@link #fixedCellWidth} property. This property
   * may be <code>-1</code> to indicate that no cell width has been
   * set. This property is also set implicitly when the {@link
   * #prototypeCellValue} property is set, but setting it explicitly
   * overrides the width computed from {@link #prototypeCellValue}.
   *
   * @param w  the width.
   * 
   * @see #getFixedCellHeight
   * @see #getPrototypeCellValue
   */
  public void setFixedCellWidth(int w)
  {
    if (fixedCellWidth == w)
      return;
    
    int old = fixedCellWidth;
    fixedCellWidth = w;
    firePropertyChange("fixedCellWidth", old, w);
  }

  /** 
   * Gets the value of the {@link #visibleRowCount} property.  The default 
   * value is 8.
   *
   * @return the current value of the property.
   * 
   * @see #setVisibleRowCount(int)
   */
  public int getVisibleRowCount()
  {
    return visibleRowCount;
  }

  /**
   * Sets the value of the {@link #visibleRowCount} property. 
   *
   * @param vc The new property value
   * 
   * @see #getVisibleRowCount()
   */
  public void setVisibleRowCount(int vc)
  {
    if (visibleRowCount != vc)
      {
        int oldValue = visibleRowCount;
        visibleRowCount = Math.max(vc, 0);
        firePropertyChange("visibleRowCount", oldValue, vc);
        revalidate();
        repaint();
      }
  }

  /**
   * Adds a {@link ListSelectionListener} to the listener list for this
   * list. The listener will be called back with a {@link
   * ListSelectionEvent} any time the list's {@link #selectionModel}
   * property changes. The source of such events will be the JList,
   * not the selection model.
   *
   * @param listener The new listener to add
   */
  public void addListSelectionListener(ListSelectionListener listener)
  {
    listenerList.add (ListSelectionListener.class, listener);
  }

  /**
   * Removes a {@link ListSelectionListener} from the listener list for
   * this list. The listener will no longer be called when the list's
   * {@link #selectionModel} changes.
   *
   * @param listener The listener to remove
   */
  public void removeListSelectionListener(ListSelectionListener listener)
  {
    listenerList.remove(ListSelectionListener.class, listener);
  }

  /**
   * Returns an array of all ListSelectionListeners subscribed to this
   * list. 
   *
   * @return The current subscribed listeners
   *
   * @since 1.4
   */
  public ListSelectionListener[] getListSelectionListeners()
  {
    return (ListSelectionListener[]) getListeners(ListSelectionListener.class);
  }

  /**
   * Returns the selection mode for the list (one of: 
   * {@link ListSelectionModel#SINGLE_SELECTION}, 
   * {@link ListSelectionModel#SINGLE_INTERVAL_SELECTION} and 
   * {@link ListSelectionModel#MULTIPLE_INTERVAL_SELECTION}).
   * 
   * @return The selection mode.
   * 
   * @see #setSelectionMode(int)
   */
  public int getSelectionMode()
  {
    return selectionModel.getSelectionMode();
  }
  
  /**
   * Sets the list's "selectionMode" property, which simply mirrors the
   * same property on the list's {@link #selectionModel} property. This
   * property should be one of the integer constants
   * <code>SINGLE_SELECTION</code>, <code>SINGLE_INTERVAL_SELECTION</code>,
   * or <code>MULTIPLE_INTERVAL_SELECTION</code> from the {@link
   * ListSelectionModel} interface.
   *
   * @param a The new selection mode
   */
  public void setSelectionMode(int a)
  {
    selectionModel.setSelectionMode(a);
  }

  /**
   * Adds the interval <code>[a,a]</code> to the set of selections managed
   * by this list's {@link #selectionModel} property. Depending on the
   * selection mode, this may cause existing selections to become invalid,
   * or may simply expand the set of selections. 
   *
   * @param a A number in the half-open range <code>[0, x)</code> where
   * <code>x = getModel.getSize()</code>, indicating the index of an
   * element in the list to select. When &lt; 0 the selection is cleared.
   *
   * @see #setSelectionMode
   * @see #selectionModel
   */
  public void setSelectedIndex(int a)
  {
    if (a < 0)
      selectionModel.clearSelection();
    else
      selectionModel.setSelectionInterval(a, a);
  }

  /**
   * For each element <code>a[i]</code> of the provided array
   * <code>a</code>, calls {@link #setSelectedIndex} on <code>a[i]</code>.
   *
   * @param a  an array of selected indices (<code>null</code> not permitted).
   * 
   * @throws NullPointerException if <code>a</code> is <code>null</code>.
   * @see #setSelectionMode
   * @see #selectionModel
   */
  public void setSelectedIndices(int [] a)
  {
    for (int i = 0; i < a.length; ++i)
      setSelectedIndex(a[i]);
  }

  /**
   * Returns the minimum index of an element in the list which is currently
   * selected.
   *
   * @return A number in the half-open range <code>[0, x)</code> where
   * <code>x = getModel.getSize()</code>, indicating the minimum index of
   * an element in the list for which the element is selected, or
   * <code>-1</code> if no elements are selected
   */
  public int getSelectedIndex()
  {
    return selectionModel.getMinSelectionIndex();
  }

  /**
   * Returns <code>true</code> if the model's selection is empty, otherwise
   * <code>false</code>. 
   *
   * @return The return value of {@link ListSelectionModel#isSelectionEmpty}
   */
  public boolean isSelectionEmpty()
  {
    return selectionModel.isSelectionEmpty();
  }

  /**
   * Returns the list index of the upper left or upper right corner of the
   * visible rectangle of this list, depending on the {@link
   * Component#getComponentOrientation} property.
   *
   * @return The index of the first visible list cell, or <code>-1</code>
   * if none is visible.
   */
  public int getFirstVisibleIndex()
  {
    ComponentOrientation or = getComponentOrientation();
    Rectangle r = getVisibleRect();
    if (or == ComponentOrientation.RIGHT_TO_LEFT)
      r.translate((int) r.getWidth() - 1, 0);
    return getUI().locationToIndex(this, r.getLocation());      
  }


  /**
   * Returns index of the cell to which specified location is closest to. If
   * the location is outside the bounds of the list, then the greatest index
   * in the list model is returned. If the list model is empty, then
   * <code>-1</code> is returned.
   *
   * @param location for which to look for in the list
   * 
   * @return index of the cell to which specified location is closest to.
   */
   public int locationToIndex(Point location)
   {
     return getUI().locationToIndex(this, location);      
   }

  /**
   * Returns location of the cell located at the specified index in the list.
   * @param index of the cell for which location will be determined
   * 
   * @return location of the cell located at the specified index in the list.
   */
   public Point indexToLocation(int index)
   {
     return getUI().indexToLocation(this, index);
   }

  /**
   * Returns the list index of the lower right or lower left corner of the
   * visible rectangle of this list, depending on the {@link
   * Component#getComponentOrientation} property.
   *
   * @return The index of the last visible list cell, or <code>-1</code>
   * if none is visible.
   */
  public int getLastVisibleIndex()
  {
    ComponentOrientation or = getComponentOrientation();
    Rectangle r = getVisibleRect();
    r.translate(0, (int) r.getHeight() - 1);
    if (or == ComponentOrientation.LEFT_TO_RIGHT)
      r.translate((int) r.getWidth() - 1, 0);
    if (getUI().locationToIndex(this, r.getLocation()) == -1
        && indexToLocation(getModel().getSize() - 1).y < r.y)
      return getModel().getSize() - 1;
    return getUI().locationToIndex(this, r.getLocation());
  }

  /**
   * Returns the indices of values in the {@link #model} property which are
   * selected.
   *
   * @return An array of model indices, each of which is selected according
   *         to the {@link #getSelectedValues} property
   */
  public int[] getSelectedIndices()
  {
    int lo, hi, n, i, j;
    if (selectionModel.isSelectionEmpty())
      return new int[0];
    lo = selectionModel.getMinSelectionIndex();
    hi = selectionModel.getMaxSelectionIndex();
    n = 0;
    for (i = lo; i <= hi; ++i)
      if (selectionModel.isSelectedIndex(i))
        n++;
    int [] v = new int[n];
    j = 0;
    for (i = lo; i <= hi; ++i)
      if (selectionModel.isSelectedIndex(i))
        v[j++] = i;
    return v;
  }

  /**
   * Indicates whether the list element at a given index value is
   * currently selected.
   *
   * @param a The index to check 
   * @return <code>true</code> if <code>a</code> is the index of a selected
   * list element
   */
  public boolean isSelectedIndex(int a)
  {
    return selectionModel.isSelectedIndex(a);
  }

  /**
   * Returns the first value in the list's {@link #model} property which is
   * selected, according to the list's {@link #selectionModel} property.
   * This is equivalent to calling
   * <code>getModel()getElementAt(getSelectedIndex())</code>, with a check
   * for the special index value of <code>-1</code> which returns null
   * <code>null</code>.
   *
   * @return The first selected element, or <code>null</code> if no element
   * is selected.
   *
   * @see #getSelectedValues
   */
  public Object getSelectedValue()
  {
    int index = getSelectedIndex();
    if (index == -1)
      return null;
    return getModel().getElementAt(index);
  }

  /**
   * Returns all the values in the list's {@link #model} property which are
   * selected, according to the list's {@link #selectionModel} property.
   * 
   * @return An array containing all the selected values
   * @see #setSelectedValue
   */
  public Object[] getSelectedValues()
  {
    int[] idx = getSelectedIndices();
    Object[] v = new Object[idx.length];
    for (int i = 0; i < idx.length; ++i)
      v[i] = getModel().getElementAt(idx[i]);
    return v;
  }

  /**
   * Gets the value of the {@link #selectionBackground} property.
   *
   * @return The current value of the property
   */
  public Color getSelectionBackground()
  {
    return selectionBackground;
  }

  /**
   * Sets the value of the {@link #selectionBackground} property.
   *
   * @param c The new value of the property
   */
  public void setSelectionBackground(Color c)
  {
    if (selectionBackground == c)
      return;

    Color old = selectionBackground;
    selectionBackground = c;
    firePropertyChange("selectionBackground", old, c);
    repaint();
  }

  /**
   * Gets the value of the {@link #selectionForeground} property.
   *
   * @return The current value of the property
   */
  public Color getSelectionForeground()
  {
    return selectionForeground;
  }
  
  /**
   * Sets the value of the {@link #selectionForeground} property.
   *
   * @param c The new value of the property
   */
  public void setSelectionForeground(Color c)
  {
    if (selectionForeground == c)
      return;

    Color old = selectionForeground;
    selectionForeground = c;
    firePropertyChange("selectionForeground", old, c);
  }

  /**
   * Sets the selection to cover only the specified value, if it
   * exists in the model. 
   *
   * @param obj The object to select
   * @param scroll Whether to scroll the list to make the newly selected
   * value visible
   *
   * @see #ensureIndexIsVisible
   */

  public void setSelectedValue(Object obj, boolean scroll)
  {
    for (int i = 0; i < model.getSize(); ++i)
      {
        if (model.getElementAt(i).equals(obj))
          {
            setSelectedIndex(i);
            if (scroll)
              ensureIndexIsVisible(i);
            break;
          }
      }
  }

  /**
   * Scrolls this list to make the specified cell visible. This
   * only works if the list is contained within a viewport.
   *
   * @param i The list index to make visible
   *
   * @see JComponent#scrollRectToVisible
   */
  public void ensureIndexIsVisible(int i)
  {
    Rectangle r = getUI().getCellBounds(this, i, i);
    if (r != null)
      scrollRectToVisible(r);
  }

  /**
   * Sets the {@link #model} property of the list to a new anonymous
   * {@link AbstractListModel} subclass which accesses the provided Object
   * array directly.
   *
   * @param listData The object array to build a new list model on
   * @see #setModel
   */
  public void setListData(Object[] listData)
  {
    setModel(createListModel(listData));
  }

  /**
   * Returns a {@link ListModel} backed by the specified array.
   * 
   * @param items  the list items (don't use <code>null</code>).
   * 
   * @return A list model containing the specified items.
   */
  private ListModel createListModel(final Object[] items)
  {
    return new AbstractListModel()
      {
        public int getSize()
        {
          return items.length;
        }
        public Object getElementAt(int i)
        {
          return items[i];
        }
      };
  }
  
  /**
   * Returns a {@link ListModel} backed by the specified vector.
   * 
   * @param items  the list items (don't use <code>null</code>).
   * 
   * @return A list model containing the specified items.
   */
  private ListModel createListModel(final Vector items)
  {
    return new AbstractListModel()
      {
        public int getSize()
        {
          return items.size();
        }
        public Object getElementAt(int i)
        {
          return items.get(i);
        }
      };
  }

  /**
   * Sets the {@link #model} property of the list to a new anonymous {@link
   * AbstractListModel} subclass which accesses the provided vector
   * directly.
   *
   * @param listData The object array to build a new list model on
   * @see #setModel
   */
  public void setListData(final Vector<?> listData)
  {
    setModel(new AbstractListModel()
      {
	public int getSize()
	{
	  return listData.size();
	}
	
	public Object getElementAt(int i)
	{
	  return listData.elementAt(i);
	}
      });
  }

  /**
   * Gets the value of the {@link #cellRenderer} property. 
   *
   * @return The current value of the property
   */
  public ListCellRenderer getCellRenderer()
  {
    return cellRenderer;
  }

  /**
   * Sets the value of the {@link #getCellRenderer} property.
   *
   * @param renderer The new property value
   */
  public void setCellRenderer(ListCellRenderer renderer)
  {
    if (cellRenderer == renderer)
      return;
    
    ListCellRenderer old = cellRenderer;
    cellRenderer = renderer;
    firePropertyChange("cellRenderer", old, renderer);
    revalidate();
    repaint();
  }

  /**
   * Gets the value of the {@link #model} property. 
   *
   * @return The current value of the property
   */
  public ListModel getModel()
  {
    return model;
  }

  /**
   * Sets the value of the {@link #model} property. The list's {@link
   * #listListener} is unsubscribed from the existing model, if it exists,
   * and re-subscribed to the new model.
   *
   * @param model  the new model (<code>null</code> not permitted).
   * 
   * @throws IllegalArgumentException if <code>model</code> is 
   *         <code>null</code>.
   */
  public void setModel(ListModel model)
  {
    if (model == null) 
      throw new IllegalArgumentException("Null 'model' argument.");
    if (this.model == model)
      return;
    
    if (this.model != null)
      this.model.removeListDataListener(listListener);
    
    ListModel old = this.model;
    this.model = model;
    
    if (this.model != null)
      this.model.addListDataListener(listListener);
    
    firePropertyChange("model", old, model);
    revalidate();
    repaint();
  }

  /**
   * Returns the selection model for the {@link JList} component.  Note that
   * this class contains a range of convenience methods for configuring the
   * selection model:<br>
   * <ul>
   *   <li>{@link #clearSelection()};</li>
   *   <li>{@link #setSelectionMode(int)};</li>
   *   <li>{@link #addSelectionInterval(int, int)};</li>
   *   <li>{@link #setSelectedIndex(int)};</li>
   *   <li>{@link #setSelectedIndices(int[])};</li>
   *   <li>{@link #setSelectionInterval(int, int)}.</li>
   * </ul>
   * 
   * @return The selection model.
   */
  public ListSelectionModel getSelectionModel()
  {
    return selectionModel;
  }

  /**
   * Sets the value of the {@link #selectionModel} property. The list's
   * {@link #listListener} is unsubscribed from the existing selection
   * model, if it exists, and re-subscribed to the new selection model.
   *
   * @param model The new property value
   */
  public void setSelectionModel(ListSelectionModel model)
  {
    if (selectionModel == model)
      return;
    
    if (selectionModel != null)
      selectionModel.removeListSelectionListener(listListener);
    
    ListSelectionModel old = selectionModel;
    selectionModel = model;
    
    if (selectionModel != null)
      selectionModel.addListSelectionListener(listListener);
    
    firePropertyChange("selectionModel", old, model);
    revalidate();
    repaint();
  }

  /**
   * Gets the value of the UI property.
   *
   * @return The current property value
   */
  public ListUI getUI()
  {
    return (ListUI) ui;
  }

  /**
   * Sets the value of the UI property.
   *
   * @param ui The new property value
   */
  public void setUI(ListUI ui)
  {
    super.setUI(ui);
  }

  /**
   * Calls {@link #setUI} with the {@link ListUI} subclass
   * returned from calling {@link UIManager#getUI}.
   */
  public void updateUI()
  {
    setUI((ListUI) UIManager.getUI(this));
  }

  /**
   * Return the class identifier for the list's UI property.  This should
   * be the constant string <code>"ListUI"</code>, and map to an
   * appropriate UI class in the {@link UIManager}.
   *
   * @return The class identifier
   */
  public String getUIClassID()
  {
    return "ListUI";
  }


  /**
   * Returns the current value of the {@link #prototypeCellValue}
   * property. This property holds a reference to a "prototype" data value
   * -- typically a String -- which is used to calculate the {@link
   * #fixedCellWidth} and {@link #fixedCellHeight} properties, using the
   * {@link #cellRenderer} property to acquire a component to render the
   * prototype.
   *
   * @return The current prototype cell value
   * @see #setPrototypeCellValue
   */
  public Object getPrototypeCellValue()
  {
    return prototypeCellValue;
  }

  /**
   * <p>Set the {@link #prototypeCellValue} property. This property holds a
   * reference to a "prototype" data value -- typically a String -- which
   * is used to calculate the {@link #fixedCellWidth} and {@link
   * #fixedCellHeight} properties, using the {@link #cellRenderer} property
   * to acquire a component to render the prototype.</p>
   *
   * <p>It is important that you <em>not</em> set this value to a
   * component. It has to be a <em>data value</em> such as the objects you
   * would find in the list's model. Setting it to a component will have
   * undefined (and undesirable) affects. </p>
   *
   * @param obj The new prototype cell value
   * @see #getPrototypeCellValue
   */
  public void setPrototypeCellValue(Object obj)
  {
    if (prototypeCellValue == obj)
      return;

    Object old = prototypeCellValue;
    Component comp = getCellRenderer()
      .getListCellRendererComponent(this, obj, 0, false, false); 
    Dimension d = comp.getPreferredSize();
    fixedCellWidth = d.width;
    fixedCellHeight = d.height;
    prototypeCellValue = obj;
    firePropertyChange("prototypeCellValue", old, obj);
  }

  public AccessibleContext getAccessibleContext()
  {
    return new AccessibleJList();
  }

  /**
   * Returns a size indicating how much space this list would like to
   * consume, when contained in a scrollable viewport. This is part of the
   * {@link Scrollable} interface, which interacts with {@link
   * ScrollPaneLayout} and {@link JViewport} to define scrollable objects.
   *
   * @return The preferred size
   */
  public Dimension getPreferredScrollableViewportSize()
  {
    //If the layout orientation is not VERTICAL, then this will 
    //return the value from getPreferredSize. The current ListUI is 
    //expected to override getPreferredSize to return an appropriate value.
    if (getLayoutOrientation() != VERTICAL)
      return getPreferredSize();        

    int size = getModel().getSize();
    
    // Trivial case: if fixedCellWidth and fixedCellHeight were set 
    // just use them
    if (fixedCellHeight != -1 && fixedCellWidth != -1)
      return new Dimension(fixedCellWidth, size * fixedCellHeight);
        
    // If the model is empty we use 16 * the number of visible rows
    // for the height and either fixedCellWidth (if set) or 256
    // for the width
    if (size == 0)
      {
        if (fixedCellWidth == -1)
          return new Dimension(256, 16 * getVisibleRowCount());
        else
          return new Dimension(fixedCellWidth, 16 * getVisibleRowCount());
      }

    // Calculate the width: if fixedCellWidth was set use that, otherwise
    // use the preferredWidth
    int prefWidth;
    if (fixedCellWidth != -1)
      prefWidth = fixedCellWidth;
    else
      prefWidth = getPreferredSize().width;

    // Calculate the height: if fixedCellHeight was set use that, otherwise
    // use the height of the first row multiplied by the number of visible
    // rows
    int prefHeight;
    if (fixedCellHeight != -1)
      prefHeight = fixedCellHeight;
    else
      prefHeight = getVisibleRowCount() * getCellBounds(0, 0).height;

    return new Dimension (prefWidth, prefHeight);
  }

  /**
   * <p>Return the number of pixels the list must scroll in order to move a
   * "unit" of the list into the provided visible rectangle. When the
   * provided direction is positive, the call describes a "downwards"
   * scroll, which will be exposing a cell at a <em>greater</em> index in
   * the list than those elements currently showing. Then the provided
   * direction is negative, the call describes an "upwards" scroll, which
   * will be exposing a cell at a <em>lesser</em> index in the list than
   * those elements currently showing.</p>
   *
   * <p>If the provided orientation is <code>HORIZONTAL</code>, the above
   * comments refer to "rightwards" for positive direction, and "leftwards"
   * for negative.</p>
   * 
   *
   * @param visibleRect The rectangle to scroll an element into
   * @param orientation One of the numeric consants <code>VERTICAL</code>
   * or <code>HORIZONTAL</code>
   * @param direction An integer indicating the scroll direction: positive means
   * forwards (down, right), negative means backwards (up, left)
   *
   * @return The scrollable unit increment, in pixels
   */
  public int getScrollableUnitIncrement(Rectangle visibleRect,
                                        int orientation, int direction)
  {
    int unit = -1;
    if (orientation == SwingConstants.VERTICAL)
      {
        int row = getFirstVisibleIndex();
        if (row == -1)
          unit = 0;
        else if (direction > 0)
          {
            // Scrolling down.
            Rectangle bounds = getCellBounds(row, row);
            if (bounds != null)
              unit = bounds.height - (visibleRect.y - bounds.y);
            else
              unit = 0;
          }
        else
          {
            // Scrolling up.
            Rectangle bounds = getCellBounds(row, row);
            // First row.
            if (row == 0 && bounds.y == visibleRect.y)
              unit = 0; // No need to scroll.
            else if (bounds.y == visibleRect.y)
              {
                // Scroll to previous row.
                Point loc = bounds.getLocation();
                loc.y--;
                int prev = locationToIndex(loc);
                Rectangle prevR = getCellBounds(prev, prev);
                if (prevR == null || prevR.y >= bounds.y)
                  unit = 0; // For multicolumn lists.
                else
                  unit = prevR.height;
              }
            else
              unit = visibleRect.y - bounds.y;
          }
      }
    else if (orientation == SwingConstants.HORIZONTAL && getLayoutOrientation() != VERTICAL)
      {
        // Horizontal scrolling.
        int i = locationToIndex(visibleRect.getLocation());
        if (i != -1)
          {
            Rectangle b = getCellBounds(i, i);
            if (b != null)
              {
                if (b.x != visibleRect.x)
                  {
                    if (direction < 0)
                      unit = Math.abs(b.x - visibleRect.x);
                    else
                      unit = b.width + b.x - visibleRect.x;
                  }
                else
                  unit = b.width;
              }
          }
      }

    if (unit == -1)
      {
        // This fallback seems to be used by the RI for the degenerate cases
        // not covered above.
        Font f = getFont();
        unit = f != null ? f.getSize() : 1;
      }
    return unit;
  }

  /**
   * <p>Return the number of pixels the list must scroll in order to move a
   * "block" of the list into the provided visible rectangle. When the
   * provided direction is positive, the call describes a "downwards"
   * scroll, which will be exposing a cell at a <em>greater</em> index in
   * the list than those elements currently showing. Then the provided
   * direction is negative, the call describes an "upwards" scroll, which
   * will be exposing a cell at a <em>lesser</em> index in the list than
   * those elements currently showing.</p>
   *
   * <p>If the provided orientation is <code>HORIZONTAL</code>, the above
   * comments refer to "rightwards" for positive direction, and "leftwards"
   * for negative.</p>
   * 
   *
   * @param visibleRect The rectangle to scroll an element into
   * @param orientation One of the numeric consants <code>VERTICAL</code>
   * or <code>HORIZONTAL</code>
   * @param direction An integer indicating the scroll direction: positive means
   * forwards (down, right), negative means backwards (up, left)
   *
   * @return The scrollable unit increment, in pixels
   */
  public int getScrollableBlockIncrement(Rectangle visibleRect,
                                         int orientation, int direction)
  {
    int block = -1;
    if (orientation == SwingConstants.VERTICAL)
      {
        // Default block scroll. Special cases are handled below for
        // better usability.
        block = visibleRect.height;
        if (direction > 0)
          {
            // Scroll down.
            // Scroll so that after scrolling the last line aligns with
            // the lower boundary of the visible area.
            Point p = new Point(visibleRect.x,
                                visibleRect.y + visibleRect.height - 1);
            int last = locationToIndex(p);
            if (last != -1)
              {
                Rectangle lastR = getCellBounds(last, last);
                if (lastR != null)
                  {
                    block = lastR.y - visibleRect.y;
                    if (block == 0&& last < getModel().getSize() - 1)
                      block = lastR.height;
                  }
              }
          }
        else
          {
            // Scroll up.
            // Scroll so that after scrolling the first line aligns with
            // the upper boundary of the visible area.
            Point p = new Point(visibleRect.x,
                                visibleRect.y - visibleRect.height);
            int newFirst = locationToIndex(p);
            if (newFirst != -1)
              {
                int first = getFirstVisibleIndex();
                if (first == -1)
                  first = locationToIndex(visibleRect.getLocation());
                Rectangle newFirstR = getCellBounds(newFirst, newFirst);
                Rectangle firstR = getCellBounds(first, first);
                if (newFirstR != null && firstR != null)
                  {
                    // Search first item that would left the current first
                    // item visible when scrolled to.
                    while (newFirstR.y + visibleRect.height
                           < firstR.y + firstR.height
                           && newFirstR.y < firstR.y)
                      {
                        newFirst++;
                        newFirstR = getCellBounds(newFirst, newFirst);
                      }
                    block = visibleRect.y - newFirstR.y;
                    if (block <= 0 && newFirstR.y > 0)
                      {
                        newFirst--;
                        newFirstR = getCellBounds(newFirst, newFirst);
                        if (newFirstR != null)
                          block = visibleRect.y - newFirstR.y;
                      }
                  }
              }
          }
      }
    else if (orientation == SwingConstants.HORIZONTAL
             && getLayoutOrientation() != VERTICAL)
      {
        // Default block increment. Special cases are handled below for
        // better usability.
        block = visibleRect.width;
        if (direction > 0)
          {
            // Scroll right.
            Point p = new Point(visibleRect.x + visibleRect.width + 1,
                                visibleRect.y);
            int last = locationToIndex(p);
            if (last != -1)
              {
                Rectangle lastR = getCellBounds(last, last);
                if (lastR != null)
                  {
                    block = lastR.x  - visibleRect.x;
                    if (block < 0)
                      block += lastR.width;
                    else if (block == 0 && last < getModel().getSize() - 1)
                      block = lastR.width;
                  }
              }
          }
        else
          {
            // Scroll left.
            Point p = new Point(visibleRect.x - visibleRect.width,
                                visibleRect.y);
            int first = locationToIndex(p);
            if (first != -1)
              {
                Rectangle firstR = getCellBounds(first, first);
                if (firstR != null)
                  {
                    if (firstR.x < visibleRect.x - visibleRect.width)
                      {
                        if (firstR.x + firstR.width > visibleRect.x)
                          block = visibleRect.x - firstR.x;
                        else
                          block = visibleRect.x - firstR.x - firstR.width;
                      }
                    else
                      block = visibleRect.x - firstR.x;
                  }
              }
          }
      }

    return block;
  }

  /**
   * Gets the value of the <code>scrollableTracksViewportWidth</code> property.
   *
   * @return <code>true</code> if the viewport is larger (horizontally)
   * than the list and the list should be expanded to fit the viewport;
   * <code>false</code> if the viewport is smaller than the list and the
   * list should scroll (horizontally) within the viewport
   */
  public boolean getScrollableTracksViewportWidth()
  {
    Component parent = getParent();
    boolean retVal = false;
    if (parent instanceof JViewport)
      {
        JViewport viewport = (JViewport) parent;
        Dimension pref = getPreferredSize();
        if (viewport.getSize().width > pref.width)
          retVal = true;
        if ((getLayoutOrientation() == HORIZONTAL_WRAP)
            && (getVisibleRowCount() <= 0))
          retVal = true;
      }
    return retVal;
  }

  /**
   * Gets the value of the </code>scrollableTracksViewportWidth</code> property.
   *
   * @return <code>true</code> if the viewport is larger (vertically)
   * than the list and the list should be expanded to fit the viewport;
   * <code>false</code> if the viewport is smaller than the list and the
   * list should scroll (vertically) within the viewport
   */
  public boolean getScrollableTracksViewportHeight()
  {
    Component parent = getParent();
    boolean retVal = false;
    if (parent instanceof JViewport)
      {
        JViewport viewport = (JViewport) parent;
        Dimension pref = getPreferredSize();
        if (viewport.getSize().height > pref.height)
          retVal = true;
        if ((getLayoutOrientation() == VERTICAL_WRAP)
            && (getVisibleRowCount() <= 0))
          retVal = true;
      }
    return retVal;
  }

  /**
   * Returns the index of the anchor item in the current selection, or
   * <code>-1</code> if there is no anchor item.
   * 
   * @return The item index.
   */
  public int getAnchorSelectionIndex()
  {
    return selectionModel.getAnchorSelectionIndex();
  }

  /**
   * Returns the index of the lead item in the current selection, or
   * <code>-1</code> if there is no lead item.
   * 
   * @return The item index.
   */
  public int getLeadSelectionIndex()
  {
    return selectionModel.getLeadSelectionIndex();
  }

  /**
   * Returns the lowest item index in the current selection, or <code>-1</code>
   * if there is no selection.
   * 
   * @return The index.
   * 
   * @see #getMaxSelectionIndex()
   */
  public int getMinSelectionIndex()
  {
    return selectionModel.getMinSelectionIndex();
  }

  /**
   * Returns the highest item index in the current selection, or 
   * <code>-1</code> if there is no selection.
   * 
   * @return The index.
   * 
   * @see #getMinSelectionIndex()
   */
  public int getMaxSelectionIndex()
  {
    return selectionModel.getMaxSelectionIndex();
  }

  /**
   * Clears the current selection.
   */
  public void clearSelection()
  {
    selectionModel.clearSelection();
  }

  /**
   * Sets the current selection to the items in the specified range (inclusive).
   * Note that <code>anchor</code> can be less than, equal to, or greater than 
   * <code>lead</code>.
   * 
   * @param anchor  the index of the anchor item.
   * @param lead  the index of the anchor item.
   */
  public void setSelectionInterval(int anchor, int lead)
  {
    selectionModel.setSelectionInterval(anchor, lead);
  }

  /**
   * Adds the specified interval to the current selection.  Note that 
   * <code>anchor</code> can be less than, equal to, or greater than 
   * <code>lead</code>.
   * 
   * @param anchor  the index of the anchor item.
   * @param lead  the index of the lead item.
   */
  public void addSelectionInterval(int anchor, int lead)
  {
    selectionModel.addSelectionInterval(anchor, lead);
  }

  /**
   * Removes the specified interval from the current selection.  Note that 
   * <code>index0</code> can be less than, equal to, or greater than 
   * <code>index1</code>.
   * 
   * @param index0  an index for one end of the range.
   * @param index1  an index for the other end of the range.
   */
  public void removeSelectionInterval(int index0, int index1)
  {
    selectionModel.removeSelectionInterval(index0, index1);
  }

  /**
   * Returns the <code>valueIsAdjusting</code> flag from the list's selection
   * model.
   *
   * @return the value
   */
  public boolean getValueIsAdjusting()
  {
    return selectionModel.getValueIsAdjusting();
  }

  /**
   * Sets the <code>valueIsAdjusting</code> flag in the list's selection 
   * model.
   *
   * @param isAdjusting the new value
   */
  public void setValueIsAdjusting(boolean isAdjusting)
  {
    selectionModel.setValueIsAdjusting(isAdjusting);
  }

  /**
   * Return the value of the <code>dragEnabled</code> property.
   *
   * @return the value
   * 
   * @since 1.4
   */
  public boolean getDragEnabled()
  {
    return dragEnabled;
  }

  /**
   * Set the <code>dragEnabled</code> property.
   *
   * @param enabled new value
   * 
   * @since 1.4
   */
  public void setDragEnabled(boolean enabled)
  {
    dragEnabled = enabled;
  }

  /**
   * Returns the layout orientation, which will be one of {@link #VERTICAL}, 
   * {@link #VERTICAL_WRAP} and {@link #HORIZONTAL_WRAP}.  The default value
   * is {@link #VERTICAL}.
   *
   * @return the orientation.
   *
   * @see #setLayoutOrientation(int)
   * @since 1.4
   */
  public int getLayoutOrientation()
  {
    return layoutOrientation;
  }

  /**
   * Sets the layout orientation (this is a bound property with the name
   * 'layoutOrientation').  Valid orientations are {@link #VERTICAL}, 
   * {@link #VERTICAL_WRAP} and {@link #HORIZONTAL_WRAP}.
   *
   * @param orientation the orientation.
   *
   * @throws IllegalArgumentException if <code>orientation</code> is not one
   *     of the specified values.
   * @since 1.4
   * @see #getLayoutOrientation()
   */
  public void setLayoutOrientation(int orientation)
  {
    if (orientation < JList.VERTICAL || orientation > JList.HORIZONTAL_WRAP)
      throw new IllegalArgumentException();
    if (layoutOrientation == orientation)
      return;

    int old = layoutOrientation;
    layoutOrientation = orientation;
    firePropertyChange("layoutOrientation", old, orientation);
  }

  /**
   * Returns the bounds of the rectangle that encloses both list cells
   * with index0 and index1.
   *
   * @param index0 the index of the first cell
   * @param index1 the index of the second cell
   *
   * @return  the bounds of the rectangle that encloses both list cells
   *     with index0 and index1, <code>null</code> if one of the indices is
   *     not valid
   */
  public Rectangle getCellBounds(int index0, int index1)
  {
    ListUI ui = getUI();
    Rectangle bounds = null;
    if (ui != null)
      {
        bounds = ui.getCellBounds(this, index0, index1);
      }
    // When the UI is null, this method also returns null in the RI.
    return bounds;
  }

  /**
   * Returns the index of the next list element (beginning at 
   * <code>startIndex</code> and moving in the specified direction through the
   * list, looping around if necessary) that starts with <code>prefix</code>
   * (ignoring case).
   *
   * @param prefix the prefix to search for in the cell values
   * @param startIndex the index where to start searching from
   * @param direction the search direction, either {@link Position.Bias#Forward}
   *     or {@link Position.Bias#Backward} (<code>null</code> is interpreted
   *     as {@link Position.Bias#Backward}.
   *
   * @return the index of the found element or -1 if no such element has
   *     been found
   *
   * @throws IllegalArgumentException if prefix is <code>null</code> or
   *     startIndex is not valid
   *
   * @since 1.4
   */
  public int getNextMatch(String prefix, int startIndex, 
                          Position.Bias direction)
  {
    if (prefix == null)
      throw new IllegalArgumentException("The argument 'prefix' must not be"
                                         + " null.");
    if (startIndex < 0)
      throw new IllegalArgumentException("The argument 'startIndex' must not"
                                         + " be less than zero.");

    int size = model.getSize();
    if (startIndex >= model.getSize())
      throw new IllegalArgumentException("The argument 'startIndex' must not"
                                         + " be greater than the number of"
                                         + " elements in the ListModel.");

    int result = -1;
    int current = startIndex;
    int delta = -1;
    int itemCount = model.getSize();
    boolean finished = false;
    prefix = prefix.toUpperCase();
    
    if (direction == Position.Bias.Forward)
      delta = 1;
    while (!finished)
      {
        String itemStr = model.getElementAt(current).toString().toUpperCase();
        if (itemStr.startsWith(prefix))
          return current;
        current = (current + delta);
        if (current == -1)
          current += itemCount;
        else
          current = current % itemCount; 
        finished = current == startIndex;
      }
    return result;
  }
  
  /**
   * Returns a string describing the attributes for the <code>JList</code>
   * component, for use in debugging.  The return value is guaranteed to be 
   * non-<code>null</code>, but the format of the string may vary between
   * implementations.
   *
   * @return A string describing the attributes of the <code>JList</code>.
   */
  protected String paramString()
  {
    CPStringBuilder sb = new CPStringBuilder(super.paramString());
    sb.append(",fixedCellHeight=").append(getFixedCellHeight());
    sb.append(",fixedCellWidth=").append(getFixedCellWidth());
    sb.append(",selectionBackground=");
    if (getSelectionBackground() != null)
      sb.append(getSelectionBackground());
    sb.append(",selectionForeground=");
    if (getSelectionForeground() != null)
      sb.append(getSelectionForeground());
    sb.append(",visibleRowCount=").append(getVisibleRowCount());
    sb.append(",layoutOrientation=").append(getLayoutOrientation());
    return sb.toString();
  }
}
