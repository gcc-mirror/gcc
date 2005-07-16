/* JList.java --
   Copyright (C) 2002, 2003, 2004, 2005  Free Software Foundation, Inc.

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

import java.awt.Color;
import java.awt.Component;
import java.awt.ComponentOrientation;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.Rectangle;
import java.util.Vector;

import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;
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
   * the list. When {@link ListCellRenderer.getListCellRendererComponent}
   * is called with a selected cell object, the component returned will
   * have its "foreground" set to this color.
   */
  Color selectionBackground;

  /** 
   * This property specifies a background color for the selected cells in
   * the list. When {@link ListCellRenderer.getListCellRendererComponent}
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
   * This property indicates that the list's selection is currently
   * "adjusting" -- perhaps due to a user actively dragging the mouse over
   * multiple list elements -- and is therefore likely to change again in
   * the near future. A {@link ListSelectionListener} might choose to delay
   * updating its view of the list's selection until this property is
   * false, meaning that the adjustment has completed.
   */
  boolean valueIsAdjusting;

  /** 
   * This property indicates a <em>preference</em> for the number of rows
   * displayed in the list, and will scale the
   * {@link #preferredScrollableViewportSize} property accordingly. The actual
   * number of displayed rows, when the list is placed in a real {@link
   * Viewport} or other component, may be greater or less than this number.
   */
  int visibleRowCount;

  /**
   * Fire a {@link ListSelectionEvent} to all the registered ListSelectionListeners.
   */
  protected void fireSelectionValueChanged(int firstIndex, int lastIndex, boolean isAdjusting) 
  {
    ListSelectionEvent evt = new ListSelectionEvent(this, firstIndex, lastIndex, isAdjusting);
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
  };

  /** 
   * Shared ListListener instance, subscribed to both the current {@link
   * #model} and {@link #selectionModel} properties of the list.
   */
  ListListener listListener;


  /**
   * Creates a new JList object.
   */
  public JList()
  {
    init();
  }

  /**
   * Creates a new JList object.
   *
   * @param listData Initial data to populate the list with
   */
  public JList(Object[] listData)
  {
    init();
    setListData(listData);
  }

  /**
   * Creates a new JList object.
   *
   * @param listData Initial data to populate the list with
   */
  public JList(Vector listData)
  {
    init();
    setListData(listData);
  }

  /**
   * Creates a new JList object.
   *
   * @param listData Initial data to populate the list with
   */
  public JList(ListModel listData)
  {
    init();
    setModel(listData);
  }

  void init()
  {
    dragEnabled = false;
    fixedCellHeight = -1;
    fixedCellWidth = -1;
    layoutOrientation = VERTICAL;
    opaque = true;
    valueIsAdjusting = false;
    visibleRowCount = 8;

    cellRenderer = new DefaultListCellRenderer();
    listListener = new ListListener();

    setModel(new DefaultListModel());
    setSelectionModel(createSelectionModel());

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
   * @see #getFixedCellHeight
   * @see #getPrototypeCellValue
   */
  public void setFixedCellHeight(int h)
  {
    if (fixedCellHeight == h)
      return;

    int old = fixedCellHeight;
    fixedCellHeight = h;
    firePropertyChange("fixedCellWidth", old, h);
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
   * Gets the value of the {@link #visibleRowCount} property. 
   *
   * @return the current value of the property.
   */

  public int getVisibleRowCount()
  {
    return visibleRowCount;
  }

  /**
   * Sets the value of the {@link #visibleRowCount} property. 
   *
   * @param visibleRowCount The new property value
   */
  public void setVisibleRowCount(int vc)
  {
    visibleRowCount = vc;
    revalidate();
    repaint();
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
   * element in the list to select.
   *
   * @see #setSelectionMode
   * @see #selectionModel
   */
  public void setSelectedIndex(int a)
  {
    selectionModel.setSelectionInterval(a, a);
  }

  /**
   * For each element <code>a[i]</code> of the provided array
   * <code>a</code>, calls {@link #setSelectedIndex} on <code>a[i]</code>.
   *
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
   * {@link #visibleRect} property, depending on the {@link
   * #componentOrientation} property.
   *
   * @return The index of the first visible list cell, or <code>-1</code>
   * if none is visible.
   */
  public int getFirstVisibleIndex()
  {
    ComponentOrientation or = getComponentOrientation();
    Rectangle r = getVisibleRect();
    if (or == ComponentOrientation.RIGHT_TO_LEFT)
      r.translate((int) r.getWidth(), 0);
    return getUI().locationToIndex(this, r.getLocation());      
  }


  /**
   * Returns index of the cell to which specified location is closest to
   * @param location for which to look for in the list
   * 
   * @return index of the cell to which specified location is closest to.
   */
   public int locationToIndex(Point location) {
     return getUI().locationToIndex(this, location);      
   }

  /**
   * Returns location of the cell located at the specified index in the list.
   * @param index of the cell for which location will be determined
   * 
   * @return location of the cell located at the specified index in the list.
   */
   public Point indexToLocation(int index){
   	//FIXME: Need to implement.
	return null;
   }

  /**
   * Returns the list index of the lower right or lower left corner of the
   * {@link #visibleRect} property, depending on the {@link
   * #componentOrientation} property.
   *
   * @return The index of the first visible list cell, or <code>-1</code>
   * if none is visible.
   */
  public int getLastVisibleIndex()
  {
    ComponentOrientation or = getComponentOrientation();
    Rectangle r = getVisibleRect();
    r.translate(0, (int) r.getHeight());
    if (or == ComponentOrientation.LEFT_TO_RIGHT)
      r.translate((int) r.getWidth(), 0);
    return getUI().locationToIndex(this, r.getLocation());      
  }

  /**
   * Returns the indices of values in the {@link #model} property which are
   * selected.
   *
   * @return An array of model indices, each of which is selected according
   * to the {@link #selection} property
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
    for (i = lo; i < hi; ++i)
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
   * @see getSelectedValues
   */
  public Object getSelectedValue()
  {
    int index = getSelectedIndex();
    if (index == -1)
      return null;
    return getModel().getElementAt(index);
  }

  /**
   * Returns all the values in the list's {@link #model} property which
   * are selected, according to the list's {@link #selectionModel} property.
   *
   * @return An array containing all the selected values
   *
   * @see getSelectedValue
   */
  public Object[] getSelectedValues()
  {
    int [] idx = getSelectedIndices();
    Object [] v = new Object[idx.length];
    for (int i = 0; i < idx.length; ++i)
      v[i] = getModel().getElementAt(i);
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
    scrollRectToVisible(getUI().getCellBounds(this, i, i));
  }

  /**
   * Sets the {@link #model} property of the list to a new anonymous
   * {@link AbstractListModel} subclass which accesses the provided Object
   * array directly.
   *
   * @param listData The object array to build a new list model on
   * @see #setModel
   */
  public void setListData(final Object[] listData)
  {
    setModel(new AbstractListModel()
        {
          public int getSize()
          {
            return listData.length;
          }

          public Object getElementAt(int i)
          {
            return listData[i];
          }
        });
  }

  /**
   * Sets the {@link #model} property of the list to a new anonymous {@link
   * AbstractListModel} subclass which accesses the provided vector
   * directly.
   *
   * @param listData The object array to build a new list model on
   * @see #setModel
   */
  public void setListData(final Vector listData)
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
   * Sets the value of the {@link #celLRenderer} property.
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
   * @param model The new property value
   */
  public void setModel(ListModel model)
  {
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
    return null;
  }

  /**
   * Returns a size indicating how much space this list would like to
   * consume, when contained in a scrollable viewport. This is part of the
   * {@link Scrollable} interface, which interacts with {@link
   * ScrollPaneLayout} and {@link Viewport} to define scrollable objects.
   *
   * @return The preferred size
   */
  public Dimension getPreferredScrollableViewportSize()
  {

    Dimension retVal = getPreferredSize();
    if (getLayoutOrientation() == VERTICAL)
      {
        if (fixedCellHeight != -1)
          {
            if (fixedCellWidth != -1)
              {
                int size = getModel().getSize();
                retVal = new Dimension(fixedCellWidth, size * fixedCellHeight);
              } // TODO: add else clause (preferredSize is ok for now)
          } // TODO: add else clause (preferredSize is ok for now)
      }
    return retVal;
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
    ListUI lui = this.getUI();
    if (orientation == SwingConstants.VERTICAL)
      {
        if (direction > 0)
          {
            // Scrolling down
            Point bottomLeft = new Point(visibleRect.x,
                                         visibleRect.y + visibleRect.height);
            int curIdx = lui.locationToIndex(this, bottomLeft);
            Rectangle curBounds = lui.getCellBounds(this, curIdx, curIdx);
            if (curBounds.y + curBounds.height == bottomLeft.y)
              {
                // we are at the exact bottom of the current cell, so we 
                // are being asked to scroll to the end of the next one
                if (curIdx + 1 < model.getSize())
                  {
                    // there *is* a next item in the list
                    Rectangle nxtBounds = lui.getCellBounds(this, curIdx + 1, curIdx + 1);
                    return nxtBounds.height;
                  }
                else
                  {
                    // no next item, no advance possible
                    return 0;
                  }
              }
            else
              {
                // we are part way through an existing cell, so we are being
                // asked to scroll to the bottom of it
                return (curBounds.y + curBounds.height) - bottomLeft.y;
              }		      
          }
        else
          {
            // scrolling up
            Point topLeft = new Point(visibleRect.x, visibleRect.y);
            int curIdx = lui.locationToIndex(this, topLeft);
            Rectangle curBounds = lui.getCellBounds(this, curIdx, curIdx);
            if (curBounds.y == topLeft.y)
              {
                // we are at the exact top of the current cell, so we 
                // are being asked to scroll to the top of the previous one
                if (curIdx > 0)
                  {
                    // there *is* a previous item in the list
                    Rectangle nxtBounds = lui.getCellBounds(this, curIdx - 1, curIdx - 1);
                    return -nxtBounds.height;
                  }
                else
                  {
                    // no previous item, no advance possible
                    return 0;
                  }
              }
            else
              {
                // we are part way through an existing cell, so we are being
                // asked to scroll to the top of it
                return curBounds.y - topLeft.y;
              }		      
          }
      }

    // FIXME: handle horizontal scrolling (also wrapping?)
    return 1;
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
      if (orientation == VERTICAL)
	  return visibleRect.height * direction;
      else
	  return visibleRect.width * direction;
  }

  /**
   * Gets the value of the {@link #scrollableTracksViewportWidth} property.
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
   * Gets the value of the {@link #scrollableTracksViewportWidth} property.
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

  public int getAnchorSelectionIndex()
  {
    return selectionModel.getAnchorSelectionIndex();
  }

  public int getLeadSelectionIndex()
  {
    return selectionModel.getLeadSelectionIndex();
  }

  public int getMinSelectionIndex()
  {
    return selectionModel.getMaxSelectionIndex();
  }

  public int getMaxSelectionIndex()
  {
    return selectionModel.getMaxSelectionIndex();
  }

  public void clearSelection()
  {
    selectionModel.clearSelection();
  }

  public void setSelectionInterval(int anchor, int lead)
  {
    selectionModel.setSelectionInterval(anchor, lead);
  }

  public void addSelectionInterval(int anchor, int lead)
  {
    selectionModel.addSelectionInterval(anchor, lead);
  }

  public void removeSelectionInterval(int index0, int index1)
  {
    selectionModel.removeSelectionInterval(index0, index1);
  }

  /**
   * Returns the value of the <code>valueIsAdjusting</code> property.
   *
   * @return the value
   */
  public boolean getValueIsAdjusting()
  {
    return valueIsAdjusting;
  }

  /**
   * Sets the <code>valueIsAdjusting</code> property.
   *
   * @param isAdjusting the new value
   */
  public void setValueIsAdjusting(boolean isAdjusting)
  {
    valueIsAdjusting = isAdjusting;
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
   * Returns the layout orientation.
   *
   * @return the orientation, one of <code>JList.VERTICAL</code>,
   * <code>JList.VERTICAL_WRAP</code> and <code>JList.HORIZONTAL_WRAP</code>
   *
   * @since 1.4
   */
  public int getLayoutOrientation()
  {
    return layoutOrientation;
  }

  /**
   * Sets the layout orientation.
   *
   * @param orientation the orientation to set, one of <code>JList.VERTICAL</code>,
   * <code>JList.VERTICAL_WRAP</code> and <code>JList.HORIZONTAL_WRAP</code>
   *
   * @since 1.4
   */
  public void setLayoutOrientation(int orientation)
  {
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
    return ((ListUI) ui).getCellBounds(this, index0, index1);
  }

  /**
   * Returns the next list element (beginning from <code>startIndex</code>
   * that starts with <code>prefix</code>. Searching is done in the direction
   * specified by <code>bias</code>.
   *
   * @param prefix the prefix to search for in the cell values
   * @param startIndex the index where to start searching from
   * @param bias the search direction, either {@link Position.Bias.Forward}
   *     or {@link Position.Bias.Backward}
   *
   * @return the index of the found element or -1 if no such element has
   *     been found
   *
   * @throws IllegalArgumentException if prefix is <code>null</code> or
   *     startIndex is not valid
   *
   * @since 1.4
   */
  public int getNextMatch(String prefix, int startIndex, Position.Bias bias)
  {
    if (prefix == null)
      throw new IllegalArgumentException("The argument 'prefix' must not be"
                                         + " null.");
    if (startIndex < 0)
      throw new IllegalArgumentException("The argument 'startIndex' must not"
                                         + " be less than zero.");

    int size = model.getSize();
    if (startIndex > model.getSize())
      throw new IllegalArgumentException("The argument 'startIndex' must not"
                                         + " be greater than the number of"
                                         + " elements in the ListModel.");

    int index = -1;
    if (bias == Position.Bias.Forward)
      {
        for (int i = startIndex; i < size; i++)
          {
            String item = model.getElementAt(i).toString();
            if (item.startsWith(prefix))
              {
                index = i;
                break;
              }
          }
      }
    else
      {
        for (int i = startIndex; i >= 0; i--)
          {
            String item = model.getElementAt(i).toString();
            if (item.startsWith(prefix))
              {
                index = i;
                break;
              }
          }
      }
    return index;
  }
}
