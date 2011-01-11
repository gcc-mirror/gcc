/* DefaultComboBoxModel.java --
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

package javax.swing;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Vector;

import javax.swing.event.ListDataEvent;


/**
 * A model that stores a list of elements and a selected item (which may be
 * <code>null</code>).  Changes to the model are signalled to listeners using
 * {@link ListDataEvent}.  This model is designed for use by the
 * {@link JComboBox} component.
 *
 * @author Andrew Selkirk
 * @author Olga Rodimina
 * @author Robert Schuster
 */
public class DefaultComboBoxModel extends AbstractListModel
  implements MutableComboBoxModel, Serializable
{
  private static final long serialVersionUID = 6698657703676921904L;

  /**
   * Storage for the elements in the model's list.
   */
  private Vector list;

  /**
   * The selected item (<code>null</code> indicates no selection).
   */
  private Object selectedItem = null;

  /**
   * Creates a new model, initially empty.
   */
  public DefaultComboBoxModel()
  {
    list = new Vector();
  }

  /**
   * Creates a new model and initializes its item list to the values in the
   * given array.  The selected item is set to the first item in the array, or
   * <code>null</code> if the array length is zero.
   *
   * @param items  an array containing items for the model (<code>null</code>
   *               not permitted).
   *
   * @throws NullPointerException if <code>items</code> is <code>null</code>.
   */
  public DefaultComboBoxModel(Object[] items)
  {
    list = new Vector(Arrays.asList(items));
    if (list.size() > 0)
      selectedItem = list.get(0);
  }

  /**
   * Creates a new model and initializes its item list to the values in the
   * given vector.  The selected item is set to the first item in the vector,
   * or <code>null</code> if the vector length is zero.
   *
   * @param vector  a vector containing items for the model (<code>null</code>
   *                not permitted).
   *
   * @throws NullPointerException if <code>vector</code> is <code>null</code>.
   */
  public DefaultComboBoxModel(Vector<?> vector)
  {
    this.list = vector;
    if (getSize() > 0)
      selectedItem = vector.get(0);
  }

  /**
   * Adds an element to the model's item list and sends a {@link ListDataEvent}
   * to all registered listeners.  If the new element is the first item added
   * to the list, and the selected item is <code>null</code>, the new element
   * is set as the selected item.
   *
   * @param object item to add to the model's item list.
   */
  public void addElement(Object object)
  {
    list.addElement(object);
    int index = list.size() - 1;
    fireIntervalAdded(this, index, index);
    if (list.size() == 1 && selectedItem == null)
      setSelectedItem(object);
  }

  /**
   * Removes the element at the specified index from the model's item list
   * and sends a {@link ListDataEvent} to all registered listeners.  If the
   * element removed was the selected item, then the preceding element becomes
   * the new selected item (or the next element, if there is no preceding
   * element).
   *
   * @param index  the index of the item to remove.
   *
   * @throws ArrayIndexOutOfBoundsException if <code>index</code> is out of
   *         bounds.
   */
  public void removeElementAt(int index)
  {
    int selected = getIndexOf(selectedItem);
    if (selected == index) // choose a new selected item
      {
        if (selected > 0)
          setSelectedItem(getElementAt(selected - 1));
        else
          setSelectedItem(getElementAt(selected + 1));
      }
    list.removeElementAt(index);
    fireIntervalRemoved(this, index, index);
  }

  /**
   * Adds an element at the specified index in the model's item list
   * and sends a {@link ListDataEvent} to all registered listeners.
   *
   * @param object element to insert
   * @param index index specifing position in the list where given element
   *        should be inserted.
   *
   * @throws ArrayIndexOutOfBoundsException if <code>index</code> is out of
   *         bounds.
   *
   * @see #addElement(Object)
   */
  public void insertElementAt(Object object, int index)
  {
    list.insertElementAt(object, index);
    fireIntervalAdded(this, index, index);
  }

  /**
   * Removes an element from the model's item list and sends a
   * {@link ListDataEvent} to all registered listeners.  If the item to be
   * removed is the current selected item, a new selected item will be set.
   * If the element is not found in the model's item list, this method does
   * nothing.
   *
   * @param object  the element to remove.
   */
  public void removeElement(Object object)
  {
    int index = getIndexOf(object);
    if (index != -1)
      removeElementAt(index);
  }

  /**
   * Removes all the items from the model's item list, resets and selected item
   * to <code>null</code>, and sends a {@link ListDataEvent} to all registered
   * listeners.
   */
  public void removeAllElements()
  {
    selectedItem = null;
    int size = getSize();
    if (size > 0)
      {
        list.clear();
        fireIntervalRemoved(this, 0, size - 1);
      }
  }

  /**
   * Returns the number of items in the model's item list.
   *
   * @return The number of items in the model's item list.
   */
  public int getSize()
  {
    return list.size();
  }

  /**
   * Sets the selected item for the model and sends a {@link ListDataEvent} to
   * all registered listeners.  The start and end index of the event is set to
   * -1 to indicate the model's selection has changed, and not its contents.
   *
   * @param object  the new selected item (<code>null</code> permitted).
   */
  public void setSelectedItem(Object object)
  {
    // No item is selected and object is null, so no change required.
    if (selectedItem == null && object == null)
      return;

    // object is already selected so no change required.
    if (selectedItem != null && selectedItem.equals(object))
      return;

    // Simply return if object is not in the list.
    if (object != null && getIndexOf(object) == -1)
      return;

    // Here we know that object is either an item in the list or null.

    // Handle the three change cases: selectedItem is null, object is
    // non-null; selectedItem is non-null, object is null;
    // selectedItem is non-null, object is non-null and they're not
    // equal.
    selectedItem = object;
    fireContentsChanged(this, -1, -1);
  }

  /**
   * Returns the selected item.
   *
   * @return The selected item (possibly <code>null</code>).
   */
  public Object getSelectedItem()
  {
    return selectedItem;
  }

  /**
   * Returns the element at the specified index in the model's item list.
   *
   * @param index  the element index.
   *
   * @return The element at the specified index in the model's item list, or
   *         <code>null</code> if the <code>index</code> is outside the bounds
   *         of the list.
   */
  public Object getElementAt(int index)
  {
    if (index < 0 || index >= list.size())
      return null;
    return list.elementAt(index);
  }

  /**
   * Returns the index of the specified element in the model's item list.
   *
   * @param object  the element.
   *
   * @return The index of the specified element in the model's item list.
   */
  public int getIndexOf(Object object)
  {
    return list.indexOf(object);
  }
}
