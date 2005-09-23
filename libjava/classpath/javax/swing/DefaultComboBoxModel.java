/* DefaultComboBoxModel.java --
   Copyright (C) 2002, 2004 Free Software Foundation, Inc.

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


/**
 * The default implementation of {@link MutableComboBoxModel}.
 * This model keeps track
 * of elements contained in the JComboBox as well as the current combo box
 * selection. Whenever selection in the JComboBox changes, the ComboBoxModel
 * will fire ListDataEvents to ComboBox's ListDataListeners.
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
   * List containing items in the combo box
   */
  private Vector list;

  /**
   * Currently selected item in the combo box list
   */
  private Object selectedItem = null;

  /**
   * Constructor DefaultComboBoxModel. Create empty JComboBox.
   */
  public DefaultComboBoxModel()
  {
    list = new Vector();
  }

  /**
   * Constructs new DefaultComboBoxModel object and initializes its item list
   * to values in the given array.
   *
   * @param items array containing items of the combo box.
   */
  public DefaultComboBoxModel(Object[] items)
  {
    list = new Vector(Arrays.asList(items));
  }

  /**
   * Consturcts new DefaultComboBoxModel object and initializes its item list
   * to values in the given vector.
   *
   * @param vector Vector containing items for this combo box.
   */
  public DefaultComboBoxModel(Vector vector)
  {
    this.list = vector;
  }

  /**
   * This method adds element to the combo box list. It fires ListDataEvent
   * indicating that component was added to the combo box  to all of the
   * JComboBox's registered ListDataListeners.
   *
   * @param object item to add to the combo box list
   */
  public void addElement(Object object)
  {
    list.add(object);
    fireIntervalAdded(this, list.size() - 1, list.size());
  }

  /**
   * This method removes element at the specified index from the combo box
   * list. It fires ListDataEvent indicating that component was removed from
   * the combo box list to all of the JComboBox's registered
   * ListDataListeners.
   *
   * @param index index specifying location of the element to remove in the
   *        combo box list.
   */
  public void removeElementAt(int index)
  {
    list.remove(index);
    fireIntervalRemoved(this, index, index);
  }

  /**
   * This method inserts given object to the combo box list at the specified
   * index. It fires ListDataEvent indicating that component was inserted to
   * the combo box list to all of the JComboBox's registered
   * ListDataListeners.
   *
   * @param object element to insert
   * @param index index specifing position in the list where given element
   *        should be inserted.
   */
  public void insertElementAt(Object object, int index)
  {
    list.insertElementAt(object, index);
    fireIntervalAdded(this, index, index);
  }

  /**
   * Removes given object from the combo box list. It fires ListDataEvent
   * indicating that component was removed from the combo box list to all of
   * the JComboBox's registered ListDataListeners.
   *
   * @param object Element that will be removed from the combo box list
   */
  public void removeElement(Object object)
  {
    int index = getIndexOf(object);
    if (index != -1)
      removeElementAt(index);
  }

  /**
   * Removes all the items from the JComboBox's item list. It fires
   * ListDataEvent indicating that all the elements were removed from the
   * combo box list to all of the JComboBox's registered ListDataListeners.
   */
  public void removeAllElements()
  {
    list.clear();
    int listSize = getSize();
    fireIntervalAdded(this, 0, listSize);
  }

  /**
   * Returns number of items in the combo box list
   *
   * @return number of items in the combo box list
   */
  public int getSize()
  {
    return list.size();
  }

  /**
   * Selects given object in the combo box list. This method fires
   * ListDataEvent to all registered ListDataListeners of the JComboBox. The
   * start and end index of the event is set to -1 to indicate combo box's
   * selection has changed, and not its contents.
   * 
   * <p>If the given object is not contained in the combo box list then nothing
   * happens.</p>
   *
   * @param object item to select in the JComboBox
   */
  public void setSelectedItem(Object object)
  {
    
    // Updates the selected item only if the given object
    // is null or in the list (this is how the JDK behaves).
    if(object == null || list.contains(object)) {
	selectedItem = object;
	fireContentsChanged(this, -1, -1);
    }
  	
  }

  /**
   * Returns currently selected item in the combo box list
   *
   * @return currently selected item in the combo box list
   */
  public Object getSelectedItem()
  {
    return selectedItem;
  }

  /**
   * Returns element in the combo box list located at the given index
   *
   * @param index specifying location of the element in the list
   *
   * @return return element in the combo box list located at the given index
   */
  public Object getElementAt(int index)
  {
    return list.elementAt(index);
  }

  /**
   * Returns index of the specified object in the combo box list.
   *
   * @param object element to look for in the combo box list .
   *
   * @return Index specifying position of the specified element in combo box
   *         list.
   */
  public int getIndexOf(Object object)
  {
    return list.indexOf(object);
  }
}
