/* Copyright (C) 2000  Free Software Foundation

   This file is part of libjava.

This software is copyrighted work licensed under the terms of the
Libjava License.  Please consult the file "LIBJAVA_LICENSE" for
details.  */

package java.awt;
import java.awt.event.*;

/** This interface is implemented by components that support the
 * select of items.  For instance, Checkbox implements this
 * interface.
 * @author Tom Tromey <tromey@cygnus.com>
 * @date April 8, 2000
 */
public interface ItemSelectable
{
  /** This method adds a listener to receive item events fired by the
   * component.
   * @param l The item listener to add.
   */
  public void addItemListener (ItemListener l);

  /** This method returns the items in this component which are
   * currently selected.
   * @returns A non-null array containing the items.
   */
  public Object[] getSelectedObjects ();

  /** This method removes an item listener.
   * @param l The item listener to remove.
   */
  public void removeItemListener (ItemListener l);
}
