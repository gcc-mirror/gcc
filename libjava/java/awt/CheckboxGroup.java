/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.awt;

import java.io.Serializable;

/** This class is used to groups checkbox components.
 * @author Tom Tromey <tromey@redhat.com>
 * @date December 25, 2000
 */
public class CheckboxGroup implements Serializable
{
  // Current set checkbox.
  Checkbox selectedCheckbox;

  /** Create a new instance of CheckboxGroup.  */
  public CheckboxGroup ()
  {
  }

  /** Returns the currently selected checkbox in the group.
   * @deprecated
   */
  public Checkbox getCurrent ()
  {
    return getSelectedCheckbox ();
  }

  /** Returns the currently selected checkbox in the group.  */
  public Checkbox getSelectedCheckbox ()
  {
    return selectedCheckbox;
  }

  /** Set the selected checkbox.
   * @deprecated
   */
  public synchronized void setCurrent (Checkbox checkbox)
  {
    setSelectedCheckbox (checkbox);
  }

  /** Set the selected checkbox.  */
  public synchronized void setSelectedCheckbox (Checkbox checkbox)
  {
    if (checkbox != null && checkbox.group != this)
      return;

    selectedCheckbox.setState (false);
    selectedCheckbox = checkbox;
    if (checkbox != null)
      checkbox.setState (true);
  }

  /** Return String representation of this class and current Checkbox.  */
  public String toString ()
  {
    return "[CheckboxGroup: " + selectedCheckbox + "]";
  }
}
