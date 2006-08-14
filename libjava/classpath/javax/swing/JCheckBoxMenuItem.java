/* JCheckBoxMenuItem.java --
 Copyright (C) 2002, 2004, 2006, Free Software Foundation, Inc.

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

import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleRole;

/**
 * A menu item that displays a checkbox. Its behaviour is very similar to
 * {@link JCheckBox}. Just like the <code>JCheckBox</code>, user can check
 * and uncheck this menu item by clicking on it. Also
 * {@link AbstractButton#setSelected} and {@link #setState} can be use used for
 * the same purpose. <code>JCheckBoxMenuItem</code> uses
 * <code>ToggleButtonModel</code> to keep track of its selection.
 * 
 * @author original author unknown
 */
public class JCheckBoxMenuItem
    extends JMenuItem
    implements SwingConstants, Accessible
{
  private static final long serialVersionUID = - 6676402307973384715L;

  /** name for the UI delegate for this menuItem. */
  private static final String uiClassID = "CheckBoxMenuItemUI";

  /** Indicates whether this menu item is checked. */
  private boolean state;

  /**
   * This array contains text of this menu item if this menu item is in checked
   * state and null it is not.
   */
  private Object[] selectedObjects = new Object[1];

  /**
   * Creates a new JCheckBoxMenuItem object.
   */
  public JCheckBoxMenuItem()
  {
    this(null, null);
  }

  /**
   * Creates a new JCheckBoxMenuItem with given icon
   * 
   * @param icon Icon for this menu item
   */
  public JCheckBoxMenuItem(Icon icon)
  {
    this(null, icon);
  }

  /**
   * Creates a new JCheckBoxMenuItem with given label
   * 
   * @param text Label for this menu item
   */
  public JCheckBoxMenuItem(String text)
  {
    this(text, null);
  }

  /**
   * Creates a new JCheckBoxMenuItem using given action
   * 
   * @param action Action for this menu item.
   */
  public JCheckBoxMenuItem(Action action)
  {
    this();
    setAction(action);
  }

  /**
   * Creates a new JCheckBoxMenuItem object with given label and icon
   * 
   * @param text Label for this menu item
   * @param icon Icon for this menu item
   */
  public JCheckBoxMenuItem(String text, Icon icon)
  {
    this(text, icon, false);
  }

  /**
   * Creates a new JCheckBoxMenuItem object using specified label and marked as
   * checked if given 'state' is true.
   * 
   * @param text Label for this menu item
   * @param state <code>true</code> if this item should be in checked state
   *          and <code>false</code> otherwise
   */
  public JCheckBoxMenuItem(String text, boolean state)
  {
    this(text, null, state);
  }

  /**
   * Creates a new JCheckBoxMenuItem object with given label, icon, and marked
   * as checked if given 'state' is true.
   * 
   * @param text Label for this menu item
   * @param icon icon for this menu item
   * @param state <code>true</code> if this item should be in checked state
   *          and false otherwise
   */
  public JCheckBoxMenuItem(String text, Icon icon, boolean state)
  {
    super(text, icon);
    setModel(new JToggleButton.ToggleButtonModel());
    this.state = state;
    if (state == true)
      this.setSelected(true);
    setFocusable(false);
  }

  /**
   * This method returns a name to identify which look and feel class will be
   * the UI delegate for the menuItem.
   * 
   * @return The Look and Feel classID. "JCheckBoxMenuItemUI"
   */
  public String getUIClassID()
  {
    return uiClassID;
  }

  /**
   * Returns checked state for this check box menu item.
   * 
   * @return Returns true if this menu item is in checked state and false
   *         otherwise.
   */
  public boolean getState()
  {
    return state;
  }

  /**
   * Sets state for this check box menu item. If given 'state' is true, then
   * mark menu item as checked, and uncheck this menu item otherwise.
   * 
   * @param state new state for this menu item
   */
  public synchronized void setState(boolean state)
  {
    this.state = state;
  }

  /**
   * This method returns array containing label of this menu item if it is
   * selected and null otherwise.
   * 
   * @return Array containing label of this menu item if this menu item is
   *         selected or null otherwise.
   */
  public Object[] getSelectedObjects()
  {
    if (state == true)
      selectedObjects[0] = this.getText();
    else
      selectedObjects[0] = null;

    return selectedObjects;
  }

  /**
   * This method overrides JComponent.requestFocus with an empty implementation,
   * since JCheckBoxMenuItems should not receive focus in general.
   */
  public void requestFocus()
  {
    // Should do nothing here
  }

  /**
   * Returns a string describing the attributes for the
   * <code>JCheckBoxMenuItem</code> component, for use in debugging. The
   * return value is guaranteed to be non-<code>null</code>, but the format
   * of the string may vary between implementations.
   * 
   * @return A string describing the attributes of the
   *         <code>JCheckBoxMenuItem</code>.
   */
  protected String paramString()
  {
    // calling super seems to be sufficient to match the reference
    // implementation here...
    return super.paramString();
  }

  /**
   * Returns the object that provides accessibility features for this
   * <code>JCheckBoxMenuItem</code> component.
   * 
   * @return The accessible context (an instance of
   *         {@link AccessibleJCheckBoxMenuItem}).
   */
  public AccessibleContext getAccessibleContext()
  {
    if (accessibleContext == null)
      accessibleContext = new AccessibleJCheckBoxMenuItem();

    return accessibleContext;
  }

  /**
   * Provides the accessibility features for the <code>JCheckBoxMenuItem</code>
   * component.
   * 
   * @see JCheckBoxMenuItem#getAccessibleContext()
   */
  protected class AccessibleJCheckBoxMenuItem
      extends AccessibleJMenuItem
  {
    private static final long serialVersionUID = 1079958073579370777L;

    /**
     * Creates a new <code>AccessibleJCheckBoxMenuItem</code> instance.
     */
    protected AccessibleJCheckBoxMenuItem()
    {
      // Nothing to do here.
    }

    /**
     * Returns the accessible role for the <code>JCheckBoxMenuItem</code>
     * component.
     * 
     * @return {@link AccessibleRole#CHECK_BOX}.
     */
    public AccessibleRole getAccessibleRole()
    {
      return AccessibleRole.CHECK_BOX;
    }
  }
}
