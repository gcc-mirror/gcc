/* JRadioButtonMenuItem.java --
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
 * This class represents JRadioButtonMenuItem. Its behaviour is very similar
 * to JRadioButton. Just like JRadioButton, user can check and uncheck this
 * menu item by clicking on it. JRadioButtonMenuItem uses ToggleButtonModel
 * to keep track of its selection. If the JRadioButtonMenuItem is included in
 * the button group, then only one JRadioButtonMenuItem can be selected at
 * one time.
 */
public class JRadioButtonMenuItem extends JMenuItem implements Accessible
{
  private static final long serialVersionUID = 8482658191548521743L;

  /** name for the UI delegate for this radio button menu item. */
  private static final String uiClassID = "RadioButtonMenuItemUI";

  /**
   * Creates a new JRadioButtonMenuItem object.
   */
  public JRadioButtonMenuItem()
  {
    this(null, null);
  }

  /**
   * Creates a new JRadioButtonMenuItem with specified icon
   *
   * @param icon Icon to be used for this menu item
   */
  public JRadioButtonMenuItem(Icon icon)
  {
    this(null, icon);
  }

  /**
   * Creates a new JRadioButtonMenuItem with specified label
   *
   * @param text Label for this menu item
   */
  public JRadioButtonMenuItem(String text)
  {
    this(text, null);
  }

  /**
   * Creates a new JRadioButtonMenuItem using specified action
   *
   * @param action Action for this menu item
   */
  public JRadioButtonMenuItem(Action action)
  {
    this();
    setAction(action);
  }

  /**
   * Creates a new JRadioButtonMenuItem with specified label and icon
   *
   * @param text Label for this menu item
   * @param icon Icon for this menu item
   */
  public JRadioButtonMenuItem(String text, Icon icon)
  {
    this(text, icon, false);
  }

  /**
   * Creates a new JRadioButtonMenuItem with specified label
   * and marked selected if 'selected' is true.
   *
   * @param text Text for this menu item
   * @param selected Selected state of this menu item
   */
  public JRadioButtonMenuItem(String text, boolean selected)
  {
    this(text, null, selected);
  }

  /**
   * Creates a new JRadioButtonMenuItem with specified icon
   * and given selected state
   *
   * @param icon Icon for this menu item
   * @param selected Selected state for this menu item
   */
  public JRadioButtonMenuItem(Icon icon, boolean selected)
  {
    this(null, icon, selected);
  }

  /**
   * Creates a new JRadioButtonMenuItem with specified label,
   * icon and selected state.
   *
   * @param text Label for this menu item
   * @param icon Icon to be use for this menu item
   * @param selected selected state of this menu item
   */
  public JRadioButtonMenuItem(String text, Icon icon, boolean selected)
  {
    super(text, icon);
    setModel(new JToggleButton.ToggleButtonModel());
    model.setSelected(selected);
    setFocusable(false);
  }

  /**
   * This method returns a name to identify which look and feel class will be
   * the UI delegate for the menuItem.
   *
   * @return The Look and Feel classID. "JRadioButtonMenuItemUI"
   */
  public String getUIClassID()
  {
    return uiClassID;
  }

  /**
   * This method overrides JComponent.requestFocus with an empty
   * implementation, since JRadioButtonMenuItems should not
   * receve focus in general.
   */
  public void requestFocus()
  {
    //  Should do nothing here
  }

  /**
   * Returns a string describing the attributes for the 
   * <code>JRadioButtonMenuItem</code> component, for use in debugging.  The 
   * return value is guaranteed to be non-<code>null</code>, but the format of 
   * the string may vary between implementations.
   *
   * @return A string describing the attributes of the 
   *     <code>JRadioButtonMenuItem</code>.
   */
  protected String paramString()
  {
    // calling super seems to be sufficient here...
    return super.paramString();
  }

  /**
   * Returns the object that provides accessibility features for this
   * <code>JRadioButtonMenuItem</code> component.
   *
   * @return The accessible context (an instance of 
   *     {@link AccessibleJRadioButtonMenuItem}).
   */
  public AccessibleContext getAccessibleContext()
  {
    if (accessibleContext == null)
      accessibleContext = new AccessibleJRadioButtonMenuItem();

    return accessibleContext;
  }

  /**
   * Provides the accessibility features for the 
   * <code>JRadioButtonMenuItem</code> component.
   * 
   * @see JRadioButtonMenuItem#getAccessibleContext()
   */
  protected class AccessibleJRadioButtonMenuItem extends AccessibleJMenuItem
  {
    private static final long serialVersionUID = 4381471510145292179L;

    /**
     * Creates a new <code>AccessibleJRadioButtonMenuItem</code> instance.
     */
    protected AccessibleJRadioButtonMenuItem()
    {
      // Nothing to do here.
    }

    /**
     * Returns the accessible role for the <code>JRadioButtonMenuItem</code> 
     * component.
     *
     * @return {@link AccessibleRole#RADIO_BUTTON}.
     */
    public AccessibleRole getAccessibleRole()
    {
      return AccessibleRole.RADIO_BUTTON;
    }
  }
}
