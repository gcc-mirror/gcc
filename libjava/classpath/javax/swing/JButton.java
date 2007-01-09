/* JButton.java --
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

import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleRole;
import javax.swing.plaf.ButtonUI;


/**
 * A general purpose push button. <code>JButton</code>s can display a label,
 * an {@link Icon} or both.
 *
 * @author Ronald Veldema (rveldema@cs.vu.nl)
 */
public class JButton extends AbstractButton
  implements Accessible
{

  /**
   * Accessibility support for JButtons.
   */
  protected class AccessibleJButton
    extends AbstractButton.AccessibleAbstractButton
  {
    /**
     * Returns the accessible role that this component represents.
     * This is {@link AccessibleRole#PUSH_BUTTON} for <code>JButton</code>s.
     *
     * @return the accessible role that this component represents
     */
    public AccessibleRole getAccessibleRole()
    {
      return AccessibleRole.PUSH_BUTTON;
    }
  }

  private static final long serialVersionUID = -1907255238954382202L;

  /**
   * Indicates if this button is capable to become the default button.
   */
  private boolean defaultCapable;

  /**
   * Creates a new button with an empty string for the button text and no
   * icon.
   */
  public JButton()
  {
    this(null, null);
  }

  /**
   * Creates a new button from the specified action.
   * 
   * @param a  the action (<code>null</code> permitted).
   * 
   * @see AbstractButton#setAction(Action)
   */
  public JButton(Action a)
  {
    this();
    setAction(a);
  }

  /**
   * Creates a new button with the specified icon (and an empty string for
   * the button text).
   * 
   * @param icon  the icon (<code>null</code> permitted).
   */
  public JButton(Icon icon)
  {
    this(null, icon);
  }

  /**
   * Creates a new button with the specified text and no icon.
   * 
   * @param text  the button text (<code>null</code> permitted, will be
   *     substituted by an empty string).
   */
  public JButton(String text)
  {
    this(text, null);
  }

  /**
   * Creates a new button with the specified text and icon.
   * 
   * @param text  the button text (<code>null</code> permitted, will be
   *     substituted by an empty string).
   * @param icon  the icon (<code>null</code> permitted).
   */
  public JButton(String text, Icon icon)
  {
    super();
    setModel(new DefaultButtonModel());
    init(text, icon);
    defaultCapable = true;
  }

  protected void configurePropertiesFromAction(Action a)
  { 
    super.configurePropertiesFromAction(a);
  }

  /**
   * Returns the object that provides accessibility features for this
   * <code>JButton</code> component.
   *
   * @return The accessible context (an instance of {@link AccessibleJButton}).
   */
  public AccessibleContext getAccessibleContext()
  {
    if (accessibleContext == null)
      accessibleContext = new AccessibleJButton();
    return accessibleContext;
  }

  /**
   * Returns the suffix (<code>"ButtonUI"</code> in this case) used to 
   * determine the class name for a UI delegate that can provide the look and 
   * feel for a <code>JButton</code>.
   *
   * @return <code>"ButtonUI"</code>.
   */
  public String getUIClassID()
  {
    // Returns a string that specifies the name of the L&F class that renders
    // this component.  
    return "ButtonUI";
  }

  /**
   * Returns <code>true</code> if this button is the default button in
   * its <code>JRootPane</code>. The default button gets automatically
   * activated when the user presses <code>ENTER</code> (or whatever
   * key this is bound to in the current Look and Feel).
   *
   * @return <code>true</code> if this button is the default button in
   *         its <code>JRootPane</code>
   *
   * @see #isDefaultCapable()
   * @see #setDefaultCapable(boolean)
   * @see JRootPane#getDefaultButton()
   * @see JRootPane#setDefaultButton(JButton)
   */
  public boolean isDefaultButton()
  {
    // The default button is managed by the JRootPane, so the safest way
    // to determine this property is to ask the root pane of this button,
    // if it exists.
    JRootPane rp = SwingUtilities.getRootPane(this);
    boolean isDefault = false;
    if (rp != null)
      isDefault = rp.getDefaultButton() == this;
    return isDefault;
  }

  /**
   * Returns <code>true</code> if this button can act as the default button.
   * This is <code>true</code> by default.
   *
   * @return <code>true</code> if this button can act as the default button
   *
   * @see #setDefaultCapable(boolean)
   * @see #isDefaultButton()
   * @see JRootPane#getDefaultButton()
   * @see JRootPane#setDefaultButton(JButton)
   */
  public boolean isDefaultCapable()
  {
    // Returns whether or not this button is capable of being the default
    // button on the RootPane. 
    return defaultCapable;
  }

  /**
   * Returns an implementation-dependent string describing the attributes of
   * this <code>JButton</code>.
   *
   * @return A string describing the attributes of this <code>JButton</code>
   *         (never <code>null</code>).
   */
  protected String paramString()
  {
    String superParam = super.paramString();

    // 41 is the maximum number of chars which may be needed.
    StringBuffer sb = new StringBuffer(41);
    sb.append(",defaultButton=").append(isDefaultButton());
    sb.append(",defaultCapable=").append(defaultCapable);

    return superParam + sb.toString();
  }

  /**
   * Overrides JComponent.removeNotify to check if this button is currently
   * set as the default button on the RootPane, and if so, sets the RootPane's
   * default button to null to ensure the RootPane doesn't hold onto an invalid
   * button reference.
   */
  public void removeNotify()
  {
    JRootPane root = SwingUtilities.getRootPane(this);
    if (root != null && root.getDefaultButton() == this)
      root.setDefaultButton(null);
    super.removeNotify();
  }

  /**
   * Sets the <code>defaultCapable</code> property which indicates if
   * this button may become the default button in its <code>JRootPane</code>.
   *
   * @param defaultCapable <code>true</code> if this button can become the
   *        default button in its JRootPane, <code>false</code> otherwise
   *
   * @see #setDefaultCapable(boolean)
   * @see #isDefaultButton()
   * @see JRootPane#getDefaultButton()
   * @see JRootPane#setDefaultButton(JButton)
   */
  public void setDefaultCapable(boolean defaultCapable)
  {
    this.defaultCapable = defaultCapable;
  }

  /**
   * Sets this button's UI delegate to the default (obtained from the
   * {@link UIManager}) for the current look and feel.
   */
  public void updateUI()
  {
    setUI((ButtonUI) UIManager.getUI(this));
  }
}
