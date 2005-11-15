/* JButton.java --
   Copyright (C) 2002, 2004, 2005 Free Software Foundation, Inc.

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
  boolean def;
  boolean is_def;

  public JButton()
  {
    this(null, null);
  }

  public JButton(Action a)
  {
    this();
    setAction(a);
  }

  public JButton(Icon icon)
  {
    this(null, icon);
  }

  public JButton(String text)
  {
    this(text, null);
  }

  public JButton(String text, Icon icon)
  {
    super();
    init(text, icon);
    setModel(new DefaultButtonModel());
  }

  public Object[] getSelectedObjects()
  {
    return null;
  }

  protected void configurePropertiesFromAction(Action a)
  {
    // Factory method which sets the AbstractButton's properties according to
    // values from the Action instance. 
    super.configurePropertiesFromAction(a);
  }

  public AccessibleContext getAccessibleContext()
  {
    if (accessibleContext == null)
      accessibleContext = new AccessibleJButton();
    return accessibleContext;
  }

  public String getUIClassID()
  {
    // Returns a string that specifies the name of the L&F class that renders
    // this component.  
    return "ButtonUI";
  }

  public boolean isDefaultButton()
  {
    // Returns whether or not this button is the default button on the
    // RootPane.
    return is_def;
  }

  public boolean isDefaultCapable()
  {
    // Returns whether or not this button is capable of being the default
    // button on the RootPane. 
    return def;
  }

  protected String paramString()
  {
    String superParam = super.paramString();

    // 41 is the maximum number of chars which may be needed.
    StringBuffer sb = new StringBuffer(41);
    sb.append(",defaultButton=").append(is_def);
    sb.append(",defaultCapable=").append(def);

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

  public void setDefaultCapable(boolean defaultCapable)
  {
    def = defaultCapable;
  }

  public void updateUI()
  {
    setUI((ButtonUI) UIManager.getUI(this));
  }
}
