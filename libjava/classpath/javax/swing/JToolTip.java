/* JToolTip.java --
   Copyright (C) 2002, 2004  Free Software Foundation, Inc.

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

import java.awt.AWTEvent;
import java.beans.PropertyChangeEvent;

import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleRole;
import javax.swing.plaf.ToolTipUI;

/**
 * This class is used to display ToolTips. ToolTips are small floating windows
 * that display text when the mouse comes to rest over a Component. ToolTips
 * are set for JComponents using JComponent.setToolTipText(String).
 */
public class JToolTip extends JComponent implements Accessible
{

  private static final long serialVersionUID = -1138929898906751643L;

  /**
   * Provides the accessibility features for the <code>JToolTip</code>
   * component.
   */
  protected class AccessibleJToolTip extends AccessibleJComponent
  {
    private static final long serialVersionUID = -6222548177795408476L;

    /**
     * Creates a new AccessibleJToolTip object.
     */
    protected AccessibleJToolTip()
    {
      // Nothing to do here.
    }

    /**
     * Returns a description for the accessible component.
     *
     * @return A description for the accessible component.
     */
    public String getAccessibleDescription()
    {
      String result = super.getAccessibleDescription();
      if (result == null)
        result = text;
      return result;
    }

    /**
     * Returns the accessible role for the <code>JToolTip</code> component.
     *
     * @return {@link AccessibleRole#TOOL_TIP}.
     */
    public AccessibleRole getAccessibleRole()
    {
      return AccessibleRole.TOOL_TIP;
    }
  }

  /** The text to display in the JToolTip. */
  String text;

  /** The component that the tool tip is associated with. */
  JComponent component;

  /**
   * Creates a new <code>JToolTip</code> instance.
   */
  public JToolTip()
  {
    disableEvents(AWTEvent.MOUSE_EVENT_MASK);
    updateUI();
  }

  /**
   * Returns the text displayed by the tool tip.
   *
   * @return The text (possibly <code>null</code>).
   * 
   * @see #setTipText(String)
   */
  public String getTipText()
  {
    return text;
  }

  /**
   * Returns the object that provides accessibility features for this
   * <code>JToolTip</code> component.
   *
   * @return The accessible context (an instance of {@link AccessibleJToolTip}).
   */
  public AccessibleContext getAccessibleContext()
  {
    if (accessibleContext == null)
      accessibleContext = new AccessibleJToolTip();
    return accessibleContext;
  }

  /**
   * Returns the component that the tool tip is associated with.
   *
   * @return The component (possibly <code>null</code>).
   * 
   * @see #setComponent(JComponent)
   */
  public JComponent getComponent()
  {
    return component;
  }

  /**
   * Returns the current UI delegate for this component.
   *
   * @return The UI delegate.
   */
  public ToolTipUI getUI()
  {
    return (ToolTipUI) ui;
  }

  /**
   * Returns the string suffix used to identify the UI class, in this case
   * <code>"ToolTipUI"</code>.
   *
   * @return <code>"ToolTipUI"</code>.
   */
  public String getUIClassID()
  {
    return "ToolTipUI";
  }

  /**
   * Returns a string describing the attributes for the <code>JToolTip</code>
   * component, for use in debugging.  The return value is guaranteed to be 
   * non-<code>null</code>, but the format of the string may vary between
   * implementations.
   *
   * @return A string describing the attributes of the <code>JToolTip</code>.
   */
  protected String paramString()
  {
    StringBuffer sb = new StringBuffer(super.paramString());
    sb.append(",tiptext=");
    if (text != null)
      sb.append(text);
    return sb.toString();
  }

  /**
   * Sets the component that the tool tip is associated with and sends a 
   * {@link PropertyChangeEvent} (with the property name 'component') to all 
   * registered listeners.
   *
   * @param c  the component (<code>null</code> permitted).
   * 
   * @see #getComponent()
   */
  public void setComponent(JComponent c)
  {
    JComponent oldValue = component;
    component = c;
    firePropertyChange("component", oldValue, c);
  }

  /**
   * Sets the text to be displayed by the tool tip and sends a 
   * {@link PropertyChangeEvent} (with the property name 'tiptext') to all 
   * registered listeners.
   *
   * @param tipText the text (<code>null</code> permitted).
   * 
   * @see #getTipText()
   */
  public void setTipText(String tipText)
  {
    String oldValue = text;
    text = tipText;
    firePropertyChange("tiptext", oldValue, tipText);
  }

  /**
   * This method resets the UI used to the Look and Feel default.
   */
  public void updateUI()
  {
    setUI((ToolTipUI) UIManager.getUI(this));
  }

  /**
   * Returns <code>true</code> if the component is guaranteed to be painted
   * on top of others. This returns false by default and is overridden by
   * components like JMenuItem, JPopupMenu and JToolTip to return true for
   * added efficiency.
   *
   * @return <code>true</code> if the component is guaranteed to be painted
   *         on top of others
   */
  boolean onTop()
  {
    return true;
  }
}
