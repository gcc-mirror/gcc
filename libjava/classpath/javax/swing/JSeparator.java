/* JSeparator.java --
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

import java.beans.PropertyChangeEvent;

import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleRole;
import javax.swing.plaf.SeparatorUI;

/**
 * The JSeparator. It is mostly used to divide/space out
 * components.
 */
public class JSeparator extends JComponent implements SwingConstants,
                                                      Accessible
{
  /**
   * Provides the accessibility features for the <code>JSeparator</code>
   * component.
   */
  protected class AccessibleJSeparator extends AccessibleJComponent
  {
    private static final long serialVersionUID = 916332890553201095L;

    /**
     * Creates a new <code>AccessibleJSeparator</code> instance.
     */
    protected AccessibleJSeparator()
    {
      // Nothing to do here.
    }

    /**
     * Returns the accessible role for the <code>JSeparator</code> component.
     *
     * @return {@link AccessibleRole#SEPARATOR}.
     */
    public AccessibleRole getAccessibleRole()
    {
      return AccessibleRole.SEPARATOR;
    }
  }

  private static final long serialVersionUID = 125301223445282357L;

  /** The orientation of the JSeparator. */
  private transient int orientation = HORIZONTAL;

  /**
   * Creates a new horizontal <code>JSeparator</code> object.
   */
  public JSeparator()
  {
    this(HORIZONTAL);
  }

  /**
   * Creates a new <code>JSeparator</code> object with the given orientation.
   *
   * @param orientation  the orientation (either {@link #HORIZONTAL} or
   *     {@link #VERTICAL}).
   *
   * @throws IllegalArgumentException if <code>orientation</code> is not
   *     one of the specified values.
   */
  public JSeparator(int orientation)
  {
    if (orientation != HORIZONTAL && orientation != VERTICAL)
      throw new IllegalArgumentException(orientation
                                         + " is not a valid orientation.");
    this.orientation = orientation;
    updateUI();
  }

  /**
   * Returns the UI delegate being used with the <code>JSeparator</code>.
   *
   * @return The JSeparator's UI delegate.
   */
  public SeparatorUI getUI()
  {
    return (SeparatorUI) ui;
  }

  /**
   * Sets the separator's UI delegate.
   *
   * @param ui  the UI delegate.
   */
  public void setUI(SeparatorUI ui)
  {
    super.setUI(ui);
  }

  /**
   * Sets this separator's UI delegate to the default (obtained from the
   * {@link UIManager}) for the current look and feel.
   */
  public void updateUI()
  {
    setUI((SeparatorUI) UIManager.getUI(this));
  }

  /**
   * Returns the suffix (<code>"SeparatorUI"</code> in this case) used to
   * determine the class name for a UI delegate that can provide the look and
   * feel for a <code>JSeparator</code>.
   *
   * @return <code>"SeparatorUI"</code>.
   */
  public String getUIClassID()
  {
    return "SeparatorUI";
  }

  /**
   * Returns the orientation of the <code>JSeparator</code>.
   *
   * @return The orientation (one of {@link #HORIZONTAL} and {@link #VERTICAL}).
   *
   * @see #setOrientation(int)
   */
  public int getOrientation()
  {
    return orientation;
  }

  /**
   * Sets the orientation for the <code>JSeparator</code> and sends a
   * {@link PropertyChangeEvent} (with the property name
   * <code>orientation</code>) to all registered listeners.
   *
   * @param orientation  the orientation (either {@link #HORIZONTAL} or
   *     {@link #VERTICAL}).
   *
   * @throws IllegalArgumentException if <code>orientation</code> is not
   *     one of the specified values.
   *
   * @see #getOrientation()
   */
  public void setOrientation(int orientation)
  {
    if (orientation != HORIZONTAL && orientation != VERTICAL)
      throw new IllegalArgumentException(orientation
                                         + " is not a valid orientation.");
    int old = this.orientation;
    this.orientation = orientation;
    firePropertyChange("orientation", old, orientation);
  }

  /**
   * Returns an implementation-dependent string describing the attributes of
   * this <code>JSeparator</code>.
   *
   * @return A string describing the attributes of this <code>JSeparator</code>
   *         (never <code>null</code>).
   */
  protected String paramString()
  {
    String superParamStr = super.paramString();
    if (orientation == HORIZONTAL)
      return superParamStr + ",orientation=HORIZONTAL";
    else
      return superParamStr + ",orientation=VERTICAL";
  }

  /**
   * Returns the object that provides accessibility features for this
   * <code>JSeparator</code> component.
   *
   * @return The accessible context (an instance of
   *     {@link AccessibleJSeparator}).
   */
  public AccessibleContext getAccessibleContext()
  {
    if (accessibleContext == null)
      accessibleContext = new AccessibleJSeparator();

    return accessibleContext;
  }
}
