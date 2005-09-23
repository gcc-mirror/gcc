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
   * AccessibleJSeparator
   */
  protected class AccessibleJSeparator extends AccessibleJComponent
  {
    private static final long serialVersionUID = 916332890553201095L;
  
    /**
     * Constructor AccessibleJSeparator
     */
    protected AccessibleJSeparator()
    {
    }

    /**
     * getAccessibleRole
     *
     * @return AccessibleRole
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
   * Creates a new horizontal JSeparator object.
   */
  public JSeparator()
  {
    this(HORIZONTAL);
  }

  /**
   * Creates a new JSeparator object with the given orientation.
   *
   * @param orientation The orientation of the JSeparator.
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
   * This method returns the UI delegate being
   * used with the JSeparator.
   *
   * @return SeparatorUI The JSeparator's UI delegate.
   */
  public SeparatorUI getUI()
  {
    return (SeparatorUI) ui;
  }

  /**
   * This method sets the UI delegate to use
   * with the JSeparator.
   *
   * @param ui The UI delegate to use.
   */
  public void setUI(SeparatorUI ui)
  {
    super.setUI(ui);
  }

  /**
   * This method resets the UI delegate to the 
   * default for the current look and feel.
   */
  public void updateUI()
  {
    setUI((SeparatorUI) UIManager.getUI(this));
    invalidate();
  }

  /**
   * This method returns the identifier string
   * that is used to determine the UI delegate
   * from the current look and feel.
   *
   * @return String The identifier string for the UI.
   */
  public String getUIClassID()
  {
    return "SeparatorUI";
  }

  /**
   * This method returns the JSeparator's orientation.
   *
   * @return int The JSeparator's orientation.
   */
  public int getOrientation()
  {
    return orientation;
  }

  /**
   * This method changes the JSeparator's orientation.
   *
   * @param orientation The JSeparator's orientation.
   */
  public void setOrientation(int orientation)
  {
    if (orientation != HORIZONTAL && orientation != VERTICAL)
      throw new IllegalArgumentException(orientation
                                         + " is not a valid orientation.");
    this.orientation = orientation;
  }

  /**
   * This method returns a string desribing the JSeparator.
   * Normally only used in debugging.
   *
   * @return String A string describing the JSeparator.
   */
  protected String paramString()
  {
    return "JSeparator";
  }

  /**
   * getAccessibleContext
   *
   * @return AccessibleContext
   */
  public AccessibleContext getAccessibleContext()
  {
    if (accessibleContext == null)
      accessibleContext = new AccessibleJSeparator();
    
    return accessibleContext;
  }
}
