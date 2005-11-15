/* JCheckBox.java -- 
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

/**
 * A small box that displays a check or not, depending on it's
 * <code>selected</code> state. This works very similar to
 * {@link JToggleButton} and {@link JRadioButton}, but in UI design it
 * has different semantics. <code>JCheckBox</code>es are usually
 * used in multiple-choice scenarios, where a user can select 0..n
 * of n different options. (This is in contrast to the general RadioButton
 * semantics where the user can select exactly one of n options).
 *
 * Note however that this semantics is in no way enforced by the
 * <code>JCheckBox</code>.
 *
 * @author Ronald Veldema (rveldema@cs.vu.nl)
 */
public class JCheckBox extends JToggleButton implements Accessible
{

  /**
   * Provides accessibility support for <code>JCheckBox</code>.
   */
  protected class AccessibleJCheckBox extends AccessibleJToggleButton
  {
    /**
     * Creates a new instance of <code>AccessibleJCheckBox</code>.
     */
    public AccessibleJCheckBox()
    {
      // Nothing to do here.
    }

    /**
     * Returns the accessble role of <code>JCheckBox</code>,
     * {@link AccessibleRole#CHECK_BOX}. 
     */
    public AccessibleRole getAccessibleRole()
    {
      return AccessibleRole.CHECK_BOX;
    }
  }

  private static final long serialVersionUID = -5246739313864538930L;

  public static final String BORDER_PAINTED_FLAT_CHANGED_PROPERTY =
    "borderPaintedFlat";
  
  private boolean borderPaintedFlat;

  private void init()
  {
    borderPainted = false;
    contentAreaFilled = false;
  }
  
  public JCheckBox()
  {
    this(null, null, false);
  }

  public JCheckBox(Action action)
  {
    super(action);
  }

  public JCheckBox(Icon icon)
  { 
    this(null, icon, false);
  }    
  
  public JCheckBox(Icon icon, boolean selected)
  { 
    this(null, icon, selected);
  }    
  
  public JCheckBox(String text)
  {
    this(text, null, false);
  }
      
  public JCheckBox(String text, boolean selected)
  {
    this(text, null, selected);
  }
      
  public JCheckBox(String text, Icon icon)
  {
    this(text, icon, false);
  }

  public JCheckBox(String text, Icon icon, boolean selected)
  {
    super(text, icon, selected);
    setHorizontalAlignment(LEADING);
    setBorderPainted(false);
  }

  /**
   * Returns a string that specifies the name of the Look and Feel class
   * that renders this component.
   */
  public String getUIClassID()
  {
    return "CheckBoxUI";
  }
  
  protected  String paramString()
  {
    return super.paramString() + ",borderPaintedFlat=" + borderPaintedFlat;
  }

  public boolean isBorderPaintedFlat()
  {
    return borderPaintedFlat;
  }

  public void setBorderPaintedFlat(boolean newValue)
  {
    firePropertyChange("borderPaintedFlat", borderPaintedFlat, newValue);
    borderPaintedFlat = newValue;
  }

  /**
   * Returns the accessible context for this <code>JCheckBox</code>.
   *
   * @return the accessible context for this <code>JCheckBox</code>
   */
  public AccessibleContext getAccessibleContext()
  {
    if (accessibleContext == null)
      accessibleContext = new AccessibleJCheckBox();
    return accessibleContext;
  }
}
