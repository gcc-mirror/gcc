/* JRadioButtonMenuItem.java --
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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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

import java.io.IOException;
import java.io.ObjectOutputStream;

import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleRole;


public class JRadioButtonMenuItem extends JMenuItem implements Accessible
{
  //-------------------------------------------------------------
  // Variables --------------------------------------------------
  //-------------------------------------------------------------
  private static final String uiClassID = "RadioButtonMenuItemUI";

  //-------------------------------------------------------------
  // Initialization ---------------------------------------------
  //-------------------------------------------------------------
  public JRadioButtonMenuItem()
  {
    this(null, null);
  } // JRadioButtonMenuItem()

  public JRadioButtonMenuItem(Icon icon)
  {
    this(null, icon);
  } // JRadioButtonMenuItem()

  public JRadioButtonMenuItem(String text)
  {
    this(text, null);
  } // JRadioButtonMenuItem()

  public JRadioButtonMenuItem(Action action)
  {
    this();
    setAction(action);
  } // JRadioButtonMenuItem()

  public JRadioButtonMenuItem(String text, Icon icon)
  {
    this(text, icon, false);
  } // JRadioButtonMenuItem()

  public JRadioButtonMenuItem(String text, boolean selected)
  {
    this(text, null, selected);
  } // JRadioButtonMenuItem()

  public JRadioButtonMenuItem(Icon icon, boolean selected)
  {
    this(null, icon, selected);
  } // JRadioButtonMenuItem()

  public JRadioButtonMenuItem(String text, Icon icon, boolean selected)
  {
    super(text, icon);  
    setModel(new JToggleButton.ToggleButtonModel());	
    model.setSelected(selected);
  } // JRadioButtonMenuItem()

  //-------------------------------------------------------------
  // Methods ----------------------------------------------------
  //-------------------------------------------------------------
  private void writeObject(ObjectOutputStream stream) throws IOException
  {
    // TODO
  } // writeObject()

  public String getUIClassID()
  {
    return uiClassID;
  } // getUIClassID()

  public void requestFocus()
  {
    // TODO
  } // requestFocus()

  protected String paramString()
  {
    return "JRadioButtonMenuItem";
  } // paramString()

  public AccessibleContext getAccessibleContext()
  {
    if (accessibleContext == null)
      {
        accessibleContext = new AccessibleJRadioButtonMenuItem(this);
      }
    return accessibleContext;
  } // getAccessibleContext()

  //-------------------------------------------------------------
  // Classes ----------------------------------------------------
  //-------------------------------------------------------------
  protected class AccessibleJRadioButtonMenuItem extends AccessibleJMenuItem
  {
    //-------------------------------------------------------------
    // Initialization ---------------------------------------------
    //-------------------------------------------------------------
    protected AccessibleJRadioButtonMenuItem(JRadioButtonMenuItem component)
    {
      super(component);

      // TODO
    } // AccessibleJRadioButtonMenuItem()

    //-------------------------------------------------------------
    // Methods ----------------------------------------------------
    //-------------------------------------------------------------
    public AccessibleRole getAccessibleRole()
    {
      return AccessibleRole.RADIO_BUTTON;
    } // getAccessibleRole()
  } // AccessibleJRadioButtonMenuItem
} // JRadioButtonMenuItem
