/* JToggleButton.java -- 
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

import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;
import javax.swing.plaf.ButtonUI;

public class JToggleButton extends AbstractButton implements Accessible
{

  public static class ToggleButtonModel extends DefaultButtonModel
  {
    private static final long serialVersionUID = -1589950750899943974L;
  
    public void setPressed(boolean b)  
    {
      if (! isEnabled())
        return;
      
      super.setPressed(b);
      
      // setPressed(false) == mouse release on us,
      // if we were armed, we flip the selected state.
      if (!b && isArmed())
        setSelected(! isSelected());
    }
  }


  private static final long serialVersionUID = -3128248873429850443L;
    
  public JToggleButton()
  {
    this(null, null);
  }
  public JToggleButton(Action a)
  {
    this();
    setAction(a);
  }

  public JToggleButton(Icon icon)
  { 
    this(null, icon);
  }    
  
  public JToggleButton (Icon icon, boolean selected) 
  {
    this(null, icon, selected);
  }
  
  public JToggleButton(String text)
  {
    this(text, null);
  }
      
  public JToggleButton(String text, boolean selected)
  {
    this(text, null, selected);
  }

  public JToggleButton(String text, Icon icon)
  {
    this(text, icon, false);
  }

  public JToggleButton (String text, Icon icon, boolean selected) 
  {
    super(text, icon);

    horizontalAlignment = LEADING;
    setModel(new ToggleButtonModel());	
    model.setSelected(selected);
  }

  /**
   * Gets the AccessibleContext associated with this <code>JToggleButton</code>.
   *
   * @return the associated context
   */
  public AccessibleContext getAccessibleContext()
  {
    return null;
  }
  
  /**
   * Returns a string that specifies the name of the Look and Feel
   * class that renders this component.
   */
  public String getUIClassID()
  {
    return "ToggleButtonUI";
  }
  
  protected  String paramString()
  {
    return "JToggleButton";
  }
  
  
  public void updateUI()
  {	
    ButtonUI b = (ButtonUI)UIManager.getUI(this);
    setUI(b);
  }
}



