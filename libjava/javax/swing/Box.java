/* Box.java --
   Copyright (C) 2002 Free Software Foundation, Inc.

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
import javax.accessibility.AccessibleRole;
import java.awt.LayoutManager;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.AWTError;

/**
 * Needs some work I guess....
 *
 * @author Ronald Veldema (rveldema@cs.vu.nl)
 */
public class Box extends JComponent implements Accessible
{
  private static final long serialVersionUID = 1525417495883046342L;
  
  protected class AccessibleBox extends AccessibleAWTContainer
  {
    protected AccessibleBox()
    {
    }
    
    public AccessibleRole getAccessibleRole()
    {
      return null;
    }
  }
  
  public static class Filler extends JComponent implements Accessible
  {
    protected class AccessibleBoxFiller// extends AccessibleAWTComponent
    {
      protected AccessibleBoxFiller()
      {
      }
      
      public AccessibleRole getAccessibleRole()
      {
        return null;
      }
    }
    
    protected AccessibleContext accessibleContext;
    
    private transient Dimension min, pref, max;
    
    public Filler(Dimension min, Dimension pref, Dimension max)
    {
      changeShape(min, pref, max);
    }
    
    public void changeShape(Dimension min, Dimension pref, Dimension max)
    {
      this.min = min;
      this.pref = pref;
      this.max = max;    
    }
    
    public AccessibleContext getAccessibleContext()
    {
//      if (accessibleContext == null)
//        accessibleContext = new AccessibleBoxFiller();
      return accessibleContext;
    }
    
    public Dimension getMaximumSize()
    {
      return max;
    }
    
    public Dimension getMinimumSize()
    {
      return min;
    }
    
    public Dimension getPreferredSize()
    {
      return pref;
    }
  }
  
  public Box(int axis)
  {
    setLayout(new BoxLayout(this, axis));	
  }
  
  public static Component createGlue()
  {
    return null;
  }
  
  public static Box createHorizontalBox()
  {
    return null;
  }
  
  public static Component createHorizontalGlue()
  {
    return null;
  }
  
  public static Component createHorizontalStrut(int width)
  {
    return null;
  }
  
  public static Component createRigidArea(Dimension d)
  {
    return null;
  }
  
  public static Box createVerticalBox()
  {
    return null;
  }
  
  public static Component createVerticalGlue()
  {
    return null;
  }
  
  public static Component createVerticalStrut(int height)
  {
    return null;
  }
  
  public void setLayout(LayoutManager l)
  {
    throw new AWTError("Not allowed to set layout managers for boxes.");
  }
  
  public AccessibleContext getAccessibleContext()
  {
    if (accessibleContext == null)
      accessibleContext = new AccessibleBox();
    return accessibleContext;
  }
  
  
}
