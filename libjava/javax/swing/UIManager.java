/* UIManager.java -- 
   Copyright (C) 2002, 2003 Free Software Foundation, Inc.

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

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Insets;
import java.beans.PropertyChangeListener;
import java.io.Serializable;
import javax.swing.border.Border;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.metal.MetalLookAndFeel;

public class UIManager implements Serializable
{
  public static class LookAndFeelInfo
  {
    String name, clazz;
	
    LookAndFeelInfo(String name, 
                    String clazz)
    {
      this.name  = name;
      this.clazz = clazz;
    }

    String getName()      { return name;  }
    String getClassName() { return clazz; }
  }

  private static final long serialVersionUID = -5547433830339189365L;

  static LookAndFeelInfo [] installed = {
    new LookAndFeelInfo ("Metal", "javax.swing.plaf.metal.MetalLookAndFeel")
  };

  static LookAndFeel[] aux_installed;
  
  static LookAndFeel look_and_feel = new MetalLookAndFeel();
    
  public UIManager()
  {
    // Do nothing here.
  }

  public static void addPropertyChangeListener (PropertyChangeListener listener)
  {
    // FIXME
  }

  public static void removePropertyChangeListener (PropertyChangeListener listener)
    // Remove a PropertyChangeListener from the listener list. 
  {
    // FIXME
  }

  /**
   * @since 1.4
   */
  public static PropertyChangeListener[] getPropertyChangeListeners ()
  {
    // FIXME
    throw new Error ("Not implemented");
  }

  public static void addAuxiliaryLookAndFeel (LookAndFeel l)
  {
    // Add a LookAndFeel to the list of auxiliary look and feels. 
    if (aux_installed == null)
      {
        aux_installed = new LookAndFeel[1];
        aux_installed[0] = l;
        return;
      }
	
    LookAndFeel[] T = new LookAndFeel[ aux_installed.length+1 ];
    System.arraycopy(aux_installed, 0, T, 0, aux_installed.length);			 
    aux_installed = T;
    aux_installed[aux_installed.length-1] = l;
  }
    
  public static boolean removeAuxiliaryLookAndFeel(LookAndFeel laf)
  {
    if (aux_installed == null)
      return false;

    for (int i=0;i<aux_installed.length;i++)
      {
        if (aux_installed[i] == laf)
          {
            aux_installed[ i ] = aux_installed[aux_installed.length-1];
            LookAndFeel[] T = new LookAndFeel[ aux_installed.length-1 ];
            System.arraycopy (aux_installed, 0, T, 0, aux_installed.length-1);
            aux_installed = T;
            return true;
          }		
      }
    return false;
  }

  public static  LookAndFeel[] getAuxiliaryLookAndFeels()
  {	return aux_installed;    }

  public static  Object get(Object key)
  {	return getLookAndFeel().getDefaults().get(key);    }
    
  /**
   * Returns a border from the defaults table. 
   */
  public static Border getBorder(Object key)
  {
    return (Border) getLookAndFeel().getDefaults().get(key);
  }
    
  /**
   * Returns a drawing color from the defaults table. 
   */
  public static  Color getColor(Object key)
  {
    return (Color) getLookAndFeel().getDefaults().get(key);
  }

  /**
   * this string can be passed to Class.forName()
   */
  public static  String getCrossPlatformLookAndFeelClassName()
  {	
    return "javax.swing.plaf.metal.MetalLookAndFeel";
  }

  /**
   * Returns the default values for this look and feel. 
   */
  public static UIDefaults getDefaults()
  {
    return getLookAndFeel().getDefaults();
  }

  /**
   * Returns a dimension from the defaults table. 
   */
  public static Dimension getDimension(Object key)
  {
    System.out.println("UIManager.getDim");
    return new Dimension(200,100);
  }

  /**
   * Retrieves a font from the defaults table of the current
   * LookAndFeel.
   *
   * @param key an Object that specifies the font. Typically,
   *        this is a String such as
   *        <code>&quot;TitledBorder.font&quot;</code>.
   */
  public static Font getFont(Object key)
  {
    return (Font) getLookAndFeel().getDefaults().get(key);
  }

  public static Icon getIcon(Object key)
    // Returns an Icon from the defaults table. 
  {
    return (Icon) getLookAndFeel().getDefaults().get(key);
  }
  
  public static Insets getInsets(Object key)
    // Returns an Insets object from the defaults table. 
  {
    return (Insets) getLookAndFeel().getDefaults().getInsets(key);
  }

  public static LookAndFeelInfo[] getInstalledLookAndFeels()
  {
    return installed;
  }

  public static int getInt(Object key)
  {
    Integer x = (Integer) getLookAndFeel().getDefaults().get(key);
    if (x == null)
      return 0;
    return x.intValue();
  }

  public static LookAndFeel getLookAndFeel()
  {
    return look_and_feel;
  }

  /**
   * Returns the <code>UIDefaults</code> table of the currently active
   * look and feel.
   */
  public static UIDefaults getLookAndFeelDefaults()
  {
    return getLookAndFeel().getDefaults();
  }

  public static String getString(Object key)
    // Returns a string from the defaults table. 
  {
    return (String) getLookAndFeel().getDefaults().get(key);
  }
  
  public static String getSystemLookAndFeelClassName()
    // Returns the name of the LookAndFeel class that implements the native systems look and feel if there is one, otherwise the name of the default cross platform LookAndFeel class. 
  {
    return getCrossPlatformLookAndFeelClassName();
  }

  public static ComponentUI getUI(JComponent target)
    // Returns the L&F object that renders the target component. 
  {
    ComponentUI ui = getDefaults().getUI(target);
    //System.out.println("GET-UI-> " + ui + ", for " + target);
    return ui;
  }

  public static void installLookAndFeel(String name, String className)
    // Creates a new look and feel and adds it to the current array. 
  {
  }

  public static void installLookAndFeel(LookAndFeelInfo info)
    // Adds the specified look and feel to the current array and then calls setInstalledLookAndFeels(javax.swing.UIManager.LookAndFeelInfo[]). 
  {
  }

  public static Object put(Object key, Object value)
    // Stores an object in the defaults table. 
  {
    return getLookAndFeel().getDefaults().put(key,value);
  }

  public static void setInstalledLookAndFeels(UIManager.LookAndFeelInfo[] infos)
    // Replaces the current array of installed LookAndFeelInfos. 
  {
  }
  
  public static void setLookAndFeel(LookAndFeel newLookAndFeel)
  {
    if (look_and_feel != null)
      look_and_feel.uninitialize();

    // Set the current default look and feel using a LookAndFeel object. 
    look_and_feel = newLookAndFeel;
    look_and_feel.initialize();
	
    //revalidate();
    //repaint();
  }

  public static void setLookAndFeel (String className)
    throws ClassNotFoundException, InstantiationException, IllegalAccessException,
    UnsupportedLookAndFeelException
  {
    //          Set the current default look and feel using a class name.
    Class c = Class.forName(className);
    LookAndFeel a = (LookAndFeel) c.newInstance(); // throws class-cast-exception
    setLookAndFeel(a);
  }
}
