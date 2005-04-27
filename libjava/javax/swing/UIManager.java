/* UIManager.java -- 
   Copyright (C) 2002, 2003, 2004, 2005  Free Software Foundation, Inc.

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
import java.util.Locale;

import javax.swing.border.Border;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.metal.MetalLookAndFeel;

public class UIManager implements Serializable
{
  public static class LookAndFeelInfo
  {
    String name, clazz;
	
    public LookAndFeelInfo(String name, 
			   String clazz)
    {
      this.name  = name;
      this.clazz = clazz;
    }

    public String getName()
    {
      return name;
    }
    
    public String getClassName()
    {
      return clazz;
    }
  }

  private static final long serialVersionUID = -5547433830339189365L;

  static LookAndFeelInfo [] installed = {
    new LookAndFeelInfo ("Metal", "javax.swing.plaf.metal.MetalLookAndFeel")
  };

  static LookAndFeel[] aux_installed;
  
  static LookAndFeel look_and_feel = new MetalLookAndFeel();

  static
  {
    String defaultlaf = System.getProperty("swing.defaultlaf");
    try {
      if (defaultlaf != null)
        {
          Class lafClass = Class.forName(defaultlaf);
          LookAndFeel laf = (LookAndFeel) lafClass.newInstance();
          setLookAndFeel(laf);
        }
    }
    catch (Exception ex)
      {
        System.err.println("cannot initialize Look and Feel: " + defaultlaf);
        System.err.println("errot: " + ex.getMessage());
        System.err.println("falling back to Metal Look and Feel");
      }
  }

  public UIManager()
  {
    // Do nothing here.
  }

  /**
   * Add a <code>PropertyChangeListener</code> to the listener list.
   *
   * @param listener the listener to add
   */
  public static void addPropertyChangeListener(PropertyChangeListener listener)
  {
    // FIXME
  }

  /**
   * Remove a <code>PropertyChangeListener</code> from the listener list.
   *
   * @param listener the listener to remove
   */
  public static void removePropertyChangeListener(PropertyChangeListener listener)
  {
    // FIXME
  }

  /**
   * Returns an array of all added <code>PropertyChangeListener</code> objects.
   *
   * @return an array of listeners
   *
   * @since 1.4
   */
  public static PropertyChangeListener[] getPropertyChangeListeners()
  {
    // FIXME
    throw new Error ("Not implemented");
  }

  /**
   * Add a LookAndFeel to the list of auxiliary look and feels.
   */
  public static void addAuxiliaryLookAndFeel (LookAndFeel l)
  {
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
  {
    return aux_installed;
  }

  public static  Object get(Object key)
  {
    return getLookAndFeel().getDefaults().get(key);
  }

  public static  Object get(Object key, Locale locale)
  {
    return getLookAndFeel().getDefaults().get(key ,locale);
  }

  /**
   * Returns a boolean value from the defaults table,
   * <code>false</code> if key is not present.
   *
   * @since 1.4
   */
  public static boolean getBoolean(Object key)
  {
    Boolean value = (Boolean) getLookAndFeel().getDefaults().get(key);
    return value != null ? value.booleanValue() : false;
  }
  
  /**
   * Returns a boolean value from the defaults table,
   * <code>false</code> if key is not present.
   *
   * @since 1.4
   */
  public static boolean getBoolean(Object key, Locale locale)
  {
    Boolean value = (Boolean) getLookAndFeel().getDefaults().get(key, locale);
    return value != null ? value.booleanValue() : false;
  }
    
  /**
   * Returns a border from the defaults table. 
   */
  public static Border getBorder(Object key)
  {
    return (Border) getLookAndFeel().getDefaults().get(key);
  }
    
  /**
   * Returns a border from the defaults table.
   *
   * @since 1.4
   */
  public static Border getBorder(Object key, Locale locale)
  {
    return (Border) getLookAndFeel().getDefaults().get(key, locale);
  }
    
  /**
   * Returns a drawing color from the defaults table. 
   */
  public static  Color getColor(Object key)
  {
    return (Color) getLookAndFeel().getDefaults().get(key);
  }

  /**
   * Returns a drawing color from the defaults table. 
   */
  public static  Color getColor(Object key, Locale locale)
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
    return (Dimension) getLookAndFeel().getDefaults().get(key);
  }

  /**
   * Returns a dimension from the defaults table. 
   */
  public static Dimension getDimension(Object key, Locale locale)
  {
    return (Dimension) getLookAndFeel().getDefaults().get(key, locale);
  }

  /**
   * Retrieves a font from the defaults table of the current
   * LookAndFeel.
   *
   * @param key an Object that specifies the font. Typically,
   *        this is a String such as
   *        <code>TitledBorder.font</code>.
   */
  public static Font getFont(Object key)
  {
    return (Font) getLookAndFeel().getDefaults().get(key);
  }

  /**
   * Retrieves a font from the defaults table of the current
   * LookAndFeel.
   *
   * @param key an Object that specifies the font. Typically,
   *        this is a String such as
   *        <code>TitledBorder.font</code>.
   */
  public static Font getFont(Object key, Locale locale)
  {
    return (Font) getLookAndFeel().getDefaults().get(key ,locale);
  }

  /**
   * Returns an Icon from the defaults table.
   */
  public static Icon getIcon(Object key)
  {
    return (Icon) getLookAndFeel().getDefaults().get(key);
  }
  
  /**
   * Returns an Icon from the defaults table.
   */
  public static Icon getIcon(Object key, Locale locale)
  {
    return (Icon) getLookAndFeel().getDefaults().get(key, locale);
  }
  
  /**
   * Returns an Insets object from the defaults table.
   */
  public static Insets getInsets(Object key)
  {
    return (Insets) getLookAndFeel().getDefaults().getInsets(key);
  }

  /**
   * Returns an Insets object from the defaults table.
   */
  public static Insets getInsets(Object key, Locale locale)
  {
    return (Insets) getLookAndFeel().getDefaults().getInsets(key, locale);
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

  public static int getInt(Object key, Locale locale)
  {
    Integer x = (Integer) getLookAndFeel().getDefaults().get(key, locale);
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

  /**
   * Returns a string from the defaults table.
   */
  public static String getString(Object key)
  {
    return (String) getLookAndFeel().getDefaults().get(key);
  }
  
  /**
   * Returns a string from the defaults table.
   */
  public static String getString(Object key, Locale locale)
  {
    return (String) getLookAndFeel().getDefaults().get(key, locale);
  }
  
  /**
   * Returns the name of the LookAndFeel class that implements the
   * native systems look and feel if there is one, otherwise the name
   * of the default cross platform LookAndFeel class.
   */
  public static String getSystemLookAndFeelClassName()
  {
    return getCrossPlatformLookAndFeelClassName();
  }

  /**
   * Returns the Look and Feel object that renders the target component.
   */
  public static ComponentUI getUI(JComponent target)
  {
    return getDefaults().getUI(target);
  }

  /**
   * Creates a new look and feel and adds it to the current array.
   */
  public static void installLookAndFeel(String name, String className)
  {
  }

  /**
   * Adds the specified look and feel to the current array and then calls
   * setInstalledLookAndFeels(javax.swing.UIManager.LookAndFeelInfo[]).
   */
  public static void installLookAndFeel(LookAndFeelInfo info)
  {
  }

  /**
   * Stores an object in the defaults table.
   */
  public static Object put(Object key, Object value)
  {
    return getLookAndFeel().getDefaults().put(key,value);
  }

  /**
   * Replaces the current array of installed LookAndFeelInfos.
   */
  public static void setInstalledLookAndFeels(UIManager.LookAndFeelInfo[] infos)
  {
  }
  
  /**
   * Set the current default look.
   */
  public static void setLookAndFeel(LookAndFeel newLookAndFeel)
    throws UnsupportedLookAndFeelException
  {
    if (! newLookAndFeel.isSupportedLookAndFeel())
      throw new UnsupportedLookAndFeelException(newLookAndFeel.getName());
    
    if (look_and_feel != null)
      look_and_feel.uninitialize();

    // Set the current default look and feel using a LookAndFeel object. 
    look_and_feel = newLookAndFeel;
    look_and_feel.initialize();
	
    //revalidate();
    //repaint();
  }

  /**
   * Set the current default look and feel using a class name.
   */
  public static void setLookAndFeel (String className)
    throws ClassNotFoundException, InstantiationException, IllegalAccessException,
    UnsupportedLookAndFeelException
  {
    Class c = Class.forName(className);
    LookAndFeel a = (LookAndFeel) c.newInstance(); // throws class-cast-exception
    setLookAndFeel(a);
  }
}
