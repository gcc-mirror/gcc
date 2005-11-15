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

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Insets;
import java.beans.PropertyChangeListener;
import java.io.Serializable;
import java.util.Locale;

import javax.swing.border.Border;
import javax.swing.event.SwingPropertyChangeSupport;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.metal.MetalLookAndFeel;

/**
 * Manages the current {@link LookAndFeel} and any auxiliary {@link LookAndFeel}
 * instances.
 */
public class UIManager implements Serializable
{
  /**
   * Represents the basic information about a {@link LookAndFeel} (LAF), so 
   * that a list of installed LAFs can be presented without actually loading 
   * the LAF class(es).
   */
  public static class LookAndFeelInfo
  {
    String name, clazz;
	
    /**
     * Creates a new instance.
     * 
     * @param name  the look and feel name.
     * @param clazz  the look and feel class name.
     */
    public LookAndFeelInfo(String name, 
			   String clazz)
    {
      this.name  = name;
      this.clazz = clazz;
    }

    /**
     * Returns the name of the look and feel.
     * 
     * @return The name of the look and feel.
     */
    public String getName()
    {
      return name;
    }
    
    /**
     * Returns the fully qualified class name for the {@link LookAndFeel}.
     * 
     * @return The fully qualified class name for the {@link LookAndFeel}.
     */
    public String getClassName()
    {
      return clazz;
    }

    /**
     * Returns a String representation of the LookAndFeelInfo object.
     *
     * @return a String representation of the LookAndFeelInfo object
     */
    public String toString()
    {
      StringBuffer s = new StringBuffer();
      s.append(getClass().getName());
      s.append('[');
      s.append(getName());
      s.append(' ');
      s.append(getClassName());
      s.append(']');
      return s.toString();
    }
  }

  private static final long serialVersionUID = -5547433830339189365L;

  /** The installed look and feel(s). */
  static LookAndFeelInfo [] installed = {
    new LookAndFeelInfo("Metal", "javax.swing.plaf.metal.MetalLookAndFeel")
  };

  /** The installed auxiliary look and feels. */
  static LookAndFeel[] auxLookAndFeels;
  
  /** The current look and feel. */
  static LookAndFeel currentLookAndFeel;
  
  static UIDefaults currentUIDefaults;

  /** Property change listener mechanism. */
  static SwingPropertyChangeSupport listeners 
      = new SwingPropertyChangeSupport(UIManager.class);

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
      else
        {
          setLookAndFeel(new MetalLookAndFeel());
        }
    }
    catch (Exception ex)
      {
        System.err.println("cannot initialize Look and Feel: " + defaultlaf);
        System.err.println("error: " + ex.toString());
        System.err.println("falling back to Metal Look and Feel");
        try
          {
            setLookAndFeel(new MetalLookAndFeel());
          }
        catch (Exception ex2)
        {
          throw (Error) new AssertionError("There must be no problem installing"
                                           + " the MetalLookAndFeel.")
                                           .initCause(ex2);
        }
      }
  }

  /**
   * Creates a new instance of the <code>UIManager</code>.  There is no need
   * to construct an instance of this class, since all methods are static.
   */
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
    listeners.addPropertyChangeListener(listener);
  }

  /**
   * Remove a <code>PropertyChangeListener</code> from the listener list.
   *
   * @param listener the listener to remove
   */
  public static void removePropertyChangeListener(PropertyChangeListener 
          listener)
  {
    listeners.removePropertyChangeListener(listener);
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
    return listeners.getPropertyChangeListeners();
  }

  /**
   * Add a {@link LookAndFeel} to the list of auxiliary look and feels.
   * 
   * @param laf  the auxiliary look and feel (<code>null</code> not permitted).
   * 
   * @throws NullPointerException if <code>laf</code> is <code>null</code>.
   * 
   * @see #getAuxiliaryLookAndFeels()
   */
  public static void addAuxiliaryLookAndFeel(LookAndFeel laf)
  {
    if (laf == null)
      throw new NullPointerException("Null 'laf' argument.");
    if (auxLookAndFeels == null)
      {
        auxLookAndFeels = new LookAndFeel[1];
        auxLookAndFeels[0] = laf;
        return;
      }
	
    LookAndFeel[] temp = new LookAndFeel[auxLookAndFeels.length + 1];
    System.arraycopy(auxLookAndFeels, 0, temp, 0, auxLookAndFeels.length);			 
    auxLookAndFeels = temp;
    auxLookAndFeels[auxLookAndFeels.length - 1] = laf;
  }
    
  /**
   * Removes a {@link LookAndFeel} (LAF) from the list of auxiliary LAFs.
   * 
   * @param laf  the LAF to remove.
   * 
   * @return <code>true</code> if the LAF was removed, and <code>false</code>
   *         otherwise.
   */
  public static boolean removeAuxiliaryLookAndFeel(LookAndFeel laf)
  {
    if (auxLookAndFeels == null)
      return false;
    int count = auxLookAndFeels.length;
    if (count == 1 && auxLookAndFeels[0] == laf)
      {
        auxLookAndFeels = null;
        return true;
      }
    for (int i = 0; i < count; i++)
      {
        if (auxLookAndFeels[i] == laf)
          {
            LookAndFeel[] temp = new LookAndFeel[auxLookAndFeels.length - 1];
            if (i == 0)
              {
                System.arraycopy(auxLookAndFeels, 1, temp, 0, count - 1);  
              }
            else if (i == count - 1)
              {
                System.arraycopy(auxLookAndFeels, 0, temp, 0, count - 1);
              }
            else 
              {
                System.arraycopy(auxLookAndFeels, 0, temp, 0, i);
                System.arraycopy(auxLookAndFeels, i + 1, temp, i, 
                        count - i - 1);
              }
            auxLookAndFeels = temp;
            return true;
          }		
      }
    return false;
  }

  /**
   * Returns an array (possibly <code>null</code>) containing the auxiliary
   * {@link LookAndFeel}s that are in use.  These are used by the 
   * {@link javax.swing.plaf.multi.MultiLookAndFeel} class.
   * 
   * @return The auxiliary look and feels (possibly <code>null</code>).
   * 
   * @see #addAuxiliaryLookAndFeel(LookAndFeel)
   */
  public static LookAndFeel[] getAuxiliaryLookAndFeels()
  {
    return auxLookAndFeels;
  }

  /**
   * Returns an object from the {@link UIDefaults} table for the current
   * {@link LookAndFeel}.
   * 
   * @param key  the key.
   * 
   * @return The object.
   */
  public static Object get(Object key)
  {
    return getLookAndFeelDefaults().get(key);
  }

  /**
   * Returns an object from the {@link UIDefaults} table for the current
   * {@link LookAndFeel}.
   * 
   * @param key  the key.
   * 
   * @return The object.
   */
  public static Object get(Object key, Locale locale)
  {
    return getLookAndFeelDefaults().get(key ,locale);
  }

  /**
   * Returns a boolean value from the defaults table,
   * <code>false</code> if key is not present.
   *
   * @since 1.4
   */
  public static boolean getBoolean(Object key)
  {
    Boolean value = (Boolean) getLookAndFeelDefaults().get(key);
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
    Boolean value = (Boolean) getLookAndFeelDefaults().get(key, locale);
    return value != null ? value.booleanValue() : false;
  }
    
  /**
   * Returns a border from the defaults table. 
   */
  public static Border getBorder(Object key)
  {
    return (Border) getLookAndFeelDefaults().get(key);
  }
    
  /**
   * Returns a border from the defaults table.
   *
   * @since 1.4
   */
  public static Border getBorder(Object key, Locale locale)
  {
    return (Border) getLookAndFeelDefaults().get(key, locale);
  }
    
  /**
   * Returns a drawing color from the defaults table. 
   */
  public static Color getColor(Object key)
  {
    return (Color) getLookAndFeelDefaults().get(key);
  }

  /**
   * Returns a drawing color from the defaults table. 
   */
  public static Color getColor(Object key, Locale locale)
  {
    return (Color) getLookAndFeelDefaults().get(key);
  }

  /**
   * The fully qualified class name of the cross platform (Metal) look and feel.
   * This string can be passed to Class.forName()
   * 
   * @return <code>"javax.swing.plaf.metal.MetalLookAndFeel"</code>
   */
  public static String getCrossPlatformLookAndFeelClassName()
  {	
    return "javax.swing.plaf.metal.MetalLookAndFeel";
  }

  /**
   * Returns the default values for this look and feel. 
   * 
   * @return The {@link UIDefaults} for the current {@link LookAndFeel}.
   */
  public static UIDefaults getDefaults()
  {
    return currentUIDefaults;
  }

  /**
   * Returns a dimension from the defaults table. 
   */
  public static Dimension getDimension(Object key)
  {
    return (Dimension) getLookAndFeelDefaults().get(key);
  }

  /**
   * Returns a dimension from the defaults table. 
   */
  public static Dimension getDimension(Object key, Locale locale)
  {
    return (Dimension) getLookAndFeelDefaults().get(key, locale);
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
    return (Font) getLookAndFeelDefaults().get(key);
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
    return (Font) getLookAndFeelDefaults().get(key ,locale);
  }

  /**
   * Returns an Icon from the defaults table.
   */
  public static Icon getIcon(Object key)
  {
    return (Icon) getLookAndFeelDefaults().get(key);
  }
  
  /**
   * Returns an Icon from the defaults table.
   */
  public static Icon getIcon(Object key, Locale locale)
  {
    return (Icon) getLookAndFeelDefaults().get(key, locale);
  }
  
  /**
   * Returns an Insets object from the defaults table.
   */
  public static Insets getInsets(Object key)
  {
    return getLookAndFeelDefaults().getInsets(key);
  }

  /**
   * Returns an Insets object from the defaults table.
   */
  public static Insets getInsets(Object key, Locale locale)
  {
    return getLookAndFeelDefaults().getInsets(key, locale);
  }

  /**
   * Returns an array containing information about the {@link LookAndFeel}s
   * that are installed.
   * 
   * @return A list of the look and feels that are available (installed).
   */
  public static LookAndFeelInfo[] getInstalledLookAndFeels()
  {
    return installed;
  }

  public static int getInt(Object key)
  {
    Integer x = (Integer) getLookAndFeelDefaults().get(key);
    if (x == null)
      return 0;
    return x.intValue();
  }

  public static int getInt(Object key, Locale locale)
  {
    Integer x = (Integer) getLookAndFeelDefaults().get(key, locale);
    if (x == null)
      return 0;
    return x.intValue();
  }

  /**
   * Returns the current look and feel (which may be <code>null</code>).
   * 
   * @return The current look and feel.
   * 
   * @see #setLookAndFeel(LookAndFeel)
   */
  public static LookAndFeel getLookAndFeel()
  {
    return currentLookAndFeel;
  }

  /**
   * Returns the <code>UIDefaults</code> table of the currently active
   * look and feel.
   * 
   * @return The {@link UIDefaults} for the current {@link LookAndFeel}.
   */
  public static UIDefaults getLookAndFeelDefaults()
  {
    return currentUIDefaults;
  }

  /**
   * Returns a string from the defaults table.
   */
  public static String getString(Object key)
  {
    return (String) getLookAndFeelDefaults().get(key);
  }
  
  /**
   * Returns a string from the defaults table.
   */
  public static String getString(Object key, Locale locale)
  {
    return (String) getLookAndFeelDefaults().get(key, locale);
  }
  
  /**
   * Returns the name of the {@link LookAndFeel} class that implements the
   * native systems look and feel if there is one, otherwise the name
   * of the default cross platform LookAndFeel class.
   * 
   * @return The fully qualified class name for the system look and feel.
   * 
   * @see #getCrossPlatformLookAndFeelClassName()
   */
  public static String getSystemLookAndFeelClassName()
  {
    return getCrossPlatformLookAndFeelClassName();
  }

  /**
   * Returns UI delegate from the current {@link LookAndFeel} that renders the 
   * target component.
   * 
   * @param target  the target component.
   */
  public static ComponentUI getUI(JComponent target)
  {
    return getLookAndFeelDefaults().getUI(target);
  }

  /**
   * Creates a new look and feel and adds it to the current array.
   * 
   * @param name  the look and feel name.
   * @param className  the fully qualified name of the class that implements the
   *                   look and feel.
   */
  public static void installLookAndFeel(String name, String className)
  {
    installLookAndFeel(new LookAndFeelInfo(name, className));
  }

  /**
   * Adds the specified look and feel to the current array and then calls
   * setInstalledLookAndFeels(javax.swing.UIManager.LookAndFeelInfo[]).
   */
  public static void installLookAndFeel(LookAndFeelInfo info)
  {
    // FIXME: not yet implemented
  }

  /**
   * Stores an object in the defaults table.
   */
  public static Object put(Object key, Object value)
  {
    return getLookAndFeelDefaults().put(key,value);
  }

  /**
   * Replaces the current array of installed LookAndFeelInfos.
   */
  public static void setInstalledLookAndFeels(UIManager.LookAndFeelInfo[] infos)
  {
    // FIXME: not yet implemented.
  }
  
  /**
   * Sets the current {@link LookAndFeel}.
   * 
   * @param newLookAndFeel  the new look and feel (<code>null</code> permitted).
   * 
   * @throws UnsupportedLookAndFeelException if the look and feel is not 
   *         supported on the current platform.
   * 
   * @see LookAndFeel#isSupportedLookAndFeel()
   */
  public static void setLookAndFeel(LookAndFeel newLookAndFeel)
    throws UnsupportedLookAndFeelException
  {
    if (newLookAndFeel != null && ! newLookAndFeel.isSupportedLookAndFeel())
      throw new UnsupportedLookAndFeelException(newLookAndFeel.getName());
    
    LookAndFeel oldLookAndFeel = currentLookAndFeel;
    if (oldLookAndFeel != null)
      oldLookAndFeel.uninitialize();

    // Set the current default look and feel using a LookAndFeel object. 
    currentLookAndFeel = newLookAndFeel;
    if (newLookAndFeel != null)
      {
        newLookAndFeel.initialize();
        currentUIDefaults = newLookAndFeel.getDefaults();
      }
    else
      {
        currentUIDefaults = null;    
      }
    listeners.firePropertyChange("lookAndFeel", oldLookAndFeel, newLookAndFeel);
    //revalidate();
    //repaint();
  }

  /**
   * Set the current default look and feel using a class name.
   * 
   * @param className  the look and feel class name.
   * 
   * @throws UnsupportedLookAndFeelException if the look and feel is not 
   *         supported on the current platform.
   * 
   * @see LookAndFeel#isSupportedLookAndFeel()
   */
  public static void setLookAndFeel(String className)
    throws ClassNotFoundException, InstantiationException, IllegalAccessException,
    UnsupportedLookAndFeelException
  {
    Class c = Class.forName(className);
    LookAndFeel a = (LookAndFeel) c.newInstance(); // throws class-cast-exception
    setLookAndFeel(a);
  }
}
