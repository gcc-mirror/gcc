/* UIManager.java --
   Copyright (C) 2002, 2003, 2004, 2005, 2006,  Free Software Foundation, Inc.

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

import gnu.java.lang.CPStringBuilder;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Insets;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.io.Serializable;
import java.util.Enumeration;
import java.util.Locale;

import javax.swing.border.Border;
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
      CPStringBuilder s = new CPStringBuilder();
      s.append(getClass().getName());
      s.append('[');
      s.append(getName());
      s.append(' ');
      s.append(getClassName());
      s.append(']');
      return s.toString();
    }
  }

  /**
   * A UIDefaults subclass that multiplexes between itself and a 'fallback'
   * UIDefaults instance. This is used to protect the L&F UIDefaults from beeing
   * overwritten by applications.
   */
  private static class MultiplexUIDefaults
    extends UIDefaults
  {
    private class MultiplexEnumeration
      implements Enumeration
    {
      Enumeration[] enums;
      int i;
      MultiplexEnumeration(Enumeration e1, Enumeration e2)
      {
        enums = new Enumeration[]{ e1, e2 };
        i = 0;
      }

      public boolean hasMoreElements()
      {
        return enums[i].hasMoreElements() || i < enums.length - 1;
      }

      public Object nextElement()
      {
        Object val = enums[i].nextElement();
        if (! enums[i].hasMoreElements() && i < enums.length - 1)
          i++;
        return val;
      }

    }

    UIDefaults fallback;

    /**
     * Creates a new <code>MultiplexUIDefaults</code> instance with
     * <code>d</code> as the fallback defaults.
     *
     * @param d  the fallback defaults (<code>null</code> not permitted).
     */
    MultiplexUIDefaults(UIDefaults d)
    {
      if (d == null)
        throw new NullPointerException();
      fallback = d;
    }

    public Object get(Object key)
    {
      Object val = super.get(key);
      if (val == null)
        val = fallback.get(key);
      return val;
    }

    public Object get(Object key, Locale l)
    {
      Object val = super.get(key, l);
      if (val == null)
        val = fallback.get(key, l);
      return val;
    }

    public Object remove(Object key)
    {
      Object val = super.remove(key);
      if (val == null)
        val = fallback.remove(key);
      return val;
    }

    public int size()
    {
      return super.size() + fallback.size();
    }

    public Enumeration keys()
    {
      return new MultiplexEnumeration(super.keys(), fallback.keys());
    }

    public Enumeration elements()
    {
      return new MultiplexEnumeration(super.elements(), fallback.elements());
    }
  }

  private static final long serialVersionUID = -5547433830339189365L;

  /** The installed look and feel(s). */
  static LookAndFeelInfo [] installed = {
    new LookAndFeelInfo("Metal", "javax.swing.plaf.metal.MetalLookAndFeel"),
    new LookAndFeelInfo("GNU", "gnu.javax.swing.plaf.gnu.GNULookAndFeel")
  };

  /** The installed auxiliary look and feels. */
  static LookAndFeel[] auxLookAndFeels;

  /** The current look and feel. */
  static LookAndFeel currentLookAndFeel;

  static MultiplexUIDefaults currentUIDefaults;

  static UIDefaults lookAndFeelDefaults;

  /** Property change listener mechanism. */
  static PropertyChangeSupport listeners
      = new PropertyChangeSupport(UIManager.class);

  static
  {
    String defaultlaf = System.getProperty("swing.defaultlaf");
    try
      {
        if (defaultlaf != null)
          {
            setLookAndFeel(defaultlaf);
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
        ex.printStackTrace();
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
    return getDefaults().get(key);
  }

  /**
   * Returns an object from the {@link UIDefaults} table for the current
   * {@link LookAndFeel}.
   *
   * @param key  the key.
   *
   * @return The object.
   *
   * @since 1.4
   */
  public static Object get(Object key, Locale locale)
  {
    return getDefaults().get(key, locale);
  }

  /**
   * Returns a boolean value from the defaults table.  If there is no value
   * for the specified key, or the value is not an instance of {@link Boolean},
   * this method returns <code>false</code>.
   *
   * @param key  the key (<code>null</code> not permitted).
   *
   * @return The boolean value associated with the specified key.
   *
   * @throws NullPointerException if <code>key</code> is <code>null</code>.
   *
   * @since 1.4
   */
  public static boolean getBoolean(Object key)
  {
    Object value = get(key);
    if (value instanceof Boolean)
      return ((Boolean) value).booleanValue();
    return false;
  }

  /**
   * Returns a boolean value from the defaults table.  If there is no value
   * for the specified key, or the value is not an instance of {@link Boolean},
   * this method returns <code>false</code>.
   *
   * @param key  the key (<code>null</code> not permitted).
   * @param locale  the locale.
   *
   * @return The boolean value associated with the specified key.
   *
   * @throws NullPointerException if <code>key</code> is <code>null</code>.
   *
   * @since 1.4
   */
  public static boolean getBoolean(Object key, Locale locale)
  {
    Object value = get(key, locale);
    if (value instanceof Boolean)
      return ((Boolean) value).booleanValue();
    return false;
  }

  /**
   * Returns a border from the defaults table.
   *
   * @param key  the key (<code>null</code> not permitted).
   *
   * @return The border associated with the given key, or <code>null</code>.
   *
   * @throws NullPointerException if <code>key</code> is <code>null</code>.
   */
  public static Border getBorder(Object key)
  {
    Object value = get(key);
    if (value instanceof Border)
      return (Border) value;
    return null;
  }

  /**
   * Returns a border from the defaults table.
   *
   * @param key  the key (<code>null</code> not permitted).
   * @param locale  the locale.
   *
   * @return The border associated with the given key, or <code>null</code>.
   *
   * @throws NullPointerException if <code>key</code> is <code>null</code>.
   *
   * @since 1.4
   */
  public static Border getBorder(Object key, Locale locale)
  {
    Object value = get(key, locale);
    if (value instanceof Border)
      return (Border) value;
    return null;
  }

  /**
   * Returns a drawing color from the defaults table.
   *
   * @param key  the key (<code>null</code> not permitted).
   *
   * @return The color associated with the given key, or <code>null</code>.
   *
   * @throws NullPointerException if <code>key</code> is <code>null</code>.
   */
  public static Color getColor(Object key)
  {
    Object value = get(key);
    if (value instanceof Color)
      return (Color) value;
    return null;
  }

  /**
   * Returns a drawing color from the defaults table.
   *
   * @param key  the key (<code>null</code> not permitted).
   * @param locale  the locale.
   *
   * @return The color associated with the given key, or <code>null</code>.
   *
   * @throws NullPointerException if <code>key</code> is <code>null</code>.
   *
   * @since 1.4
   */
  public static Color getColor(Object key, Locale locale)
  {
    Object value = get(key, locale);
    if (value instanceof Color)
      return (Color) value;
    return null;
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
    if (currentUIDefaults == null)
      currentUIDefaults = new MultiplexUIDefaults(new UIDefaults());
    return currentUIDefaults;
  }

  /**
   * Returns a dimension from the defaults table.
   *
   * @param key  the key (<code>null</code> not permitted).
   *
   * @return The color associated with the given key, or <code>null</code>.
   *
   * @throws NullPointerException if <code>key</code> is <code>null</code>.
   */
  public static Dimension getDimension(Object key)
  {
    Object value = get(key);
    if (value instanceof Dimension)
      return (Dimension) value;
    return null;
  }

  /**
   * Returns a dimension from the defaults table.
   *
   * @param key  the key (<code>null</code> not permitted).
   * @param locale  the locale.
   *
   * @return The color associated with the given key, or <code>null</code>.
   *
   * @throws NullPointerException if <code>key</code> is <code>null</code>.
   * @since 1.4
   */
  public static Dimension getDimension(Object key, Locale locale)
  {
    Object value = get(key, locale);
    if (value instanceof Dimension)
      return (Dimension) value;
    return null;
  }

  /**
   * Retrieves a font from the defaults table of the current
   * LookAndFeel.
   *
   * @param key an Object that specifies the font. Typically,
   *        this is a String such as
   *        <code>TitledBorder.font</code>.
   *
   * @return The font associated with the given key, or <code>null</code>.
   *
   * @throws NullPointerException if <code>key</code> is <code>null</code>.
   */
  public static Font getFont(Object key)
  {
    Object value = get(key);
    if (value instanceof Font)
      return (Font) value;
    return null;
  }

  /**
   * Retrieves a font from the defaults table of the current
   * LookAndFeel.
   *
   * @param key an Object that specifies the font. Typically,
   *        this is a String such as
   *        <code>TitledBorder.font</code>.
   * @param locale  the locale.
   *
   * @return The font associated with the given key, or <code>null</code>.
   *
   * @throws NullPointerException if <code>key</code> is <code>null</code>.
   *
   * @since 1.4
   */
  public static Font getFont(Object key, Locale locale)
  {
    Object value = get(key, locale);
    if (value instanceof Font)
      return (Font) value;
    return null;
  }

  /**
   * Returns an icon from the defaults table.
   *
   * @param key  the key (<code>null</code> not permitted).
   *
   * @return The icon associated with the given key, or <code>null</code>.
   *
   * @throws NullPointerException if <code>key</code> is <code>null</code>.
   */
  public static Icon getIcon(Object key)
  {
    Object value = get(key);
    if (value instanceof Icon)
      return (Icon) value;
    return null;
  }

  /**
   * Returns an icon from the defaults table.
   *
   * @param key  the key (<code>null</code> not permitted).
   * @param locale  the locale.
   *
   * @return The icon associated with the given key, or <code>null</code>.
   *
   * @throws NullPointerException if <code>key</code> is <code>null</code>.
   * @since 1.4
   */
  public static Icon getIcon(Object key, Locale locale)
  {
    Object value = get(key, locale);
    if (value instanceof Icon)
      return (Icon) value;
    return null;
  }

  /**
   * Returns an Insets object from the defaults table.
   *
   * @param key  the key (<code>null</code> not permitted).
   *
   * @return The insets associated with the given key, or <code>null</code>.
   *
   * @throws NullPointerException if <code>key</code> is <code>null</code>.
   */
  public static Insets getInsets(Object key)
  {
    Object o = get(key);
    if (o instanceof Insets)
      return (Insets) o;
    else
      return null;
  }

  /**
   * Returns an Insets object from the defaults table.
   *
   * @param key  the key (<code>null</code> not permitted).
   * @param locale  the locale.
   *
   * @return The insets associated with the given key, or <code>null</code>.
   *
   * @throws NullPointerException if <code>key</code> is <code>null</code>.
   * @since 1.4
   */
  public static Insets getInsets(Object key, Locale locale)
  {
    Object o = get(key, locale);
    if (o instanceof Insets)
      return (Insets) o;
    else
      return null;
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

  /**
   * Returns the integer value of the {@link Integer} associated with the
   * given key.  If there is no value, or the value is not an instance of
   * {@link Integer}, this method returns 0.
   *
   * @param key  the key (<code>null</code> not permitted).
   *
   * @return The integer value associated with the given key, or 0.
   */
  public static int getInt(Object key)
  {
    Object x = get(key);
    if (x instanceof Integer)
      return ((Integer) x).intValue();
    return 0;
  }

  /**
   * Returns the integer value of the {@link Integer} associated with the
   * given key.  If there is no value, or the value is not an instance of
   * {@link Integer}, this method returns 0.
   *
   * @param key  the key (<code>null</code> not permitted).
   * @param locale  the locale.
   *
   * @return The integer value associated with the given key, or 0.
   *
   * @since 1.4
   */
  public static int getInt(Object key, Locale locale)
  {
    Object x = get(key, locale);
    if (x instanceof Integer)
      return ((Integer) x).intValue();
    return 0;
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
    return lookAndFeelDefaults;
  }

  /**
   * Returns the {@link String} associated with the given key.  If the value
   * is not a {@link String}, this method returns <code>null</code>.
   *
   * @param key  the key (<code>null</code> not permitted).
   *
   * @return The string associated with the given key, or <code>null</code>.
   */
  public static String getString(Object key)
  {
    Object s = get(key);
    if (s instanceof String)
      return (String) s;
    return null;
  }

  /**
   * Returns the {@link String} associated with the given key.  If the value
   * is not a {@link String}, this method returns <code>null</code>.
   *
   * @param key  the key (<code>null</code> not permitted).
   * @param locale  the locale.
   *
   * @return The string associated with the given key, or <code>null</code>.
   *
   * @since 1.4
   */
  public static String getString(Object key, Locale locale)
  {
    Object s = get(key, locale);
    if (s instanceof String)
      return (String) s;
    return null;
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
    return getDefaults().getUI(target);
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
    LookAndFeelInfo[] newInstalled = new LookAndFeelInfo[installed.length + 1];
    System.arraycopy(installed, 0, newInstalled, 0, installed.length);
    newInstalled[newInstalled.length - 1] = info;
    setInstalledLookAndFeels(newInstalled);
  }

  /**
   * Stores an object in the defaults table.
   *
   * @param key  the key.
   * @param value  the value.
   */
  public static Object put(Object key, Object value)
  {
    return getDefaults().put(key, value);
  }

  /**
   * Replaces the current array of installed LookAndFeelInfos.
   */
  public static void setInstalledLookAndFeels(UIManager.LookAndFeelInfo[] infos)
  {
    installed = infos;
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
      throw new UnsupportedLookAndFeelException(newLookAndFeel.getName()
                                         + " not supported on this platform");
    LookAndFeel oldLookAndFeel = currentLookAndFeel;
    if (oldLookAndFeel != null)
      oldLookAndFeel.uninitialize();

    // Set the current default look and feel using a LookAndFeel object.
    currentLookAndFeel = newLookAndFeel;
    if (newLookAndFeel != null)
      {
        newLookAndFeel.initialize();
        lookAndFeelDefaults = newLookAndFeel.getDefaults();
        if (currentUIDefaults == null)
          currentUIDefaults =
            new MultiplexUIDefaults(lookAndFeelDefaults);
        else
          currentUIDefaults.fallback = lookAndFeelDefaults;
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
    Class c = Class.forName(className, true,
                            Thread.currentThread().getContextClassLoader());
    LookAndFeel a = (LookAndFeel) c.newInstance(); // throws class-cast-exception
    setLookAndFeel(a);
  }
}
