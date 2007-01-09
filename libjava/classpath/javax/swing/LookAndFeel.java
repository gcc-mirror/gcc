/* LookAndFeel.java --
   Copyright (C) 2002, 2004, 2005, 2006,  Free Software Foundation, Inc.

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
import java.awt.Component;
import java.awt.Font;
import java.awt.Toolkit;
import java.net.URL;

import javax.swing.border.Border;
import javax.swing.plaf.ComponentInputMapUIResource;
import javax.swing.plaf.IconUIResource;
import javax.swing.plaf.InputMapUIResource;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.metal.MetalLookAndFeel;
import javax.swing.text.JTextComponent;

/**
 * A <i>look-and-feel</i> controls most aspects of the appearance and 
 * operation of user interface components in <code>javax.swing</code>.  A 
 * cross-platform look-and-feel (the {@link MetalLookAndFeel}) is provided.
 * 
 * @see UIManager#getInstalledLookAndFeels()
 * @see UIManager#setLookAndFeel(LookAndFeel)
 */
public abstract class LookAndFeel
{
  /**
   * Creates and returns a look-and-feel specific defaults table.  This method 
   * is called once by {@link UIManager#setLookAndFeel(LookAndFeel)} and 
   * shouldn't be called again (as it creates a large table of defaults).
   *
   * @return The UI defaults.
   */
  public UIDefaults getDefaults()
  {
    return null;
  }

  /**
   * Returns a description of the look and feel.
   * 
   * @return A description of the look and feel.
   */
  public abstract String getDescription();

  /**
   * Returns the value of <code>Toolkit.getDefaultToolkit()
   * .getDesktopProperty(systemPropertyName)</code>, or 
   * <code>fallbackValue</code> if no such property is defined.
   * 
   * @param systemPropertyName  the system property name.
   * @param fallbackValue  the fallback value.
   * 
   * @return The property value or <code>fallbackValue</code>.
   */
  public static Object getDesktopPropertyValue(String systemPropertyName, 
      Object fallbackValue)
  {
    Object value = Toolkit.getDefaultToolkit().getDesktopProperty(
        systemPropertyName);
    return value != null ? value : fallbackValue;
  }
  
  /**
   * Returns an identifier for the look and feel.
   * 
   * @return An identifier for the look and feel.
   */
  public abstract String getID();

  /**
   * Returns the name for the look and feel.
   * 
   * @return The name for the look and feel.
   */
  public abstract String getName();

  /**
   * Returns <code>true</code> when the look-and-feel supports window 
   * decorations, and <code>false</code> otherwise. This default implementation
   * always returns <code>false</code> and needs to be overridden when the 
   * derived look-and-feel supports this.
   *
   * @return <code>false</code>.
   *
   * @since 1.4
   */
  public boolean getSupportsWindowDecorations()
  {
    return false;
  }
  
  /**
   * Initializes the look-and-feel.  The 
   * {@link UIManager#setLookAndFeel(LookAndFeel)} method calls this method 
   * before the first call (and typically the only call) to 
   * {@link #getDefaults()}.  This default implementation does nothing, but
   * subclasses can override this behaviour. 
   */
  public void initialize()
  {
    // We do nothing here. This method is meant to be overridden by
    // LookAndFeel implementations.
  }

  /**
   * Convenience method for installing a component's default {@link Border} 
   * object on the specified component if either the border is currently 
   * <code>null</code> or already an instance of {@link UIResource}. 
   * 
   * @param c  the component (<code>null</code> not permitted).
   * @param defaultBorderName  the border name (for lookup in the UIDefaults 
   *     table).
   */
  public static void installBorder(JComponent c, String defaultBorderName)
  {
    Border b = c.getBorder();
    if (b == null || b instanceof UIResource)
      c.setBorder(UIManager.getBorder(defaultBorderName));
  }

  /**
   * Convenience method for initializing a component's foreground and
   * background color properties with values from the current defaults table.
   * 
   * @param c  the component (<code>null</code> not permitted).
   * @param defaultBgName  the key for the background color in the UIDefaults 
   *     table.
   * @param defaultFgName  the key for the foreground color in the UIDefaults
   *     table.
   */
  public static void installColors(JComponent c, String defaultBgName,
                                   String defaultFgName)
  {
    // Install background.
    Color bg = c.getBackground();
    if (bg == null || bg instanceof UIResource)
      c.setBackground(UIManager.getColor(defaultBgName));

    // Install foreground.
    Color fg = c.getForeground();
    if (fg == null || fg instanceof UIResource)
      c.setForeground(UIManager.getColor(defaultFgName));
  }

  /**
   * Convenience method for initializing a component's foreground, background
   * and font properties with values from the current defaults table.
   * 
   * @param component  the component (<code>null</code> not permitted).
   * @param defaultBgName  the key for the background color in the UIDefaults 
   *     table.
   * @param defaultFgName  the key for the foreground color in the UIDefaults
   *     table.
   * @param defaultFontName  the key for the font in the UIDefaults table.
   */
  public static void installColorsAndFont(JComponent component,
                                          String defaultBgName,
                                          String defaultFgName,
                                          String defaultFontName)
  {
    // Install colors.
    installColors(component, defaultBgName, defaultFgName);
    // Install font.
    Font f = component.getFont();
    if (f == null || f instanceof UIResource)
      component.setFont(UIManager.getFont(defaultFontName));
  }

  /**
   * Returns <code>true</code> if the look-and-feel is the "native" 
   * look-and-feel for the current platform, and <code>false</code> otherwise.
   * A native look-and-feel emulates the appearance and behaviour of the 
   * default windowing system on the host operating system.
   * 
   * @return A flag indicating whether or not this is the native look and feel
   *         for the current platform.
   */
  public abstract boolean isNativeLookAndFeel();

  /**
   * Returns <code>true</code> if the look-and-feel is supported on the 
   * current operating system, and <code>false</code> otherwise.  This 
   * mechanism is provided so that it is possible to prevent a look-and-feel
   * from being used on some operating systems (usually for legal, not
   * technical, reasons).
   * 
   * @return A flag indicating whether or not the look-and-feel is supported
   *         on the current platform.
   */
  public abstract boolean isSupportedLookAndFeel();

  /**
   * Loads the bindings in keys into retMap. Does not remove existing entries
   * from retMap.  <code>keys</code> describes the InputMap, every even indexed
   * item is either a KeyStroke or a String representing a KeyStroke and every
   * odd indexed item is the Object associated with that KeyStroke in an 
   * ActionMap.
   * 
   * @param retMap the InputMap into which we load bindings
   * @param keys the Object array describing the InputMap as above
   */
  public static void loadKeyBindings(InputMap retMap, Object[] keys)
  {
    if (keys == null)
      return;
    for (int i = 0; i < keys.length - 1; i += 2)
      {
        Object key = keys[i];
        KeyStroke keyStroke;
        if (key instanceof KeyStroke)
          keyStroke = (KeyStroke) key;
        else
          keyStroke = KeyStroke.getKeyStroke((String) key);
        retMap.put(keyStroke, keys[i + 1]);
      }
  }

  /**
   * Creates a ComponentInputMap from keys.  
   * <code>keys</code> describes the InputMap, every even indexed
   * item is either a KeyStroke or a String representing a KeyStroke and every
   * odd indexed item is the Object associated with that KeyStroke in an 
   * ActionMap.
   * 
   * @param c the JComponent associated with the ComponentInputMap
   * @param keys the Object array describing the InputMap as above
   * 
   * @return A new input map.
   */
  public static ComponentInputMap makeComponentInputMap(JComponent c,
							Object[] keys)
  {
    ComponentInputMap retMap = new ComponentInputMapUIResource(c);
    loadKeyBindings(retMap, keys);
    return retMap;
  }

  /**
   * Utility method that creates a UIDefaults.LazyValue that creates an
   * ImageIcon UIResource for the specified gifFile filename.
   * 
   * @param baseClass  the base class for accessing the icon resource.
   * @param gifFile  the file name.
   * 
   * @return A {@link UIDefaults.LazyValue} that serves up an 
   *     {@link IconUIResource}.
   */
  public static Object makeIcon(Class<?> baseClass, String gifFile)
  {
    final URL file = baseClass.getResource(gifFile);
    return new UIDefaults.LazyValue() 
      {
        public Object createValue(UIDefaults table)
        {
          return new IconUIResource(new ImageIcon(file));
        }
      };
  }

  /**
   * Creates a InputMap from keys. 
   * <code>keys</code> describes the InputMap, every even indexed
   * item is either a KeyStroke or a String representing a KeyStroke and every
   * odd indexed item is the Object associated with that KeyStroke in an 
   * ActionMap.
   * 
   * @param keys the Object array describing the InputMap as above
   * 
   * @return A new input map.
   */
  public static InputMap makeInputMap(Object[] keys)
  {
    InputMap retMap = new InputMapUIResource();
    loadKeyBindings(retMap, keys);
    return retMap;
  }

  /**
   * Convenience method for building lists of KeyBindings.
   * <code>keyBindingList</code> is an array of KeyStroke-Action pairs where
   * even indexed elements are KeyStrokes or Strings representing KeyStrokes
   * and odd indexed elements are the associated Actions.
   * 
   * @param keyBindingList the array of KeyStroke-Action pairs
   * @return a JTextComponent.KeyBinding array
   */
  public static JTextComponent.KeyBinding[] makeKeyBindings(
      Object[] keyBindingList)
  {
    JTextComponent.KeyBinding[] retBindings = 
      new JTextComponent.KeyBinding[keyBindingList.length / 2];
    for (int i = 0; i < keyBindingList.length - 1; i += 2)
      {
        KeyStroke stroke;
        if (keyBindingList[i] instanceof KeyStroke)
          stroke = (KeyStroke) keyBindingList[i];
        else
          stroke = KeyStroke.getKeyStroke((String) keyBindingList[i]);
        retBindings[i / 2] = new JTextComponent.KeyBinding(stroke, 
            (String) keyBindingList[i + 1]);
      }
    return retBindings;
  }

  /**
   * Invoked when the user attempts an invalid operation. The default 
   * implementation just beeps. Subclasses that wish to change this need to 
   * override this method.
   *
   * @param component the component the error occured in
   */
  public void provideErrorFeedback(Component component)
  {
    Toolkit.getDefaultToolkit().beep();
  }

  /**
   * Returns a string that displays and identifies this object's properties.
   *
   * @return string containing the description and class name.
   */
  public String toString()
  {
    return getDescription() + " " + getClass().getName();
  }

  /**
   * UIManager.setLookAndFeel calls this method just before we're replaced by
   * a new default look and feel. 
   */
  public void uninitialize()
  {
    // We do nothing here. This method is meant to be overridden by
    // LookAndFeel implementations.
  }

  /**
   * Convenience method for un-installing a component's default border on the
   * specified component if the border is currently an instance of UIResource.
   * 
   * @param c  the component (<code>null</code> not permitted).
   */
  public static void uninstallBorder(JComponent c)
  {
    if (c.getBorder() instanceof UIResource)
      c.setBorder(null);
  }

  /**
   * This methods installs a UI property if it hasn't already been set by an
   * application. This method is used by UI delegates that install a default
   * value for a property with a primitive type but do not want to override
   * a value that has been set by an application.
   *
   * The supported properties depend on the actual type of the component and
   * are listed in the table below. The supported properties are of course
   * inherited to subclasses.
   *
   * <table>
   * <tr><th>Type</th><th>Supported properties</th></tr>
   * <tr><td><code>JComponent</code></td>
   *     <td><code>opaque, autoscrolls</code></td></tr>
   * <tr><td><code>AbstractButton</code></td>
   *     <td><code>borderPainted, rolloverEnabled, iconTextGap,
   *      contentAreaFilled</code></td></tr>
   * <tr><td><code>JDesktopPane</code></td>
   *     <td><code>dragMode</code></td></tr>
   * <tr><td><code>JSplitPane</code></td>
   *     <td><code>dividerSize, oneTouchExpandable</code></td></tr>
   * <tr><td><code>JTable</code></td>
   *     <td><code>rowHeight</code></td></tr>
   * <tr><td><code>JTree</code></td>
   *     <td><code>rowHeight, scrollsOnExpand, showsRootHandles</code></td></tr>
   * </table>
   *
   * @param c the component to install the property to
   * @param propertyName the name of the property
   * @param value the value of the property
   *
   * @throws IllegalArgumentException if the specified property cannot be set
   *         by this method
   * @throws ClassCastException if the property value does not match the
   *         property type
   * @throws NullPointerException if <code>c</code> or
   *         <code>propertyValue</code> is <code>null</code>
   *
   * @since 1.5
   */
  public static void installProperty(JComponent c, String propertyName,
                                     Object value)
  {
    c.setUIProperty(propertyName, value);
  }
}
