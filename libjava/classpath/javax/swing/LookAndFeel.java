/* LookAndFeel.java --
   Copyright (C) 2002, 2004, 2005  Free Software Foundation, Inc.

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
import javax.swing.text.JTextComponent;

public abstract class LookAndFeel
{
  /**
   * This method is called once by UIManager.setLookAndFeel to create
   * the look and feel specific defaults table.
   *
   * @return the UI defaults
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

  public static Object getDesktopPropertyValue(String systemPropertyName, 
      Object fallbackValue)
  {
    Object value = Toolkit.getDefaultToolkit().getDesktopProperty(systemPropertyName);
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
   * Returns true when the Look and Feel supports window decorations,
   * false others. This method returns always false and needs to be overwritten
   * when the derived Look and Feel supports this.
   *
   * @return false
   *
   * @since 1.4
   */
  public boolean getSupportsWindowDecorations()
  {
    return false;
  }
  
  /**
   * UIManager.setLookAndFeel calls this method before the first call
   * (and typically the only call) to getDefaults(). 
   */
  public void initialize()
  {
    // We do nothing here. This method is meant to be overridden by
    // LookAndFeel implementations.
  }

  /**
   * Convenience method for installing a component's default Border object
   * on the specified component if either the border is currently null
   * or already an instance of UIResource. 
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
   * Convenience method for initializing a components foreground background
   * and font properties with values from the current defaults table. 
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
   * Returns <code>true</code> if the look and feel is the "native" look and
   * feel for the current platform, and <code>false</code> otherwise.
   * 
   * @return A flag indicating whether or not this is the native look and feel
   *         for the current platform.
   */
  public abstract boolean isNativeLookAndFeel();

  /**
   * Returns <code>true</code> if the look and feel is supported on the 
   * current operating system, and <code>false</code> otherwise.  This 
   * mechanism is provided so that it is possible to prevent a look and feel
   * from being used on some operating systems (usually for legal, not
   * technical, reasons).
   * 
   * @return A flag indicating whether or not the look and feel is supported
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
    for (int i = 0; i < keys.length - 1; i+= 2)
      {
        Object key = keys[i];
        KeyStroke keyStroke;
        if (key instanceof KeyStroke)
          keyStroke = (KeyStroke)key;
        else
          keyStroke = KeyStroke.getKeyStroke((String)key);
        retMap.put(keyStroke, keys[i+1]);
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
   */
  public static Object makeIcon(Class baseClass, String gifFile)
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
  public static JTextComponent.KeyBinding[] makeKeyBindings(Object[] keyBindingList)
  {
    JTextComponent.KeyBinding[] retBindings = 
      new JTextComponent.KeyBinding[keyBindingList.length / 2];
    for (int i = 0; i < keyBindingList.length - 1; i+= 2)
      {
        KeyStroke stroke;
        if (keyBindingList[i] instanceof KeyStroke)
          stroke = (KeyStroke)keyBindingList[i];
        else
          stroke = KeyStroke.getKeyStroke((String)keyBindingList[i]);
        retBindings[i/2] = new JTextComponent.KeyBinding(stroke, (String)keyBindingList[i+1]);
      }
    return retBindings;
  }

  /**
   * Invoked when the user attempts an invalid operation. The default implement
   * just beeps. Subclasses that wish to change this need to override this
   * method.
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
   * @return the string "LookAndFeel"
   */
  public String toString()
  {
    return "LookAndFeel";
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
   */
  public static void uninstallBorder(JComponent c)
  {
    if (c.getBorder() instanceof UIResource)
      c.setBorder(null);
  }
}
