/* UIDefaults.java -- database for all settings and interface bindings.
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
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Insets;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.lang.reflect.Method;
import java.util.Hashtable;
import java.util.LinkedList;
import java.util.ListIterator;
import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

import javax.swing.border.Border;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.InputMapUIResource;

/**
 * UIDefaults is a database where all settings and interface bindings are
 * stored into. A PLAF implementation fills one of these (see for example
 * plaf/basic/BasicLookAndFeel.java) with "ButtonUI" -&gt; new BasicButtonUI().
 *
 * @author Ronald Veldema (rveldema@cs.vu.nl)
 */
public class UIDefaults extends Hashtable<Object, Object>
{

  /** Our ResourceBundles. */
  private LinkedList bundles;

  /** The default locale. */
  private Locale defaultLocale;

  /** We use this for firing PropertyChangeEvents. */
  private PropertyChangeSupport propertyChangeSupport;

  /**
   * Used for lazy instantiation of UIDefaults values so that they are not
   * all loaded when a Swing application starts up, but only the values that
   * are really needed. An <code>ActiveValue</code> is newly instantiated
   * every time when the value is requested, as opposed to the normal
   * {@link LazyValue} that is only instantiated once.
   */
  public static interface ActiveValue
  {
    Object createValue(UIDefaults table);
  }

  public static class LazyInputMap implements LazyValue
  {
    Object[] bind;
    public LazyInputMap(Object[] bindings)
    {
      bind = bindings;
    }
    public Object createValue(UIDefaults table)
    {
      InputMapUIResource im = new InputMapUIResource();
      for (int i = 0; 2 * i + 1 < bind.length; ++i)
        {
          Object curr = bind[2 * i];
          if (curr instanceof KeyStroke)
            im.put((KeyStroke) curr, bind[2 * i + 1]);
          else
            im.put(KeyStroke.getKeyStroke((String) curr),
                  bind[2 * i + 1]);
        }
      return im;
    }
  }

  /**
   * Used for lazy instantiation of UIDefaults values so that they are not
   * all loaded when a Swing application starts up, but only the values that
   * are really needed. A <code>LazyValue</code> is only instantiated once,
   * as opposed to the {@link ActiveValue} that is newly created every time
   * it is requested.
   */
  public static interface LazyValue
  {
    Object createValue(UIDefaults table);
  }

  public static class ProxyLazyValue implements LazyValue
  {
    LazyValue inner;
    public ProxyLazyValue(String s)
    {
      final String className = s;
      inner = new LazyValue()
        { 
          public Object createValue(UIDefaults table) 
          {
            try
              {
                return Class
                  .forName(className)
                  .getConstructor(new Class[] {})
                  .newInstance(new Object[] {});
              }
            catch (Exception e)
              {
                return null;
              }
          }
        };
    }

    public ProxyLazyValue(String c, String m)
    {
      final String className = c;
      final String methodName = m;
      inner = new LazyValue()
        { 
          public Object createValue(UIDefaults table) 
          {
            try 
              {                
                return Class
                  .forName(className)
                  .getMethod(methodName, new Class[] {})
                  .invoke(null, new Object[] {});
              }
            catch (Exception e)
              {
                return null;
              }
          }
        };
    }
    
    public ProxyLazyValue(String c, Object[] os)
    {
      final String className = c;
      final Object[] objs = os;
      final Class[] clss = new Class[objs.length];
      for (int i = 0; i < objs.length; ++i)
        {
          clss[i] = objs[i].getClass();
        }      
      inner = new LazyValue()
        { 
          public Object createValue(UIDefaults table) 
          {            
            try
              {
                return Class
                  .forName(className)
                  .getConstructor(clss)
                  .newInstance(objs);
	      }
            catch (Exception e)
	      {
                return null;
              }
          }
        };
    }

    public ProxyLazyValue(String c, String m, Object[] os)
    {
      final String className = c;
      final String methodName = m;
      final Object[] objs = os;
      final Class[] clss = new Class[objs.length];
      for (int i = 0; i < objs.length; ++i)
	{
          clss[i] = objs[i].getClass();
	}
      inner = new LazyValue()
        { 
	  public Object createValue(UIDefaults table)
	  {
            try 
              {
                return Class
                  .forName(className)
                  .getMethod(methodName, clss)
                  .invoke(null, objs);
              }
            catch (Exception e)
              {
                return null;
              }
          }
        };
    }
    
    public Object createValue(UIDefaults table)
    {
      return inner.createValue(table);
    }
  }

  /** Our serialVersionUID for serialization. */
  private static final long serialVersionUID = 7341222528856548117L;

  /**
   * Constructs a new empty UIDefaults instance.
   */
  public UIDefaults()
  {
    bundles = new LinkedList();
    defaultLocale = Locale.getDefault();
    propertyChangeSupport = new PropertyChangeSupport(this);
  }

  /**
   * Constructs a new UIDefaults instance and loads the specified entries.
   * The entries are expected to come in pairs, that means
   * <code>entries[0]</code> is a key, <code>entries[1]</code> is a value,
   * <code>entries[2]</code> a key and so forth.
   *
   * @param entries the entries to initialize the UIDefaults instance with
   */
  public UIDefaults(Object[] entries)
  {
    this();
    
    for (int i = 0; (2 * i + 1) < entries.length; ++i)
      put(entries[2 * i], entries[2 * i + 1]);
  }

  /**
   * Returns the entry for the specified <code>key</code> in the default
   * locale.
   *
   * @return the entry for the specified <code>key</code>
   */
  public Object get(Object key)
  {
    return this.get(key, getDefaultLocale());
  }

  /**
   * Returns the entry for the specified <code>key</code> in the Locale
   * <code>loc</code>.
   *
   * @param key the key for which we return the value
   * @param loc the locale
   */
  public Object get(Object key, Locale loc)
  {
    Object obj = null;

    if (super.containsKey(key))
      {
        obj = super.get(key);
      }
    else if (key instanceof String)
      {
        String keyString = (String) key;
        ListIterator i = bundles.listIterator(0);
        while (i.hasNext())
	  {
            String bundle_name = (String) i.next();
            ResourceBundle res =
              ResourceBundle.getBundle(bundle_name, loc);
            if (res != null)
              {
                try 
                  {                    
                    obj = res.getObject(keyString);
                    break;
                  }
                catch (MissingResourceException me)
                  {
                    // continue, this bundle has no such key
                  }
              }
          }
      }

    // now we've found the object, resolve it.
    // nb: LazyValues aren't supported in resource bundles, so it's correct
    // to insert their results in the locale-less hashtable.

    if (obj == null)
      return null;

    if (obj instanceof LazyValue)
      {
        Object resolved = ((LazyValue) obj).createValue(this);
        super.remove(key);
        super.put(key, resolved);
        return resolved;
      }
    else if (obj instanceof ActiveValue)
      {
        return ((ActiveValue) obj).createValue(this);
      }    

    return obj;
  }

  /**
   * Puts a key and value into this UIDefaults object.<br>
   * In contrast to
   * {@link java.util.Hashtable}s <code>null</code>-values are accepted
   * here and treated like #remove(key).
   * <br>
   * This fires a PropertyChangeEvent with key as name and the old and new
   * values.
   *
   * @param key the key to put into the map
   * @param value the value to put into the map
   *
   * @return the old value for key or <code>null</code> if <code>key</code>
   *     had no value assigned
   */
  public Object put(Object key, Object value)
  {
    Object old = checkAndPut(key, value);

    if (key instanceof String && old != value)
      firePropertyChange((String) key, old, value);
    return old;
  }

  /**
   * Puts a set of key-value pairs into the map.
   * The entries are expected to come in pairs, that means
   * <code>entries[0]</code> is a key, <code>entries[1]</code> is a value,
   * <code>entries[2]</code> a key and so forth.
   * <br>
   * If a value is <code>null</code> it is treated like #remove(key).
   * <br>
   * This unconditionally fires a PropertyChangeEvent with
   * <code>&apos;UIDefaults&apos;</code> as name and <code>null</code> for
   * old and new value.
   *
   * @param entries the entries to be put into the map
   */
  public void putDefaults(Object[] entries)
  {
    for (int i = 0; (2 * i + 1) < entries.length; ++i)
  {
        checkAndPut(entries[2 * i], entries[2 * i + 1]);
      }
    firePropertyChange("UIDefaults", null, null);
  }

  /**
   * Checks the value for <code>null</code> and put it into the Hashtable, if
   * it is not <code>null</code>. If the value is <code>null</code> then
   * remove the corresponding key.
   *
   * @param key the key to put into this UIDefauls table
   * @param value the value to put into this UIDefaults table
   *
   * @return the old value for <code>key</code>
   */
  private Object checkAndPut(Object key, Object value)
  {
    Object old;

    if (value != null)
      old = super.put(key, value);
    else
      old = super.remove(key);

    return old;
  }

  /**
   * Returns a font entry for the default locale.
   *
   * @param key the key to the requested entry
   *
   * @return the font entry for <code>key</code> or null if no such entry
   *     exists
   */
  public Font getFont(Object key)
  {
    Object o = get(key);
    return o instanceof Font ? (Font) o : null;
  }

  /**
   * Returns a font entry for a specic locale.
   *
   * @param key the key to the requested entry
   * @param locale the locale to the requested entry
   *
   * @return the font entry for <code>key</code> or null if no such entry
   *     exists
   */
  public Font getFont(Object key, Locale locale)
  {
    Object o = get(key, locale);
    return o instanceof Font ? (Font) o : null;
  }

  /**
   * Returns a color entry for the default locale.
   *
   * @param key the key to the requested entry
   *
   * @return the color entry for <code>key</code> or null if no such entry
   *     exists
   */
  public Color getColor(Object key)
  {
    Object o = get(key);
    return o instanceof Color ? (Color) o : null;
  }

  /**
   * Returns a color entry for a specic locale.
   *
   * @param key the key to the requested entry
   * @param locale the locale to the requested entry
   *
   * @return the color entry for <code>key</code> or null if no such entry
   *     exists
   */
  public Color getColor(Object key, Locale locale)
  {
    Object o = get(key, locale);
    return o instanceof Color ? (Color) o : null;
  }

  /**
   * Returns an icon entry for the default locale.
   *
   * @param key the key to the requested entry
   *
   * @return the icon entry for <code>key</code> or null if no such entry
   *     exists
   */
  public Icon getIcon(Object key)
  {
    Object o = get(key);
    return o instanceof Icon ? (Icon) o : null;
  }

  /**
   * Returns an icon entry for a specic locale.
   *
   * @param key the key to the requested entry
   * @param locale the locale to the requested entry
   *
   * @return the icon entry for <code>key</code> or null if no such entry
   *     exists
   */
  public Icon getIcon(Object key, Locale locale)
  {
    Object o = get(key, locale);
    return o instanceof Icon ? (Icon) o : null;
  }

  /**
   * Returns a border entry for the default locale.
   *
   * @param key the key to the requested entry
   *
   * @return the border entry for <code>key</code> or null if no such entry
   *     exists
   */
  public Border getBorder(Object key)
  {
    Object o = get(key);
    return o instanceof Border ? (Border) o : null;
  }

  /**
   * Returns a border entry for a specic locale.
   *
   * @param key the key to the requested entry
   * @param locale the locale to the requested entry
   *
   * @return the border entry for <code>key</code> or null if no such entry
   *     exists
   */
  public Border getBorder(Object key, Locale locale)
  {
    Object o = get(key, locale);
    return o instanceof Border ? (Border) o : null;
  }

  /**
   * Returns a string entry for the default locale.
   *
   * @param key the key to the requested entry
   *
   * @return the string entry for <code>key</code> or null if no such entry
   *     exists
   */
  public String getString(Object key)
  {
    Object o = get(key);
    return o instanceof String ? (String) o : null;
  }

  /**
   * Returns a string entry for a specic locale.
   *
   * @param key the key to the requested entry
   * @param locale the locale to the requested entry
   *
   * @return the string entry for <code>key</code> or null if no such entry
   *     exists
   */
  public String getString(Object key, Locale locale)
  {
    Object o = get(key, locale);
    return o instanceof String ? (String) o : null;
  }

  /**
   * Returns an integer entry for the default locale.
   *
   * @param key the key to the requested entry
   *
   * @return the integer entry for <code>key</code> or null if no such entry
   *     exists
   */
  public int getInt(Object key)
  {
    Object o = get(key);
    return o instanceof Integer ? ((Integer) o).intValue() : 0;
  }

  /**
   * Returns an integer entry for a specic locale.
   *
   * @param key the key to the requested entry
   * @param locale the locale to the requested entry
   *
   * @return the integer entry for <code>key</code> or null if no such entry
   *     exists
   */
  public int getInt(Object key, Locale locale)
  {
    Object o = get(key, locale);
    return o instanceof Integer ? ((Integer) o).intValue() : 0;
  }

  /**
   * Returns a boolean entry for the default locale.
   *
   * @param key the key to the requested entry
   *
   * @return The boolean entry for <code>key</code> or <code>false</code> if no 
   *         such entry exists.
   */
  public boolean getBoolean(Object key)
  {
    return Boolean.TRUE.equals(get(key));
  }

  /**
   * Returns a boolean entry for a specic locale.
   *
   * @param key the key to the requested entry
   * @param locale the locale to the requested entry
   *
   * @return the boolean entry for <code>key</code> or null if no such entry
   *     exists
   */
  public boolean getBoolean(Object key, Locale locale)
  {
    return Boolean.TRUE.equals(get(key, locale));
  }

  /**
   * Returns an insets entry for the default locale.
   *
   * @param key the key to the requested entry
   *
   * @return the insets entry for <code>key</code> or null if no such entry
   *     exists
   */
  public Insets getInsets(Object key) 
  {
    Object o = get(key);
    return o instanceof Insets ? (Insets) o : null;
  }

  /**
   * Returns an insets entry for a specic locale.
   *
   * @param key the key to the requested entry
   * @param locale the locale to the requested entry
   *
   * @return the boolean entry for <code>key</code> or null if no such entry
   *     exists
   */
  public Insets getInsets(Object key, Locale locale) 
  {
    Object o = get(key, locale);
    return o instanceof Insets ? (Insets) o : null;
  }

  /**
   * Returns a dimension entry for the default locale.
   *
   * @param key the key to the requested entry
   *
   * @return the dimension entry for <code>key</code> or null if no such entry
   *     exists
   */
  public Dimension getDimension(Object key) 
  {
    Object o = get(key);
    return o instanceof Dimension ? (Dimension) o : null;
  }

  /**
   * Returns a dimension entry for a specic locale.
   *
   * @param key the key to the requested entry
   * @param locale the locale to the requested entry
   *
   * @return the boolean entry for <code>key</code> or null if no such entry
   *     exists
   */
  public Dimension getDimension(Object key, Locale locale) 
  {
    Object o = get(key, locale);
    return o instanceof Dimension ? (Dimension) o : null;
  }

  /**
   * Returns the ComponentUI class that renders a component. <code>id</code>
   * is the ID for which the String value of the classname is stored in
   * this UIDefaults map.
   *
   * @param id the ID of the UI class
   * @param loader the ClassLoader to use
   *
   * @return the UI class for <code>id</code>
   */
  public Class<? extends ComponentUI> getUIClass(String id, ClassLoader loader)
  {
    String className = (String) get(id);
    if (className == null)
      return null;
    try 
      {
        if (loader == null)
          loader = ClassLoader.getSystemClassLoader();
        return (Class<? extends ComponentUI>) loader.loadClass (className);
      }
    catch (Exception e)
      {
        return null;
      }
  }

  /**
   * Returns the ComponentUI class that renders a component. <code>id</code>
   * is the ID for which the String value of the classname is stored in
   * this UIDefaults map.
   *
   * @param id the ID of the UI class
   *
   * @return the UI class for <code>id</code>
   */
  public Class<? extends ComponentUI> getUIClass(String id)
  {
    return getUIClass (id, null);
  }

  /**
   * If a key is requested in #get(key) that has no value, this method
   * is called before returning <code>null</code>.
   *
   * @param msg the error message
   */
  protected void getUIError(String msg)
  {
    System.err.println ("UIDefaults.getUIError: " + msg);
  }

  /**
   * Returns the {@link ComponentUI} for the specified {@link JComponent}.
   *
   * @param target the component for which the ComponentUI is requested
   *
   * @return the {@link ComponentUI} for the specified {@link JComponent}
   */
  public ComponentUI getUI(JComponent target)
  {
    String classId = target.getUIClassID ();
    Class cls = getUIClass (classId);
    if (cls == null)
      {
        getUIError ("failed to locate UI class:" + classId);
        return null;
      }

    Method factory;

    try 
      {
        factory = cls.getMethod ("createUI", new Class[] { JComponent.class } );
      }
    catch (NoSuchMethodException nme)
      {
        getUIError ("failed to locate createUI method on " + cls.toString ());
        return null;
      }

    try
      {
        return (ComponentUI) factory.invoke (null, new Object[] { target });
      }
    catch (java.lang.reflect.InvocationTargetException ite)
      {
        getUIError ("InvocationTargetException ("+ ite.getTargetException() 
		    +") calling createUI(...) on " + cls.toString ());
        return null;        
      }
    catch (Exception e)
      {
        getUIError ("exception calling createUI(...) on " + cls.toString ());
        return null;        
      }
  }

  /**
   * Adds a {@link PropertyChangeListener} to this UIDefaults map.
   * Registered PropertyChangeListener are notified when values
   * are beeing put into this UIDefaults map.
   *
   * @param listener the PropertyChangeListener to add
   */
  public void addPropertyChangeListener(PropertyChangeListener listener)
  {
    propertyChangeSupport.addPropertyChangeListener(listener);
  }

  /**
   * Removes a PropertyChangeListener from this UIDefaults map.
   *
   * @param listener the PropertyChangeListener to remove
   */
  public void removePropertyChangeListener(PropertyChangeListener listener)
  {
    propertyChangeSupport.removePropertyChangeListener(listener);
  }

  /**
   * Returns an array of all registered PropertyChangeListeners.
   *
   * @return all registered PropertyChangeListeners
   */
  public PropertyChangeListener[] getPropertyChangeListeners()
  {
    return propertyChangeSupport.getPropertyChangeListeners();
  }

  /**
   * Fires a PropertyChangeEvent.
   *
   * @param property the property name
   * @param oldValue the old value
   * @param newValue the new value
   */
  protected void firePropertyChange(String property,
				    Object oldValue, Object newValue)
  {
    propertyChangeSupport.firePropertyChange(property, oldValue, newValue);
  }

  /**
   * Adds a ResourceBundle for localized values.
   *
   * @param name the name of the ResourceBundle to add
   */
  public void addResourceBundle(String name)
  {
    bundles.addFirst(name);
  }

  /**
   * Removes a ResourceBundle.
   *
   * @param name the name of the ResourceBundle to remove
   */
  public void removeResourceBundle(String name)
  {
    bundles.remove(name);
  }

  /**
   * Sets the current locale to <code>loc</code>.
   *
   * @param loc the Locale to be set
   */
  public void setDefaultLocale(Locale loc)
  {
    defaultLocale = loc;
  }

  /**
   * Returns the current default locale.
   *
   * @return the current default locale
   */
  public Locale getDefaultLocale()
  {
    return defaultLocale;
  }
}
