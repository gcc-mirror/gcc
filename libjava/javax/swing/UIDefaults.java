/* UIDefaults.java -- database for all settings and interface bindings.
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

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Insets;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.lang.reflect.Method;
import java.lang.reflect.Constructor;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.LinkedList;
import java.util.Locale;
import java.util.Set;
import java.util.HashSet;
import java.util.MissingResourceException;
import java.util.ResourceBundle;
import javax.swing.border.Border;
import javax.swing.plaf.ComponentUI;

/**
 * UIDefaults is a database where all settings and interface bindings are
 * stored into. An PLAF implementation fills one of these (see for example
 * plaf/basic/BasicDefaults.java) with "JButton" -> new BasicButtonUI().
 *
 * @author Ronald Veldema (rveldema@cs.vu.nl)
 */
public class UIDefaults extends Hashtable
{

  LinkedList bundles;
  Set listeners;
  Locale defaultLocale;

  interface ActiveValue
  {
    Object createValue(UIDefaults table);
  } // interface ActiveValue

  public static class LazyInputMap implements LazyValue
  {
    Object[] bind;
    public LazyInputMap(Object[] bindings)
    {
      bind = bindings;
    }
    public Object createValue(UIDefaults table)
    {
      InputMap im = new InputMap ();
      for (int i = 0; 2*i+1 < bind.length; ++i)
        {
          im.put (KeyStroke.getKeyStroke ((String) bind[2*i]),
                  bind[2*i+1]);
        }
      return im;
    }
  } // class LazyInputMap

  interface LazyValue
  {
    Object createValue(UIDefaults table);
  } // interface LazyValue

  public static class ProxyLazyValue implements LazyValue
  {
    LazyValue inner;
    public ProxyLazyValue(String s)
    {
      final String className = s;
      inner = new LazyValue ()
        { 
          public Object createValue (UIDefaults table) 
          {
            try
              {
                return Class
                  .forName (className)
                  .getConstructor (new Class[] {})
                  .newInstance (new Object[] {});
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
      inner = new LazyValue ()
        { 
          public Object createValue (UIDefaults table) 
          {
            try 
              {                
                return Class
                  .forName (className)
                  .getMethod (methodName, new Class[] {})
                  .invoke (null, new Object[] {});
              }
            catch (Exception e)
              {
                return null;
              }
          }
        };
    }
    
    public ProxyLazyValue (String c, Object[] os)
    {
      final String className = c;
      final Object[] objs = os;
      final Class[] clss = new Class[objs.length];
      for (int i = 0; i < objs.length; ++i)
        {
          clss[i] = objs[i].getClass ();
        }      
      inner = new LazyValue ()
        { 
          public Object createValue (UIDefaults table) 
          {            
            try
              {
                return Class
                  .forName (className)
                  .getConstructor (clss)
                  .newInstance (objs);
    }
            catch (Exception e)
    {
                return null;
              }
          }
        };
    }

    public ProxyLazyValue (String c, String m, Object[] os)
    {
      final String className = c;
      final String methodName = m;
      final Object[] objs = os;
      final Class[] clss = new Class[objs.length];
      for (int i = 0; i < objs.length; ++i)
    {
          clss[i] = objs[i].getClass ();
    }
      inner = new LazyValue ()
        { 
    public Object createValue(UIDefaults table)
    {
            try 
              {
                return Class
                  .forName (className)
                  .getMethod (methodName, clss)
                  .invoke (null, objs);
              }
            catch (Exception e)
              {
                return null;
              }
          }
        };
    }
    
    public Object createValue (UIDefaults table)
    {
      return inner.createValue (table);
    }
  } // class ProxyLazyValue

  private static final long serialVersionUID = 7341222528856548117L;

  public UIDefaults()
  {
    bundles = new LinkedList ();
    listeners = new HashSet ();
    defaultLocale = Locale.getDefault ();
  }

  public UIDefaults(Object[] entries)
  {
    bundles = new LinkedList ();
    listeners = new HashSet ();
    defaultLocale = Locale.getDefault ();

    for (int i = 0; (2*i+1) < entries.length; ++i)
      {
        put (entries[2*i], entries[2*i+1]);
      }
  }

  public Object get(Object key)
  {
    return this.get (key, getDefaultLocale ());
  }

  public Object get (Object key, Locale loc)
  {
    Object obj = null;

    if (super.containsKey (key))
      {
        obj = super.get (key);
      }
    else if (key instanceof String)
      {
        String keyString = (String) key;
        ListIterator i = bundles.listIterator (0);
        while (i.hasNext ())
  {
            String bundle_name = (String) i.next ();
            ResourceBundle res =
              ResourceBundle.getBundle (bundle_name, loc);
            if (res != null)
              {
                try 
                  {                    
                    obj = res.getObject (keyString);
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
        Object resolved = ((LazyValue)obj).createValue (this);
        super.remove (key);
        super.put (key, resolved);
        return resolved;
      }
    else if (obj instanceof ActiveValue)
      {
        return ((ActiveValue)obj).createValue (this);
      }    

    return obj;
  }

  public Object put(Object key, Object value)
  {
    Object old = super.put (key, value);
    if (key instanceof String && old != value)
      firePropertyChange ((String) key, old, value);
    return old;
  }

  public void putDefaults(Object[] entries)
  {
    for (int i = 0; (2*i+1) < entries.length; ++i)
  {
        super.put (entries[2*i], entries[2*i+1]);
      }
    firePropertyChange ("UIDefaults", null, null);
  }

  public Font getFont(Object key)
  {
    Object o = get(key);
    return o instanceof Font ? (Font) o : null;
  }

  public Font getFont(Object key, Locale l)
  {
    Object o = get(key, l);
    return o instanceof Font ? (Font) o : null;
  }

  public Color getColor(Object key)
  {
    Object o = get(key);
    return o instanceof Color ? (Color) o : null;
  }

  public Color getColor(Object key, Locale l)
  {
    Object o = get(key, l);
    return o instanceof Color ? (Color) o : null;
  }

  public Icon getIcon(Object key)
  {
    Object o = get(key);
    return o instanceof Icon ? (Icon) o : null;
  }

  public Icon getIcon(Object key, Locale l)
  {
    Object o = get(key, l);
    return o instanceof Icon ? (Icon) o : null;
  }

  public Border getBorder(Object key)
  {
    Object o = get(key);
    return o instanceof Border ? (Border) o : null;
  }

  public Border getBorder(Object key, Locale l)
  {
    Object o = get(key, l);
    return o instanceof Border ? (Border) o : null;
  }

  public String getString(Object key)
  {
    Object o = get(key);
    return o instanceof String ? (String) o : null;
  }

  public String getString(Object key, Locale l)
  {
    Object o = get(key, l);
    return o instanceof String ? (String) o : null;
  }

  int getInt(Object key)
  {
    Object o = get(key);
    return o instanceof Integer ? ((Integer) o).intValue() : 0;
  }

  int getInt(Object key, Locale l)
  {
    Object o = get(key, l);
    return o instanceof Integer ? ((Integer) o).intValue() : 0;
  }

  public boolean getBoolean(Object key)
  {
    return Boolean.TRUE.equals(get(key));
  }

  public boolean getBoolean(Object key, Locale l)
  {
    return Boolean.TRUE.equals(get(key, l));
  }

  public Insets getInsets(Object key) 
  {
    Object o = get(key);
    return o instanceof Insets ? (Insets) o : null;
  }

  public Insets getInsets(Object key, Locale l) 
  {
    Object o = get(key, l);
    return o instanceof Insets ? (Insets) o : null;
  }

  public Dimension getDimension(Object key) 
  {
    Object o = get(key);
    return o instanceof Dimension ? (Dimension) o : null;
  }

  public Dimension getDimension(Object key, Locale l) 
  {
    Object o = get(key, l);
    return o instanceof Dimension ? (Dimension) o : null;
  }

  public Class getUIClass(String id, ClassLoader loader)
  {
    String className = (String) get (id);
    if (className == null)
      return null;
    try 
      {
        if (loader != null)
          return loader.loadClass (className);    
        return Class.forName (className);
      }
    catch (Exception e)
      {
        return null;
      }
  }

  public Class getUIClass(String id)
  {
    return getUIClass (id, null);
  }

  protected void getUIError(String msg)
  {
    System.err.println ("UIDefaults.getUIError: " + msg);
  }

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

  void addPropertyChangeListener(PropertyChangeListener listener)
  {
    listeners.add (listener);
  }

  void removePropertyChangeListener(PropertyChangeListener listener)
  {
    listeners.remove (listener);
  }

  public PropertyChangeListener[] getPropertyChangeListeners()
  {
    return (PropertyChangeListener[]) listeners.toArray ();
  }

  protected void firePropertyChange(String property, Object o, Object n)
  {
    Iterator i = listeners.iterator ();
    PropertyChangeEvent pce = new PropertyChangeEvent (this, property, o, n);
    while (i.hasNext ())
      {
        PropertyChangeListener pcl = (PropertyChangeListener) i.next ();
        pcl.propertyChange (pce);
      }
  }

  void addResourceBundle(String name)
  {
    bundles.addFirst (name);
  }

  void removeResourceBundle(String name)
  {
    bundles.remove (name);
  }

  void setDefaultLocale(Locale loc)
  {
    defaultLocale = loc;
  }

  public Locale getDefaultLocale()
  {
    return defaultLocale;
  }
} // class UIDefaults
