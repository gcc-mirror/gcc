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
import java.beans.PropertyChangeListener;
import java.util.Hashtable;
import java.util.Locale;
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
  public interface ActiveValue
  {
    Object createValue(UIDefaults table);
  } // interface ActiveValue

  public static class LazyInputMap implements LazyValue
  {
    public LazyInputMap(Object[] bindings)
    {
    }
    public Object createValue(UIDefaults table)
    {
      throw new Error("not implemented");
    }
  } // class LazyInputMap

  public interface LazyValue
  {
    Object createValue(UIDefaults table);
  } // interface LazyValue

  public static class ProxyLazyValue
  {
    public ProxyLazyValue(String s)
    {
      throw new Error("not implemented");
    }
    public ProxyLazyValue(String c, String m)
    {
      throw new Error("not implemented");
    }
    public ProxyLazyValue(String c, Object[] o)
    {
      throw new Error("not implemented");
    }
    public ProxyLazyValue(String c, String m, Object[] o)
    {
      throw new Error("not implemented");
    }
    public Object createValue(UIDefaults table)
    {
      throw new Error("not implemented");
    }
  } // class ProxyLazyValue

  public UIDefaults()
  {
  }

  public UIDefaults(Object[] entries)
  {
    // XXX
  }

  public Object get(Object key)
  {
    // XXX Obey 1.4 specs
    return super.get(key);
  }

  public Object get(Object key, Locale l)
  {
    throw new Error("not implemented");
  }

  public Object put(Object key, Object value)
  {
    throw new Error("not implemented");
  }

  public void putDefaults(Object[] list)
  {
    throw new Error("not implemented");
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

  public int getInt(Object key)
  {
    Object o = get(key);
    return o instanceof Integer ? ((Integer) o).intValue() : 0;
  }

  public int getInt(Object key, Locale l)
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
    throw new Error("not implemented");
  }

  public Class getUIClass(String id)
  {
    throw new Error("not implemented");
  }

  protected void getUIError(String msg)
  {
    // Does nothing unless overridden.
  }

  public ComponentUI getUI(JComponent a)
  {
    String pp = a.getUIClassID();
    ComponentUI p = (ComponentUI) get(pp);
    if (p == null)
      getUIError("failed to locate UI:" + pp);
    return p;
  }

  public void addPropertyChangeListener(PropertyChangeListener l)
  {
    throw new Error("not implemented");
  }

  public void removePropertyChangeListener(PropertyChangeListener l)
  {
    throw new Error("not implemented");
  }

  public PropertyChangeListener[] getPropertyChangeListeners()
  {
    throw new Error("not implemented");
  }

  protected void firePropertyChange(String property, Object o, Object n)
  {
    throw new Error("not implemented");
  }

  public void addResourceBundle(String name)
  {
    throw new Error("not implemented");
  }

  public void removeResourceBundle(String name)
  {
    throw new Error("not implemented");
  }

  public void setDefaultLocale(Locale l)
  {
    throw new Error("not implemented");
  }

  public Locale getDefaultLocale()
  {
    throw new Error("not implemented");
  }
} // class UIDefaults
