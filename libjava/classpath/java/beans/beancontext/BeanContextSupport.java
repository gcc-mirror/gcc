/* BeanContextSupport.java --
   Copyright (C) 2003, 2005  Free Software Foundation, Inc.

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


package java.beans.beancontext;

import gnu.classpath.NotImplementedException;

import java.beans.DesignMode;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyVetoException;
import java.beans.VetoableChangeListener;
import java.beans.Visibility;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Locale;

/**
 * @author Michael Koch
 * @since 1.2
 */
public class BeanContextSupport extends BeanContextChildSupport
  implements BeanContext, Serializable, PropertyChangeListener,
  VetoableChangeListener
{
  private static final long serialVersionUID = -4879613978649577204L;

  // This won't show up in japi, but we mark it as a stub anyway,
  // so that searches for NotImplementedException will find it.
  private void readObject (ObjectInputStream s)
    throws ClassNotFoundException, IOException, NotImplementedException
  {
    throw new Error ("Not implemented");
  }

  // This won't show up in japi, but we mark it as a stub anyway,
  // so that searches for NotImplementedException will find it.
  private void writeObject (ObjectOutputStream s)
    throws ClassNotFoundException, IOException, NotImplementedException
  {
    throw new Error ("Not implemented");
  }

  protected class BCSChild implements Serializable
  {
    private static final long serialVersionUID = -5815286101609939109L;

    private Object targetChild;
    private Object peer;

    BCSChild(Object targetChild, Object peer)
    {
      this.targetChild = targetChild;
      this.peer = peer;
    }
  }

  protected static final class BCSIterator implements Iterator
  {
    private Iterator child;

    BCSIterator(Iterator child)
    {
      this.child = child;
    }

    public boolean hasNext ()
    {
      return child.hasNext();
    }

    public Object next ()
    {
      return child.next();
    }

    public void remove ()
    {
      // This must be a noop remove operation.
    }
  }

  protected transient ArrayList bcmListeners;

  protected transient HashMap children;

  protected transient boolean designTime;

  protected transient Locale locale;

  protected transient boolean okToUseGui;

  /**
   * Construct a BeanContextSupport instance.
   */
  public BeanContextSupport ()
  {
    this (null, null, true, true);
  }

  /**
   * Construct a BeanContextSupport instance.
   */
  public BeanContextSupport (BeanContext peer)
  {
    this (peer, null, true, true);
  }

  /**
   * Construct a BeanContextSupport instance.
   */
  public BeanContextSupport (BeanContext peer, Locale lcle)
  {
    this (peer, lcle, true, true);
  }

  /**
   * Construct a BeanContextSupport instance.
   */
  public BeanContextSupport (BeanContext peer, Locale lcle, boolean dtime)
  {
    this (peer, lcle, dtime, true);
  }

  /**
   * Construct a BeanContextSupport instance.
   */
  public BeanContextSupport (BeanContext peer, Locale lcle, boolean dtime,
                             boolean visible)
  {
    super(peer);

    locale = lcle == null ? Locale.getDefault() : lcle;
    designTime = dtime;
    okToUseGui = visible;

    initialize ();
  }

  public boolean add (Object targetChild)
  {
    if (targetChild == null)
      throw new IllegalArgumentException();

    BCSChild child;
    synchronized (children)
      {
        if (children.containsKey(targetChild)
            || ! validatePendingAdd(targetChild))
          return false;
        child = createBCSChild(targetChild, beanContextChildPeer);
        children.put(targetChild, child);
      }
    synchronized (targetChild)
      {
        childJustAddedHook(targetChild, child);
      }
    fireChildrenAdded(new BeanContextMembershipEvent(this,
                                                     new Object[] { targetChild }));
    return true;
  }

  public boolean addAll (Collection c)
  {
    // Intentionally throws an exception.
    throw new UnsupportedOperationException();
  }

  public void addBeanContextMembershipListener
    (BeanContextMembershipListener listener)
  {
    synchronized (bcmListeners)
      {
        if (! bcmListeners.contains(listener))
          bcmListeners.add(listener);
      }
  }

  public boolean avoidingGui ()
    throws NotImplementedException
  {
    throw new Error ("Not implemented");
  }

  protected Iterator bcsChildren ()
  {
    synchronized (children)
      {
        return new BCSIterator(children.values().iterator());
      }
  }

  protected void bcsPreDeserializationHook (ObjectInputStream ois)
    throws ClassNotFoundException, IOException, NotImplementedException
  {
    throw new Error ("Not implemented");
  }

  protected void bcsPreSerializationHook (ObjectOutputStream oos)
    throws IOException, NotImplementedException
  {
    throw new Error ("Not implemented");
  }

  protected void childDeserializedHook (Object child, BeanContextSupport.BCSChild bcsc)
    throws NotImplementedException
  {
    throw new Error ("Not implemented");
  }

  protected void childJustAddedHook (Object child, BeanContextSupport.BCSChild bcsc)
  {
    // Do nothing in the base class.
  }

  protected void childJustRemovedHook (Object child, BeanContextSupport.BCSChild bcsc)
  {
    // Do nothing in the base class.
  }

  protected static final boolean classEquals (Class first, Class second)
  {
    // Lame function!
    return (first == second || first.getName().equals(second.getName()));
  }

  public void clear ()
  {
    // This is the right thing to do.
    // The JDK docs are really bad here.
    throw new UnsupportedOperationException();
  }

  public boolean contains (Object o)
  {
    synchronized (children)
      {
        return children.containsKey(o);
      }
  }

  public boolean containsAll (Collection c)
  {
    synchronized (children)
      {
        Iterator it = c.iterator();
        while (it.hasNext())
          if (! children.containsKey(it.next()))
            return false;
      }
    return true;
  }

  public boolean containsKey (Object o)
  {
    synchronized (children)
      {
        return children.containsKey(o);
      }
  }

  protected final Object[] copyChildren ()
  {
    synchronized (children)
      {
        return children.keySet().toArray();
      }
  }

  protected BeanContextSupport.BCSChild createBCSChild (Object targetChild, Object peer)
  {
    return new BCSChild(targetChild, peer);
  }

  protected final void deserialize (ObjectInputStream ois, Collection coll)
    throws ClassNotFoundException, IOException, NotImplementedException
  {
    throw new Error ("Not implemented");
  }

  public void dontUseGui ()
    throws NotImplementedException
  {
    throw new Error ("Not implemented");
  }

  protected final void fireChildrenAdded (BeanContextMembershipEvent bcme)
  {
    synchronized (bcmListeners)
      {
        Iterator it = bcmListeners.iterator();
        while (it.hasNext())
          {
            BeanContextMembershipListener l
              = (BeanContextMembershipListener) it.next();
            l.childrenAdded(bcme);
          }
      }
  }

  protected final void fireChildrenRemoved (BeanContextMembershipEvent bcme)
  {
    synchronized (bcmListeners)
      {
        Iterator it = bcmListeners.iterator();
        while (it.hasNext())
          {
            BeanContextMembershipListener l
            = (BeanContextMembershipListener) it.next();
            l.childrenRemoved(bcme);
          }
      }
  }

  public BeanContext getBeanContextPeer ()
    throws NotImplementedException
  {
    throw new Error ("Not implemented");
  }

  protected static final BeanContextChild getChildBeanContextChild (Object child)
    throws NotImplementedException
  {
    throw new Error ("Not implemented");
  }

  protected static final BeanContextMembershipListener getChildBeanContextMembershipListener (Object child)
    throws NotImplementedException
  {
    throw new Error ("Not implemented");
  }

  protected static final PropertyChangeListener getChildPropertyChangeListener (Object child)
    throws NotImplementedException
  {
    throw new Error ("Not implemented");
  }

  protected static final Serializable getChildSerializable (Object child)
    throws NotImplementedException
  {
    throw new Error ("Not implemented");
  }

  protected static final VetoableChangeListener getChildVetoableChangeListener (Object child)
    throws NotImplementedException
  {
    throw new Error ("Not implemented");
  }

  protected static final Visibility getChildVisibility (Object child)
    throws NotImplementedException
  {
    throw new Error ("Not implemented");
  }

  public Locale getLocale ()
  {
    return locale;
  }

  public URL getResource (String name, BeanContextChild bcc)
  {
    if (! contains(bcc))
      throw new IllegalArgumentException("argument not a child");
    ClassLoader loader = bcc.getClass().getClassLoader();
    return (loader == null ? ClassLoader.getSystemResource(name)
            : loader.getResource(name));
  }

  public InputStream getResourceAsStream (String name, BeanContextChild bcc)
  {
    if (! contains(bcc))
      throw new IllegalArgumentException("argument not a child");
    ClassLoader loader = bcc.getClass().getClassLoader();
    return (loader == null ? ClassLoader.getSystemResourceAsStream(name)
            : loader.getResourceAsStream(name));
  }

  protected void initialize ()
  {
    bcmListeners = new ArrayList();
    children = new HashMap();
  }

  public Object instantiateChild (String beanName)
    throws IOException, ClassNotFoundException, NotImplementedException
  {
    throw new Error ("Not implemented");
  }

  public boolean isDesignTime ()
  {
    return designTime;
  }

  public boolean isEmpty ()
  {
    synchronized (children)
      {
        return children.isEmpty();
      }
  }

  public boolean isSerializing ()
    throws NotImplementedException
  {
    throw new Error ("Not implemented");
  }

  public Iterator iterator ()
  {
    synchronized (children)
      {
        return children.keySet().iterator();
      }
  }

  public boolean needsGui ()
    throws NotImplementedException
  {
    throw new Error ("Not implemented");
  }

  public void okToUseGui ()
    throws NotImplementedException
  {
    throw new Error ("Not implemented");
  }

  public void propertyChange (PropertyChangeEvent pce)
    throws NotImplementedException
  {
    throw new Error ("Not implemented");
  }

  public final void readChildren (ObjectInputStream ois)
    throws IOException, ClassNotFoundException, NotImplementedException
  {
    throw new Error ("Not implemented");
  }

  public boolean remove (Object targetChild)
  {
    return remove(targetChild, true);
  }

  protected boolean remove (Object targetChild, boolean callChildSetBC)
    throws NotImplementedException
  {
    if (targetChild == null)
      throw new IllegalArgumentException();
    
    throw new Error ("Not implemented");
  }

  public boolean removeAll (Collection c)
  {
    // Intentionally throws an exception.
    throw new UnsupportedOperationException();
  }

  public void removeBeanContextMembershipListener (BeanContextMembershipListener bcml)
  {
    synchronized (bcmListeners)
      {
        bcmListeners.remove(bcml);
      }
  }

  public boolean retainAll (Collection c)
  {
    // Intentionally throws an exception.
    throw new UnsupportedOperationException();
  }

  protected final void serialize (ObjectOutputStream oos, Collection coll)
    throws IOException, NotImplementedException
  {
    throw new Error ("Not implemented");
  }

  public void setDesignTime (boolean dtime)
  {
    boolean save = designTime;
    designTime = dtime;
    firePropertyChange(DesignMode.PROPERTYNAME, Boolean.valueOf(save),
                       Boolean.valueOf(dtime));
  }

  public void setLocale (Locale newLocale)
    throws PropertyVetoException
  {
    if (newLocale == null || locale == newLocale)
      return;
    fireVetoableChange("locale", locale, newLocale);
    Locale oldLocale = locale;
    locale = newLocale;
    firePropertyChange("locale", oldLocale, newLocale);
  }

  public int size ()
  {
    synchronized (children)
      {
        return children.size();
      }
  }

  public Object[] toArray ()
  {
    synchronized (children)
      {
        return children.keySet().toArray();
      }
  }

  public Object[] toArray(Object[] array)
    throws NotImplementedException
  {
    // This implementation is incorrect, I think.
    synchronized (children)
      {
        return children.keySet().toArray(array);
      }
  }

  protected boolean validatePendingAdd (Object targetChild)
  {
    return true;
  }

  protected boolean validatePendingRemove (Object targetChild)
  {
    return true;
  }

  public void vetoableChange (PropertyChangeEvent pce)
    throws PropertyVetoException, NotImplementedException
  {
    throw new Error ("Not implemented");
  }

  public final void writeChildren (ObjectOutputStream oos)
    throws IOException, NotImplementedException
  {
    throw new Error ("Not implemented");
  }
}
