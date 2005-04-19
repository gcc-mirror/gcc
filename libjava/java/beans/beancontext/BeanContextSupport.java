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


package java.beans.beancontext;

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
  
  private void readObject (ObjectInputStream s)
    throws ClassNotFoundException, IOException
  {
    throw new Error ("Not implemented");
  }

  private void writeObject (ObjectOutputStream s)
    throws ClassNotFoundException, IOException
  {
    throw new Error ("Not implemented");
  }

  protected class BCSChild implements Serializable
  {
    private static final long serialVersionUID = 3289144128843950629L;
  }

  protected static final class BCSIterator implements Iterator
  {
    public boolean hasNext ()
    {
      throw new Error ("Not implemented");
    }

    public Object next ()
    {
      throw new Error ("Not implemented");
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
    locale = lcle;
    designTime = dtime;
    okToUseGui = visible;

    initialize ();
  }

  public boolean add (Object targetChild)
  {
    if (targetChild == null)
      throw new IllegalArgumentException();

    if (children.containsKey(targetChild))
      return false;

    // FIXME: The second argument is surely wrong.
    children.put(targetChild, targetChild);
    return true;
  }

  public boolean addAll (Collection c)
  {
    throw new UnsupportedOperationException();
  }

  public void addBeanContextMembershipListener
    (BeanContextMembershipListener listener)
  {
    if (! bcmListeners.contains(listener))
      bcmListeners.add(listener);
  }

  public boolean avoidingGui ()
  {
    throw new Error ("Not implemented");
  }

  protected Iterator bcsChildren ()
  {
    throw new Error ("Not implemented");
  }

  protected void bcsPreDeserializationHook (ObjectInputStream ois)
    throws ClassNotFoundException, IOException 
  {
    throw new Error ("Not implemented");
  }

  protected void bcsPreSerializationHook (ObjectOutputStream oos)
    throws IOException
  {
    throw new Error ("Not implemented");
  }

  protected void childDeserializedHook (Object child, BeanContextSupport.BCSChild bcsc)
  {
    throw new Error ("Not implemented");
  }

  protected void childJustAddedHook (Object child, BeanContextSupport.BCSChild bcsc)
  {
    throw new Error ("Not implemented");
  }

  protected void childJustRemovedHook (Object child, BeanContextSupport.BCSChild bcsc)
  {
    throw new Error ("Not implemented");
  }

  protected static final boolean classEquals (Class first, Class second)
  {
    throw new Error ("Not implemented");
  }

  public void clear ()
  {
    throw new UnsupportedOperationException();
  }

  public boolean contains (Object o)
  {
    throw new Error ("Not implemented");
  }

  public boolean containsAll (Collection c)
  {
    throw new Error ("Not implemented");
  }

  public boolean containsKey (Object o)
  {
    throw new Error ("Not implemented");
  }

  protected final Object[] copyChildren ()
  {
    throw new Error ("Not implemented");
  }

  protected BeanContextSupport.BCSChild createBCSChild (Object targetChild, Object peer)
  {
    throw new Error ("Not implemented");
  }

  protected final void deserialize (ObjectInputStream ois, Collection coll)
    throws ClassNotFoundException, IOException
  {
    throw new Error ("Not implemented");
  }

  public void dontUseGui ()
  {
    throw new Error ("Not implemented");
  }

  protected final void fireChildrenAdded (BeanContextMembershipEvent bcme)
  {
    throw new Error ("Not implemented");
  }

  protected final void fireChildrenRemoved (BeanContextMembershipEvent bcme)
  {
    throw new Error ("Not implemented");
  }

  public BeanContext getBeanContextPeer ()
  {
    throw new Error ("Not implemented");
  }

  protected static final BeanContextChild getChildBeanContextChild (Object child)
  {
    throw new Error ("Not implemented");
  }

  protected static final BeanContextMembershipListener getChildBeanContextMembershipListener (Object child)
  {
    throw new Error ("Not implemented");
  }

  protected static final PropertyChangeListener getChildPropertyChangeListener (Object child)
  {
    throw new Error ("Not implemented");
  }

  protected static final Serializable getChildSerializable (Object child)
  {
    throw new Error ("Not implemented");
  }

  protected static final VetoableChangeListener getChildVetoableChangeListener (Object child)
  {
    throw new Error ("Not implemented");
  }

  protected static final Visibility getChildVisibility (Object child)
  {
    throw new Error ("Not implemented");
  }

  public Locale getLocale ()
  {
    return locale;
  }

  public URL getResource (String name, BeanContextChild bcc)
  {
    throw new Error ("Not implemented");
  }

  public InputStream getResourceAsStream (String name, BeanContextChild bcc)
  {
    throw new Error ("Not implemented");
  }

  protected void initialize ()
  {
    bcmListeners = new ArrayList();
    children = new HashMap();
  }

  public Object instantiateChild (String beanName)
    throws IOException, ClassNotFoundException
  {
    throw new Error ("Not implemented");
  }

  public boolean isDesignTime ()
  {
    throw new Error ("Not implemented");
  }

  public boolean isEmpty ()
  {
    throw new Error ("Not implemented");
  }

  public boolean isSerializing ()
  {
    throw new Error ("Not implemented");
  }

  public Iterator iterator ()
  {
    return children.keySet().iterator();
  }

  public boolean needsGui ()
  {
    throw new Error ("Not implemented");
  }

  public void okToUseGui ()
  {
    throw new Error ("Not implemented");
  }

  public void propertyChange (PropertyChangeEvent pce)
  {
    throw new Error ("Not implemented");
  }

  public final void readChildren (ObjectInputStream ois)
    throws IOException, ClassNotFoundException
  {
    throw new Error ("Not implemented");
  }

  public boolean remove (Object targetChild)
  {
    return remove(targetChild, true);
  }

  protected boolean remove (Object targetChild, boolean callChildSetBC)
  {
    if (targetChild == null)
      throw new IllegalArgumentException();
    
    throw new Error ("Not implemented");
  }

  public boolean removeAll (Collection c)
  {
    throw new UnsupportedOperationException();
  }

  public void removeBeanContextMembershipListener (BeanContextMembershipListener bcml)
  {
    throw new Error ("Not implemented");
  }

  public boolean retainAll (Collection c)
  {
    throw new UnsupportedOperationException();
  }

  protected final void serialize (ObjectOutputStream oos, Collection coll)
    throws IOException
  {
    throw new Error ("Not implemented");
  }

  public void setDesignTime (boolean dtime)
  {
    throw new Error ("Not implemented");
  }

  public void setLocale (Locale newLocale)
    throws PropertyVetoException
  {
    throw new Error ("Not implemented");
  }

  public int size ()
  {
    throw new Error ("Not implemented");
  }

  public Object[] toArray ()
  {
    return children.keySet().toArray();
  }

  public Object[] toArray(Object[] array)
  {
    return children.keySet().toArray(array);
  }

  protected boolean validatePendingAdd (Object targetChild)
  {
    throw new Error ("Not implemented");
  }

  protected boolean validatePendingRemove (Object targetChild)
  {
    throw new Error ("Not implemented");
  }

  public void vetoableChange (PropertyChangeEvent pce)
    throws PropertyVetoException
  {
    throw new Error ("Not implemented");
  }

  public final void writeChildren (ObjectOutputStream oos)
    throws IOException
  {
    throw new Error ("Not implemented");
  }
}
