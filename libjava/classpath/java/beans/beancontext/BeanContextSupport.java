/* BeanContextSupport.java --
   Copyright (C) 2003, 2005, 2006  Free Software Foundation, Inc.

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

import java.beans.Beans;
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
import java.util.List;
import java.util.Locale;

/**
 * This is a helper class for implementing a bean context.  It is
 * intended to be used either by subclassing or by calling methods
 * of this implementation from another.
 *
 * @author Michael Koch
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.2
 */
public class BeanContextSupport extends BeanContextChildSupport
  implements BeanContext, Serializable, PropertyChangeListener,
  VetoableChangeListener
{
  private static final long serialVersionUID = -4879613978649577204L;

  /**
   * Deserializes a stored bean context.  Hook methods are provided to allow
   * subclasses to perform their own deserialization after the default
   * deserialization but prior to the deserialization of the children.  Note that
   * {@link #readChildren(ObjectInputStream)} is only called if there
   * is no distinct peer.  If there is, the peer is expected to call
   * the method instead.
   *
   * @param s the stream to deserialize.
   * @throws ClassNotFoundException if the class of an object being deserialized
   *                                could not be found.
   * @throws IOException if an I/O error occurs.
   */
  private void readObject (ObjectInputStream s)
    throws ClassNotFoundException, IOException
  {
    s.defaultReadObject();
    bcsPreDeserializationHook(s);
    BeanContext peer = getBeanContextPeer();
    if (peer == null || peer == this)
      readChildren(s);
  }

  /**
   * Serializes a bean context.  Hook methods are provided to allow
   * subclasses to perform their own serialization after the default
   * serialization but prior to serialization of the children.  Note that
   * {@link #writeChildren(ObjectOutputStream)} is only called if there
   * is no distinct peer.  If there is, the peer is expected to call
   * the method instead.
   *
   * @param s the stream to serialize.
   * @throws ClassNotFoundException if the class of an object being deserialized
   *                                could not be found.
   * @throws IOException if an I/O error occurs.
   */
  private void writeObject (ObjectOutputStream s)
    throws ClassNotFoundException, IOException
  {
    serializing = true;
    s.defaultWriteObject();
    bcsPreSerializationHook(s);
    BeanContext peer = getBeanContextPeer();
    if (peer == null || peer == this)
      writeChildren(s);
    serializing = false;
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

    private Object getTargetChild()
    {
      return targetChild;
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

  private transient boolean serializing;

  /**
   * Construct a BeanContextSupport instance.
   */
  public BeanContextSupport ()
  {
    this (null, null, false, true);
  }

  /**
   * Construct a BeanContextSupport instance.
   * 
   * @param peer  the bean context peer (<code>null</code> permitted).
   */
  public BeanContextSupport(BeanContext peer)
  {
    this (peer, null, false, true);
  }

  /**
   * Construct a BeanContextSupport instance.
   * 
   * @param peer  the bean context peer (<code>null</code> permitted).
   * @param locale  the locale (<code>null</code> permitted, equivalent to 
   *     the default locale).
   */
  public BeanContextSupport (BeanContext peer, Locale locale)
  {
    this (peer, locale, false, true);
  }

  /**
   * Construct a BeanContextSupport instance.
   * 
   * @param peer  the bean context peer (<code>null</code> permitted).
   * @param locale  the locale (<code>null</code> permitted, equivalent to 
   *     the default locale).
   * @param dtime  a flag indicating whether or not the bean context is in
   *     design time mode.
   */
  public BeanContextSupport (BeanContext peer, Locale locale, boolean dtime)
  {
    this (peer, locale, dtime, true);
  }

  /**
   * Construct a BeanContextSupport instance.
   * 
   * @param peer  the bean context peer (<code>null</code> permitted).
   * @param locale  the locale (<code>null</code> permitted, equivalent to 
   *     the default locale).
   * @param dtime  a flag indicating whether or not the bean context is in
   *     design time mode.
   * @param visible  initial value of the <code>okToUseGui</code> flag.
   */
  public BeanContextSupport (BeanContext peer, Locale locale, boolean dtime,
                             boolean visible)
  {
    super(peer);

    this.locale = locale == null ? Locale.getDefault() : locale;
    designTime = dtime;
    okToUseGui = visible;

    initialize ();
  }

  /**
   * <p>
   * Add a child to the bean context.  A child can be a simple
   * <code>Object</code>, a <code>BeanContextChild</code>
   * or another <code>BeanContext</code>.  
   * </p>
   * <p>
   * The children of a <code>BeanContext</code> form a set.  As
   * a result, this method returns <code>false</code> if the given
   * object is already a child of this context.
   * </p>
   * <p>
   * If the child is a <code>BeanContextChild</code>, or a proxy
   * for such a child, the <code>setBeanContext()</code> method
   * is invoked on the child.  If this operation is vetoed by the
   * child, via throwing a <code>PropertyVetoException</code>,
   * then the current completion state of the <code>add()</code>
   * operation is rolled back and a <code>IllegalStateException</code>
   * is thrown.  If the <code>BeanContextChild</code> is successfully
   * added, then the context registers with its
   * <code>PropertyChangeListener</code> and
   * <code>VetoableChangeListener</code> for "beanContext" events.
   * </p>
   * <p>
   * If the child implements <code>java.beans.Visibility</code>,
   * then its ability to use a GUI is set based on that of
   * this context.
   * </p>
   * <p> 
   * A <code>BeanContextMembershipEvent</code> is fired when the
   * child is successfully added to the bean context.
   * </p>
   * <p>
   * This method is synchronized over the global hierarchy lock.
   * </p>
   *
   * @param targetChild the child to add.
   * @return false if the child has already been added.
   * @throws IllegalArgumentException if the child is null.
   * @throws IllegalStateException if the child vetos the setting
   *                               of its context.
   */
  public boolean add(Object targetChild)
  {
    synchronized (globalHierarchyLock)
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
	    BeanContextChild bcChild = null;
	    if (targetChild instanceof BeanContextChild)
	      bcChild = (BeanContextChild) targetChild;
	    if (targetChild instanceof BeanContextProxy)
	      bcChild = ((BeanContextProxy) targetChild).getBeanContextProxy();
	    if (bcChild != null)
	      try
		{
		  bcChild.setBeanContext(this);
		  bcChild.addVetoableChangeListener("beanContext", this);
		  bcChild.addPropertyChangeListener("beanContext", this);
		}
	      catch (PropertyVetoException e)
		{
		  synchronized (children)
		    {
		      children.remove(targetChild);
		    }
		  throw new IllegalStateException("The child refused to " +
						  "associate itself with " +
						  "this context.", e);
		}
	    if (targetChild instanceof Visibility)
	      {
		Visibility visibleChild = (Visibility) targetChild;
		if (okToUseGui)
		  visibleChild.okToUseGui();
		else
		  visibleChild.dontUseGui();
	      }
	    childJustAddedHook(targetChild, child);
	  }
	fireChildrenAdded(new BeanContextMembershipEvent(this,
							 new Object[]{ targetChild }));
	return true;
      }
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

  /**
   * Returns true if this bean needs a GUI
   * but is being prevented from using one.
   *
   * @return true if <code>needsGui()</code>
   *              is true but the bean has been
   *              told not to use it.
   */
  public boolean avoidingGui()
  {
    return needsGui() && (!okToUseGui);
  }

  protected Iterator bcsChildren ()
  {
    synchronized (children)
      {
        return new BCSIterator(children.values().iterator());
      }
  }

  /**
   * Subclasses may use this method to perform their own deserialization
   * after the default deserialization process has taken place, but
   * prior to the deserialization of the children.  It should not
   * be used to replace the implementation of <code>readObject</code>
   * in the subclass.
   *
   * @param ois the input stream.
   * @throws ClassNotFoundException if the class of an object being deserialized
   *                                could not be found.
   * @throws IOException if an I/O error occurs.
   */
  protected void bcsPreDeserializationHook (ObjectInputStream ois)
    throws ClassNotFoundException, IOException
  {
    /* Purposefully left empty */
  }

  /**
   * Subclasses may use this method to perform their own serialization
   * after the default serialization process has taken place, but
   * prior to the serialization of the children.  It should not
   * be used to replace the implementation of <code>writeObject</code>
   * in the subclass.
   *
   * @param oos the output stream.
   * @throws IOException if an I/O error occurs.
   */
  protected void bcsPreSerializationHook (ObjectOutputStream oos)
    throws IOException
  {
    /* Purposefully left empty */
  }

  /**
   * Called when a child is deserialized.
   * 
   * @param child the deserialized child.
   * @param bcsc the deserialized context wrapper for the child.
   */
  protected void childDeserializedHook (Object child, BeanContextSupport.BCSChild bcsc)
  {
    // Do nothing in the base class.
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

  /**
   * Deserializes objects (written by {@link #serialize(ObjectOutputStream, 
   * Collection)}) and adds them to the specified collection.
   * 
   * @param ois  the input stream (<code>null</code> not permitted).
   * @param coll  the collection to add the objects to (<code>null</code> not
   *     permitted).
   *     
   * @throws ClassNotFoundException
   * @throws IOException
   * 
   * @see #serialize(ObjectOutputStream, Collection)
   */
  protected final void deserialize (ObjectInputStream ois, Collection coll)
    throws ClassNotFoundException, IOException
  {
    int itemCount = ois.readInt();
    for (int i = 0; i < itemCount; i++)
      coll.add(ois.readObject());
  }

  /**
   * Informs this bean that is should not make
   * use of the GUI.
   */
  public void dontUseGui()
  {
    okToUseGui = false;
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

  /**
   * Returns the bean context peer.
   * 
   * @return The bean context peer.
   * 
   * @see BeanContextChildSupport#beanContextChildPeer
   */
  public BeanContext getBeanContextPeer()
  {
    return (BeanContext) beanContextChildPeer;
  }

  /**
   * Returns the {@link BeanContextChild} implementation for the given child.
   * 
   * @param child  the child (<code>null</code> permitted).
   * 
   * @return The bean context child.
   * 
   * @throws IllegalArgumentException if <code>child</code> implements both
   *     the {@link BeanContextChild} and {@link BeanContextProxy} interfaces.
   */
  protected static final BeanContextChild getChildBeanContextChild(Object child)
  {
    if (child == null)
      return null;
    if (child instanceof BeanContextChild && child instanceof BeanContextProxy)
      throw new IllegalArgumentException("Child cannot implement " 
          + "BeanContextChild and BeanContextProxy simultaneously.");
    if (child instanceof BeanContextChild)
      return (BeanContextChild) child;
    if (child instanceof BeanContextProxy)
      return ((BeanContextProxy) child).getBeanContextProxy();
    return null;
  }

  /**
   * Returns <code>child</code> as an instance of 
   * {@link BeanContextMembershipListener}, or <code>null</code> if 
   * <code>child</code> does not implement that interface.
   * 
   * @param child  the child (<code>null</code> permitted).
   * 
   * @return The child cast to {@link BeanContextMembershipListener}.
   */
  protected static final BeanContextMembershipListener 
      getChildBeanContextMembershipListener(Object child)
  {
    if (child instanceof BeanContextMembershipListener) 
      return (BeanContextMembershipListener) child;
    else 
      return null;
  }

  /**
   * Returns <code>child</code> as an instance of 
   * {@link PropertyChangeListener}, or <code>null</code> if <code>child</code>
   * does not implement that interface.
   * 
   * @param child  the child (<code>null</code> permitted).
   * 
   * @return The child cast to {@link PropertyChangeListener}.
   */
  protected static final PropertyChangeListener getChildPropertyChangeListener(
      Object child)
  {
    if (child instanceof PropertyChangeListener) 
      return (PropertyChangeListener) child;
    else 
      return null;
  }

  /**
   * Returns <code>child</code> as an instance of {@link Serializable}, or 
   * <code>null</code> if <code>child</code> does not implement that 
   * interface.
   * 
   * @param child  the child (<code>null</code> permitted).
   * 
   * @return The child cast to {@link Serializable}.
   */
  protected static final Serializable getChildSerializable(Object child)
  {
    if (child instanceof Serializable) 
      return (Serializable) child;
    else 
      return null;
  }

  /**
   * Returns <code>child</code> as an instance of 
   * {@link VetoableChangeListener}, or <code>null</code> if <code>child</code>
   * does not implement that interface.
   * 
   * @param child  the child (<code>null</code> permitted).
   * 
   * @return The child cast to {@link VetoableChangeListener}.
   */
  protected static final VetoableChangeListener getChildVetoableChangeListener(
      Object child)
  {
    if (child instanceof VetoableChangeListener) 
      return (VetoableChangeListener) child;
    else 
      return null;
  }

  /**
   * Returns <code>child</code> as an instance of {@link Visibility}, or
   * <code>null</code> if <code>child</code> does not implement that interface.
   * 
   * @param child  the child (<code>null</code> permitted).
   * 
   * @return The child cast to {@link Visibility}.
   */
  protected static final Visibility getChildVisibility(Object child)
  {
    if (child instanceof Visibility) 
      return (Visibility) child;
    else 
      return null;
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

  /**
   * This is a convenience method for instantiating a bean inside this
   * context.  It delegates to the appropriate method in
   * <code>java.beans.Beans</code> using the context's classloader.
   *
   * @param beanName the name of the class of bean to instantiate.
   * @throws IOException if an I/O error occurs in loading the class.
   * @throws ClassNotFoundException if the class, <code>beanName</code>,
   *                                can not be found.
   */
  public Object instantiateChild (String beanName)
    throws IOException, ClassNotFoundException
  {
    return Beans.instantiate(getClass().getClassLoader(), beanName, this);
  }

  /**
   * Returns <code>true</code> if the <code>BeanContext</code> is in 
   * design time mode, and <code>false</code> if it is in runtime mode.
   * 
   * @return A boolean.
   * 
   * @see #setDesignTime(boolean)
   */
  public boolean isDesignTime()
  {
    return designTime;
  }

  /**
   * Returns true if this bean context has no children.
   *
   * @return true if there are no children.
   */
  public boolean isEmpty ()
  {
    synchronized (children)
      {
        return children.isEmpty();
      }
  }

  /**
   * Returns true if the bean context is in the process
   * of being serialized.
   *
   * @return true if the context is being serialized.
   */
  public boolean isSerializing()
  {
    return serializing;
  }

  public Iterator iterator ()
  {
    synchronized (children)
      {
        return children.keySet().iterator();
      }
  }

  /**
   * Returns false as this bean does not a
   * GUI for its operation.
   *
   * @return false
   */
  public boolean needsGui()
  {
    return false;
  }

  /**
   * Informs this bean that it is okay to make use of
   * the GUI.
   */
  public void okToUseGui ()
  {
    okToUseGui = true;
  }

  /**
   * Subclasses may use this method to catch property changes
   * arising from the children of this context.  At present,
   * we just listen for the beans being assigned to a different
   * context and remove them from here if such an event occurs.
   *
   * @param pce the property change event.
   */
  public void propertyChange (PropertyChangeEvent pce)
  {
    if (pce.getNewValue() != this)
      remove(pce.getSource(), false);
  }

  /**
   * Deserializes the children using the
   * {@link #deserialize(ObjectInputStream, Collection} method
   * and then calls {@link childDeserializedHook(Object, BCSChild)}
   * for each child deserialized.
   *
   * @param ois the input stream.
   * @throws IOException if an I/O error occurs.
   */
  public final void readChildren (ObjectInputStream ois)
    throws IOException, ClassNotFoundException
  {
    List temp = new ArrayList();
    deserialize(ois, temp);
    Iterator i = temp.iterator();
    synchronized (globalHierarchyLock)
      {
	synchronized (children)
	  {
	    while (i.hasNext())
	      {
		BCSChild bcs = (BCSChild) i.next();
		childDeserializedHook(bcs.getTargetChild(), bcs);
		children.put(bcs.getTargetChild(), bcs);
	      }
	  }
      }
  }

  /**
   * Remove the specified child from the context.  This is
   * the same as calling <code>remove(Object,boolean)</code>
   * with a request for the <code>setBeanContext()</code> method
   * of the child to be called (i.e. the second argument is true).
   *
   * @param targetChild the child to remove.
   */
  public boolean remove (Object targetChild)
  {
    return remove(targetChild, true);
  }

  /**
   * <p>
   * Removes a child from the bean context.  A child can be a simple
   * <code>Object</code>, a <code>BeanContextChild</code>
   * or another <code>BeanContext</code>.  If the given child is not
   * a child of this context, this method returns <code>false</code>.
   * </p>
   * <p>
   * If the child is a <code>BeanContextChild</code>, or a proxy
   * for such a child, the <code>setBeanContext()</code> method
   * is invoked on the child (if specified).  If this operation is vetoed
   * by the child, via throwing a <code>PropertyVetoException</code>,
   * then the current completion state of the <code>remove()</code>
   * operation is rolled back and a <code>IllegalStateException</code>
   * is thrown.  If the <code>BeanContextChild</code> is successfully
   * removed, then the context deregisters with its
   * <code>PropertyChangeListener</code> and
   * <code>VetoableChangeListener</code> for "beanContext" events.
   * </p>
   * <p> 
   * A <code>BeanContextMembershipEvent</code> is fired when the
   * child is successfully removed from the bean context.
   * </p>
   * <p>
   * This method is synchronized over the global hierarchy lock.
   * </p>
   *
   * @param targetChild the child to remove.
   * @param callChildSetBC true if the <code>setBeanContext()</code>
   *                       method of the child should be called.
   * @return false if the child doesn't exist.
   * @throws IllegalArgumentException if the child is null.
   * @throws IllegalStateException if the child vetos the setting
   *                               of its context.
   */
  protected boolean remove (Object targetChild, boolean callChildSetBC)
  {
    synchronized (globalHierarchyLock)
      {
	if (targetChild == null)
	  throw new IllegalArgumentException();

	BCSChild child;
	synchronized (children)
	  {
	    if (!children.containsKey(targetChild)
		|| !validatePendingRemove(targetChild))
	      return false;
	    child = (BCSChild) children.remove(targetChild);
	  }
	synchronized (targetChild)
	  {
	    BeanContextChild bcChild = null;
	    if (targetChild instanceof BeanContextChild)
	      bcChild = (BeanContextChild) targetChild;
	    if (targetChild instanceof BeanContextProxy)
	      bcChild = ((BeanContextProxy) targetChild).getBeanContextProxy();
	    if (bcChild != null)
	      try
		{
		  if (callChildSetBC)
		    bcChild.setBeanContext(null);
		  bcChild.removeVetoableChangeListener("beanContext", this);
		  bcChild.removePropertyChangeListener("beanContext", this);
		}
	      catch (PropertyVetoException e)
		{
		  synchronized (children)
		    {
		      children.put(targetChild, child);
		    }
		  throw new IllegalStateException("The child refused to " +
						  "disassociate itself with " +
						  "this context.", e);
		}
	    childJustRemovedHook(targetChild, child);
	  }
	fireChildrenRemoved(new BeanContextMembershipEvent(this,
							 new Object[]{ targetChild }));
	return true;
      }
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

  /**
   * Writes the items in the collection to the specified output stream.  Items
   * in the collection that are not instances of {@link Serializable} 
   * (this includes <code>null</code>) are simply ignored.
   * 
   * @param oos  the output stream (<code>null</code> not permitted).
   * @param coll  the collection (<code>null</code> not permitted).
   * 
   * @throws IOException
   * 
   * @see #deserialize(ObjectInputStream, Collection)
   */
  protected final void serialize(ObjectOutputStream oos, Collection coll)
    throws IOException
  {
    Object[] items = coll.toArray();
    int itemCount = 0;
    for (int i = 0; i < items.length; i++)
      {
        if (items[i] instanceof Serializable)
          itemCount++;
      }
    oos.writeInt(itemCount);
    for (int i = 0; i < items.length; i++)
      {
        if (items[i] instanceof Serializable)
          oos.writeObject(items[i]);
      }
  }

  /**
   * Sets the flag that indicates whether or not the 
   * <code>BeanContext</code> is in design mode.  If the flag changes
   * value, a {@link PropertyChangeEvent} (with the property name 'designMode')
   * is sent to registered listeners.  Note that the property name used here
   * does NOT match the specification in the {@link DesignMode} interface, we
   * match the reference implementation instead - see bug parade entry 4295174.
   * 
   * @param dtime  the new value for the flag.
   * 
   * @see #isDesignTime()
   */
  public void setDesignTime(boolean dtime)
  {
    boolean save = designTime;
    designTime = dtime;
    // note that we use the same property name as Sun's implementation,
    // even though this is a known bug: see bug parade entry 4295174
    firePropertyChange("designMode", Boolean.valueOf(save),
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

  /**
   * Returns an array containing the children of this <code>BeanContext</code>.
   * 
   * @return An array containing the children.
   */
  public Object[] toArray()
  {
    synchronized (children)
      {
        return children.keySet().toArray();
      }
  }

  /**
   * Populates, then returns, the supplied array with the children of this 
   * <code>BeanContext</code>.  If the array is too short to hold the 
   * children, a new array is allocated and returned.  If the array is too 
   * long, it is padded with <code>null</code> items at the end.
   * 
   * @param array  an array to populate (<code>null</code> not permitted).
   */
  public Object[] toArray(Object[] array)
  {
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

  /**
   * Subclasses may use this method to veto changes arising
   * from the children of this context.
   *
   * @param pce the vetoable property change event fired.
   */
  public void vetoableChange (PropertyChangeEvent pce)
    throws PropertyVetoException
  {
    /* Purposefully left empty */
  }

  /**
   * Serializes the children using the
   * {@link #serialize(ObjectOutputStream, Collection} method.
   *
   * @param oos the output stream.
   * @throws IOException if an I/O error occurs.
   */
  public final void writeChildren (ObjectOutputStream oos)
    throws IOException
  {
    synchronized (children)
      {
	serialize(oos, children.values());
      }
  }

}
