/* java.beans.beancontext.BeanContextChildSupport
   Copyright (C) 1999 Free Software Foundation, Inc.

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
import java.beans.PropertyChangeSupport;
import java.beans.PropertyVetoException;
import java.beans.VetoableChangeListener;
import java.beans.VetoableChangeSupport;
import java.io.Serializable;

/**
 * Support for creating a <code>BeanContextChild</code>.
 * This class contains the most common implementations of the methods in
 * the <code>BeanContextChild</code>
 *
 * @specnote This class is not very well specified.  I had to "fill in the
 *           blanks" in most places with what I thought was reasonable
 *           behavior.  If there are problems, let me know.
 *
 * @author John Keiser
 * @since 1.2
 * @see java.beans.beancontext.BeanContextChild
 */
public class BeanContextChildSupport
  implements BeanContextChild, BeanContextServicesListener, Serializable
{
  static final long serialVersionUID = 6328947014421475877L;

	/**
	 * The peer on which to perform <code>set</code> actions.
	 * This is here so that this class can be used as a peer.
	 * <P>
	 *
	 * When extending this class, this variable will be set to
	 * <code>this</code>.
	 */
	public BeanContextChild beanContextChildPeer;

	/**
	 * The parent <code>BeanContext</code>.
	 */
	protected transient BeanContext beanContext;

	/**
	 * If <code>setBeanContext()</code> was vetoed once before, this
	 * is set to <code>true</code> so that the next time, vetoes will
	 * be ignored.
	 */
	protected transient boolean rejectedSetBCOnce;

	/**
	 * Listeners are registered here and events are fired through here.
	 */
	protected PropertyChangeSupport pcSupport;

	/**
	 * Listeners are registered here and events are fired through here.
	 */
	protected VetoableChangeSupport vcSupport;

	/**
	 * Create a new <code>BeanContextChildSupport</code> with itself as the peer.
	 * This is meant to be used when you subclass
	 * <code>BeanContextChildSupport</code> to create your child.
	 */
	public BeanContextChildSupport()
  {
		this (null);
	}

	/**
	 * Create a new <code>BeanContextChildSupport</code> with the specified peer.
	 * @param peer the peer to use, or <code>null</code> to specify
	 *        <code>this</code>.
	 */
	public BeanContextChildSupport (BeanContextChild peer)
  {
		if (peer == null)
      {
        peer = this;
      }

		beanContextChildPeer = peer;
		pcSupport = new PropertyChangeSupport (peer);
		vcSupport = new VetoableChangeSupport (peer);
	}

	/**
	 * Set the parent <code>BeanContext</code>.
	 * <P>
	 *
	 * When this Object is being added to a new BeanContext or moved
	 * from an old one, a non-null value will be passed in.
	 * <P>
	 *
	 * When this Object is being removed from the current
	 * <code>BeanContext</code>, <code>setBeanContext()</code> will
	 * receive the parameter <code>null</code>.
	 * <P>
	 *
	 * Order of events:
	 * <OL>
	 *   <LI>
	 *     If the new <code>BeanContext</code> is the same as the old
	 *     one, nothing happens.
	 *   </LI>
	 *   <LI>
	 *     If the change has not been rejected or vetoed before, call
	 *     <code>validatePendingSetBeanContext()</code>.  If this call
	 *     returns <code>false</code>, the change is rejected and a
	 *     <code>PropertyVetoException</code> is thrown.
	 *   </LI>
	 *   <LI>
	 *     If the change has not been rejected or vetoed before,
	 *     <code>VetoableChangeEvent</code>s are fired with the name
	 *     <code>"beanContext"</code>, using the
	 *     <code>fireVetoableChange()</code> method.  If a veto
	 *     occurs, reversion events are fired using the same method,
	 *     the change is rejected, and the veto is rethrown.
	 *   </LI>
	 *   <LI>
	 *     <code>releaseBeanContextResources()</code> is called.
	 *   </LI>
	 *   <LI>
	 *     The change is made.
	 *   </LI>
	 *   <LI>
	 *     <code>PropertyChangeEvent</code>s are fired using the
	 *     <code>firePropertyChange()</code> method.
	 *   </LI>
	 *   <LI>
	 *     <code>initializeBeanContextResources()</code> is called.
	 *   </LI>
	 * </OL>
	 * <P>
	 *
	 * @param newBeanContext the new parent for the
	 *        <code>BeanContextChild</code>, or <code>null</code> to
	 *        signify removal from a tree.
	 * @exception PropertyVetoException if the
	 *            <code>BeanContextChild</code> implementor does not
	 *            wish to have its parent changed.
	 */
  public void setBeanContext(BeanContext newBeanContext)
    throws PropertyVetoException
  {
    synchronized (beanContextChildPeer)
      {
        if (newBeanContext == beanContext)
          return;

        if (!rejectedSetBCOnce)
          {
            if (!validatePendingSetBeanContext (newBeanContext))
              {
                rejectedSetBCOnce = true;
                throw new PropertyVetoException ("validatePendingSetBeanContext() rejected change",
                                                 new PropertyChangeEvent(beanContextChildPeer, "beanContext", beanContext, newBeanContext));
              }
            
            try
              {
                fireVetoableChange ("beanContext", beanContext, newBeanContext);
              }
            catch (PropertyVetoException e)
              {
                rejectedSetBCOnce = true;
                throw e;
              }
          }

			releaseBeanContextResources ();

			beanContext = newBeanContext;
			rejectedSetBCOnce = false;

			firePropertyChange ("beanContext", beanContext, newBeanContext);

			initializeBeanContextResources ();
		}
	}

	/**
	 * Get the parent <code>BeanContext</code>.
	 * @return the parent <code>BeanContext</code>.
	 */
	public BeanContext getBeanContext()
  {
		return beanContext;
	}

	/**
	 * Get the peer (or <code>this</code> if there is no peer).
	 * @return the peer, or <code>this</code> if there is no peer.
	 */
	public BeanContextChild getBeanContextChildPeer() {
		return beanContextChildPeer;
	}

	/**
	 * Determine whether there is a peer.
	 * This is true iff <code>getBeanContextChildPeer() == this</code>.
	 * @return whether there is a peer.
	 */
	public boolean isDelegated() {
		return beanContextChildPeer == this;
	}

	/**
	 * Add a listener that will be notified when a specific property changes.
	 * @param propertyName the name of the property to listen on.
	 * @param listener the listener to listen on the property.
	 */
	public void addPropertyChangeListener(String propertyName, PropertyChangeListener listener) {
		pcSupport.addPropertyChangeListener(propertyName, listener);
	}

	/**
	 * Remove a listener to a certain property.
	 * 
	 * @param propertyName the name of the property being listened on.
	 * @param listener the listener listening on the property.
	 */
	public void removePropertyChangeListener(String propertyName, PropertyChangeListener listener) {
		pcSupport.removePropertyChangeListener(propertyName, listener);
	}

	/**
	 * Add a listener that will be notified when a specific property
	 * change is requested (a PropertyVetoException may be thrown) as
	 * well as after the change is successfully made.
	 *
	 * @param propertyName the name of the property to listen on.
	 * @param listener the listener to listen on the property.
	 */
	public void addVetoableChangeListener(String propertyName, VetoableChangeListener listener) {
		vcSupport.addVetoableChangeListener(propertyName, listener);
	}

	/**
	 * Remove a listener to a certain property.
	 *
	 * @param propertyName the name of the property being listened on
	 * @param listener the listener listening on the property.
	 */
	public void removeVetoableChangeListener(String propertyName, VetoableChangeListener listener) {
		vcSupport.removeVetoableChangeListener(propertyName, listener);
	}

	/**
	 * Fire a property change.
	 *
	 * @param propertyName the name of the property that changed
	 * @param oldVal the old value of the property
	 * @param newVal the new value of the property
	 */
	public void firePropertyChange(String propertyName, Object oldVal, Object newVal) {
		pcSupport.firePropertyChange(propertyName, oldVal, newVal);
	}

	/**
	 * Fire a vetoable property change.
	 *
	 * @param propertyName the name of the property that changed
	 * @param oldVal the old value of the property
	 * @param newVal the new value of the property
	 * @exception PropertyVetoException if the change is vetoed.
	 */
	public void fireVetoableChange(String propertyName, Object oldVal, Object newVal)
                        throws PropertyVetoException {
		vcSupport.fireVetoableChange(propertyName, oldVal, newVal);
	}

	/**
	 * Called by <code>BeanContextServices.revokeService()</code> to indicate that a service has been revoked.
	 * If you have a reference to such a service, it should be
	 * discarded and may no longer function properly.
	 * <code>getService()</code> will no longer work on the specified
	 * service class after this event has been fired.
	 * <P>
	 *
	 * <EM>This method is meant to be overriden.</EM>
	 * <code>BeanContextChildSupport</code>'s implementation does
	 * nothing.
	 *
	 * @param event the service revoked event.
	 * @see java.beans.beancontext.BeanContextServices#revokeService(java.lang.Class,java.beans.beancontext.BeanContextServiceProvider,boolean)
	 */
	public void serviceRevoked(BeanContextServiceRevokedEvent event) {
	}

	/**
	 * Called by <code>BeanContextServices</code> whenever a service is made available.
	 * <P>
	 *
	 * <EM>This method is meant to be overriden.</EM>
	 * <code>BeanContextChildSupport</code>'s implementation does
	 * nothing.
	 *
	 * @param event the service revoked event, with useful information
	 *        about the new service.
	 */
	public void serviceAvailable(BeanContextServiceAvailableEvent event) {
	}

	/**
	 * Called by <code>setBeanContext()</code> to determine whether the set should be rejected.
	 * <P>
	 *
	 * <EM>This method is meant to be overriden.</EM>
	 * <code>BeanContextChildSupport</code>'s implementation simply
	 * returns <code>true</code>.
	 *
	 * @param newBeanContext the new parent.
	 * @return whether to allow the parent to be changed to the new
	 *         value.
	 */
	public boolean validatePendingSetBeanContext(BeanContext newBeanContext) {
		return true;
	}

	/**
	 * Called by <code>setBeanContext()</code> to release resources of a what will soon no longer be the parent.
	 * <P>
	 *
	 * <EM>This method is meant to be overriden.</EM>
	 * <code>BeanContextChildSupport</code>'s implementation does
	 * nothing.
	 */
	protected void releaseBeanContextResources() {
	}

	/**
	 * Called by <code>setBeanContext()</code> to grab resources when the parent has been set.
	 * <P>
	 *
	 * <EM>This method is meant to be overriden.</EM>
	 * <code>BeanContextChildSupport</code>'s implementation does
	 * nothing.
	 */
	protected void initializeBeanContextResources() {
	}
}
