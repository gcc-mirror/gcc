/* java.beans.beancontext.BeanContextServices
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

import java.util.Iterator;

/**
 * Allows a <code>BeanContext</code> to provide services to its children.
 *
 * @specnote it is unclear whether a <code>BeanContextServices</code>
 *           should delegate unhandled requests to parents.  I assume so.
 * @author John Keiser
 * @since JDK1.2
 */

public interface BeanContextServices extends BeanContext, BeanContextServicesListener {
	/**
	 * Register a service to make it available to others.
	 * This class may refuse to add the service based on whatever
	 * information it can gather, including whether the service
	 * provider is trusted.
	 *
	 * @param serviceClass the service class.
	 * @param provider the factory that will actually provide the service.
	 * @return whether the service was added or not.
	 */
	public boolean addService(Class serviceClass, BeanContextServiceProvider provider);

	/**
	 * Make it so that no one else can use this service.
	 * <P>
	 *
	 * If <code>revokeNow</code> is <code>false</code>, the only
	 * effect of this method is to make all subsequent calls to
	 * <code>getService()</code> on this service class fail.
	 * <P>
	 *
	 * If it is <code>true</code>, a message is also sent out to all
	 * listeners on the service and all references to it are released.
	 *
	 * @param serviceClass the service class to revoke.
	 * @param provider the service provider providing the service class.
	 * @param revokeNow whether to release all current references to
	 *        the service.
	 */
	public void revokeService(Class serviceClass, BeanContextServiceProvider provider, boolean revokeNow);

	/**
	 * Release your copy of this service.
	 * <P>
	 *
	 * If all copies of the service's class have been relinquished by
	 * the requestor, the <code>BeanContextServiceRevokedListener</code>
	 * previously registered by <code>getService()</code> will be
	 * unregistered.
	 *
	 * @param requestorChild the original <code>BeanContextChild</code>
	 *        requesting the service.
	 * @param requestor the original requestor of the service.
	 * @param service the service to relinquish
	 * @see #getService(java.beans.beancontext.BeanContextChild,java.lang.Object,java.lang.Class,java.lang.Object,java.beans.beancontext.BeanContextServiceRevokedListener)
	 */
	public void releaseService(BeanContextChild requestorChild, Object requestor, Object service);

	/**
	 * Get a service from this <code>BeanContextServices</code>.
	 * <P>
	 *
	 * The specified listener will be registered to receive a
	 * revocation notice for the specified serviceClass.  One
	 * notification per service class per requestor object will be
	 * sent.
	 * <P>
	 *
	 * The listener will be unregistered when all services that were
	 * obtained by that requestor for that service class are released.
	 * <P>
	 *
	 * If the requested service class is not available, or if this
	 * <code>BeanContextServices</code> object chooses not honor the
	 * request because the service class has been revoked or for some
	 * other reason, then this method will return <code>null</code>.
	 * <P>
	 *
	 * This method may throw unchecked exceptions, so watch out.
	 *
	 * @specnote it is not specified what happens when two subsequent
	 *           calls are made to <code>getService()</code> with the
	 *           same requestor object and service class but different
	 *           listeners.  Which listener is to be notified?
	 *
	 * @param requestorChild the <code>BeanContextChild</code>
	 *        associated with the requestor.  Typically this will be
	 *        the same as the requestor itself, but since any
	 *        <code>Object</code>, even one outside the hierarchy, may
	 *        make a request, this parameter is necessary.  Only weak
	 *        references to this will be retained, and it will never
	 *        be changed, only queried in a read-only manner.
	 * @param requestor the actual requestor of the service.  Only
	 *        weak references to this will be retained, and it will
	 *        never be changed, only queried in a read-only manner.
	 * @param serviceClass the <code>Class</code> of the service being
	 *        requested.
	 * @param serviceSelector a parameter to customize the service
	 *        returned with.
	 * @param listener a listener that will be notified if the service
	 *        being requested is revoked.
	 * @return an instance of <code>serviceClass</code> (such that
	 *        <code>instanceof</code> serviceClass is true), or
	 *        <code>null</code>.
	 */
	public Object getService(BeanContextChild requestorChild, Object requestor, Class serviceClass, Object serviceSelector, BeanContextServiceRevokedListener listener);

	/**
	 * Get a list of all service classes supported.
	 * <P>
	 *
	 * This method must synchronize on
	 * <code>BeanContext.globalHierarchyLock</code>.
	 *
	 * @return a list of all service classes supported.
	 * @see java.beans.beancontext.BeanContext#globalHierarchyLock
	 */
	public Iterator getCurrentServiceClasses();

	/**
	 * Get a list of valid service selectors for the specified service class.
	 * <P>
	 *
	 * If the specified service class does not have a finite number of
	 * valid service selectors, it should return <code>null</code>.
	 * If it takes a general <code>Integer</code> parameter, for
	 * example, you may as well return <code>null</code> or the poor
	 * soul who called this method will be iterating all day.
	 * <P>
	 *
	 * If it has no valid service selectors, it should still return an empty
	 * <code>Iterator</code>.
	 *
	 * @param serviceClass the service class to get selectors for.
	 * @return a list of valid service selectors for the service
	 *         class, or <code>null</code>.
	 */
	public Iterator getCurrentServiceSelectors(Class serviceClass);

	/**
	 * Tell whether the specified service class is available.
	 * Iff getService() could return a non-null value for the
	 * specified service, this method will return <code>true</code>.
	 *
	 * @param serviceClass the service class to check on.
	 * @return whether the specified service class is availabe.
	 */
	public boolean hasService(Class serviceClass);

	/**
	 * Add a listener on all adds and removes of services.
	 * @param listener the listener to add.
	 */
	public void addBeanContextServicesListener(BeanContextServicesListener listener);

	/**
	 * Remove a listener on all adds and removes of services.
	 * @specnote it is not certain whether this should remove this
	 *           listener if it was specified in
	 *           <code>getService()</code>.
	 * @param listener the listener to add.
	 */
	public void removeBeanContextServicesListener(BeanContextServicesListener listener);
}
