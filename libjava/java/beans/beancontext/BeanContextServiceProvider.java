/* java.beans.beancontext.BeanContextServiceProvider
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

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */


package java.beans.beancontext;

import java.util.Iterator;

/**
 * An actual factory for services.
 * <P>
 *
 * It is the <code>BeanContextServiceProvider</code>'s responsibility to
 * register itself with whatever <code>BeanContextServices</code> object
 * it wishes to provide services through using the
 * <code>addService()</code> method.
 * <P>
 *
 * If for some reason it can no longer provide services for a particular
 * class, this class must invoke
 * <code>BeanContextServices.revokeService(serviceClass,this,true)</code>
 * for all the places it has registered the service.
 *
 * @author John Keiser
 * @since JDK1.2
 */

public interface BeanContextServiceProvider {
	/**
	 * Get a service.
	 * Called from <code>BeanContextServices.getService().
	 * <P>
	 *
	 * If the requested service class is not available, or if this
	 * <code>BeanContextServiceProvider</code> chooses not honor the
	 * request for some reason, then this method will return
	 * <code>null</code>.
	 * <P>
	 *
	 * This method may throw unchecked exceptions, so watch out.
	 *
	 * @param services the <code>BeanContextServices</code> that wants
	 *        to get the service.  Only weak references to this will
	 *        be retained, and it will never be changed, only queried
	 *        in a read-only manner.
	 * @param requestor the actual requestor of the service.  Only
	 *        weak references to this will be retained, and it will
	 *        never be changed, only queried in a read-only manner.
	 * @param serviceClass the <code>Class</code> of the service being
	 *        requested.
	 * @param serviceSelector a parameter to customize the service
	 *        returned with.
	 * @return an instance of <code>serviceClass</code> (such that
	 *        <code>instanceof</code> serviceClass is true), or
	 *        <code>null</code>.
	 * @see java.beans.beancontext.BeanContextServices#getService(java.beans.beancontext.BeanContextChild,java.lang.Object,java.lang.Class,java.lang.Object,java.beans.beancontext.BeanContextServiceRevokedListener)
	 */
	public Object getService(BeanContextServices services, Object requestor, Class serviceClass, Object serviceSelector);

	/**
	 * Release the service.
	 * <P>
	 *
	 * Called by <code>BeanContextServices.releaseService()</code>.
	 * <P>
	 *
	 * Most <code>BeanContextServiceProvider</code>s won't have to do
	 * anything here.
	 *
	 * @param services the <code>BeanContextServices</code> that wants
	 *        to release the service.  Only weak references to this will
	 *        be retained, and it will never be changed, only queried
	 *        in a read-only manner.
	 * @param requestor the original requestor of the service.
	 * @param service the service to relinquish
	 * @see java.beans.beancontext.BeanContextServices#releaseService(java.beans.beancontext.BeanContextChild,java.lang.Object,java.lang.Object)
	 */
	public void releaseService(BeanContextServices services, Object requestor, Object service);

	/**
	 * Get a list of valid service selectors for the specified service class.
	 * This method is called from
	 * <code>BeanContextServices.getCurrentServiceSelectors()</code>.
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
	 * @param services the <code>BeanContextServices</code> that wants
	 *        to get the service selectors.  Only weak references to this will
	 *        be retained, and it will never be changed, only queried
	 *        in a read-only manner.
	 * @param serviceClass the service class to get selectors for.
	 * @return a list of valid service selectors for the service
	 *         class, or <code>null</code>.
	 * @see java.beans.beancontext.BeanContextServices#getCurrentServiceSelectors(java.lang.Class)
	 */
	public Iterator getCurrentServiceSelectors(BeanContextServices services, Class serviceClass);
}
