/* java.beans.beancontext.BeanContext
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

import java.beans.DesignMode;
import java.beans.Visibility;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.Collection;

/**
 * Acts as a container for sub-beans and as a sub-bean,
 * so that an entire hierarchy of beans can be made up of
 * <code>BeanContext</code>s.
 * <P>
 *
 * Since I can't sprinkle the <code>Collections</code> interface
 * documentation with special information for <code>BeanContext</code>
 * implementors, I'll have to document special requirements for
 * implementors of those functions here.
 * <P>
 *
 * <code><strong>add()</strong></code> or <code>addAll()</code>:
 * <br>
 * <OL>
 *   <LI>
 *     May add any <code>Object</code> into the hierarchy as well as a
 *     <code>BeanContextChild</code>, <code>BeanContext</code> or
 *     <code>BeanContextProxy</code> object.
 *     This way, any Bean can be in the hierarchy.
 *   </LI>
 *   <LI>
 *     Must synchronize on <code>BeanContext.globalHierarchyLock</code>.
 *   </LI>
 *   <LI>
 *     Don't add the <code>Object</code> if it's already there (only once
 *     per <code>BeanContext</code>).
 *   </LI>
 *   <LI>
 *     If it is a <code>BeanContextChild</code> implementor, call
 *     <code>setBeanContext()</code> on it.  If it's a
 *     <code>BeanContextProxy</code> implementor, call
 *     <code>getBeanContextProxy().setBeanContext()</code> on it.
 *     If <code>setBeanContext()</code> vetoes the change, back out
 *     all changes so far and throw <code>IllegalStateException</code>.
 *   </LI>
 *   <LI>
 *     If it (or its proxy) implements <code>Visibility</code>, call
 *     <code>dontUseGui()</code> or <code>okToUseGui()</code> on it,
 *     depending on whether you (the <code>BeanContext</code>) feel like
 *     allowing it to use the GUI or not.
 *   </LI>
 *   <LI>
 *     If it implements <code>BeanContextChild</code> or
 *     <code>BeanContextProxy</code>, register yourself (the
 *     <code>BeanContext</code>) as both a
 *     <code>PropertyChangeListener</code> and
 *     <code>VetoableChangeListener</code> on the "beanContext"
 *     property (it may also add itself on any other properties it wishes
 *     to).
 *   </LI>
 *   <LI>
 *     If it is a listener or event source that you (the
 *     <code>BeanContext</code>) are interested in, you may register
 *     yourself to it or register it to you.
 *   </LI>
 *   <LI>
 *     Fire a <code>java.beans.beancontext.BeanContextMembershipEvent</code>
 *     before exiting.  <code>addAll()</code> should wait until everything
 *     is done changing before firing the event (or events) so that if a
 *     failure occurs, the backing-out process can proceed without any
 *     events being fired at all.
 *   </LI>
 * </OL>
 * <P>
 *
 * <code><strong>remove()</strong></code> or <code>removeAll()</code>:
 * <br>
 * <OL>
 *   <LI>
 *     Must synchronize on <code>BeanContext.globalHierarchyLock</code>.
 *   </LI>
 *   <LI>
 *     If the specified <code>Object</code> is not a child of this
 *     <code>BeanContext</code>, just exit without performing any actions.
 *   </LI>
 *   <LI>
 *     Remove the <code>Object</code> from your collection of children.
 *   </LI>
 *   <LI>
 *     If it is a <code>BeanContextChild</code> implementor, call
 *     <code>setBeanContext(null)</code> on it.  If it's a
 *     <code>BeanContextProxy</code> implementor, call
 *     <code>getBeanContextProxy().setBeanContext(null)</code> on it.
 *     If <code>setBeanContext()</code> vetoes the change, back out
 *     all changes so far and throw <code>IllegalStateException</code>.
 *   </LI>
 *   <LI>
 *     If you registered the <code>Object</code> to listen to you or
 *     registered yourself as a listener on the <code>Object</code> during
 *     <code>add()</code> or <code>addAll()</code>, undo the registration
 *     bycalling the appropriate <code>removeListener()</code> method.
 *   </LI>
 *   <LI>
 *     Fire a <code>java.beans.beancontext.BeanContextMembershipEvent</code>
 *     before exiting.  <code>removeAll()</code> should wait until
 *     everything is done changing before firing the event (or events) so
 *     that if a failure occurs, the backing-out process can proceed
 *     without any events being fired at all.
 *   </LI>
 * </OL>
 * <P>
 *
 * <code>addAll()</code>, <code>removeAll()</code>,
 * <code>retainAll()</code> and <code>clear()</code> do not need to be
 * implemented, but may be if so desired.
 * <P>
 *
 * Similarly, <code>Visibility</code> and <code>DesignMode</code> methods
 * should propagate changed values to children that implement interfaces
 * of the same name.
 * <P>
 *
 * A hierarchy of beans is mainly useful so that different sets of beans
 * can be established, each with their own set of resources.
 *
 * @author John Keiser
 * @since JDK1.2
 */

public interface BeanContext
        extends Collection, BeanContextChild, Visibility, DesignMode {

        /**
         * The global lock on changing any BeanContext hierarchy.
         * It kinda sucks that there is only one lock, since there can be
         * multiple hierarchies.  Oh well, I didn't design, I just code.
         * <P>
         *
         * Methods that must (or do) synchronize on the global lock:
         * <BR>
         * <UL>
         *   <LI>
         *     Implementors of <CODE>BeanContext.add()</CODE> and <code>addAll()</code>
         *   </LI>
         * </UL>
         * @fixme fill in the rest of the methods which use the global lock.
         */
        Object globalHierarchyLock = new Object();

        /**
         * Instantiate a Bean using this Bean's <code>ClassLoader</code>
         * and this <code>BeanContext</code> as the parent.
         * <P>
         *
         * This method exists mainly so that <code>BeanContext</code>
         * implementations can perform extra actions on Beans that are
         * created within them.
         *
         * @param beanName the name of the bean to instantiate
         * @return the created Bean
         *
         * @see java.beans.Beans#instantiate(java.lang.ClassLoader,java.lang.String)
         * @see java.beans.Beans#instantiate(java.lang.ClassLoader,java.lang.String,java.beans.beancontext.BeanContext)
         * @exception IOException if there is an I/O problem during
         *            instantiation.
         * @exception ClassNotFoundException if a serialized Bean's class
         *            is not found.
         */
        Object instantiateChild(String beanName)
                        throws IOException,
                               ClassNotFoundException;

        /**
         * Get a resource.  The <code>BeanContext</code> will typically
         * call <code>ClassLoader.getResource()</code>, but may do it any
         * way it wants to.  This allows a <code>BeanContext</code> to
         * have its own set of resources separate from the rest of the
         * system.
         * <P>
         *
         * Beans should call this method on their parent rather than the
         * associated <code>ClassLoader</code> method.
         * <P>
         *
         * I am assuming, but am not entirely sure, that if a
         * <code>BeanContext</code> cannot find a resource, its
         * responsibility is to call the <code>getResource</code> method
         * of its parent <code>BeanContext</code>.
         *
         * @return a URL to the requested resource.
         * @param resourceName the name of the resource requested.
         * @param requestor a reference to the child requesting the resource.
         * @see java.lang.ClassLoader#getResource(java.lang.String)
         */
        URL getResource(String resourceName, BeanContextChild requestor);

        /**
         * Get a resource as a stream.  The <code>BeanContext</code> will
         * typically call <code>ClassLoader.getResourceAsStream()</code>,
         * but may do it any way it wants to.  This allows a
         * <code>BeanContext</code>'s children to have their own set of
         * resources separate from the rest of the system.
         * <P>
         *
         * Beans should call this method on their parent rather than the
         * associated <code>ClassLoader</code> method.
         * <P>
         *
         * I am assuming, but am not entirely sure, that if a
         * <code>BeanContext</code> cannot find a resource, its
         * responsibility is to call the <code>getResourceAsStream</code>
         * method of its parent <code>BeanContext</code>.
         *
         * @return the requested resource as a stream.
         * @param resourceName the name of the resource requested.
         * @param requestor a reference to the child requesting the resource.
         * @see java.lang.ClassLoader#getResourceAsStream(java.lang.String)
         */
        InputStream getResourceAsStream(String resourceName, BeanContextChild requestor);

        /**
         * Add a listener on changes to the membership of this
         * <code>BeanContext</code> object.
         * @param listener the listener to add.
         */
        void addBeanContextMembershipListener(BeanContextMembershipListener listener);

        /**
         * Remove a listener on changes to the membership of this
         * <code>BeanContext</code> object.
         * @param listener the listener to remove.
         */
        void removeBeanContextMembershipListener(BeanContextMembershipListener listener);
}
