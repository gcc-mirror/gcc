/* java.beans.beancontext.BeanContextChild
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

import java.beans.PropertyChangeListener;
import java.beans.PropertyVetoException;
import java.beans.VetoableChangeListener;

/**
 * Beans implement this to get information about the execution environment and
 * its services and to be placed in the hierarchy.
 * <P>
 *
 * The difference between a <code>BeanContext</code> and a
 * <code>BeanContextChild</code>, mainly, is that a
 * <code>BeanContext</code> may be a parent.
 * <P>
 *
 * <code>BeanContextChild</code> instances will be serialized at some
 * point in their life, but you need to make sure your bean context does
 * not contain a serializable reference (directly or indirectly) to the
 * parent <code>BeanContext</code>, to any of the other
 * <code>BeanContext</code>s in the tree, or to any resources obtained
 * via the <code>BeanContextServices</code> interface.  One way to do this
 * is to mark any fields that contain such references as
 * <code>transient</code>.  Another way is to use a custom serializer.
 * <P>
 *
 * If you do not do this, when the <code>BeanContext</code> is serialized,
 * all the other <code>BeanContext</code>s and other unnecessary things
 * will be serialized along with it.
 * <P>
 *
 * Before dying, a <code>BeanContextChild</code> should call
 * <code>getBeanContext().remove(this)</code> to detach from the
 * hierarchy and exit cleanly.
 *
 * @author John Keiser
 * @since JDK1.2
 * @see java.beans.beancontext.BeanContext
 */

public interface BeanContextChild {
	/**
	 * Set the parent <code>BeanContext</code>.
	 * <P>
	 *
	 * This method is called from <code>BeanContext.add()</code> and
	 * should not be called directly.
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
	 * When being removed from the current <code>BeanContext</code>,
	 * it is the <code>BeanContextChild</code>'s responsibility to
	 * release all services it has obtained.
	 * <P>
	 *
	 * This change should generate <code>PropertyChangeEvent</code>
	 * and <code>VetoableChangeEvent</code>s with the property name
	 * "beanContext".  If the change is vetoed, it must re-throw the
	 * exception and not change anything.  In this way, the parent
	 * <code>BeanContextChild</code>, who has registered himself with
	 * you, will have a chance to remove this child from its
	 * collection.
	 * <P>
	 *
	 * If the Bean does not wish to change the parent or be removed
	 * from one, it may throw the <code>PropertyVetoException</code>.
	 * If you veto a <code>setBeanContext(null)</code> call, then you
	 * should try your hardest to remedy whatever problem is keeping
	 * you from being removed from the <code>BeanContext</code> so
	 * that you can <em>not</em> veto it the next time.
	 * Otherwise, nasty pathological recursion stuff could occur in
	 * certain situations.
	 * <P>
	 *
	 * If you do veto the change, you must first back out any changes
	 * you made prior to the veto.  Best not to make any such changes
	 * prior to the veto in the first place.
	 * <P>
	 *
	 * This method is called from <code>BeanContext.add()</code> and
	 * should not be called directly.
	 *
	 * @param parent the new parent for the <code>BeanContextChild</code>,
	 *        or <code>null</code> to signify removal from a tree.
	 * @exception PropertyVetoException if the
	 *            <code>BeanContextChild</code> implementor does not
	 *            wish to have its parent changed.
	 */
	void setBeanContext(BeanContext parent)
		throws PropertyVetoException;

	/**
	 * Get the parent <code>BeanContext</code>.
	 * @return the parent <code>BeanContext</code>.
	 */
	BeanContext getBeanContext();

	/**
	 * Add a listener that will be notified when a specific property changes.
	 * @param prop the name of the property to listen on
	 * @param listener the listener to listen on the property.
	 */
	void addPropertyChangeListener(String prop, PropertyChangeListener listener);

	/**
	 * Remove a listener to a certain property.
	 * @param prop the name of the property being listened on
	 * @param listener the listener listening on the property.
	 */
	void removePropertyChangeListener(String prop, PropertyChangeListener listener);

	/**
	 * Add a listener that will be notified when a specific property
	 * change is requested (a PropertyVetoException may be thrown) as
	 * well as after the change is successfully made.
	 *
	 * @param prop the name of the property to listen on
	 * @param listener the listener to listen on the property.
	 */
	void addVetoableChangeListener(String prop, VetoableChangeListener listener);

	/**
	 * Remove a listener to a certain property.
	 * @param prop the name of the property being listened on
	 * @param listener the listener listening on the property.
	 */
	void removeVetoableChangeListener(String prop, VetoableChangeListener listener);
}
