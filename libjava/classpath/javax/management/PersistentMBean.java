/* PersistentMBean.java -- Interface for beans that should persist.
   Copyright (C) 2007 Free Software Foundation, Inc.

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

package javax.management;

/**
 * Beans may implement this interface in order to become
 * persistent.  The {@link #load()} method should be
 * called on construction in order to reload the stored
 * state.  The {@link #store()} method should be called
 * sometime during the bean's lifetime in order to create
 * a persistent copy of the bean's instance data.  This
 * method may also be called by the {@link MBeanServer}
 * as a result of the {@link Descriptor} of an
 * {@link javax.management.modelmbean.ModelMBean}.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public interface PersistentMBean
{

  /**
   * Instantiates the bean with the data previously stored
   * using a call to {@link #store()}.  The data stored can
   * include values held by attributes as well as those returned
   * by operations.  This method should be called during
   * construction or initialisation of the bean, before
   * it becomes registered with an {@link MBeanServer}.
   *
   * @throws MBeanException if persistence is not supported,
   *                        or another exception is thrown
   *                        (which this then wraps).
   * @throws RuntimeOperationsException if the persistence
   *                                    mechanism throws an
   *                                    exception.
   * @throws InstanceNotFoundException if the bean can not
   *                                   be found in the
   *                                   persistent store.
   */
  void load()
    throws MBeanException, RuntimeOperationsException,
           InstanceNotFoundException;

  /**
   * <p>
   * Captures the current state of the bean and stores it
   * for future retrieval by the {@link #load()} method.
   * The data stored can include values held by attributes
   * as well as those returned by operations.
   * </p>
   * <p>
   * Whether the state is stored or not depends on the
   * <code>persistPolicy</code> field of the MBean/attribute
   * descriptor.  The state should be stored if the policy
   * is set to any of the following:
   * </p>
   * <ul>
   * <li><code>always</code></li>
   * <li><code>onTimer</code> and <code>now</code> is
   * greater than or equal to <code>lastPersistTime +
   * persistPeriod</code>.</li>
   * <li><code>noMoreOftenThan</code> and <code>now</code> is
   * greater than or equal to <code>lastPersistTime +
   * persistPeriod</code>.</li>
   * <li>onUnregister</li>
   * </ul>
   * <p>If the policy is set to any of the following, the state
   * should not be stored:</p>
   * <ul>
   * <li><code>never</code></li>
   * <li><code>onUpdate</code></li>
   * <li><code>onTimer</code> and <code>now</code> is
   * less than <code>lastPersistTime + persistPeriod</code>.
   * </li>
   * </ul>
   *
   * @throws MBeanException if persistence is not supported,
   *                        or another exception is thrown
   *                        (which this then wraps).
   * @throws RuntimeOperationsException if the persistence
   *                                    mechanism throws an
   *                                    exception.
   * @throws InstanceNotFoundException if the persistent
   *                                   store can not be found
   *                                   or accessed.
   */
  void store()
    throws MBeanException, RuntimeOperationsException,
           InstanceNotFoundException;

}
