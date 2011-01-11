/* MBeanServerBuilder.java -- Creates a default management server.
   Copyright (C) 2006 Free Software Foundation, Inc.

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

import gnu.javax.management.Server;

/**
 * Constructs a default implementation of an {@link MBeanServer}.
 * The {@link MBeanServerFactory} allows custom implementations of
 * {@link MBeanServer} to be used by providing subclasses of this.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public class MBeanServerBuilder
{

  /**
   * Constructs a new {@link MBeanServerBuilder}.
   */
  public MBeanServerBuilder()
  {
  }

  /**
   * <p>
   * Creates a new {@link MBeanServer} implementation with the specified
   * default domain, delegate and outer server.  The latter is the server
   * passed to the {@link MBeanRegistration} interface of management beans,
   * allowing an {@link MBeanServer} implementation to wrap another in order
   * to provide additional checks, etc.  If this value is <code>null</code>,
   * <code>this</code> is passed to beans instead.
   * </p>
   * <p>
   * The {@link MBeanServerFactory} calls this method after having first
   * created a delegate using the {@link #newMBeanServerDelegate()} method.
   * However, the delegate used in the call to this method may not be the
   * same as that returned by {@link #newMBeanServerDelegate()} as the factory
   * can optionally wrap the delegate before calling this method.
   * </p>
   *
   * @param defaultDomain the default domain used by the new server.
   * @param outer the {@link MBeanServer} passed to the {@link MBeanRegistration}
   *              interface of management beans.
   * @param delegate the delegate bean associated with the server, which must
   *                 be registered as a management bean by the server.
   * @return a new instance of a server implementation.
   */
  public MBeanServer newMBeanServer(String defaultDomain, MBeanServer outer,
                                    MBeanServerDelegate delegate)
  {
    return new Server(defaultDomain, outer, delegate);
  }

  /**
   * Creates a new {@link MBeanServerDelegate}, which will be used by
   * a management server.  The returned delegate may either be used directly
   * by the server, or may be further wrapped to add additional checks.
   *
   * @return a new instance of {@link MBeanServerDelegate}.
   */
  public MBeanServerDelegate newMBeanServerDelegate()
  {
    return new MBeanServerDelegate();
  }

}
