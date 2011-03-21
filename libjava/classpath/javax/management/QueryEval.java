/* QueryEval.java -- An evaluation context for a MBean server query.
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

import java.io.Serializable;

/**
 * Represents the evaluation context of a {@link MBeanServer}
 * query by retaining the server used on a thread-by-thread
 * basis.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public class QueryEval
  implements Serializable
{

  /**
   * Compatible with JDK 1.5
   */
  private static final long serialVersionUID = 2675899265640874796L;

  /**
   * Stores the server used, on a
   * thread-by-thread basis.
   */
  private static InheritableThreadLocal<MBeanServer> server =
    new InheritableThreadLocal<MBeanServer>();

  /**
   * Returns the {@link MBeanServer} last supplied to the
   * {@link #setMBeanServer(MBeanServer)} method.  If this method
   * has not been called for this thread, then the value will be
   * inherited from any parent thread on which the method has
   * been called.  If the method has never been called, then
   * <code>null</code> is returned.
   *
   * @return the server.
   * @see #setMBeanServer(MBeanServer)
   */
  public static MBeanServer getMBeanServer()
  {
    return server.get();
  }

  /**
   * Sets the {@link MBeanServer} on which the query will be
   * performed.  This value is inherited automatically by
   * child threads.  This method is only non-static for historical
   * reasons; it makes no use of instance-related values.
   *
   * @param svr the server to use.
   */
  public void setMBeanServer(MBeanServer svr)
  {
    server.set(svr);
  }

}
