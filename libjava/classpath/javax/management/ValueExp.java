/* ValueExp.java -- Represents values that can be passed to queries.
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

import java.io.Serializable;

/**
 * Represents values that may be passed as arguments to
 * {@link QueryExp}s.  Strings, numbers and bean attributes
 * are all valid argument types for query expressions, and
 * so should be represented as implementations of this
 * interface.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public interface ValueExp
  extends Serializable
{

  /**
   * Applies the value expression to the specified management bean.
   *
   * @param name the name of the management bean.
   * @return the value expression.
   * @throws BadStringOperationException if an invalid string
   *                                     operation is used by
   *                                     the value expression.
   * @throws BadBinaryOpValueExpException if an invalid expression
   *                                      is used by the value expression.
   * @throws BadAttributeValueExpException if an invalid attribute
   *                                       is used by the value expression.
   * @throws InvalidApplicationException if the value expression is applied
   *                                     to the wrong type of bean.
   */
  ValueExp apply(ObjectName name)
    throws BadStringOperationException, BadBinaryOpValueExpException,
	   BadAttributeValueExpException, InvalidApplicationException;

  /**
   * Changes the {@link MBeanServer} on which this query is performed.
   *
   * @param server the new server to use.
   * @deprecated This method is superfluous, as the {@link ValueExp}
   *             can access the server using
   *             {@link QueryEval#getMBeanServer()}.
   */
  void setMBeanServer(MBeanServer server);

}

