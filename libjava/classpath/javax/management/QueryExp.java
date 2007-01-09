/* QueryExp.java -- Represents a query expression.
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
 * Applies the type of relational constraints seen in the
 * <code>where</code> clauses of databases to an
 * {@link ObjectName}.  Instances of this class are usually
 * returned by the static methods of the {@link Query} classes.
 * If a custom implementation is required, it is better to
 * extend the {@link QueryEval} class, rather than simply
 * implementing this interface, in order to ensure that
 * the {@link #setMBeanServer(MBeanServer)} method functions
 * correctly.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public interface QueryExp
  extends Serializable
{

  /**
   * Applies the query to the specified management bean.
   *
   * @param name the name of the management bean.
   * @return true if the query was applied successfully.
   * @throws BadStringOperationException if an invalid string
   *                                     operation is used by
   *                                     the query.
   * @throws BadBinaryOpValueExpException if an invalid expression
   *                                      is used by the query.
   * @throws BadAttributeValueExpException if an invalid attribute
   *                                       is used by the query.
   * @throws InvalidApplicationException if the query is applied
   *                                     to the wrong type of bean.
   */
  boolean apply(ObjectName name)
    throws BadStringOperationException, BadBinaryOpValueExpException,
	   BadAttributeValueExpException, InvalidApplicationException;

  /**
   * Changes the {@link MBeanServer} on which this query is performed.
   *
   * @param server the new server to use.
   */
  void setMBeanServer(MBeanServer server);

}

