/* StringValueExp.java -- Represents strings to be passed to queries.
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
 * Represents a string being used as an argument
 * to a relational constraint.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public class StringValueExp
  implements ValueExp
{

  /**
   * Compatible with JDK 1.5
   */
  private static final long serialVersionUID = -3256390509806284044L;

  /**
   * The string value.
   */
  private String val;

  /**
   * Constructs a new {@link StringValueExp}.
   */
  public StringValueExp()
  {
  }

  /**
   * Constructs a new {@link StringValueExp} using the
   * specified value.
   *
   * @param val the string value used for this expression.
   */
  public StringValueExp(String val)
  {
    this.val = val;
  }

  /**
   * Applies the {@link StringValueExp} to the specified
   * management bean by simply returning the value.
   *
   * @param name the {@link ObjectName} of the bean.
   * @return the {@link StringValueExp} itself.
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
  public ValueExp apply(ObjectName name)
    throws BadStringOperationException, BadBinaryOpValueExpException,
           BadAttributeValueExpException, InvalidApplicationException
  {
    return this;
  }

  /**
   * Returns the value.
   *
   * @return the value.
   */
  public String getValue()
  {
    return val;
  }

  /**
   * Sets the {@link MBeanServer} on which the query
   * will be performed.
   *
   * @param server the new server.
   */
  public void setMBeanServer(MBeanServer server)
  {
    /* This seems to do nothing any more */
  }

  /**
   * Returns the value in quotes.
   *
   * @return the value (quoted).
   */
  public String toString()
  {
    return "'" + val + "'";
  }

}
