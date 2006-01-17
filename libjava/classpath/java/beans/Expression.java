/* java.beans.Expression
   Copyright (C) 2004, 2005 Free Software Foundation, Inc.

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

package java.beans;

/**
 * <p>An Expression captures the execution of an object method
 * that returns a value.</p>
 * 
 * <p>It stores an object, the method to call, and the arguments to pass to
 * the method.</p>
 * 
 * <p>While this class can generally be used to describe method calls it is
 * part of the XML serialization API.</p> 
 * 
 * @author Robert Schuster (robertschuster@fsfe.org)
 * @since 1.4
 */
public class Expression extends Statement
{
  // This is a placeholder to indicate that value hasn't been set
  // yet;
  private static final Object UNSET = new Object();

  // The value to return. This is equal to unset until getValue is called.
  private Object value;

  /**
   * Constructor Constructs an Expression representing the invocation of
   * object.methodName(arg[0], arg[1], ...); However, it will never be executed.
   * Instead, value will always be returned.
   * 
   * @param value
   *          The value to return.
   * @param target
   *          The object to invoke the method on.
   * @param methodName
   *          The object method to invoke.
   * @param arguments
   *          An array of arguments to pass to the method.
   */
  public Expression(Object value, Object target, String methodName,
                    Object[] arguments)
  {
    super(target, methodName, arguments);
    this.value = value;
  }

  /**
   * Constructor Constructs an Expression representing the invocation of
   * object.methodName(arg[0], arg[1], ...);
   * 
   * @param target
   *          The object to invoke the method on.
   * @param methodName
   *          The object method to invoke.
   * @param arguments
   *          An array of arguments to pass to the method.
   */
  public Expression(Object target, String methodName, Object[] arguments)
  {
    super(target, methodName, arguments);
    this.value = UNSET;
  }

  /**
   * Return the result of executing the method. If the cached value has not yet
   * been set, the method is executed in the same way as Statement.execute(),
   * except that the value is cached, and then returned. If the value has been
   * set, it is returned without executing the method again.
   * 
   * @return the result of executing the method.
   * @exception Exception
   *              if an error occurs
   */
  public Object getValue() throws Exception
  {
    if (value == UNSET)
      value = doExecute();
    return value;
  }

  /**
   * Set the cached value to be returned by getValue()
   * 
   * @param value
   *          the value to cache and return.
   */
  public void setValue(Object value)
  {
    this.value = value;
  }

  /**
   * Return a string representation of this expression.
   */
  public String toString()
  {
    String result = super.toString();
    if (value != UNSET)
      return value.getClass().getName() + "=" + result;
    return result;
  }
}
