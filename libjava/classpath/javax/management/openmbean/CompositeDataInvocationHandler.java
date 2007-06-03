/* CompositeDataInvocationHandler.java - Pseudo-accessors for CompositeData.
   Copyright (C) 2007 Free Software Foundation

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

package javax.management.openmbean;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;

/**
 * <p>
 * Provides an {@link java.lang.reflect.InvocationHandler} which
 * implements a series of accessor methods (those beginning with
 * {@code "get"} or {@code "is"}) using the content of a
 * {@link CompositeData} object.  An instance of {@link CompositeData}
 * consists of a series of key-value mappings.  This handler assumes
 * these keys to be the names of attributes, and thus provides
 * accessor methods by returning the associated value.  
 * </p>
 * <p>
 * As an example, consider the following interface:
 * </p>
 * <pre>
 * public interface Person
 * {
 *   public String getName();
 *   public Date getBirthday();
 * }
 * </pre>
 * <p>
 * This specifies two accessor methods for retrieving the attributes,
 * {@code name} and {@code birthday}.  An implementation of this interface
 * can be provided by creating an instance of this class, using a
 * {@link CompositeData} object with appropriate key-value mappings
 * (e.g. "name" => "Fred", "birthday" => 30/06/1974), and then passing
 * that to {@link java.lang.reflect.Proxy#newProxyInstance} along with
 * the interface itself.  The invocation handler implements the methods
 * by calling {@link CompositeData#get(String)} with the appropriate key.
 * </p>
 * <p>
 * The attribute name is derived by taking the remainder of the method
 * name following {@code "get"}.  If the first letter of this substring
 * is uppercase, then two attempts are made to retrieve the attribute
 * from the {@link CompositeData} instance: one using the original substring,
 * and one with the first letter changed to its lower-case equivalent.
 * If the first letter is lowercase, only the exact substring is used.
 * </p>
 * <p>
 * An {@link Object#equals(Object)} implementation is provided.  This returns
 * true if the argument is a proxy with a {@link CompositeDataInvocationHandler}
 * using an equivalent {@link CompositeData} instance.  {@link Object#hashCode()}
 * is also defined so as to match this implementation and give equivalent instances
 * the same hashcode.
 * </p>
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.6
 */
public class CompositeDataInvocationHandler
  implements InvocationHandler
{

  /**
   * The {@link CompositeData} providing the key-value mappings.
   */
  private CompositeData data;

  /**
   * Constructs a new {@link CompositeDataInvocationHandler}
   * with the specified {@link CompositeData} instance.
   *
   * @param data the {@link CompositeData} instance to use.
   * @throws IllegalArgumentException if {@code data} is {@code null}.
   */
  public CompositeDataInvocationHandler(CompositeData data)
  {
    if (data == null)
      throw new IllegalArgumentException("The CompositeData instance " +
					 "must be non-null.");
    this.data = data;
  }

  /**
   * Returns the {@link CompositeData} instance which provides
   * the key-value mappings for this instance.  This is never
   * {@code null}.
   *
   * @return the {@link CompositeData} instance.
   */
  public CompositeData getCompositeData()
  {
    return data;
  }

  /**
   * Called by the proxy class whenever a method is called.  The
   * handler only deals with accessor methods (beginning with
   * {@code "get"} or {@code "is"}), {@code equals}, and
   * {@code "hashCode"}.  Accessor methods are implemented by
   * returning the appropriate value from the {@link CompositeData}
   * instance, while {@code equals} and {@code hashCode} allow
   * two proxies with a {@link CompositeDataInvocationHandler} using
   * the same {@link CompositeData} instance to be classified
   * as equivalent.
   *
   * @param proxy the proxy on which the method was called.
   * @param method the method which was called.
   * @param args the arguments supplied to the method.
   * @return the return value from the method.
   * @throws Throwable if an exception is thrown in the process.
   */
  public Object invoke(Object proxy, Method method, Object[] args)
    throws Throwable
  {
    String mName = method.getName();
    if (mName.equals("equals"))
      {
	if (args[0] instanceof Proxy)
	  {
	    InvocationHandler h = Proxy.getInvocationHandler(args[0]);
	    if (h instanceof CompositeDataInvocationHandler)
	      return data.equals(((CompositeDataInvocationHandler)
				  h).getCompositeData());
	  }
	return false;
      }
    if (mName.equals("hashCode"))
      {
	return data.hashCode();
      }
    String attrib = null;
    if (mName.startsWith("get"))
      attrib = mName.substring(3);
    else if (mName.startsWith("is"))
      attrib = mName.substring(2);
    if (attrib == null)
      throw new NoSuchMethodException(mName + " is not an accessor.");
    if (!data.containsKey(attrib))
      {
	if (Character.isLowerCase(attrib.charAt(0)))
	  throw new NoSuchMethodException("The attribute " +
					  attrib + " is not available " +
					  "in the given CompositeData " +
					  "object");
	attrib = Character.toLowerCase(attrib.charAt(0))
	  + attrib.substring(1);
	if (!data.containsKey(attrib))
	  throw new NoSuchMethodException("The attribute " +
					  attrib + " is not available " +
					  "in the given CompositeData " +
					  "object");
      }
    return data.get(attrib);
  }

}
