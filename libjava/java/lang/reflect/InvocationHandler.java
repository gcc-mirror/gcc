/* java.lang.reflect.InvocationHandler - dynamically executes methods in
   proxy instances
   Copyright (C) 2001 Free Software Foundation, Inc.

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


package java.lang.reflect;

/**
 * This interface defines an invocation handler.  Suppose you are using
 * reflection, and found a method that requires that its parameter
 * be an object of a given interface.  You want to call this method,
 * but have no idea what classes implement that interface.  So, you can
 * create a {@link Proxy} instance, a convenient way to dynamically
 * generate a class that meets all the necessary properties of that
 * interface.  But in order for the proxy instance to do any good, it
 * needs to know what to do when interface methods are invoked!  So,
 * this interface is basically a cool wrapper that provides runtime
 * code generation needed by proxy instances.<p>
 *
 * While this interface was designed for use by Proxy, it will also
 * work on any object in general.<p>
 *
 * Hints for implementing this class:<br>
 * <ul>
 *  <li>Don't forget that Object.equals, Object.hashCode, and
 *      Object.toString will call this handler.  In particular,
 *      a naive call to proxy.equals, proxy.hashCode, or proxy.toString
 *      will put you in an infinite loop.  And remember that string
 *      concatenation also invokes toString.</li>
 *  <li>Obey the contract of the Method object you are handling, or
 *      the proxy instance will be forced to throw a
 *      {@link NullPointerException}, {@link ClassCastException},
 *      or {@link UndeclaredThrowableException}.</li>
 *  <li>Be prepared to wrap/unwrap primitives as necessary.</li>
 *  <li>The Method object may be owned by a different interface than
 *      what was actually used as the qualifying type of the method
 *      invocation in the Java source code. This means that it might
 *      not always be safe to throw an exception listed as belonging
 *      to the method's throws clause.</li>
 * </ul>
 *
 * <p><small>For a fun time, create an InvocationHandler that handles the
 * methods of a proxy instance of the InvocationHandler interface!</small>
 *
 * @see Proxy
 * @see UndeclaredThrowableException
 *
 * @author Eric Blake <ebb9@email.byu.edu>
 * @since 1.3
 * @status updated to 1.4
 */
public interface InvocationHandler
{
  /**
   * When a method is invoked on a proxy instance, it is wrapped and
   * this method is called instead, so that you may decide at runtime
   * how the original method should behave.
   *
   * @param proxy the instance that the wrapped method should be
   *        invoked on.  When this method is called by a Proxy object,
   *        `proxy' will be an instance of {@link Proxy}, and oddly enough,
   *        <code>Proxy.getInvocationHandler(proxy)</code> will return
   *        <code>this</code>!
   * @param method the reflected method to invoke on the proxy.
   *        When this method is called by a Proxy object, 'method'
   *        will be the reflection object owned by the declaring
   *        class or interface, which may be a supertype of the
   *        interfaces the proxy directly implements.
   * @param args the arguments passed to the original method, or
   *        <code>null</code> if the method takes no arguments.
   *        (But also be prepared to handle a 0-length array).
   *        Arguments of primitive type, such as <code>boolean</code>
   *        or <code>int</code>, are wrapped in the appropriate
   *        class such as {@link Boolean} or {@link Integer}.
   * @return whatever is necessary to return from the wrapped method.
   *         If the wrapped method is <code>void</code>, the proxy
   *         instance will ignore it.  If the wrapped method returns
   *         a primitive, this must be the correct wrapper type whose value
   *         is exactly assignable to the appropriate type (no widening
   *         will be performed); a null object in this case causes a
   *         {@link NullPointerException}.  In all remaining cases, if
   *         the returned object is not assignment compatible to the
   *         declared type of the original method, the proxy instance
   *         will generate a {@link ClassCastException}.
   * @throws Throwable this interface is listed as throwing anything,
   *         but the implementation should only throw unchecked
   *         exceptions and exceptions listed in the throws clause of
   *         all methods being overridden by the proxy instance.  If
   *         something is thrown that is not compatible with the throws
   *         clause of all overridden methods, the proxy instance will
   *         wrap the exception in an UndeclaredThrowableException.
   *         Note that an exception listed in the throws clause of the
   *         `method' parameter might not be declared in additional
   *         interfaces also implemented by the proxy object.
   *
   * @see Proxy
   * @see UndeclaredThrowableException
   */
  Object invoke(Object proxy, Method method, Object[] args)
    throws Throwable;

}
