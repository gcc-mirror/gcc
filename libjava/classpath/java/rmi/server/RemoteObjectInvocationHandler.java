/* RemoteObjectInvocationHandler.java -- RMI stub replacement.
 Copyright (C) 2005 Free Software Foundation, Inc.

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


package java.rmi.server;

import gnu.java.rmi.server.RMIHashes;

import java.io.Serializable;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.rmi.Remote;
import java.rmi.RemoteException;
import java.rmi.UnexpectedException;
import java.rmi.registry.Registry;
import java.rmi.server.RemoteObject;
import java.rmi.server.RemoteRef;
import java.rmi.server.UnicastRemoteObject;
import java.util.Hashtable;

/**
 * Together with dynamic proxy instance, this class replaces the generated RMI
 * stub (*_Stub) classes that (following 1.5 specification) should be no longer
 * required. It is unusual to use the instances of this class directly in the
 * user program. Such instances are automatically created and returned by
 * {@link Registry} or {@link UnicastRemoteObject} methods if the remote
 * reference is known but the corresponding stub class is not accessible.
 *
 * @see Registry#lookup
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class RemoteObjectInvocationHandler extends RemoteObject implements
    InvocationHandler, Remote, Serializable
{
  /**
   * Use the jdk 1.5 SUID for interoperability.
   */
  static final long serialVersionUID = 2L;

  /**
   * The RMI method hash codes, computed once as described in the section 8.3
   * of the Java Remote Method Invocation (RMI) Specification.
   */
  static Hashtable methodHashCodes = new Hashtable();

  /**
   * The empty class array to define parameters of .hashCode and .toString.
   */
  static final Class[] noArgsC = new Class[0];

  /**
   * The class array to define parameters of .equals
   */
  static final Class[] anObjectC = new Class[] { Object.class };

  /**
   * The empty object array to replace null when no args are passed.
   */
  static final Object[] noArgs = new Object[0];

  /**
   * Construct the remote invocation handler that forwards calls to the given
   * remote object.
   *
   * @param reference the reference to the remote object where the method
   * calls should be forwarded.
   */
  public RemoteObjectInvocationHandler(RemoteRef reference)
  {
    super(reference);
  }

  /**
   * Invoke the remote method. When the known method is invoked on a created RMI
   * stub proxy class, the call is delivered to this method and then transferred
   * to the {@link RemoteRef#invoke(Remote, Method, Object[], long)} of the
   * remote reference that was passed in constructor. The methods are handled as
   * following:
   * <ul>
   * <li> The toString() method is delegated to the passed proxy instance.</li>
   * <li>The .equals method only returns true if the passed object is an
   * instance of proxy class and its invocation handler is equal to this
   * invocation handles.</li>
   * <li>The .hashCode returns the hashCode of this invocation handler (if the.</li>
   * <li>All other methods are converted to remote calls and forwarded to the
   * remote reference. </li>
   * </ul>
   *
   * @param proxyInstance
   *          the instance of the proxy stub
   * @param method
   *          the method being invoked
   * @param parameters
   *          the method parameters
   * @return the method return value, returned by RemoteRef.invoke
   * @throws IllegalAccessException
   *           if the passed proxy instance does not implement Remote interface.
   * @throws UnexpectedException
   *           if remote call throws some exception, not listed in the
   *           <code>throws</code> clause of the method being called.
   * @throws Throwable
   *           that is thrown by remote call, if that exception is listend in
   *           the <code>throws</code> clause of the method being called.
   */
  public Object invoke(Object proxyInstance, Method method, Object[] parameters)
      throws Throwable
  {
    if (!(proxyInstance instanceof Remote))
      {
        String name = proxyInstance == null ? "null"
                                           : proxyInstance.getClass().getName();
        throw new IllegalAccessException(name + " does not implement "
                                         + Remote.class.getName());
      }

    if (parameters == null)
      parameters = noArgs;

    String name = method.getName();
    switch (name.charAt(0))
      {
      case 'e':
        if (parameters.length == 1 && name.equals("equals")
            && method.getParameterTypes()[0].equals(Object.class))
          {
            if (parameters[0] instanceof Proxy)
              {
                Object handler = Proxy.getInvocationHandler(parameters[0]);
                if (handler == null)
                  return Boolean.FALSE;
                else
                  return handler.equals(this) ? Boolean.TRUE : Boolean.FALSE;
              }
            else
              return Boolean.FALSE;
          }
        break;
      case 'h':
        if (parameters.length == 0 && name.equals("hashCode"))
          {
            int hashC = Proxy.getInvocationHandler(proxyInstance).hashCode();
            return new Integer(hashC);
          }
        break;
      case 't':
        if (parameters.length == 0 && name.equals("toString"))
          return "Proxy stub:"+ref.remoteToString();
        break;
      default:
        break;
      }

    Long hash = (Long) methodHashCodes.get(method);
    if (hash == null)
      {
        hash = new Long(RMIHashes.getMethodHash(method));
        methodHashCodes.put(method, hash);
      }

    try
      {
        return getRef().invoke((Remote) proxyInstance, method, parameters,
                               hash.longValue());
      }
    catch (RuntimeException exception)
      {
        // RuntimeException is always supported.
        throw exception;
      }
    catch (RemoteException exception)
      {
        // All remote methods can throw RemoteException.
        throw exception;
      }
    catch (Error exception)
      {
        throw exception;
      }
    catch (Exception exception)
      {
        Class[] exceptions = method.getExceptionTypes();
        Class exceptionClass = exception.getClass();

        for (int i = 0; i < exceptions.length; i++)
          {
            if (exceptions[i].equals(exceptionClass))
              throw exception;
          }
        throw new UnexpectedException(method.getName(), exception);
      }
  }

}
