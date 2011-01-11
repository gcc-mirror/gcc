/* ServantLocatorOperations.java --
   Copyright (C) 2005, 2006 Free Software Foundation, Inc.

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


package org.omg.PortableServer;

import org.omg.PortableServer.ServantLocatorPackage.CookieHolder;

/**
 * Defines the operations, applicable to the {@link ServantLocator}.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public interface ServantLocatorOperations
  extends ServantManagerOperations
{
  /**
   * If the POA has the USE_SERVANT_MANAGER and NON_RETAIN policies, it
   * invokes this method whenever the object being requested that is not
   * inactive. This method has access to all details of the received
   * request and can use them to choose between servaral alternative servants.
   * It can also forward the request to another server.
   *
   * @param Object_Id the id of the object, on which the request was called.
   * @param poa the POA in those scope the object is active.
   * @param operation the name of the method or operation being invoked.
   * @param cookie_holder the holder where the servant manager can store
   * an arbitrary java.lang.Object. This object will be later passed as a
   * <code>cookie</code> parameter for {@link #postinvoke}, to create tie
   * between preinvoke and postinvoke. The application should <i>not</i>
   * suppose that each call of preinvoke is followed by the subsequent
   * postinvoke for the same invocation; under multi threaded policy these
   * calls may be intermixed.
   *
   * @return a servant that will serve the incoming request.
   *
   * @throws ForwardRequest if the locator decides to forward the request
   * to another object. The exception contains the object that should
   * handle this request. This object is usually remote, but can also
   * be local. As <code>preinvoke</code> is called on each method
   * invocation, the thrown exception will forward only this current request.
   */
  Servant preinvoke(byte[] Object_Id, POA poa, String operation,
                    CookieHolder cookie_holder
                   )
             throws ForwardRequest;

  /**
   * If the POA has the USE_SERVANT_MANAGER and NON_RETAIN policies, it
   * invokes this method whenever a servant completes a request.
   *
   * @param Object_Id the id of the object, on which the request was called.
   * @param poa the POA in those scope the object is active.
   * @param operation the name of the method or operation that was invoked.
   * @param cookie the object that has been previously set by preinvoke in
   * the <code>cookie_holder</code> parameter.
   * @param servant the servant, associated with the object.
   */
  void postinvoke(byte[] Object_Id, POA poa, String operation,
                  java.lang.Object cookie, Servant servant
                 );
}
