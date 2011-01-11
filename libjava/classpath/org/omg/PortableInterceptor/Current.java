/* Current.java --
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


package org.omg.PortableInterceptor;

import org.omg.CORBA.portable.IDLEntity;

/**
 * <p>
 * The portable interceptor Current (PICurrent) contains multiple slots where an
 * interceptor can rememeber the request - specific values between subsequent
 * calls of the interceptor methods. In multithreaded environment, it is not
 * possible just to store such data in the interceptor object fields.
 * </p>
 * <p>
 * On the client side, it is possible to set the initial slot values by
 * modifying slots on the Current, returend by ORB.resolve_initial_references
 * ("PICurrent"). The returned value is narrowed with the
 * {@link CurrentHelper#narrow}. On the subsequent invocation, made from the
 * same thread, the interceptors will see the initial slot values as they were
 * set using this approach.
 * </p>
 * <p>
 * There are no way to set the initial values for the server side interceptors,
 * the default values (Any with typecode TCKind.tk_null) should be always
 * assumed.
 * </p>
 * <p>
 * Since an Interceptor is running in a thread, it is running with a thread
 * context and there is a PICurrent on that context. If the Interceptor calls
 * ORB.resolve_initial_references ("PICurrent"), it gets the PICurrent within
 * its thread scope. This PICurrent is different than the request scope
 * PICurrent that the Interceptor obtains via calls to the Client- or Server-
 * RequestInfo object.
 * </p>
 * <p>
 * On the client side the PICurrent can be used to detect the recursive
 * invocations, performed by interceptors. If one of the interceptors makes call
 * via the same ORB, this call is then showed to all interceptors, including the
 * interceptor that made it. To avoid infinite recursion, the during each call
 * this interceptor can set some "recursion flag" into one of the slots of the
 * PICurrent. If the flag is set on the entry point, this indicates a recursive
 * call of that request.
 * </p>
 */
public interface Current extends CurrentOperations,
  org.omg.CORBA.Current,
  IDLEntity
{
}
