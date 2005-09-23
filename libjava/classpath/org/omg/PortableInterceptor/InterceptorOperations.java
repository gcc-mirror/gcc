/* InterceptorOperations.java --
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


/**
 * Defines operations, applicable for all types of {@link Interceptor}.
 * The the derived interfaces define additional operations for they
 * specific functionality.
 *
 * Portable Interceptors are hooks into the ORB through which ORB services can
 * intercept the normal flow of execution in creation of IOR, sending request,
 * receiving request and returning the reply.
 *
 * See {@link org.omg.PortableInterceptor} for more details about the possible
 * interceptors and how to register them within the ORB.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public interface InterceptorOperations
{
  /**
   * This method is called when orb is being destroyed and destroys
   * the interceptor. The ORB calls this method after completing all
   * incoming requests. The method body should not invoke methods on other
   * object, belonging to ORB being destoryed, as in this stage it is no
   * longer capable to act as server. It is still, however, capable
   * to act as a client, permitting remote invocations on other objects.
   */
  void destroy();

  /**
   * All interceptors of the same type, registered on the single ORB, must
   * either have different names or be anonymous. The name of the anonymous
   * interceptor is an empty string. The ORB supports multiple anonymous
   * interceptors of the same type.
   *
   * @return the name of the interceptor.
   */
  String name();
}