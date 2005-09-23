/* ServantActivatorOperations.java --
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


package org.omg.PortableServer;


/**
 * Defines the operations, applicable to the {@link ServantActivator}.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public interface ServantActivatorOperations
  extends ServantManagerOperations
{
  /**
   * This method is invoked whenever a servant for an object is deactivated,
   * assuming the POA has the USE_SERVANT_MANAGER and RETAIN policies.
   *
   * @param Object_Id the Id of the object being deactivated.
   *
   * @param poa the POA in those scope the object was active.
   *
   * @param servant the servant, serving the object being deactivated.
   *
   * @param cleanup_in_progress if true, this method was called from due
   * deactivation or destruction operation. False indicates that the method
   * was called due other reasons.
   *
   * @param remaining_activations if true, at the invocation moment the
   * passed servant is also associated with other objects in the active
   * object map of the given POA.
   */
  void etherealize(byte[] Object_Id, POA poa, Servant servant,
                   boolean cleanup_in_progress, boolean remaining_activations
                  );

  /**
   * This method is invoked whenever the POA receives a request for an
   * object that is not currently active, assuming the POA has the
   * USE_SERVANT_MANAGER and RETAIN policies. The user-supplied servant
   * manager is responsible for locating or creating an appropriate servant
   * that corresponds to the ObjectId value. The subsequent requests with
   * the same ObjectId value will be delivered directly to that servant
   * without invoking the servant manager.
   *
   * @param Object_Id the ObjectId value associated with the incoming request.
   * @param poa the POA in which the object is being activated.
   *
   * @return a servant that will be used to process the incoming request.
   *
   * @throws ForwardRequest if the activator decides to forward the request
   * to another object. The exception contains the object that should
   * handle this request. This object is usually remote, but can also
   * be local. The throws exception will forward all subsequent requests
   * till the new activation.
   */
  Servant incarnate(byte[] Object_Id, POA poa)
             throws ForwardRequest;
}