/* RequestInfoOperations.java --
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


package org.omg.PortableInterceptor;

import org.omg.CORBA.Any;
import org.omg.CORBA.BAD_PARAM;
import org.omg.CORBA.NO_RESOURCES;
import org.omg.CORBA.TypeCode;
import org.omg.Dynamic.Parameter;
import org.omg.IOP.ServiceContext;

/**
 * Defines operations that are applicable for both server and client request.
 * The additional operations, specific to the server and client request are
 * defined in the derived interfaces {@link ServerRequestInfoOperations} and
 * {@link ClientRequestInfoOperations}.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public interface RequestInfoOperations
{
  /**
   * Return the parameters of the operation being invoked.
   *
   * @return the array, containing parameters of the operations or an empty
   * array for the operations with no parameters.
   *
   * @throws NO_RESOURCES if the parameters are not available. The parameters
       * are only available for DII (via {@link org.omg.CORBA.Request} or DSI calls.
   * They are not available for calls via IDL - generated stubs.
   */
  Parameter[] arguments();

  /**
   * Returns the names of all contexts of the operation being invoked.
   *
   * @return the array of strings, defining contexts.
   *
   * @throws NO_RESOURCES if the contexts are not available. The contexts are
   * only available for DII (via {@link org.omg.CORBA.Request} or DSI calls.
   * They are not available for calls via IDL - generated stubs.
   */
  String[] contexts();

  /**
   * Returns the typecodes, defining all exceptions that the operation may
   * throw.
   *
   * @return the array of exception typecodes, empty array if the operation
   * should not throw any exceptions.
   *
   * @throws NO_RESOURCES if the exception list is not available. This list is
   * only available for DII (via {@link org.omg.CORBA.Request} or DSI calls and
   * only on the client side. It is not available for calls via IDL - generated
   * stubs or on the server side.
   */
  TypeCode[] exceptions();

  /**
   * If the request contains forwarding information (the reply_status attribute
   * being LOCATION_FORWARD), return the forwarding target.
   *
   * @return the object where the request should be forwarded.
   */
  org.omg.CORBA.Object forward_reference();

  /**
       * Get the service context with the given ctx_name that is associated with the
   * reply.
   *
   * @param ctx_name the name of the service context
   *
   * @return the copy of the corresponding context.
   *
   * @throws BAD_PARAM minor 26, if the context with the give ctx_name does not
   * exist.
   */
  ServiceContext get_reply_service_context(int ctx_name)
    throws BAD_PARAM;

  /**
       * Get the service context with the given ctx_name that is associated with the
   * request.
   *
   * @param ctx_name the name of the service context
   *
   * @return the copy of the corresponding context.
   *
   * @throws BAD_PARAM minor 26, if the context with the give ctx_name does not
   * exist.
   */
  ServiceContext get_request_service_context(int ctx_name)
    throws BAD_PARAM;

  /**
       * Get the data from the given slot of the PortableInterceptor.Current that is
   * in the scope of the request.
   */
  Any get_slot(int id) throws InvalidSlot;

  /**
   * Get the names of the service contexts being sent on the request.
   *
   * @return array of strings, naming the contexts.
   */
  String[] operation_context();

  /**
   * Get the name of the operation being invoked.
   *
       * @return the name of the operation, usually the name of method being called.
   */
  String operation();

  /**
   * Get the reoly state as result of the operation invocation.
   *
   * @return the value field of one of the following: {@link SUCCESSFUL},
   * {@link SYSTEM_EXCEPTION}, {@link USER_EXCEPTION},
   * {@link LOCATION_FORWARD} or {@link TRANSPORT_RETRY}.
   */
  short reply_status();

  /**
   * Get the request id.
   *
   * @return an id that uniquely identifies the current request/reply sequence.
   */
  int request_id();

  /**
   * Indicates whether request sender expected any response.
   *
   * @return true if the response was expected, false otherwise.
   */
  boolean response_expected();

  /**
   * Get the result of the operation invocation.
   *
   * @return an Any, containing the value, returned by the performed operation.
   */
  Any result();

  /**
       * Determines how far the request shall progress before control is returned to
   * the client. However up till JDK 1.5 inclusive this method always returns
   * SYNC_WITH_TRANSPORT.
   *
   * @return {@link org.omg.Messaging.SYNC_WITH_TRANSPORT#value} (1), always.
   *
   * @specnote as defined in the Suns 1.5 JDK API.
   */
  short sync_scope();
}
