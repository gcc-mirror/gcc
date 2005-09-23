/* ServerRequestInterceptorOperations.java --
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
 * Defines operations, applicable to the server side request interceptor. The
 * operations are called by ORB at the appropriate interception points.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public interface ServerRequestInterceptorOperations
  extends InterceptorOperations
{
  /**
   * ORB calls this method before invoking the servant manager. Operation
       * parameters are not available at this point. The interceptor has possibility
   * to forward the request by throwing {@link ForwardRequest}.
   *
       * @throws SystemException if it does, the receive_request_service_contexts is
       * not called for the subsequent interceptors, calling send_exception instead.
   * The completion status of such exception must be COMPLETED_NO.
   *
   * @throws ForwardRequest to forward the invocation to another target. The
   * receive_request_service_contexts is not called for the subsequent
   * interceptors, calling send_other instead.
   */
  void receive_request_service_contexts(ServerRequestInfo info)
    throws ForwardRequest;

  /**
   * ORB calls this method after all the information, including operation
   * parameters, are available. The interceptor has possibility to forward the
   * request by throwing {@link ForwardRequest}.
   *
   * @param info the object for accessing and manipulating the request
   * information.
   *
   * @throws SystemException if it does, the receive_request is not called for
       * the subsequent interceptors, calling send_exception instead. The completion
   * status of such exception must be COMPLETED_NO.
   *
   * @throws ForwardRequest to forward the invocation to another target. The
   * receive_request is not called for the subsequent interceptors, calling
   * send_other instead.
   */
  void receive_request(ServerRequestInfo info) throws ForwardRequest;

  /**
   * ORB calls this method after the target operation has been invoked and
   * before the reply is returned to the client. This interception point shall
   * execute in the same thread as the target invocation.
   *
   * @param info the object for accessing and manipulating the request
   * information.
   *
   * @throws SystemException if it does, the send_reply is not called for the
   * subsequent interceptors, calling send_exception instead. The completion
   * status of such exception must be COMPLETED_YES.
   */
  void send_reply(ServerRequestInfo info);

  /**
   * ORB calls this method if the exception has been throw during the request
   * processing. The interceptor has possibility to forward the request by
   * throwing {@link ForwardRequest}.
   *
   * @param info the object for accessing and manipulating the request
   * information.
   *
   * @throws SystemException has the effect of changing the exception that
   * successive interceptors receive on their calls to send_exception. If the
   * original exception is a system exception, the completion_status of the new
   * exception must match the exception being replaced. If the original
   * exception is a user exception, then the completion_status of the new
   * exception must be COMPLETED_YES.
   *
   * @throws ForwardRequest to forward the invocation to another target. The
   * send_exception is not called for the subsequent interceptors, calling
   * send_other instead. If the completion_status of the original exception is
   * not a COMPLETED_NO, the ForwardRequest must not be raised.
   */
  void send_exception(ServerRequestInfo info) throws ForwardRequest;

  /**
   * ORB normally calls this method if the request has been forwarded.
   *
   * @param info the object for accessing and manipulating the request
   * information.
   *
   * @throws SystemException if it does, the send_other is not called for the
   * subsequent interceptors, calling send_exception instead.
   *
   * @throws ForwardRequest has the effect of changing the redirection that
   * successive interceptors receive on their calls to send_other.
   */
  void send_other(ServerRequestInfo info) throws ForwardRequest;
}