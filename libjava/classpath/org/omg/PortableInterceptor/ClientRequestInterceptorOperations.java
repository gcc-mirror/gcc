/* ClientRequestInterceptorOperations.java --
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
 * Defines operations, applicable to the client side request interceptor. The
 * operations are called by ORB at the appropriate interception points.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public interface ClientRequestInterceptorOperations
  extends InterceptorOperations
{
  /**
   * ORB calls this method before sending the request to the server.
   *
   * @param info the object for accessing and manipulating the request
   * information.
   *
   * @throws SystemException if it does, the send_request is not called for the
   * subsequent interceptors, calling receive_exception instead. The completion
   * status of this exception must be COMPLETED_NO.
   *
   * @throws ForwardRequest to forward the invocation to another target. The
   * send_request is not called for the subsequent interceptors, calling
   * receive_other instead.
   */
  void send_request(ClientRequestInfo info) throws ForwardRequest;

  /**
   * ORB calls this method after the normal reply is received from the server
   * and before the control is returned to the calling client code.
   *
   * @param info the object for accessing and manipulating the request
   * information.
   *
       * @throws SystemException if it does, the receive_reply is not called for the
   * subsequent interceptors, calling receive_exception instead. The completion
   * status of this exception must be COMPLETED_YES.
   */
  void receive_reply(ClientRequestInfo info);

  /**
   * ORB calls this method after the receiving the message that a remote
   * exception has been thrown on a server side and before raising this
   * exception in the client side.
   *
   * @param info the object for accessing and manipulating the request
   * information.
   *
   * @throws SystemException has the effect of changing the exception that
   * successive interceptors receive on their calls to receive_other. If the
   * original exception is a system exception, the completion_status of the new
   * exception must match the exception being replaced. If the original
   * exception is a user exception, then the completion_status of the new
   * exception must be COMPLETED_YES.
   *
   * @throws ForwardRequest to forward the invocation to another target. The
   * receive_exception is not called for the subsequent interceptors, calling
   * receive_other instead. If the completion_status of the original exception
   * is not a COMPLETED_NO, the ForwardRequest must not be raised.
   */
  void receive_exception(ClientRequestInfo info) throws ForwardRequest;

  /**
   * /** ORB normally calls this method after receiving the forwarding message.
    *
    * @param info the object for accessing and manipulating the request
    * information.
    *
        * @throws SystemException if it does, the receive_other is not called for the
    * subsequent interceptors, calling receive_exception instead.
    *
    * @throws ForwardRequest has the effect of changing the redirection that
    * successive interceptors receive on their calls to receive_other.
    */
  void receive_other(ClientRequestInfo info) throws ForwardRequest;

  /**
       * This method is called by if ORB uses the Time- Independent Invocation (TII)
   * polling.
   *
   * @param info the object for accessing and manipulating the request
   * information.
   *
   * @throws SystemException if it does, the send_poll is not called for the
   * subsequent interceptors, calling receive_exception instead. The completion
   * status of this exception must be COMPLETED_NO.
   */
  void send_poll(ClientRequestInfo info);
}