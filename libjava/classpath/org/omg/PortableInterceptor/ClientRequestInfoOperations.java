/* ClientRequestInfoOperations.java --
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

import org.omg.CORBA.Any;
import org.omg.CORBA.BAD_PARAM;
import org.omg.CORBA.INV_POLICY;
import org.omg.CORBA.Policy;
import org.omg.IOP.ServiceContext;
import org.omg.IOP.TaggedComponent;
import org.omg.IOP.TaggedProfile;

/**
 * Provides request information, accessible for the
 * {@linkplain ClientRequestInterceptor}. Some methods of this interface are
 * not valid at all interception points. The following table shows the validity
 * of each method. If it is not valid, BAD_INV_ORDER minor 14 will be thrown.
 *
 * <table border="1">
 * <tr>
 * <th></th>
 * <th>{@linkplain ClientRequestInterceptorOperations#send_request send_request}</th>
 * <th>{@linkplain ClientRequestInterceptorOperations#send_poll send_poll}</th>
 * <th>{@linkplain ClientRequestInterceptorOperations#receive_reply receive_reply}</th>
 * <th>{@linkplain ClientRequestInterceptorOperations#receive_exception receive_exception}</th>
 * <th>{@linkplain ClientRequestInterceptorOperations#receive_other receive_other}</th>
 * </tr>
 * <tr>
 * <td colspan="6" align="center" bgcolor="#E0E0FF"><i>Inherited from
 * {@linkplain RequestInfoOperations}:</i></td>
 * </tr>
 * <tr>
 * <th>{@linkplain RequestInfoOperations#arguments arguments}</th>
 * <td bgcolor="#E0E0E0" title="in and inout only">yes <sub><a href="#1">1</a></sub></td>
 * <td bgcolor="lightgray">no </td>
 * <td>yes</td>
 * <td bgcolor="lightgray">no </td>
 * <td bgcolor="lightgray">no </td>
 * </tr>
 * <tr>
 * <th>{@linkplain RequestInfoOperations#exceptions exceptions}</th>
 * <td>yes</td>
 * <td bgcolor="lightgray">no </td>
 * <td colspan="3" align ="center">yes</td>
 * </tr>
 * <tr>
 * <th>{@linkplain RequestInfoOperations#contexts contexts}</th>
 * <td>yes</td>
 * <td bgcolor="lightgray">no </td>
 * <td colspan="3" align ="center">yes</td>
 * </tr>
 * <tr>
 * <th>{@linkplain RequestInfoOperations#operation_context operation_context}</th>
 * <td>yes</td>
 * <td bgcolor="lightgray">no </td>
 * <td colspan="3" align ="center">yes</td>
 * </tr>
 * </tr>
 * <tr>
 * <th>{@linkplain RequestInfoOperations#result result}</th>
 * <td bgcolor="lightgray">no </td>
 * <td bgcolor="lightgray">no </td>
 * <td>yes</td>
 * <td bgcolor="lightgray">no </td>
 * <td bgcolor="lightgray">no </td>
 * </tr>
 * <tr>
 * <th>{@linkplain RequestInfoOperations#sync_scope sync_scope}</th>
 * <td>yes</td>
 * <td bgcolor="lightgray">no </td>
 * <td colspan="3" align ="center">yes</td>
 * </tr>
 * </tr>
 * <tr>
 * <th>{@linkplain RequestInfoOperations#reply_status reply_status}</th>
 * <td bgcolor="lightgray">no </td>
 * <td bgcolor="lightgray">no </td>
 * <td colspan="3" align ="center">yes</td>
 * </tr>
 * <tr>
 * <th>{@linkplain RequestInfoOperations#forward_reference forward_reference}</th>
 * <td>no</td>
 * <td  bgcolor="lightgray" colspan="3" align="center">no</td>
     * <td bgcolor="#E0E0E0" title="When reply_status = LOCATION_FORWARD">yes <sub><a
 * href="#2">2</a></sub> </td>
 * </tr>
 * <tr>
 * <th>{@linkplain RequestInfoOperations#get_request_service_context get_request_service_context}</th>
 * <td>yes</td>
 * <td bgcolor="lightgray">no </td>
 * <td colspan="3" align ="center">yes</td>
 * </tr>
 * <tr>
 * <th>{@linkplain RequestInfoOperations#get_reply_service_context get_reply_service_context}</th>
 * <td bgcolor="lightgray">no </td>
 * <td bgcolor="lightgray">no </td>
 * <td colspan="3" align ="center">yes</td>
 * </tr>
 * </tr>
 * <tr>
 * <th>{@linkplain RequestInfoOperations#request_id request_id}</th>
 * <td colspan="5" align ="center">yes</td>
 * </tr>
 * <tr>
 * <th>{@linkplain RequestInfoOperations#operation operation}</th>
 * <td colspan="5" align ="center">yes</td>
 * </tr>
 * <tr>
 * <th>{@linkplain RequestInfoOperations#response_expected response_expected}</th>
 * <td colspan="5" align ="center">yes</td>
 * </tr>
 * <tr>
 * <th>{@linkplain RequestInfoOperations#get_slot get_slot}</th>
 * <td colspan="5" align ="center">yes</td>
 * </tr>
 * <tr>
 * <td colspan="6" align="center" bgcolor="#E0E0FF"><i>ClientRequestInfo-specific:</i></td>
 * </tr>
 * <tr>
 * <th>{@linkplain #target target}</th>
 * <td colspan="5" align ="center">yes</td>
 * </tr>
 * <tr>
 * <th>{@linkplain #effective_target effective_target}</th>
 * <td colspan="5" align ="center">yes</td>
 * </tr>
 * <tr>
 * <th>{@linkplain #effective_profile effective_profile}</th>
 * <td colspan="5" align ="center">yes</td>
 * </tr>
 * <tr>
 * <th>{@linkplain #received_exception received_exception}</th>
 * <td  bgcolor="lightgray" colspan="3" align="center">no</td>
 * <td>yes</td>
 * <td bgcolor="lightgray">no </td>
 * </tr>
 * <tr>
 * <th>{@linkplain #received_exception_id received_exception_id}</th>
 * <td  bgcolor="lightgray" colspan="3" align="center">no</td>
 * <td>yes</td>
 * <td bgcolor="lightgray">no </td>
 * </tr>
 * <tr>
 * <th>{@linkplain #get_effective_component get_effective_component}</th>
 * <td>yes</td>
 * <td bgcolor="lightgray">no </td>
 * <td colspan="3" align ="center">yes</td>
 * </tr>
 * <tr>
 * <th>{@linkplain #get_effective_components get_effective_components}</th>
 * <td>yes</td>
 * <td bgcolor="lightgray">no </td>
 * <td colspan="3" align ="center">yes</td>
 * </tr>
 * <tr>
 * <th>{@linkplain #get_request_policy get_request_policy}</th>
 * <td>yes</td>
 * <td bgcolor="lightgray">no </td>
 * <td colspan="3" align ="center">yes</td>
 * </tr>
 * <tr>
     * <th>{@linkplain #add_request_service_context add_request_service_context}</th>
 * <td>yes</td>
 * <td  bgcolor="lightgray" colspan="4" align="center">no</td>
 * </tr>
 * <tr>
 * <th></th>
 * <th>{@linkplain ClientRequestInterceptorOperations#send_request send_request}</th>
 * <th>{@linkplain ClientRequestInterceptorOperations#send_poll send_poll}</th>
 * <th>{@linkplain ClientRequestInterceptorOperations#receive_reply receive_reply}</th>
 * <th>{@linkplain ClientRequestInterceptorOperations#receive_exception receive_exception}</th>
 * <th>{@linkplain ClientRequestInterceptorOperations#receive_other receive_other}</th>
 * </tr>
 * </table>
 * <ol>
 * <li><a name="1">When ClientRequestInfo is passed to send_request, there is
 * an entry in the list for every argument, but only the in and inout arguments
 * will be available.</a></li>
 * <li><a name="2">If the reply_status atribute is not LOCATION_FORWARD,
 * accessing this attribute will throw BAD_INV_ORDER with a standard minor code
 * of 14.</a></li>
 * </ol>
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public interface ClientRequestInfoOperations extends RequestInfoOperations
{
  /**
       * Returns the object on that the client has invoked the the operation. If the
   * request was forwarded, it will not be the same object that actually
   * processed the request.
   *
   * @return the initial client invocation target.
   *
   * @see #effective_target()
   */
  org.omg.CORBA.Object target();

  /**
       * Returns the object on that the operation will be invoked after handling the
   * possible forwarding.
   *
   * @return the final invocation target.
   *
   * @see #target()
   */
  org.omg.CORBA.Object effective_target();

  /**
   * Returns the tagged profile (IOR) of the invocation target. If the request
       * was forwarded, the method returns the new location, shown by the forwarding
   * message.
   *
   * @return the invocation IOR.
   */
  TaggedProfile effective_profile();

  /**
   * Returns the given component of the invocation target profile. If the
   * profile contains multiple components with the same Id, it is not defined,
   * which one will be returned.
   *
   * @param id the component id.
   *
   * @return the profile component with the given Id.
   *
       * @throws BAD_PARAM minor 28 in there are no any components with the given Id
   * in the profile.
   */
  TaggedComponent get_effective_component(int id) throws BAD_PARAM;

  /**
   * Returns the given components of the invocation target profile. This method
   * is uses when the profile may contain multiple components with the same Id.
   *
   * @param id the component id.
   *
   * @return the array of all profile components with the given Id.
   *
       * @throws BAD_PARAM minor 28 in there are no any components with the given Id
   * in the profile.
   */
  TaggedComponent[] get_effective_components(int id) throws BAD_PARAM;

  /**
   * This should return the policy of the given type that applies to this
   * operation, but it is not implemented up till JDK 1.5 inclusive.
   *
   * @param type the type of the policy being requested.
   *
   * @return should return the policy that applies to this operation.
   *
   * @throws NO_IMPLEMENT always.
   */
  Policy get_request_policy(int type) throws INV_POLICY;

  /**
   * Returns the repository id of the remote exception that was thrown on the
   * server side.
   *
   * @return the exception repository id.
   *
   * @see #received_exception()
   */
  String received_exception_id();

  /**
   * Returns the remote exception that was thrown on the server side.
   *
   * @return the Any, holding this exception.
   *
   * @see #received_exception_id()
   */
  Any received_exception();

  /**
   * Allows the interceptor to add the service contexts to the request. Such
   * added contexts can carry arbitrary data and can be later accessed on the
   * server side by the server request interceptor, using
   * {@link RequestInfoOperations#get_request_service_context}.
   *
   * @param service_context the context to add.
   * @param replace if true, the existing context with the same Id will be
   * replaced. If false, the BAD_INV_ORDER will be thrown in that case.
   *
   * @throws BAD_INV_ORDER minor 15 if the context with the same Id already
   * exists and replace=false.
   */
  void add_request_service_context(ServiceContext service_context,
    boolean replace
  );
}