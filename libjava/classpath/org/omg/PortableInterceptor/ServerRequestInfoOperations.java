/* ServerRequestInfoOperations.java --
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
import org.omg.CORBA.BAD_INV_ORDER;
import org.omg.CORBA.INV_POLICY;
import org.omg.CORBA.Policy;
import org.omg.IOP.ServiceContext;

/**
 * Provides request information, accessible for the
 * {@link ClientRequestInterceptor}. Some methods of this interface are not
 * valid at all interception points. The following table shows the validity of
 * each method. If it is not valid, BAD_INV_ORDER minor 14 will be thrown.
 *
 * <table border="1">
 * <tr>
 * <th></th>
 * <th>{@link ServerRequestInterceptorOperations#receive_request_service_contexts receive_request_<br>service_contexts}</th>
 * <th>{@link ServerRequestInterceptorOperations#receive_request receive_request}</th>
 * <th>{@link ServerRequestInterceptorOperations#send_reply send_reply}</th>
 * <th>{@link ServerRequestInterceptorOperations#send_exception send_exception}</th>
 * <th>{@link ServerRequestInterceptorOperations#send_other send_other}</th>
 * </tr>
 * <tr>
 * <td colspan="6" align="center" bgcolor="#E0E0FF"><i>Inherited from
 * {@link RequestInfoOperations}:</i></td>
 * </tr>
 * <tr>
 * <th>{@linkplain RequestInfoOperations#arguments arguments}</th>
 * <td  bgcolor="lightgray">no </td>
 * <td  bgcolor="#E0E0E0" title="in and inout only">yes<sub><a href="#1">1</a></sub></td>
 * <td>yes</td>
 * <td  bgcolor="#E0E0E0" title="When reply_status = LOCATION_FORWARD">no<sub><a
 * href="#2">2</a></sub></td>
 * <td  bgcolor="#E0E0E0" title="When reply_status = LOCATION_FORWARD">no<sub><a
 * href="#2">2</a></sub> </td>
 * </tr>
 * <tr>
 * <th>{@linkplain RequestInfoOperations#exceptions exceptions}</th>
 * <td  bgcolor="lightgray">no </td>
 * <td colspan="4" align ="center">yes</td>
 * </tr>
 * <tr>
 * <th>{@linkplain RequestInfoOperations#contexts contexts}</th>
 * <td  bgcolor="lightgray">no </td>
 * <td colspan="4" align ="center">yes</td>
 * </tr>
 * <tr>
 * <th>{@linkplain RequestInfoOperations#operation_context operation_context}</th>
 * <td  bgcolor="lightgray">no </td>
 * <td>yes</td>
 * <td>yes</td>
 * <td  bgcolor="lightgray">no </td>
 * <td  bgcolor="lightgray">no </td>
 * </tr>
 * <tr>
 * <th>{@linkplain RequestInfoOperations#result result}</th>
 * <td  bgcolor="lightgray">no </td>
 * <td  bgcolor="lightgray">no </td>
 * <td>yes</td>
 * <td  bgcolor="lightgray">no </td>
 * <td  bgcolor="lightgray">no </td>
 * </tr>
 * <tr>
 * <th>{@linkplain RequestInfoOperations#reply_status reply_status}</th>
 * <td  bgcolor="lightgray">no </td>
 * <td  bgcolor="lightgray">no </td>
 * <td colspan="3" align="center">yes</td> * </tr>
 * <tr>
 * <th>{@linkplain RequestInfoOperations#forward_reference forward_reference}</th>
 * <td  bgcolor="lightgray" colspan="4" align="center">no</td>
 * <td  bgcolor="#E0E0E0" title="When reply_status = LOCATION_FORWARD">yes<sub><a
 * href="#2">2</a></sub> </td>
 * </tr>
 * <tr>
 * <th>{@linkplain RequestInfoOperations#get_request_service_context get_request_service_context}</th>
 * <td>yes</td>
 * <td  bgcolor="lightgray">no </td>
 * <td colspan="3" align="center">yes</td> * </tr>
 * <tr>
 * <th>{@linkplain RequestInfoOperations#get_reply_service_context get_reply_service_context}</th>
 * <td  bgcolor="lightgray">no </td>
 * <td  bgcolor="lightgray">no </td>
 * <td colspan="3" align="center">yes</td>
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
 * <th>{@linkplain RequestInfoOperations#sync_scope sync_scope}</th>
 * <td colspan="5" align ="center">yes</td>
 * </tr>
 * <tr>
 * <th>{@linkplain RequestInfoOperations#get_slot get_slot}</th>
 * <td colspan="5" align ="center">yes</td>
 * </tr>
 * <tr>
 * <td colspan="6" align="center" bgcolor="#E0E0FF">
 * <i>ServerRequestInfo-specific:</i></td>
 * </tr>
 * <tr>
 * <th>{@linkplain #get_server_policy get_server_policy}</th>
 * <td colspan="5" align ="center">yes</td>
 * </tr>
 * <tr>
 * <th>{@linkplain #add_reply_service_context add_reply_service_context}</th>
 * <td colspan="5" align ="center">yes</td>
 * </tr>
 * <tr>
 * <th>{@linkplain #set_slot set_slot}</th>
 * <td colspan="5" align ="center">yes</td>
 * </tr>
 * <tr>
 * <th>{@linkplain #sending_exception sending_exception}</th>
 * <td  bgcolor="lightgray" colspan="3" align="center">no</td>
 * <td>yes</td>
 * <td  bgcolor="lightgray">no </td>
 * </tr>
 * <tr>
 * <th>{@linkplain #object_id object_id}</th>
 * <td  bgcolor="lightgray">no </td>
 * <td>yes</td>
 * <td>yes</td>
 * <td  bgcolor="#E0E0E0" title="Not always (see note)">yes<sub><a
 * href="#3">3</a></sub></td>
 * <td bgcolor="#E0E0E0"  title="Not always (see note)">yes<sub><a
 * href="#3">3</a></sub> </td>
 * </tr>
 * <tr>
 * <th>{@linkplain #adapter_id adapter_id}</th>
 * <td  bgcolor="lightgray">no </td>
 * <td>yes</td>
 * <td>yes</td>
 * <td  bgcolor="#E0E0E0" title="Not always (see note)">yes<sub><a
 * href="#3">3</a></sub></td>
 * <td  bgcolor="#E0E0E0" title="Not always (see note)">yes<sub><a
 * href="#3">3</a></sub> </td>
 * </tr>
 * <tr>
 * <th>{@linkplain #target_most_derived_interface target_most_derived_interface}</th>
 * <td  bgcolor="lightgray">no </td>
 * <td>yes</td>
 * <td  bgcolor="lightgray" colspan="3" align="center">no</td>
 * </tr>
 * <tr>
 * <th>{@linkplain #target_is_a target_is_a}</th>
 * <td  bgcolor="lightgray">no </td>
 * <td>yes</td>
 * <td  bgcolor="lightgray" colspan="3" align="center">no</td>
 * </tr>
 * <tr>
 * <th></th>
 * <th>{@link ServerRequestInterceptorOperations#receive_request_service_contexts receive_request_<br>service_contexts }</th>
 * <th>{@link ServerRequestInterceptorOperations#receive_request receive_request}</th>
 * <th>{@link ServerRequestInterceptorOperations#send_reply send_reply}</th>
 * <th>{@link ServerRequestInterceptorOperations#send_exception send_exception}</th>
 * <th>{@link ServerRequestInterceptorOperations#send_other send_other}</th>
 * </tr>
 * </table>
 * <ol>
 * <li><a name="1">When ServerRequestInfo is passed to receive_request, there
 * is an entry in the list for every argument. But only the in and inout
 * arguments will be available.</a></li>
 * <li><a name="2">If the reply_status attribute is not LOCATION_FORWARD,
 * accessing this attribute throws BAD_INV_ORDER minor code of 14.</a></li>
 * <li><a name="3">If the servant locator caused a location forward, or thrown
 * an exception, this attribute/operation may not be available (NO_RESOURCES
 * with a standard minor code of 1 will be thrown).</a></li>
 * </ol>
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public interface ServerRequestInfoOperations
  extends RequestInfoOperations
{
  /**
   * Allows the interceptor to add service contexts to the request. Such added
   * contexts can carry arbitrary data and can be later accessed on the client
   * side by the client request interceptor using
   * {@link RequestInfoOperations#get_reply_service_context}.
   *
   * @param service_context the context to add.
   * @param replace if true, the existing context with the same Id will be
   * replaced. If false, the BAD_INV_ORDER will be thrown in that case.
   *
   * @throws BAD_INV_ORDER minor 15 if the context with the same Id already
   * exists and replace=false.
   */
  void add_reply_service_context(ServiceContext service_context, boolean replace);

  /**
   * Get the identifier for the object adapter (POA).
   */
  byte[] adapter_id();

  /**
   * Get the object_id describing the target of the operation invocation.
   */
  byte[] object_id();

  /**
   * Return the policy of the given type that applies to this operation. This
   * method should only be used with policies, produced by the registered
   * {@link PolicyFactory}.
   *
   * @param type the type of the policy being requested.
   *
   * @return the policy that applies to this operation.
   *
   * @throws INV_POLICY minor 2 if no factory was registered to produce this
   * type of policy or the policy is otherwise invalid.
   */
  Policy get_server_policy(int type)
    throws INV_POLICY;

  /**
   * Get the exception to be returned to the client. If the returned Any cannot
   * not support holding of that exception, it holds
   * {@link org.omg.CORBA.UNKNOWN} minor 1 instead.
   *
   * @return an Any, holding exception that has been thrown and will be returned
   * to client.
   */
  Any sending_exception();

  /**
   * Allows the interceptor to set a slot in the PortableInterceptor.Current
   * that is in the scope of the request.
   *
   * @param id the Id of the slot.
   * @param data the value of the slot, replacing the previous value.
   *
   * @throws InvalidSlot if the slot with the given Id does not exist.
   *
   * @see RequestInfoOperations#get_slot(int)
   * @see org.omg.PortableInterceptor#Current
   */
  void set_slot(int id, Any data)
    throws InvalidSlot;

  /**
   * Checks if the servant is the given repository id.
   *
   * @param id the repository id to compare.
   *
   * @return true if the servant repository id matches the parameter, false
   * otherwise.
   */
  boolean target_is_a(String id);

  /**
   * Get the most derived (most specific) repository Id of the servant.
   *
   * @return the repository id of the servant.
   */
  String target_most_derived_interface();

  /**
   * Returns the name of the adapter that is handling the current request.
   * The name is returned as a string array, representing full path from
   * the root poa till the current poa, for instance 
   * {"RootPOA", "childPOA","grandchildPOA"}.
   */
  public String[] adapter_name();

  /**
   * Returns the id of the ORB that is handling the current request. The ORB
   * id can be specified as the property org.omg.CORBA.ORBid when creating
   * the ORB. 
   */
  public String orb_id();

  /**
   * Returs the id of the server that is handling the current request. The server
   * id is the same for all POAs and ORBs in the current virtual machine and 
   * can be set as the property org.omg.CORBA.ServerId when creating one of the
   * ORBs.
   */
  public String server_id();
}