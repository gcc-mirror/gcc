/* gnuServerRequestInfo.java --
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


package gnu.CORBA.Interceptor;

import gnu.CORBA.GIOP.ReplyHeader;
import gnu.CORBA.GIOP.RequestHeader;
import gnu.CORBA.ObjectCreator;
import gnu.CORBA.Poa.gnuServantObject;
import gnu.CORBA.Unexpected;
import gnu.CORBA.gnuRequest;

import org.omg.CORBA.ARG_IN;
import org.omg.CORBA.ARG_INOUT;
import org.omg.CORBA.ARG_OUT;
import org.omg.CORBA.Any;
import org.omg.CORBA.BAD_PARAM;
import org.omg.CORBA.Bounds;
import org.omg.CORBA.CompletionStatus;
import org.omg.CORBA.ExceptionList;
import org.omg.CORBA.INV_POLICY;
import org.omg.CORBA.LocalObject;
import org.omg.CORBA.NO_RESOURCES;
import org.omg.CORBA.NVList;
import org.omg.CORBA.Object;
import org.omg.CORBA.ParameterMode;
import org.omg.CORBA.Policy;
import org.omg.CORBA.TCKind;
import org.omg.CORBA.TypeCode;
import org.omg.Dynamic.Parameter;
import org.omg.IOP.ServiceContext;
import org.omg.Messaging.SYNC_WITH_TRANSPORT;
import org.omg.PortableInterceptor.InvalidSlot;
import org.omg.PortableInterceptor.ServerRequestInfo;

/**
 * Implementation of the ServerRequestInfo, associacted with gnuServantObject.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class gnuServerRequestInfo extends LocalObject
  implements ServerRequestInfo
{
  /**
   * Use serialVersionUID for interoperability.
   */
  private static final long serialVersionUID = 1;

  /**
   * A local object that will serve the invocation.
   */
  final gnuServantObject m_object;

  /**
   * A message that the given resource is not available using this metod of
   * invocation.
   */
  static final String not_available =
    "The used invocation method provides" + "no access to this resource.";

  /**
   * An array of slots.
   */
  Any[] m_slots;

  /**
   * The request header.
   */
  public final RequestHeader m_request_header;

  /**
   * The reply header.
   */
  public final ReplyHeader m_reply_header;

  /**
   * The forward reference, if applicable.
   */
  public Object m_forward_reference;

  /**
   * The thrown systen exception.
   */
  public Exception m_sys_exception;

  /**
   * The Any, containing the thrown user exception.
   */
  public Any m_usr_exception;

  /**
   * The associated request, if any.
   */
  public gnuRequest m_request;

  /**
   * Create a new instance at the time when it is known which object will serve
   * the invocation.
   *
   * @param an_object a local object, connected to the local servant that will
   * serve the invocation.
   */
  public gnuServerRequestInfo(gnuServantObject an_object,
    RequestHeader a_request_header, ReplyHeader a_reply_header
  )
  {
    m_object = an_object;
    m_request_header = a_request_header;
    m_reply_header = a_reply_header;
    m_slots = new Any[ m_object.orb.icSlotSize ];
    reset();
  }

  /**
   * Set the give slot.
   */
  public void set_slot(int id, Any data) throws InvalidSlot
  {
    try
      {
        m_slots [ id ] = data;
      }
    catch (Exception e)
      {
        InvalidSlot ex = new InvalidSlot("Cannot set slot " + id);
        ex.initCause(e);
        throw ex;
      }
  }

  /**
   * Get the given slot.
   */
  public Any get_slot(int id) throws InvalidSlot
  {
    try
      {
        return m_slots [ id ];
      }
    catch (Exception e)
      {
        InvalidSlot ex = new InvalidSlot("Cannot get slot " + id);
        ex.initCause(e);
        throw ex;
      }
  }

  /**
   * Reset slot data.
   */
  public void reset()
  {
    TypeCode tkNull = m_object.orb.get_primitive_tc(TCKind.tk_null);
    for (int i = 0; i < m_slots.length; i++)
      {
        Any a = m_object.orb.create_any();
        a.type(tkNull);
        m_slots [ i ] = a;
      }
    m_sys_exception = null;
    m_usr_exception = null;
  }

  /**
   * Get the object id (not the object IOR key).
   */
  public byte[] object_id()
  {
    return m_object.Id;
  }

  /**
   * Check if the target is an instance of the type, represented by the given
   * repository Id.
   */
  public boolean target_is_a(String id)
  {
    return m_object._is_a(id);
  }

  /**
   * Get the POA id.
   */
  public byte[] adapter_id()
  {
    return m_object.poa.id();
  }

  /**
   * Get the POA policy of the given type that applies to the object being
   * served (request being handled).
   */
  public Policy get_server_policy(int type) throws INV_POLICY
  {
    return m_object.poa._get_policy(type);
  }

  /**
   * Get the first member of the object repository id array.
   */
  public String target_most_derived_interface()
  {
    return m_object._ids() [ 0 ];
  }

  /**
   * Get the name of the operation being performed.
   */
  public String operation()
  {
    if (m_request != null)
      {
        return m_request.operation();
      }
    else
      {
        return m_request_header.operation;
      }
  }

  /**
   * Not available.
   */
  public TypeCode[] exceptions()
  {
    if (m_request == null)
      {
        throw new NO_RESOURCES(not_available, 1,
          CompletionStatus.COMPLETED_MAYBE
        );
      }

    m_request.checkDii();

    ExceptionList ex = m_request.exceptions();
    TypeCode[] et = new TypeCode[ ex.count() ];
    try
      {
        for (int i = 0; i < et.length; i++)
          {
            et [ i ] = ex.item(i);
          }
      }
    catch (Bounds e)
      {
        throw new Unexpected(e);
      }
    return et;
  }

  /**
   * Get reply status.
   */
  public short reply_status()
  {
    return (short) m_reply_header.reply_status;
  }

  /**
   * Get request id. All local requests have request id = -1.
   */
  public int request_id()
  {
    return m_request_header.request_id;
  }

  /**
   * Check if the client expected any response.
   */
  public boolean response_expected()
  {
    return m_request_header.isResponseExpected();
  }

  /** @inheritDoc */
  public void add_reply_service_context(ServiceContext service_context,
    boolean replace
  )
  {
    m_reply_header.addContext(service_context, replace);
  }

  /**
   * Get an exception, wrapped into Any.
   */
  public Any sending_exception()
  {
    if (m_usr_exception != null)
      {
        return m_usr_exception;
      }
    else if (m_sys_exception != null)
      {
        Any a = m_object.orb.create_any();
        ObjectCreator.insertException(a, m_sys_exception);
        return a;
      }
    else
      {
        return null;
      }
  }

  public org.omg.CORBA.Object forward_reference()
  {
    return m_forward_reference;
  }

  /** @inheritDoc */
  public ServiceContext get_reply_service_context(int ctx_name)
    throws BAD_PARAM
  {
    return gnu.CORBA.GIOP.ServiceContext.findContext(ctx_name,
      m_reply_header.service_context
    );
  }

  /** @inheritDoc */
  public ServiceContext get_request_service_context(int ctx_name)
    throws BAD_PARAM
  {
    return gnu.CORBA.GIOP.ServiceContext.findContext(ctx_name,
      m_request_header.service_context
    );
  }

  /**
   * Not available
   */
  public String[] operation_context()
  {
    if (m_request == null)
      {
        throw new NO_RESOURCES(not_available);
      }
    else
      {
        return m_request.operation_context();
      }
  }

  /** @inheritDoc */
  public Any result()
  {
    if (m_request == null)
      {
        throw new NO_RESOURCES(not_available);
      }
    else
      {
        return m_request.return_value();
      }
  }

  /** @inheritDoc */
  public String[] contexts()
  {
    if (m_request == null)
      {
        throw new NO_RESOURCES(not_available);
      }
    else
      {
        return m_request.ice_contexts();
      }
  }

  /**
   * Always returns "with transport".
   */
  public short sync_scope()
  {
    return SYNC_WITH_TRANSPORT.value;
  }

  /** @inheritDoc */
  public Parameter[] arguments()
  {
    if (m_request == null)
      {
        throw new NO_RESOURCES(not_available);
      }

    m_request.checkDii();

    NVList args = m_request.arguments();
    Parameter[] p = new Parameter[ args.count() ];
    try
      {
        for (int i = 0; i < p.length; i++)
          {
            ParameterMode mode;

            switch (args.item(i).flags())
              {
                case ARG_IN.value :
                  mode = ParameterMode.PARAM_IN;
                  break;

                case ARG_OUT.value :
                  mode = ParameterMode.PARAM_OUT;
                  break;

                case ARG_INOUT.value :
                  mode = ParameterMode.PARAM_INOUT;
                  break;

                default :
                  throw new Unexpected();
              }

            p [ i ] = new Parameter(args.item(i).value(), mode);
          }
      }
    catch (Bounds e)
      {
        throw new Unexpected(e);
      }
    return p;
  }
}