/* gnuClientRequestInfo.java --
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

import gnu.CORBA.Unexpected;
import gnu.CORBA.gnuRequest;

import org.omg.CORBA.ARG_IN;
import org.omg.CORBA.ARG_INOUT;
import org.omg.CORBA.ARG_OUT;
import org.omg.CORBA.Any;
import org.omg.CORBA.BAD_PARAM;
import org.omg.CORBA.Bounds;
import org.omg.CORBA.ExceptionList;
import org.omg.CORBA.INV_POLICY;
import org.omg.CORBA.LocalObject;
import org.omg.CORBA.NVList;
import org.omg.CORBA.ORB;
import org.omg.CORBA.ParameterMode;
import org.omg.CORBA.Policy;
import org.omg.CORBA.TCKind;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.TypeCodePackage.BadKind;
import org.omg.Dynamic.Parameter;
import org.omg.IOP.ServiceContext;
import org.omg.IOP.TaggedComponent;
import org.omg.IOP.TaggedProfile;
import org.omg.PortableInterceptor.ClientRequestInfo;
import org.omg.PortableInterceptor.InvalidSlot;

/**
 * Client request info. All requests on the client side in Classpath
 * implementations are handled via gnuRequest class. This class holds the
 * instance of the gnuRequest, accessing the request info this way.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class gnuClientRequestInfo extends LocalObject
  implements ClientRequestInfo
{
  /**
   * Use serialVersionUID for interoperability.
   */
  private static final long serialVersionUID = 1;

  /**
   * The request structure, from that some methods take the needed information
   * directly. The same request structure cannot be reused in parallel threads,
   * the submission methods are synchronized.
   */
  private final gnuRequest request;

  /**
   * Provides possibility to set the wrapped thrown exception explicitly, where
   * applicable.
   */
  public Any m_wrapped_exception;

  /**
   * Create the info on the given request.
   */
  public gnuClientRequestInfo(gnuRequest a_request)
  {
    request = a_request;
  }

  /** @inheritDoc */
  public void add_request_service_context(ServiceContext service_context,
    boolean replace
  )
  {
    request.add_request_service_context(service_context, replace);
  }

  /** @inheritDoc */
  public TaggedProfile effective_profile()
  {
    return request.effective_profile();
  }

  /** @inheritDoc */
  public org.omg.CORBA.Object effective_target()
  {
    return request.effective_target();
  }

  /** @inheritDoc */
  public TaggedComponent get_effective_component(int id)
    throws BAD_PARAM
  {
    return request.get_effective_component(id);
  }

  /** @inheritDoc */
  public TaggedComponent[] get_effective_components(int id)
    throws BAD_PARAM
  {
    return request.get_effective_components(id);
  }

  /** @inheritDoc */
  public Policy get_request_policy(int type) throws INV_POLICY
  {
    return request.get_request_policy(type);
  }

  /** @inheritDoc */
  public String received_exception_id()
  {
    try
      {
        if (m_wrapped_exception != null)
          {
            return m_wrapped_exception.type().id();
          }
        else
          {
            return request.received_exception_id();
          }
      }
    catch (BadKind e)
      {
        throw new Unexpected(e);
      }
  }

  /** @inheritDoc */
  public Any received_exception()
  {
    if (m_wrapped_exception != null)
      {
        return m_wrapped_exception;
      }
    else
      {
        return request.received_exception();
      }
  }

  /** @inheritDoc */
  public org.omg.CORBA.Object target()
  {
    return request.target();
  }

  /** @inheritDoc */
  public Parameter[] arguments()
  {
    request.checkDii();

    NVList args = request.arguments();
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

  /** @inheritDoc */
  public Any result()
  {
    request.checkDii();

    Any rt = request.return_value();

    if (rt == null)
      {
        ORB orb = request.orb();
        rt = orb.create_any();
        rt.type(orb.get_primitive_tc(TCKind.tk_void));
        return rt;
      }

    return request.return_value();
  }

  /** @inheritDoc */
  public String[] contexts()
  {
    return request.ice_contexts();
  }

  /** @inheritDoc */
  public TypeCode[] exceptions()
  {
    request.checkDii();

    ExceptionList ex = request.exceptions();
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

  /** @inheritDoc */
  public org.omg.CORBA.Object forward_reference()
  {
    return request.forward_reference();
  }

  /** @inheritDoc */
  public String[] operation_context()
  {
    return request.operation_context();
  }

  /** @inheritDoc */
  public Any get_slot(int id) throws InvalidSlot
  {
    return request.get_slot(id);
  }

  /** @inheritDoc */
  public String operation()
  {
    return request.operation();
  }

  /** @inheritDoc */
  public short reply_status()
  {
    return request.reply_status();
  }

  /** @inheritDoc */
  public int request_id()
  {
    return request.request_id();
  }

  /** @inheritDoc */
  public boolean response_expected()
  {
    return request.response_expected();
  }

  /**
       * Determines how far the request shall progress before control is returned to
   * the client. However up till JDK 1.5 inclusive this method always returns
   * SYNC_WITH_TRANSPORT.
   *
   * @return {@link org.omg.Messaging.SYNC_WITH_TRANSPORT.value (1), always.
   *
   * @specnote as defined in the Suns 1.5 JDK API.
   */
  public short sync_scope()
  {
    return request.sync_scope();
  }

  /** @inheritDoc */
  public ServiceContext get_reply_service_context(int ctx_name)
    throws BAD_PARAM
  {
    return request.get_reply_service_context(ctx_name);
  }

  /** @inheritDoc */
  public ServiceContext get_request_service_context(int ctx_name)
    throws BAD_PARAM
  {
    return request.get_request_service_context(ctx_name);
  }
}
