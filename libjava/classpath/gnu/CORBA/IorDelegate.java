/* gnuDelegate.java --
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


package gnu.CORBA;

import gnu.CORBA.CDR.BufferredCdrInput;
import gnu.CORBA.GIOP.ReplyHeader;

import org.omg.CORBA.CompletionStatus;
import org.omg.CORBA.Context;
import org.omg.CORBA.ContextList;
import org.omg.CORBA.ExceptionList;
import org.omg.CORBA.MARSHAL;
import org.omg.CORBA.NVList;
import org.omg.CORBA.NamedValue;
import org.omg.CORBA.ORB;
import org.omg.CORBA.Request;
import org.omg.CORBA.portable.ApplicationException;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.OutputStream;
import org.omg.CORBA.portable.RemarshalException;
import org.omg.PortableInterceptor.ForwardRequest;

import java.io.IOException;

/**
 * The Classpath implementation of the {@link Delegate} functionality in the
 * case, when the object was constructed from an IOR object. The IOR can be
 * constructed from the stringified object reference.
 *
 * There is an different instance of this delegate for each CORBA object.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class IorDelegate extends SimpleDelegate
{
  /**
   * Contructs an instance of object using the given IOR.
   */
  public IorDelegate(ORB an_orb, IOR an_ior)
  {
    super(an_orb, an_ior);
  }

  /**
   * Creates the request to invoke the method on this object.
   *
   * @param target the object, for that the operation must be invoked.
   * @param context context (null allowed)
   * @param operation the method name
   * @param parameters the method parameters
   * @param returns the return value holder
   *
   * @return the created request.
   */
  public Request create_request(org.omg.CORBA.Object target, Context context,
    String operation, NVList parameters, NamedValue returns
  )
  {
    gnuRequest request = getRequestInstance(target);

    request.setIor(getIor());
    request.set_target(target);

    request.setOperation(operation);
    request.set_args(parameters);
    request.m_context = context;
    request.set_result(returns);
    request.setORB(orb);

    return request;
  }

  /**
   * Creates the request to invoke the method on this object.
   *
   * @param target the object, for that the operation must be invoked.
   * @param context context (null allowed)
   * @param operation the method name
   * @param parameters the method parameters
   * @param returns the return value holder
   *
   * @return the created request.
   */
  public Request create_request(org.omg.CORBA.Object target, Context context,
    String operation, NVList parameters, NamedValue returns,
    ExceptionList exceptions, ContextList ctx_list
  )
  {
    gnuRequest request = getRequestInstance(target);

    request.setIor(ior);
    request.set_target(target);

    request.setOperation(operation);
    request.set_args(parameters);
    request.m_context = context;
    request.set_result(returns);
    request.set_exceptions(exceptions);
    request.set_context_list(ctx_list);
    request.setORB(orb);

    return request;
  }

  /**
   * Get the instance of request.
   */
  protected gnuRequest getRequestInstance(org.omg.CORBA.Object target)
  {
    return new gnuRequest();
  }

  /**
   * Invoke operation on the given object, als handling temproray and permanent
   * redirections. The ReplyHeader.LOCATION_FORWARD will cause to resend the
   * request to the new direction. The ReplyHeader.LOCATION_FORWARD_PERM will
   * cause additionally to remember the new location by this delegate, so
   * subsequent calls will be immediately delivered to the new target.
   * 
   * @param target the target object.
   * @param output the output stream, previously returned by
   * {@link #request(org.omg.CORBA.Object, String, boolean)}.
   * 
   * @return the input stream, to read the response from or null for a one-way
   * request.
   * 
   * @throws SystemException if the SystemException has been thrown on the
   * remote side (the exact type and the minor code matches the data of the
   * remote exception that has been thrown).
   * 
   * @throws org.omg.CORBA.portable.ApplicationException as specified.
   * @throws org.omg.CORBA.portable.RemarshalException as specified.
   */
  public InputStream invoke(org.omg.CORBA.Object target, OutputStream output)
    throws ApplicationException, RemarshalException
  {
    StreamBasedRequest request = (StreamBasedRequest) output;
    while (true)
      {
        try
          {
            if (request.response_expected)
              {
                RawReply response = request.request.submit();

                // Read reply header.
                ReplyHeader rh = response.header.create_reply_header();
                BufferredCdrInput input = response.getStream();
                input.setOrb(orb);
                rh.read(input);
                request.request.m_rph = rh;

                boolean moved_permanently = false;

                switch (rh.reply_status)
                  {
                    case ReplyHeader.NO_EXCEPTION:
                      if (request.request.m_interceptor != null)
                        request.request.m_interceptor.receive_reply(request.request.m_info);
                      if (response.header.version.since_inclusive(1, 2))
                        input.align(8);
                      return input;

                    case ReplyHeader.SYSTEM_EXCEPTION:
                      if (response.header.version.since_inclusive(1, 2))
                        input.align(8);
                      showException(request, input);

                      throw ObjectCreator.readSystemException(input,
                        rh.service_context);

                    case ReplyHeader.USER_EXCEPTION:
                      if (response.header.version.since_inclusive(1, 2))
                        input.align(8);
                      showException(request, input);

                      throw new ApplicationException(
                        request.request.m_exception_id, input);

                    case ReplyHeader.LOCATION_FORWARD_PERM:
                      moved_permanently = true;

                    case ReplyHeader.LOCATION_FORWARD:
                      if (response.header.version.since_inclusive(1, 2))
                        input.align(8);

                      IOR forwarded = new IOR();
                      try
                        {
                          forwarded._read_no_endian(input);
                        }
                      catch (IOException ex)
                        {
                          MARSHAL t = new MARSHAL("Cant read forwarding info",
                            5102, CompletionStatus.COMPLETED_NO);
                          t.initCause(ex);
                          throw t;
                        }

                      gnuRequest prev = request.request;
                      gnuRequest r = getRequestInstance(target);

                      r.m_interceptor = prev.m_interceptor;
                      r.m_slots = prev.m_slots;

                      r.m_args = prev.m_args;
                      r.m_context = prev.m_context;
                      r.m_context_list = prev.m_context_list;
                      r.m_environment = prev.m_environment;
                      r.m_exceptions = prev.m_exceptions;
                      r.m_operation = prev.m_operation;
                      r.m_parameter_buffer = prev.m_parameter_buffer;
                      r.m_parameter_buffer.request = r;
                      r.m_result = prev.m_result;
                      r.m_target = prev.m_target;
                      r.oneWay = prev.oneWay;
                      r.m_forward_ior = forwarded;

                      if (r.m_interceptor != null)
                        r.m_interceptor.receive_other(r.m_info);

                      r.setIor(forwarded);

                      IorObject it = new IorObject(orb,
                        forwarded);

                      r.m_target = it;

                      request.request = r;

                      IOR prev_ior = getIor();

                      setIor(forwarded);

                      try
                        {
                          return invoke(it, request);
                        }
                      finally
                        {
                          if (!moved_permanently)
                            setIor(prev_ior);
                        }

                    default:
                      throw new MARSHAL("Unknow reply status: "
                        + rh.reply_status, 8000 + rh.reply_status,
                        CompletionStatus.COMPLETED_NO);
                  }
              }
            else
              {
                request.request.send_oneway();
                return null;
              }
          }
        catch (ForwardRequest forwarded)
          {
            ForwardRequest fw = forwarded;
            Forwarding2: while (true)
              {
                try
                  {
                    gnuRequest prev = request.request;
                    gnuRequest r = getRequestInstance(target);

                    r.m_interceptor = prev.m_interceptor;
                    r.m_args = prev.m_args;
                    r.m_context = prev.m_context;
                    r.m_context_list = prev.m_context_list;
                    r.m_environment = prev.m_environment;
                    r.m_exceptions = prev.m_exceptions;
                    r.m_operation = prev.m_operation;
                    r.m_parameter_buffer = prev.m_parameter_buffer;
                    r.m_parameter_buffer.request = r;
                    r.m_result = prev.m_result;
                    r.m_target = prev.m_target;
                    r.oneWay = prev.oneWay;

                    r.m_forwarding_target = fw.forward;

                    if (r.m_interceptor != null)
                      r.m_interceptor.receive_other(r.m_info);

                    r.m_target = fw.forward;
                    request.request = r;
                    break Forwarding2;
                  }
                catch (ForwardRequest e)
                  {
                    forwarded = e;
                  }
              }
          }
      }
  }

  /**
   * Show exception to interceptor.
   */
  void showException(StreamBasedRequest request, BufferredCdrInput input)
    throws ForwardRequest
  {
    input.mark(2048);
    request.request.m_exception_id = input.read_string();
    input.reset();

    if (request.request.m_interceptor != null)
      request.request.m_interceptor.receive_exception(request.request.m_info);
  }

  /**
   * Create a request to invoke the method of this CORBA object.
   *
   * @param target the CORBA object, to that this operation must be applied.
   * @param operation the name of the method to invoke.
   *
   * @return the request.
   */
  public Request request(org.omg.CORBA.Object target, String operation)
  {
    gnuRequest request = getRequestInstance(target);

    request.setIor(ior);
    request.set_target(target);

    request.setOperation(operation);
    request.setORB(orb);

    return request;
  }

  /**
   * Create a request to invoke the method of this CORBA object.
   *
   * @param target the CORBA object, to that this operation must be applied.
   * @param operation the name of the method to invoke.
   * @param response_expected specifies if this is one way message or the
   * response to the message is expected.
   *
   * @return the stream where the method arguments should be written.
   */
  public OutputStream request(org.omg.CORBA.Object target, String operation,
    boolean response_expected
  )
  {
    gnuRequest request = getRequestInstance(target);

    request.setIor(ior);
    request.set_target(target);
    request.setOperation(operation);

    StreamBasedRequest out = request.getParameterStream();
    out.response_expected = response_expected;
    request.setORB(orb);
    out.setOrb(orb);

    return out;
  }

  /**
   * If there is an opened cache socket to access this object, close that
   * socket.
   *
   * @param target The target is not used, this delegate requires a single
   * instance per object.
   */
  public void release(org.omg.CORBA.Object target)
  {
    // Do nothing here.
  }

  /**
   * Reset the remote_ior flag, forcing to check if the object is local on the
   * next getRequestInstance call.
   */
  public void setIor(IOR an_ior)
  {
    super.setIor(an_ior);
  }

  /**
   * Checks if the ior is local so far it is easy.
   */
  public boolean is_local(org.omg.CORBA.Object self)
  {
    return false;
  }
}