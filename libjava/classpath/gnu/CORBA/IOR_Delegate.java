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

import gnu.CORBA.CDR.cdrBufInput;
import gnu.CORBA.GIOP.ReplyHeader;

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

import java.io.IOException;

import java.net.Socket;

/**
 * The Classpath implementation of the {@link Delegate} functionality in the
 * case, when the object was constructed from an IOR object. The IOR can be
 * constructed from the stringified object reference.
 *
 * There is an different instance of this delegate for each CORBA object.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class IOR_Delegate
  extends Simple_delegate
{
  /**
   * Contructs an instance of object using the given IOR.
   */
  public IOR_Delegate(ORB an_orb, IOR an_ior)
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
   * @param exceptions the exceptions that can be thrown by the method
   * @param ctx_list the context list (null allowed)
   *
   * @return the created request.
   */
  public Request create_request(org.omg.CORBA.Object target, Context context,
                                String operation, NVList parameters,
                                NamedValue returns
                               )
  {
    gnuRequest request = new gnuRequest();

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
                                String operation, NVList parameters,
                                NamedValue returns, ExceptionList exceptions,
                                ContextList ctx_list
                               )
  {
    gnuRequest request = new gnuRequest();

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
   * Invoke operation on the given object, writing parameters to the given
   * output stream.
   *
   * @param target the target object.
   * @param output the output stream, previously returned by
   * {@link #request(org.omg.CORBA.Object, String, boolean)}.
   *
   * @return the input stream, to read the response from or null for a
   * one-way request.
   *
   * @throws SystemException if the SystemException has been thrown on the
   * remote side (the exact type and the minor code matches the data of
   * the remote exception that has been thrown).
   *
   * @throws org.omg.CORBA.portable.ApplicationException as specified.
   * @throws org.omg.CORBA.portable.RemarshalException as specified.
   */
  public InputStream invoke(org.omg.CORBA.Object target, OutputStream output)
                     throws ApplicationException, RemarshalException
  {
    streamRequest request = (streamRequest) output;
    if (request.response_expected)
      {
        binaryReply response = request.request.submit();

        // Read reply header.
        ReplyHeader rh = response.header.create_reply_header();
        cdrBufInput input = response.getStream();
        input.setOrb(orb);
        rh.read(input);

        boolean moved_permanently = false;

        switch (rh.reply_status)
          {
            case ReplyHeader.NO_EXCEPTION :
              if (response.header.version.since_inclusive(1, 2))
                input.align(8);
              return input;

            case ReplyHeader.SYSTEM_EXCEPTION :
              if (response.header.version.since_inclusive(1, 2))
                input.align(8);
              throw ObjectCreator.readSystemException(input);

            case ReplyHeader.USER_EXCEPTION :
              if (response.header.version.since_inclusive(1, 2))
                input.align(8);
              input.mark(2000);

              String uxId = input.read_string();
              input.reset();

              throw new ApplicationException(uxId, input);

            case ReplyHeader.LOCATION_FORWARD_PERM :
              moved_permanently = true;

            case ReplyHeader.LOCATION_FORWARD :
              if (response.header.version.since_inclusive(1, 2))
                input.align(8);

              IOR forwarded = new IOR();
              try
                {
                  forwarded._read_no_endian(input);
                }
              catch (IOException ex)
                {
                  MARSHAL t = new MARSHAL("Cant read forwarding info");
                  t.initCause(ex);
                  throw t;
                }

              request.request.setIor(forwarded);

              // If the object has moved permanently, its IOR is replaced.
              if (moved_permanently)
                setIor(forwarded);

              return invoke(target, request);

            default :
              throw new MARSHAL("Unknow reply status: " + rh.reply_status);
          }
      }
    else
      {
        request.request.send_oneway();
        return null;
      }
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
    gnuRequest request = new gnuRequest();

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
    gnuRequest request = new gnuRequest();

    request.setIor(ior);
    request.set_target(target);
    request.setOperation(operation);

    request.getParameterStream().response_expected = response_expected;
    request.setORB(orb);

    return request.getParameterStream();
  }

  /**
   * If there is an opened cache socket to access this object, close
   * that socket.
   *
   * @param target The target is not used, this delegate requires a
   * single instance per object.
   */
  public void release(org.omg.CORBA.Object target)
  {
    String key = ior.Internet.host + ":" + ior.Internet.port;

    Socket socket = SocketRepository.get_socket(key);
    try
      {
        if (socket != null)
          {
            socket.close();
          }
      }
    catch (IOException ex)
      {
        // do nothing, then.
      }
  }
}