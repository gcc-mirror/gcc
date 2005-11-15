/* LocalRequest.java --
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


package gnu.CORBA.Poa;

import gnu.CORBA.CDR.BufferedCdrOutput;
import gnu.CORBA.GIOP.MessageHeader;
import gnu.CORBA.GIOP.v1_2.ReplyHeader;
import gnu.CORBA.GIOP.v1_2.RequestHeader;
import gnu.CORBA.Interceptor.gnuClientRequestInfo;
import gnu.CORBA.Interceptor.gnuServerRequestInfo;
import gnu.CORBA.typecodes.RecordTypeCode;
import gnu.CORBA.ObjectCreator;
import gnu.CORBA.Unexpected;
import gnu.CORBA.gnuAny;
import gnu.CORBA.gnuRequest;
import gnu.CORBA.StreamHolder;
import gnu.CORBA.StreamBasedRequest;

import org.omg.CORBA.ARG_OUT;
import org.omg.CORBA.Any;
import org.omg.CORBA.BAD_INV_ORDER;
import org.omg.CORBA.BAD_OPERATION;
import org.omg.CORBA.Bounds;
import org.omg.CORBA.NamedValue;
import org.omg.CORBA.ORB;
import org.omg.CORBA.SystemException;
import org.omg.CORBA.TCKind;
import org.omg.CORBA.UnknownUserException;
import org.omg.CORBA.UserException;
import org.omg.CORBA.portable.ApplicationException;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.InvokeHandler;
import org.omg.CORBA.portable.ObjectImpl;
import org.omg.CORBA.portable.OutputStream;
import org.omg.CORBA.portable.ResponseHandler;
import org.omg.PortableInterceptor.ClientRequestInterceptorOperations;
import org.omg.PortableInterceptor.ForwardRequest;
import org.omg.PortableInterceptor.ServerRequestInterceptorOperations;
import org.omg.PortableServer.CurrentOperations;
import org.omg.PortableServer.CurrentPackage.NoContext;
import org.omg.PortableServer.DynamicImplementation;
import org.omg.PortableServer.POA;
import org.omg.PortableServer.Servant;
import org.omg.PortableServer.ServantLocatorPackage.CookieHolder;
import org.omg.PortableServer.portable.Delegate;

import java.io.IOException;

/**
 * Directs the invocation to the locally available servant. The POA servant does
 * not longer implement the CORBA object and cannot be substituted directly.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class LocalRequest extends gnuRequest implements ResponseHandler,
  CurrentOperations
{
  /**
   * Used by servant locator, if involved.
   */
  CookieHolder cookie;

  /**
   * The object Id.
   */
  final byte[] Id;

  /**
   * The message header (singleton is sufficient).
   */
  private static final MessageHeader header = new MessageHeader();

  /**
       * True if the stream was obtained by invoking {@link #createExceptionReply()},
   * false otherwise.
   */
  boolean exceptionReply;

  /**
   * The buffer to write into.
   */
  BufferedCdrOutput buffer;

  /**
   * The responsible POA.
   */
  final gnuPOA poa;

  /**
   * The servant delegate to obtain the handler.
   */
  gnuServantObject object;

  /**
   * Used (reused) with dynamic implementation.
   */
  LocalServerRequest serverRequest;

  /**
   * Create an instance of the local request.
   */
  public LocalRequest(gnuServantObject local_object, gnuPOA a_poa, byte[] an_id)
  {
    Id = an_id;
    poa = a_poa;

    // Instantiate the cookie holder only if required.
    if (poa.servant_locator != null)
      {
        cookie = new CookieHolder();
      }
    object = local_object;
    prepareStream();
  }

  /**
   * Make an invocation and return a stream from where the results can be read
   * and throw ApplicationException, where applicable.
   */
  org.omg.CORBA.portable.InputStream s_invoke(InvokeHandler handler)
    throws ApplicationException
  {
    try
      {
        poa.m_orb.currents.put(Thread.currentThread(), this);

        org.omg.CORBA.portable.InputStream input = v_invoke(handler);

        if (!exceptionReply)
          {
            return input;
          }
        else
          {
            input.mark(500);

            String id = input.read_string();
            try
              {
                input.reset();
              }
            catch (IOException ex)
              {
                InternalError ierr = new InternalError();
                ierr.initCause(ex);
                throw ierr;
              }
            throw new ApplicationException(id, input);
          }
      }
    finally
      {
        poa.m_orb.currents.remove(Thread.currentThread());
      }
  }

  /**
   * Make an invocation and return a stream from where the results can be read.
   *
   * @param the invoke handler (can be null, then it is obtained self
   * dependently).
   */
  public org.omg.CORBA.portable.InputStream v_invoke(InvokeHandler handler)
  {
    // Local request must be intercepted both by server and request
    // interceptors.
    boolean s_intercept = false;
    ServerRequestInterceptorOperations s_interceptor = null;
    gnuServerRequestInfo s_info = null;

    boolean c_intercept = false;
    ClientRequestInterceptorOperations c_interceptor = null;
    gnuClientRequestInfo c_info = null;

    try
      {
        if (poa.m_orb.iServer != null || poa.m_orb.iClient != null)
          {
            setORB(poa.m_orb);

            // These two are only needed with interceptors.
            m_rqh = new RequestHeader();
            m_rqh.operation = m_operation;
            m_rph = new ReplyHeader();

            m_rqh.object_key = object.Id;
            m_rph.request_id = m_rqh.request_id;
          }

        if (poa.m_orb.iClient != null)
          {
            c_interceptor = poa.m_orb.iClient;

            c_info = new gnuClientRequestInfo(this);
            c_intercept = true;

            c_interceptor.send_request(c_info);

            m_target = object;
          }

        if (poa.m_orb.iServer != null)
          {
            s_interceptor = poa.m_orb.iServer;

            s_info = new gnuServerRequestInfo(object, m_rqh, m_rph);
            s_info.m_request = this;

            s_intercept = true;

            s_interceptor.receive_request_service_contexts(s_info);
          }

        if (handler == null)
          {
            handler = object.getHandler(operation(), cookie, false);
          }

        BufferedCdrOutput request_part = new BufferedCdrOutput();

        request_part.setOrb(orb());

        if (m_args != null && m_args.count() > 0)
          {
            write_parameters(header, request_part);

            if (m_parameter_buffer != null)
              {
                throw new BAD_INV_ORDER("Please either add parameters or " +
                  "write them into stream, but not both " + "at once."
                );
              }
          }

        if (m_parameter_buffer != null)
          {
            write_parameter_buffer(header, request_part);
          }

        Servant servant;

        if (handler instanceof Servant)
          {
            servant = (Servant) handler;
          }
        else
          {
            throw new BAD_OPERATION("Unexpected handler type " + handler);
          }

        org.omg.CORBA.portable.InputStream input =
          request_part.create_input_stream();

        // Ensure the servant (handler) has a delegate set.
        ServantDelegateImpl sd = null;

        Delegate d = null;

        try
          {
            d = servant._get_delegate();
          }
        catch (Exception ex)
          {
            // In some cases exception is thrown if the delegate is not set.
          }
        if (d instanceof ServantDelegateImpl)
          {
            // If the delegate is already set, try to reuse the existing
            // instance.
            sd = (ServantDelegateImpl) d;
            if (sd.object != object)
              {
                sd = new ServantDelegateImpl(servant, poa, Id);
              }
          }
        else
          {
            sd = new ServantDelegateImpl(servant, poa, Id);
          }
        servant._set_delegate(sd);

        try
          {
            ORB o = orb();
            if (o instanceof ORB_1_4)
              {
                ((ORB_1_4) o).currents.put(Thread.currentThread(), this);
              }

            try
              {
                if (s_intercept)
                  {
                    s_interceptor.receive_request(s_info);
                  }
                handler._invoke(m_operation, input, this);

                // Handler is casted into i_handler.
                if ((s_intercept || c_intercept) && isExceptionReply())
                  {
                    s_info.m_reply_header.reply_status =
                      ReplyHeader.USER_EXCEPTION;
                    m_rph.reply_status = ReplyHeader.USER_EXCEPTION;

                    // Make Any, holding the user exception.
                    Any a = new gnuAny();
                    OutputStream buf = getBuffer();
                    InputStream in = buf.create_input_stream();
                    String uex_idl = "unknown";
                    try
                      {
                        in.mark(Integer.MAX_VALUE);
                        uex_idl = in.read_string();
                        m_exception_id = uex_idl;
                        in.reset();
                      }
                    catch (IOException e)
                      {
                        throw new Unexpected(e);
                      }

                    try
                      {
                        UserException exception =
                          ObjectCreator.readUserException(uex_idl, in);

                        m_environment.exception(exception);
                        ObjectCreator.insertWithHelper(a, exception);
                      }
                    catch (Exception e)
                      {
                        // Failed due any reason, insert without
                        // helper.
                        a.insert_Streamable(new StreamHolder(
                            buf.create_input_stream()
                          )
                        );

                        RecordTypeCode r =
                          new RecordTypeCode(TCKind.tk_except);
                        r.setId(uex_idl);
                        r.setName(ObjectCreator.getDefaultName(uex_idl));
                      }

                    s_info.m_usr_exception = a;
                    c_info.m_wrapped_exception = a;
                    s_interceptor.send_exception(s_info);
                    c_interceptor.receive_exception(c_info);
                  }
                else
                  {
                    if (s_intercept)
                      {
                        s_info.m_reply_header.reply_status =
                          ReplyHeader.NO_EXCEPTION;
                        s_interceptor.send_reply(s_info);
                      }
                    if (c_intercept)
                      {
                        m_rph.reply_status = ReplyHeader.NO_EXCEPTION;
                        c_interceptor.receive_reply(c_info);
                      }
                  }
              }
            catch (SystemException sys_ex)
              {
                if (s_intercept)
                  {
                    s_info.m_reply_header.reply_status =
                      ReplyHeader.SYSTEM_EXCEPTION;
                    s_info.m_sys_exception = sys_ex;
                    s_interceptor.send_exception(s_info);
                  }

                if (c_intercept)
                  {
                    m_rph.reply_status = ReplyHeader.SYSTEM_EXCEPTION;

                    Any a = new gnuAny();
                    if (ObjectCreator.insertSysException(a, sys_ex))
                      {
                        c_info.m_wrapped_exception = a;
                      }
                    c_interceptor.receive_exception(c_info);
                  }

                throw sys_ex;
              }
          }
        finally
          {
            ORB o = orb();
            if (o instanceof ORB_1_4)
              {
                ((ORB_1_4) o).currents.remove(Thread.currentThread());
              }
          }

        if (poa.servant_locator != null)
          {
            poa.servant_locator.postinvoke(object.Id, poa, operation(),
              cookie.value, object.getServant()
            );
          }
        return buffer.create_input_stream();
      }

    catch (ForwardRequest fex)
      {
        // May be thrown by interceptor.
        if (s_intercept)
          {
            Forwarding:
            while (true)
              {
                s_info.m_reply_header.reply_status =
                  ReplyHeader.LOCATION_FORWARD;
                s_info.m_forward_reference = fex.forward;
                try
                  {
                    s_interceptor.send_other(s_info);
                    break Forwarding;
                  }
                catch (ForwardRequest fex2)
                  {
                    s_info.m_forward_reference = fex2.forward;
                    fex.forward = s_info.m_forward_reference;
                  }
              }
          }

        if (c_intercept)
          {
            this.m_rph.reply_status = ReplyHeader.LOCATION_FORWARD;
            this.m_forwarding_target = fex.forward;
            try
              {
                c_interceptor.receive_other(c_info);
              }
            catch (ForwardRequest fex2)
              {
                fex.forward = fex2.forward;
              }
          }
        throw new gnuForwardRequest(fex.forward);
      }
    catch (gnuForwardRequest fex)
      {
        // May be thrown during activation.
        // May be thrown during activation.
        if (s_intercept)
          {
            Forwarding:
            while (true)
              {
                s_info.m_reply_header.reply_status =
                  ReplyHeader.LOCATION_FORWARD;
                s_info.m_forward_reference = fex.forward_reference;
                try
                  {
                    s_interceptor.send_other(s_info);
                    break Forwarding;
                  }
                catch (ForwardRequest fex2)
                  {
                    s_info.m_forward_reference = fex2.forward;
                    fex.forward_reference = (ObjectImpl) fex2.forward;
                  }
              }
          }

        if (c_intercept)
          {
            this.m_rph.reply_status = ReplyHeader.LOCATION_FORWARD;
            this.m_forwarding_target = fex.forward_reference;
            try
              {
                c_interceptor.receive_other(c_info);
              }
            catch (ForwardRequest fex2)
              {
                fex.forward_reference = (ObjectImpl) fex2.forward;
              }
          }
        throw fex;
      }
  }

  /**
   * Make an invocation and store the result in the fields of this Request. Used
   * with DII only.
   */
  public void invoke()
  {
    InvokeHandler handler = object.getHandler(operation(), cookie, false);

    if (handler instanceof DynamicImpHandler)
      {
        DynamicImplementation dyn = ((DynamicImpHandler) handler).servant;
        if (serverRequest == null)
          {
            serverRequest = new LocalServerRequest(this);
          }
        try
          {
            poa.m_orb.currents.put(Thread.currentThread(), this);
            dyn.invoke(serverRequest);
          }
        finally
          {
            poa.m_orb.currents.remove(Thread.currentThread());
          }
      }
    else
      {
        org.omg.CORBA.portable.InputStream input = v_invoke(handler);

        if (!exceptionReply)
          {
            NamedValue arg;

            // Read return value, if set.
            if (m_result != null)
              {
                m_result.value().read_value(input, m_result.value().type());
              }

            // Read returned parameters, if set.
            if (m_args != null)
              {
                for (int i = 0; i < m_args.count(); i++)
                  {
                    try
                      {
                        arg = m_args.item(i);

                        // Both ARG_INOUT and ARG_OUT have this binary flag set.
                        if ((arg.flags() & ARG_OUT.value) != 0)
                          {
                            arg.value().read_value(input, arg.value().type());
                          }
                      }
                    catch (Bounds ex)
                      {
                        Unexpected.error(ex);
                      }
                  }
              }
          }
        else// User exception reply
          {
            // Prepare an Any that will hold the exception.
            gnuAny exc = new gnuAny();

            exc.insert_Streamable(new StreamHolder(input));

            UnknownUserException unuex = new UnknownUserException(exc);
            m_environment.exception(unuex);
          }
      }
  }

  /**
   * Get an output stream for providing details about the exception. Before
   * returning the stream, the handler automatically writes the message header
   * and the reply about exception header, but not the message header.
   *
   * @return the stream to write exception details into.
   */
  public OutputStream createExceptionReply()
  {
    exceptionReply = true;
    prepareStream();
    return buffer;
  }

  /**
   * Get an output stream for writing a regular reply (not an exception).
   *
   * Before returning the stream, the handler automatically writes the regular
   * reply header, but not the message header.
   *
   * @return the output stream for writing a regular reply.
   */
  public OutputStream createReply()
  {
    exceptionReply = false;
    prepareStream();
    return buffer;
  }

  /**
   * Get the buffer, normally containing the written reply. The reply includes
   * the reply header (or the exception header) but does not include the message
   * header.
   *
   * The stream buffer can also be empty if no data have been written into
   * streams, returned by {@link #createReply()} or
   * {@link #createExceptionReply()}.
   *
   * @return the CDR output stream, containing the written output.
   */
  BufferedCdrOutput getBuffer()
  {
    return buffer;
  }

  /**
   * True if the stream was obtained by invoking {@link #createExceptionReply()},
   * false otherwise (usually no-exception reply).
   */
  boolean isExceptionReply()
  {
    return exceptionReply;
  }

  /**
   * Compute the header offset, set the correct version number and codeset.
   */
  private void prepareStream()
  {
    buffer = new BufferedCdrOutput();
    buffer.setOrb(orb());
  }

  /**
   * Get the parameter stream, where the invocation arguments should be written
   * if they are written into the stream directly.
   */
  public StreamBasedRequest getParameterStream()
  {
    m_parameter_buffer = new StreamBasedRequest();
    m_parameter_buffer.request = this;
    m_parameter_buffer.setOrb(poa.orb());
    return m_parameter_buffer;
  }

  public byte[] get_object_id() throws NoContext
  {
    return Id;
  }

  public POA get_POA() throws NoContext
  {
    return poa;
  }
}