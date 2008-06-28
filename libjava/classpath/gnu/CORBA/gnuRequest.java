/* gnuRequest.java --
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
import gnu.CORBA.CDR.BufferedCdrOutput;
import gnu.CORBA.GIOP.MessageHeader;
import gnu.CORBA.GIOP.ReplyHeader;
import gnu.CORBA.GIOP.RequestHeader;
import gnu.CORBA.GIOP.CodeSetServiceContext;
import gnu.CORBA.Interceptor.gnuClientRequestInfo;
import gnu.CORBA.Poa.ORB_1_4;

import org.omg.CORBA.ARG_IN;
import org.omg.CORBA.ARG_INOUT;
import org.omg.CORBA.ARG_OUT;
import org.omg.CORBA.Any;
import org.omg.CORBA.BAD_INV_ORDER;
import org.omg.CORBA.BAD_PARAM;
import org.omg.CORBA.Bounds;
import org.omg.CORBA.COMM_FAILURE;
import org.omg.CORBA.CompletionStatus;
import org.omg.CORBA.Context;
import org.omg.CORBA.ContextList;
import org.omg.CORBA.Environment;
import org.omg.CORBA.ExceptionList;
import org.omg.CORBA.INV_POLICY;
import org.omg.CORBA.MARSHAL;
import org.omg.CORBA.NO_IMPLEMENT;
import org.omg.CORBA.NO_RESOURCES;
import org.omg.CORBA.NVList;
import org.omg.CORBA.NamedValue;
import org.omg.CORBA.ORB;
import org.omg.CORBA.Policy;
import org.omg.CORBA.Request;
import org.omg.CORBA.SystemException;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.UnknownUserException;
import org.omg.CORBA.portable.ObjectImpl;
import org.omg.IOP.ServiceContext;
import org.omg.IOP.TAG_CODE_SETS;
import org.omg.IOP.TAG_INTERNET_IOP;
import org.omg.IOP.TaggedComponent;
import org.omg.IOP.TaggedProfile;
import org.omg.PortableInterceptor.ClientRequestInfo;
import org.omg.PortableInterceptor.ClientRequestInterceptorOperations;
import org.omg.PortableInterceptor.ForwardRequest;
import org.omg.PortableInterceptor.InvalidSlot;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import java.net.Socket;

import java.util.ArrayList;

/**
 * The implementation of the CORBA request.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class gnuRequest extends Request implements Cloneable
{
  /**
   * The maximal supported GIOP version.
   */
  public static Version MAX_SUPPORTED = new Version(1, 2);

  /**
   * The initial pause that the Request makes when the required port is not
   * available.
   */
  public static int PAUSE_INITIAL = 50;

  /**
   * The number of repretetive attempts to get a required port, if it is not
   * immediately available.
   */
  public static int PAUSE_STEPS = 12;

  /**
   * The maximal pausing interval between two repetetive attempts. The interval
   * doubles after each unsuccessful attempt, but will not exceed this value.
   */
  public static int PAUSE_MAX = 1000;

  /**
   * The interceptor, listening the major request submission points.
   */
  ClientRequestInterceptorOperations m_interceptor;

  /**
   * The request info, used by interceptor.
   */
  ClientRequestInfo m_info = new gnuClientRequestInfo(this);

  /**
   * The empty byte array.
   */
  private static final RawReply EMPTY =
    new RawReply(null, new MessageHeader(), new byte[ 0 ]);

  /**
   * The context holder for methods ctx(Context) and ctx().
   */
  protected Context m_context;

  /**
   * The context list for method contexts().
   */
  protected ContextList m_context_list;

  /**
   * The request environment for holding the exception the has possibly been
   * thrown by the method being invoked.
   */
  protected Environment m_environment = new gnuEnvironment();

  /**
   * The list of all exceptions that can be thrown by the method being invoked.
   */
  protected ExceptionList m_exceptions = new gnuExceptionList();

  /**
   * The result, returned by the invoked method (function).
   */
  protected NamedValue m_result = new gnuNamedValue();

  /**
   * The exception id, received from the server, null if none.
   */
  protected String m_exception_id;

  /**
   * The thrown system exception.
   */
  protected SystemException m_sys_ex;

  /**
   * The invocation target.
   */
  protected org.omg.CORBA.Object m_target;

  /**
   * The name of the method being invoked.
   */
  protected String m_operation;

  /**
   * This field temporary remembers the value of the forwarded ior reference. If
   * it is not null, the request was forwarded and the effective target is not
   * the same as the default target.
   */
  public IOR m_forward_ior;

  /**
   * Is set when object, and not IOR is directly available.
   */
  public org.omg.CORBA.Object m_forwarding_target;

  /**
   * The flag, indicating that the request has been sent and the result is
   * already received.
   */
  protected boolean complete;

  /**
   * The flag, indicating that the response to this request must be ignored
   * (used with {@link #send_oneway()}).
   */
  protected boolean oneWay;

  /**
   * The flag, indicating that the request has been sent and no result is yet
   * received.
   */
  protected boolean running;

  /**
   * The request arguments.
   */
  protected gnuNVList m_args = new gnuNVList();

  /**
   * The request arguments in the case when they are directly written into the
   * parameter buffer.
   */
  protected StreamBasedRequest m_parameter_buffer;

  /**
   * The array of slots.
   */
  protected Any[] m_slots;

  /**
   * The request header currently in use.
   */
  protected RequestHeader m_rqh;

  /**
   * The reply header currently in use.
   */
  protected ReplyHeader m_rph;

  /**
   * The IOR of the target.
   */
  private IOR ior;

  /**
   * The ORB of the target.
   */
  private ORB orb;

  /**
   * The encoding, used to send the message.
   *
   * The default encoding is inherited from the set IOR (that string reference
   * can be encoded in either Big or Little endian). If the IOR encoding is not
   * known (for example, by obtaining the reference from the naming service),
   * the Big Endian is used.
   */
  private boolean Big_endian = true;

  /**
   * Set the IOR data, sufficient to find the invocation target. This also sets
   * default endian encoding for invocations.
   *
   * @see IOR.parse(String)
   */
  public void setIor(IOR an_ior)
  {
    ior = an_ior;
    setBigEndian(ior.Big_Endian);
  }

  /**
   * Used when redirecting request to another target.
   */
  gnuRequest redirected;

  /**
   * Get the IOR data, sufficient to find the invocation target.
   *
   * @return the IOR data.
   */
  public IOR getIor()
  {
    return ior;
  }

  /**
   * Set the ORB, related to the invocation target.
   */
  public void setORB(ORB an_orb)
  {
    orb = an_orb;

    // Take the interceptor from the ORB.
    if (orb instanceof OrbRestricted)
      m_interceptor = ((OrbRestricted) orb).iClient;

    if (m_interceptor != null && orb instanceof ORB_1_4)
      {
        m_slots = ((ORB_1_4) orb).ic_current.clone_slots();
      }
  }

  /**
   * Set the encoding that will be used to send the message. The default
   * encoding is inherited from the set IOR (that string reference can be
   * encoded in either Big or Little endian). If the IOR encoding is not known
   * (for example, by obtaining the reference from the naming service), the Big
   * Endian is used.
   *
   * @param use_big_endian true to use the Big Endian, false to use the Little
   * Endian encoding.
   */
  public void setBigEndian(boolean use_big_endian)
  {
    Big_endian = use_big_endian;
  }

  /**
   * The the method name to invoke.
   *
   * @param operation the method name.
   */
  public void setOperation(String operation)
  {
    m_operation = operation;
  }

  /**
   * Get the parameter stream, where the invocation arguments should be written
   * if they are written into the stream directly.
   */
  public StreamBasedRequest getParameterStream()
  {
    m_parameter_buffer = new StreamBasedRequest();
    m_parameter_buffer.request = this;
    m_parameter_buffer.setVersion(ior.Internet.version);
    m_parameter_buffer.setCodeSet(CodeSetServiceContext.negotiate(ior.Internet.CodeSets));
    m_parameter_buffer.setOrb(orb);
    m_parameter_buffer.setBigEndian(Big_endian);

    // For the old iiop versions, it is important to set the size
    // correctly.
    if (ior.Internet.version.until_inclusive(1, 1))
      {
        BufferedCdrOutput measure = new BufferedCdrOutput();
        measure.setOffset(12);
        if (m_rqh == null)
          m_rqh = new gnu.CORBA.GIOP.v1_0.RequestHeader();
        m_rqh.operation = m_operation;
        m_rqh.object_key = ior.key;
        m_rqh.write(measure);
        m_parameter_buffer.setOffset(12 + measure.buffer.size());
      }

    return m_parameter_buffer;
  }

  /**
   * Creates a shallow copy of this request.
   */
  public gnuRequest Clone()
  {
    try
      {
        return (gnuRequest) clone();
      }
    catch (CloneNotSupportedException ex)
      {
        throw new Unexpected(ex);
      }
  }

  /** {@inheritDoc} */
  public Any add_in_arg()
  {
    gnuNamedValue v = new gnuNamedValue();
    v.setFlags(ARG_IN.value);
    m_args.add(v);
    return v.value();
  }

  /** {@inheritDoc} */
  public Any add_inout_arg()
  {
    gnuNamedValue v = new gnuNamedValue();
    v.setFlags(ARG_INOUT.value);
    m_args.add(v);
    return v.value();
  }

  /** {@inheritDoc} */
  public Any add_named_in_arg(String name)
  {
    gnuNamedValue v = new gnuNamedValue();
    v.setFlags(ARG_IN.value);
    v.setName(name);
    m_args.add(v);
    return v.value();
  }

  /** {@inheritDoc} */
  public Any add_named_inout_arg(String name)
  {
    gnuNamedValue v = new gnuNamedValue();
    v.setFlags(ARG_INOUT.value);
    v.setName(name);
    m_args.add(v);
    return v.value();
  }

  /** {@inheritDoc} */
  public Any add_named_out_arg(String name)
  {
    gnuNamedValue v = new gnuNamedValue();
    v.setFlags(ARG_OUT.value);
    v.setName(name);
    m_args.add(v);
    return v.value();
  }

  /** {@inheritDoc} */
  public Any add_out_arg()
  {
    gnuNamedValue v = new gnuNamedValue();
    v.setFlags(ARG_OUT.value);
    m_args.add(v);
    return v.value();
  }

  /** {@inheritDoc} */
  public NVList arguments()
  {
    return m_args;
  }

  /** {@inheritDoc} */
  public ContextList contexts()
  {
    return m_context_list;
  }

  /** {@inheritDoc} */
  public Context ctx()
  {
    return m_context;
  }

  /** {@inheritDoc} */
  public void ctx(Context a_context)
  {
    m_context = a_context;
  }

  /** {@inheritDoc} */
  public Environment env()
  {
    return m_environment;
  }

  /** {@inheritDoc} */
  public ExceptionList exceptions()
  {
    return m_exceptions;
  }

  /** {@inheritDoc} */
  public void get_response() throws org.omg.CORBA.WrongTransaction
  {
    /**
     * The response is ready after it is received. FIXME implement context
     * checks and any other functionality, if required.
     */
  }

  /**
   * Submit the request, suspending the current thread until the answer is
   * received.
   *
   * This implementation requires to set the IOR property ({@link #setIOR(IOR)}
   * before calling this method.
   *
   * @throws BAD_INV_ORDER, minor code 0, if the IOR has not been previously
   * set.
   *
   * @throws SystemException if this exception has been thrown on remote side.
   * The exact exception type and the minor code are the same as they have been
   * for the exception, thrown on remoted side.
   */
  public synchronized void invoke() throws BAD_INV_ORDER
  {
    waitWhileBusy();
    complete = false;
    running = true;

    if (ior == null)
      throw new BAD_INV_ORDER("Set IOR property first");

    try
      {
        Forwardings:
        while (true)
          {
            try
              {
                p_invoke();
                break Forwardings;
              }
            catch (ForwardRequest e)
              {
                try
                  {
                    ObjectImpl impl = (ObjectImpl) e.forward;
                    SimpleDelegate delegate =
                      (SimpleDelegate) impl._get_delegate();
                    ior = delegate.getIor();
                  }
                catch (Exception ex)
                  {
                    BAD_PARAM bad =
                      new BAD_PARAM("Unsupported forwarding target");
                    bad.initCause(ex);
                    throw bad;
                  }
              }
          }
      }
    finally
      {
        running = false;
        complete = true;
      }
  }

  /** {@inheritDoc} */
  public String operation()
  {
    return m_operation;
  }

  /**
   * Get the orb, related to the invocation target.
   */
  public ORB orb()
  {
    return orb;
  }

  /** {@inheritDoc} */
  public boolean poll_response()
  {
    return complete && !running;
  }

  /** {@inheritDoc} */
  public NamedValue result()
  {
    return m_result;
  }

  /**
   * {@inheritDoc}
   *
   */
  public Any return_value()
  {
    return m_result.value();
  }

  /** {@inheritDoc} */
  public synchronized void send_deferred()
  {
    waitWhileBusy();
    new Thread()
      {
        public void run()
        {
          invoke();
        }
      }.start();
  }

  /**
   * Send a request and forget about it, not waiting for a response. This can be
   * done also for methods that normally are expected to return some values.
   *
   * TODO It is generally recommended to reuse the threads. Reuse?
   */
  public void send_oneway()
  {
    final gnuRequest cloned = Clone();
    cloned.oneWay = true;
    
    new Thread()
      {
        public void run()
        {
          cloned.invoke();
        }
      }.start();
  }

  /**
   * Set the argument list. This field is initialised as empty non null instance
   * by default, so the method is only used in cases when the direct replacement
   * is desired.
   *
   * @param a_args the argument list.
   */
  public void set_args(NVList a_args)
  {
    if (a_args instanceof gnuNVList)
      m_args = (gnuNVList) a_args;
    else
      {
        try
          {
            // In case if this is another implementation of the NVList.
            m_args.list.clear();
            for (int i = 0; i < a_args.count(); i++)
              {
                m_args.add(a_args.item(i));
              }
          }
        catch (Bounds ex)
          {
            Unexpected.error(ex);
          }
      }
  }

  /**
   * Set the context list that is later returned by the method
   * {@link #contexts()}.
   *
   * @param a_context_list a new context list.
   */
  public void set_context_list(ContextList a_context_list)
  {
    m_context_list = a_context_list;
  }

  /**
   * Set the exception container. This field is initialised as empty non null
   * instance by default, so the method is only used in cases when the direct
   * replacement is desired.
   *
   * @param a_environment the new exception container.
   */
  public void set_environment(Environment a_environment)
  {
    m_environment = a_environment;
  }

  /**
   * Set the list of exceptions. This field is initialised as empty non null
   * instance by default, so the method is only used in cases when the direct
   * replacement is desired.
   *
   * @param a_exceptions a list of exceptions.
   */
  public void set_exceptions(ExceptionList a_exceptions)
  {
    m_exceptions = a_exceptions;
  }

  /**
   * Set the operation name.
   *
   * @param a_operation the operation name.
   */
  public void set_operation(String a_operation)
  {
    m_operation = a_operation;
  }

  /**
   * Set the named value, returned as result. This field is initialised as empty
   * non null instance by default, so the method is only used in cases when the
   * direct replacement is desired.
   *
   * @param a_result the result keeper.
   */
  public void set_result(NamedValue a_result)
  {
    m_result = a_result;
  }

  /**
   * Set the type of the named value, returned as a result. Instantiates a new
   * instance of the result value.
   */
  public void set_return_type(TypeCode returns)
  {
    if (m_result == null || !returns.equal(m_result.value().type()))
      {
        m_result = new gnuNamedValue();
        m_result.value().type(returns);
      }
  }

  /**
   * Set the invocation target.
   *
   * @param a_target the CORBA object for that the method will be invoked.
   */
  public void set_target(org.omg.CORBA.Object a_target)
  {
    m_target = a_target;
  }

  /**
   * Do the actual invocation. This implementation requires to set the IOR
   * property ({@link #setIOR(IOR)} before calling this method.
   * 
   * @throws BAD_INV_ORDER, minor code 0, if the IOR has not been previously set
   *           or if the direct argument addition is mixed with the direct
   *           argument writing into the output stream.
   * @return the server response in binary form.
   */
public synchronized RawReply submit()
    throws ForwardRequest
  {
    gnu.CORBA.GIOP.MessageHeader header = new gnu.CORBA.GIOP.MessageHeader();

    header.setBigEndian(Big_endian);

    // The byte order will be Big Endian by default.
    header.message_type = gnu.CORBA.GIOP.MessageHeader.REQUEST;
    header.version = useVersion(ior.Internet.version);

    RequestHeader rh = header.create_request_header();
    rh.operation = m_operation;
    rh.object_key = ior.key;

    // Update interceptor.
    m_rqh = rh;

    if (m_interceptor != null)
      m_interceptor.send_request(m_info);

    // Prepare the submission.
    BufferedCdrOutput request_part = new BufferedCdrOutput();

    request_part.setOffset(header.getHeaderSize());
    request_part.setVersion(header.version);
    request_part.setCodeSet(CodeSetServiceContext.negotiate(ior.Internet.CodeSets));
    request_part.setOrb(orb);
    request_part.setBigEndian(header.isBigEndian());

    // This also sets the stream encoding to the encoding, specified
    // in the header.
    rh.write(request_part);

    if (m_args != null && m_args.count() > 0)
      {
        write_parameters(header, request_part);

        if (m_parameter_buffer != null)
          throw new BAD_INV_ORDER("Please either add parameters or "
            + "write them into stream, but not both " + "at once.");
      }

    if (m_parameter_buffer != null)
      {
        write_parameter_buffer(header, request_part);
      }

    // Now the message size is available.
    header.message_size = request_part.buffer.size();

    Socket socket = null;

    java.lang.Object key = ior.Internet.host + ":" + ior.Internet.port;

    synchronized (SocketRepository.class)
      {
        socket = SocketRepository.get_socket(key);
      }

    try
      {
        long pause = PAUSE_INITIAL;

        if (socket == null)
          {
            // The IOException may be thrown under very heavy parallel
            // load. For some time, just wait, exceptiong the socket to free.
            Open: for (int i = 0; i < PAUSE_STEPS; i++)
              {
                try
                  {
                    if (orb instanceof OrbFunctional)
                      socket = ((OrbFunctional) orb).socketFactory.
                        createClientSocket(
                          ior.Internet.host, ior.Internet.port);
                    else
                      socket = new Socket(ior.Internet.host, ior.Internet.port);
                    break Open;
                  }
                catch (IOException ex)
                  {
                    try
                      {
                        // Expecting to free a socket via finaliser.
                        System.gc();
                        Thread.sleep(pause);
                        pause = pause * 2;
                        if (pause > PAUSE_MAX)
                          pause = PAUSE_MAX;
                      }
                    catch (InterruptedException iex)
                      {
                      }
                  }
              }
          }

        if (socket == null)
          throw new NO_RESOURCES(ior.Internet.host + ":" + ior.Internet.port
            + " in use");
        socket.setKeepAlive(true);

        OutputStream socketOutput = socket.getOutputStream();

        // Write the message header.
        header.write(socketOutput);

        // Write the request header and parameters (if present).
        request_part.buffer.writeTo(socketOutput);

        socketOutput.flush();
        if (!socket.isClosed() && !oneWay)
          {
            MessageHeader response_header = new MessageHeader();
            InputStream socketInput = socket.getInputStream();
            response_header.read(socketInput);

            byte[] r;
            if (orb instanceof OrbFunctional)
              {
                OrbFunctional fo = (OrbFunctional) orb;
                r = response_header.readMessage(socketInput, socket,
                  fo.TOUT_WHILE_READING, fo.TOUT_AFTER_RECEIVING);
              }
            else
              r = response_header.readMessage(socketInput, null, 0, 0);

            return new RawReply(orb, response_header, r);
          }
        else
          return EMPTY;
      }
    catch (IOException io_ex)
      {
        COMM_FAILURE m = new COMM_FAILURE("Unable to open a socket at "
          + ior.Internet.host + ":" + ior.Internet.port, 0xC9,
          CompletionStatus.COMPLETED_NO);
        m.initCause(io_ex);
        throw m;
      }
    finally
      {
        try
          {
            if (socket != null && !socket.isClosed())
              {
                socket.setSoTimeout(OrbFunctional.TANDEM_REQUESTS);
                SocketRepository.put_socket(key, socket);
              }
          }
        catch (IOException scx)
          {
            InternalError ierr = new InternalError();
            ierr.initCause(scx);
            throw ierr;
          }
      }
  }

  /** {@inheritDoc} */
  public org.omg.CORBA.Object target()
  {
    return m_target;
  }

  /**
   * Get the used version. Normally, it is better to respond using the same
   * version as it is specified in IOR, but not above the maximal supported
   * version.
   */
  public Version useVersion(Version desired)
  {
    if (desired.until_inclusive(MAX_SUPPORTED.major, MAX_SUPPORTED.minor))
      return desired;
    else
      return MAX_SUPPORTED;
  }

  /**
   * Wait while the response to request, submitted using
   * {@link #send_deferred()} or {@link #invoke()} (from other thread) is
   * returned.
   *
   * FIXME It is possible to rewrite this using Object.wait() and
   * Object.notify(), but be sure to prepare the test as well.
   */
  public synchronized void waitWhileBusy()
  {
    // Waiting constants.
    long wait = 10;
    long increment = 2;
    long max = 5000;

    while (running)
      {
        try
          {
            Thread.sleep(wait);
            if (wait < max)
              wait = wait * increment;
          }
        catch (InterruptedException ex)
          {
          }
      }
  }

  /**
   * Do actual invocation. This method recursively calls itself if the
   * redirection is detected.
   */
  private void p_invoke()
    throws SystemException, ForwardRequest
  {
    RawReply response = submit();
    
    // If this is a one way message, do not care about the response.
    if (oneWay && response == EMPTY)
      return;

    if (m_rph == null)
      m_rph = response.header.create_reply_header();

    BufferredCdrInput input = response.getStream();
    input.setOrb(orb);

    m_rph.read(input);

    // The stream must be aligned sinve v1.2, but only once.
    boolean align = response.header.version.since_inclusive(1, 2);

    switch (m_rph.reply_status)
      {
        case ReplyHeader.NO_EXCEPTION:

          NamedValue arg;

          // Read return value, if set.
          if (m_result != null)
            {
              if (align)
                {
                  input.align(8);
                  align = false;
                }
              m_result.value().read_value(input, m_result.value().type());
            }

          // Read returned parameters, if set.
          if (m_args != null)
            for (int i = 0; i < m_args.count(); i++)
              {
                try
                  {
                    arg = m_args.item(i);

                    // Both ARG_INOUT and ARG_OUT have this binary flag set.
                    if ((arg.flags() & ARG_OUT.value) != 0)
                      {
                        if (align)
                          {
                            input.align(8);
                            align = false;
                          }

                        arg.value().read_value(input, arg.value().type());
                      }
                  }
                catch (Bounds ex)
                  {
                    Unexpected.error(ex);
                  }
              }

          if (m_interceptor != null)
            m_interceptor.receive_reply(m_info);

          break;

        case ReplyHeader.SYSTEM_EXCEPTION:
          if (align)
            {
              input.align(8);
              align = false;
            }
          readExceptionId(input);

          m_sys_ex = ObjectCreator.readSystemException(input,
            m_rph.service_context);
          m_environment.exception(m_sys_ex);

          if (m_interceptor != null)
            m_interceptor.receive_exception(m_info);

          throw m_sys_ex;

        case ReplyHeader.USER_EXCEPTION:
          if (align)
            {
              input.align(8);
              align = false;
            }
          readExceptionId(input);

          // Prepare an Any that will hold the exception.
          gnuAny exc = new gnuAny();
          exc.setOrb(orb);

          exc.insert_Streamable(new StreamHolder(input));

          UnknownUserException unuex = new UnknownUserException(exc);
          m_environment.exception(unuex);

          if (m_interceptor != null)
            m_interceptor.receive_exception(m_info);

          break;

        case ReplyHeader.LOCATION_FORWARD_PERM:
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
              new MARSHAL("Cant read forwarding info", 5103,
                CompletionStatus.COMPLETED_NO);
            }

          setIor(forwarded);

          m_forward_ior = forwarded;

          if (m_interceptor != null)
            m_interceptor.receive_other(m_info);

          // Repeat with the forwarded information.
          p_invoke();
          return;

        default:
          throw new MARSHAL("Unknow reply status", 8100 + m_rph.reply_status,
            CompletionStatus.COMPLETED_NO);
      }
  }

  /**
   * Read exception id without changing the stream pointer position.
   */
  void readExceptionId(BufferredCdrInput input)
  {
    input.mark(2048);
    m_exception_id = input.read_string();
    input.reset();
  }

  /**
   * Write the operation parameters.
   *
   * @param header the message header
   * @param request_part the stream to write parameters into
   *
   * @throws MARSHAL if the attempt to write the parameters has failde.
   */
  protected void write_parameter_buffer(MessageHeader header,
    BufferedCdrOutput request_part
  ) throws MARSHAL
  {
    try
      {
        if (header.version.since_inclusive(1, 2))
          {
            request_part.align(8);
          }
        m_parameter_buffer.buffer.writeTo(request_part);
      }
    catch (IOException ex)
      {
        MARSHAL m = new MARSHAL("Unable to write method arguments to CDR output.");
        m.minor = Minor.CDR;
        throw m;
      }
  }

  /**
   * Write the operation parameters.
   *
   * @param header the message header
   * @param request_part the stream to write parameters into
   *
   * @throws MARSHAL if the attempt to write the parameters has failde.
   */
  protected void write_parameters(MessageHeader header,
    BufferedCdrOutput request_part
  ) throws MARSHAL
  {
    // Align after 1.2, but only once.
    boolean align = header.version.since_inclusive(1, 2);
    NamedValue para;

    try
      {
        // Write parameters now.
        for (int i = 0; i < m_args.count(); i++)
          {
            para = m_args.item(i);

            // This bit is set both for ARG_IN and ARG_INOUT
            if ((para.flags() & ARG_IN.value) != 0)
              {
                if (align)
                  {
                    request_part.align(8);
                    align = false;
                  }
                para.value().write_value(request_part);
              }
          }
      }
    catch (Bounds ex)
      {
        InternalError ierr = new InternalError();
        ierr.initCause(ex);
        throw ierr;
      }
  }

  /* **************Implementation of the request info operations. ***** */

  /**
   * Add context to request.
   */
  public void add_request_service_context(ServiceContext service_context,
    boolean replace
  )
  {
    m_rqh.addContext(service_context, replace);
  }

  /**
   * Get the Internet profile as an effective profile.
   */
  public TaggedProfile effective_profile()
  {
    BufferedCdrOutput buf = new BufferedCdrOutput(512);
    buf.setOrb(orb);
    ior.Internet.write(buf);

    TaggedProfile p = new TaggedProfile();
    p.tag = TAG_INTERNET_IOP.value;
    p.profile_data = buf.buffer.toByteArray();
    return p;
  }

  /**
   * Return either target or forwarded targed.
   */
  public org.omg.CORBA.Object effective_target()
  {
    return new IorObject(orb, ior);
  }

  /**
   * Get effective component with the give id from the Internet profile.
   */
  public TaggedComponent get_effective_component(int id)
    throws BAD_PARAM
  {
    if (id == TAG_CODE_SETS.value)
      {
        // Codesets are encoded separately.
        BufferedCdrOutput buf = new BufferedCdrOutput(512);
        buf.setOrb(orb);
        ior.Internet.CodeSets.write(buf);

        TaggedComponent t = new TaggedComponent();
        t.tag = TAG_CODE_SETS.value;
        t.component_data = buf.buffer.toByteArray();
        return t;
      }
    else
      {
        for (int i = 0; i < ior.Internet.components.size(); i++)
          {
            TaggedComponent c =
              (TaggedComponent) ior.Internet.components.get(i);
            if (c.tag == id)
              return c;
          }
      }
    throw new BAD_PARAM("No component " + id + " in the Internet profile", 28,
      CompletionStatus.COMPLETED_MAYBE
    );
  }

  /**
   * Get all components with the given id from the internet profile.
   */
  public TaggedComponent[] get_effective_components(int id)
    throws BAD_PARAM
  {
    if (id == TAG_CODE_SETS.value)
      return new TaggedComponent[] { get_effective_component(TAG_CODE_SETS.value) };
    else
      {
        ArrayList components = new ArrayList(ior.Internet.components.size());
        for (int i = 0; i < ior.Internet.components.size(); i++)
          {
            TaggedComponent c =
              (TaggedComponent) ior.Internet.components.get(i);
            if (c.tag == id)
              components.add(c);
          }
        if (components.size() == 0)
          throw new BAD_PARAM("No component " + id +
            " in the Internet profile", 28, CompletionStatus.COMPLETED_MAYBE
          );
        else
          {
            TaggedComponent[] t = new TaggedComponent[ components.size() ];
            for (int i = 0; i < t.length; i++)
              t [ i ] = (TaggedComponent) components.get(i);
            return t;
          }
      }
  }

  /**
   * This should be not implemented up till jdk 1.5 inclusive.
   */
  public Policy get_request_policy(int type) throws INV_POLICY
  {
    throw new NO_IMPLEMENT();
  }

  /** @inheritDoc */
  public String received_exception_id()
  {
    return m_exception_id;
  }

  /** @inheritDoc */
  public Any received_exception()
  {
    if (m_exception_id == null)
      return null;

    if (m_sys_ex != null)
      {
        Any a = orb.create_any();
        ObjectCreator.insertSysException(a, m_sys_ex);
        return a;
      }

    Exception mex = m_environment.exception();

    UnknownUserException ex = (UnknownUserException) mex;
    if (ex == null)
      return null;
    else
      return ex.except;
  }

  /**
   * Return the forwarded reference, null if none.
   */
  public org.omg.CORBA.Object forward_reference()
  {
    if (m_forwarding_target != null)
      return m_forwarding_target;

    if (m_forward_ior != null)
      return new IorObject(orb, m_forward_ior);
    else
      return null;
  }

  /**
   * Get the slot from the slot array inside this request.
   */
  public Any get_slot(int id) throws InvalidSlot
  {
    try
      {
        return m_slots [ id ];
      }
    catch (Exception e)
      {
        throw new InvalidSlot("slot id " + id + ":" + e);
      }
  }

  /**
   * Get the reply status.
   */
  public short reply_status()
  {
    if (m_rph == null)
      throw new BAD_INV_ORDER("Request not yet sent", 14,
        CompletionStatus.COMPLETED_NO
      );
    return (short) m_rph.reply_status;
  }

  /**
   * Get the request id.
   */
  public int request_id()
  {
    return m_rqh.request_id;
  }

  /**
   * Return true if the response is expected.
   */
  public boolean response_expected()
  {
    return !oneWay;
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
    return org.omg.Messaging.SYNC_WITH_TRANSPORT.value;
  }

  /** @inheritDoc */
  public ServiceContext get_request_service_context(int ctx_name)
    throws BAD_PARAM
  {
    return gnu.CORBA.GIOP.ServiceContext.findContext(ctx_name,
      m_rqh.service_context
    );
  }

  /** @inheritDoc */
  public ServiceContext get_reply_service_context(int ctx_name)
    throws BAD_PARAM
  {
    if (m_rph == null)
      throw new BAD_INV_ORDER("Reply context not yet available");
    return gnu.CORBA.GIOP.ServiceContext.findContext(ctx_name,
      m_rph.service_context
    );
  }

  /** @inheritDoc */
  public String[] operation_context()
  {
    return ice_contexts();
  }

  /**
   * Get contexts as required by interceptor.
   */
  public String[] ice_contexts()
  {
    if (m_context_list == null)
      return new String[ 0 ];
    else
      {
        try
          {
            String[] cn = new String[ m_context_list.count() ];
            for (int i = 0; i < cn.length; i++)
              cn [ i ] = m_context_list.item(i);
            return cn;
          }
        catch (Bounds e)
          {
            throw new Unexpected(e);
          }
      }
  }

  /**
   * Check if the call is done via DII.
   */
  public void checkDii()
  {
    if (m_parameter_buffer != null)
      throw new NO_RESOURCES("The invocation method provides " +
        "no access to this resource. DII call required.", 1,
        CompletionStatus.COMPLETED_MAYBE
      );
  }
}