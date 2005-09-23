/* LocalDelegate.java --
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

import gnu.CORBA.CDR.cdrOutput;
import gnu.CORBA.streamRequest;

import org.omg.CORBA.ARG_INOUT;
import org.omg.CORBA.Bounds;
import org.omg.CORBA.Context;
import org.omg.CORBA.ContextList;
import org.omg.CORBA.ExceptionList;
import org.omg.CORBA.NO_IMPLEMENT;
import org.omg.CORBA.NVList;
import org.omg.CORBA.NamedValue;
import org.omg.CORBA.OBJECT_NOT_EXIST;
import org.omg.CORBA.ORB;
import org.omg.CORBA.Request;
import org.omg.CORBA.TypeCodePackage.BadKind;
import org.omg.CORBA.UnknownUserException;
import org.omg.CORBA.portable.ApplicationException;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.InvokeHandler;
import org.omg.CORBA.portable.ObjectImpl;
import org.omg.CORBA.portable.OutputStream;
import org.omg.CORBA.portable.RemarshalException;
import org.omg.PortableServer.ServantLocatorPackage.CookieHolder;

import java.util.Arrays;

/**
 * A local delegate, transferring all object requests to the locally available
 * servant. This class is involved in handling the method invocations on the
 * local object, obtained by POA.create_reference_with_id.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class LocalDelegate extends org.omg.CORBA_2_3.portable.Delegate
{
  /**
   * The same servant as an invocation handler.
   */
  gnuServantObject object;
  String operation;
  final gnuPOA poa;
  final byte[] Id;

  /**
   * Create a local delegate, forwarding requests to the servant that must also
   * be an invocation handler.
   */
  public LocalDelegate(gnuServantObject an_object, gnuPOA a_poa, byte[] an_id)
  {
    object = an_object;
    poa = a_poa;
    Id = an_id;
  }

  public Request request(org.omg.CORBA.Object target, String method)
  {
    operation = method;

    LocalRequest rq = new LocalRequest(object, poa, Id);
    rq.setOperation(method);
    rq.setORB(orb(target));
    return rq;
  }

  public void release(org.omg.CORBA.Object target)
  {
  }

  public boolean is_equivalent(org.omg.CORBA.Object target,
    org.omg.CORBA.Object other
  )
  {
    if (target == other)
      return true;
    else if (target instanceof ObjectImpl && other instanceof ObjectImpl)
      {
        org.omg.CORBA.portable.Delegate a = null;
        org.omg.CORBA.portable.Delegate b = null;
        try
          {
            a = ((ObjectImpl) target)._get_delegate();
            b = ((ObjectImpl) other)._get_delegate();
          }
        catch (Exception ex)
          {
            // Unable to get one of the delegates.
            return false;
          }
        if (a instanceof LocalDelegate && b instanceof LocalDelegate)
          {
            byte[] k1 = ((LocalDelegate) a).Id;
            byte[] k2 = ((LocalDelegate) b).Id;
            return Arrays.equals(k1, k2);
          }
        else
          return false;
      }
    else
      return false;
  }

  /**
   * Always return false.
   */
  public boolean non_existent(org.omg.CORBA.Object target)
  {
    return false;
  }

  /**
   * Get hash code.
   */
  public int hash(org.omg.CORBA.Object target, int maximum)
  {
    return hashCode() % maximum;
  }

  /**
   * Check if this object could be named by the given repository id.
   *
   * @param idl_id the repository id to check.
   *
   * @return true if it is one of the possible repository ids of this object.
   */
  public boolean is_a(org.omg.CORBA.Object a_servant, String idl_id)
  {
    String[] maybe = object._ids();
    for (int i = 0; i < maybe.length; i++)
      {
        if (maybe [ i ].equals(idl_id))
          return true;
      }
    return false;
  }

  /**
   * Return <code>this</code>.
   */
  public org.omg.CORBA.Object duplicate(org.omg.CORBA.Object target)
  {
    return target;
  }

  /**
   * Create request for using with DII.
   */
  public Request create_request(org.omg.CORBA.Object target, Context context,
    String method, NVList parameters, NamedValue returns,
    ExceptionList exceptions, ContextList ctx_list
  )
  {
    operation = method;

    LocalRequest rq = new LocalRequest(object, poa, Id);
    rq.setOperation(method);
    rq.set_args(parameters);
    rq.set_result(returns);
    rq.set_exceptions(exceptions);
    rq.set_context_list(ctx_list);
    return rq;
  }

  /**
   * Create request for using with DII.
   */
  public Request create_request(org.omg.CORBA.Object target, Context context,
    String method, NVList parameters, NamedValue returns
  )
  {
    operation = method;

    LocalRequest rq = new LocalRequest(object, poa, Id);
    rq.setOperation(method);
    rq.set_args(parameters);
    rq.set_result(returns);
    return rq;
  }

  /**
   * Not in use.
   */
  public org.omg.CORBA.Object get_interface_def(org.omg.CORBA.Object target)
  {
    throw new NO_IMPLEMENT();
  }

  /**
   * Create a request to invoke the method of this CORBA object.
   *
   * @param operation the name of the method to invoke.
   * @param response_expected specifies if this is one way message or the
   * response to the message is expected.
   *
   * @return the stream where the method arguments should be written.
   */
  public org.omg.CORBA.portable.OutputStream request(
    org.omg.CORBA.Object target,
    String method,
    boolean response_expected
  )
  {
    operation = method;

    // Check if the object is not explicitly deactivated.
    activeObjectMap.Obj e = poa.aom.get(Id);
    if (e != null && e.isDeactiveted())
      {
        if (poa.servant_activator != null || poa.servant_locator != null)
          {
            // This will force the subsequent activation.
            object.setServant(null);
            e.setServant(null);
            e.setDeactivated(false);
          }
        else
          throw new OBJECT_NOT_EXIST("Deactivated");
      }

    LocalRequest rq = new LocalRequest(object, poa, Id);
    rq.setOperation(method);
    rq.setORB(orb(target));
    return rq.getParameterStream();
  }

  /**
   * Return the associated invocation handler.
   */
  public InvokeHandler getHandler(String method, CookieHolder cookie)
  {
    return object.getHandler(method, cookie, false);
  }

  /**
   * Return the ORB of the associated POA. The parameter is not in use.
   */
  public ORB orb(org.omg.CORBA.Object target)
  {
    return poa.orb();
  }

  /**
   * Make an invocation.
   *
   * @param target not in use.
   * @param output the stream request that should be returned by
   * {@link #m_request} in this method.
   * @throws ApplicationException if the use exception is thrown by the servant
   * method.
   */
  public InputStream invoke(org.omg.CORBA.Object target, OutputStream output)
    throws ApplicationException
  {
    try
      {
        streamRequest sr = (streamRequest) output;

        LocalRequest lr = (LocalRequest) sr.request;
        InvokeHandler handler =
          lr.object.getHandler(lr.operation(), lr.cookie, false);

        if (handler instanceof dynImpHandler)
          {
            // The local request known how to handle it, but the different
            // method must be called.
            lr.invoke();

            // The encapsulation will inherit orb, endian, charsets, etc.
            cdrOutput buf = sr.createEncapsulation();

            // Write all request parameters to the buffer stream.
            if (lr.env().exception() != null)
              {
                try
                  {
                    UnknownUserException uex =
                      (UnknownUserException) lr.env().exception();
                    throw new ApplicationException(uex.except.type().id(),
                      uex.except.create_input_stream()
                    );
                  }
                catch (BadKind ex)
                  {
                    InternalError ierr = new InternalError();
                    ierr.initCause(ex);
                    throw ierr;
                  }
              }
            if (lr.return_value() != null)
              lr.return_value().write_value(buf);

            NamedValue a;
            try
              {
                for (int i = 0; i < lr.arguments().count(); i++)
                  {
                    a = lr.arguments().item(i);
                    if (a.flags() == ARG_INOUT.value ||
                      a.flags() == ARG_INOUT.value
                    )
                      {
                        a.value().write_value(buf);
                      }
                  }
              }
            catch (Bounds ex)
              {
                InternalError ierr = new InternalError();
                ierr.initCause(ex);
                throw ierr;
              }

            return buf.create_input_stream();
          }
        else
          {
            LocalRequest lrq = (LocalRequest) sr.request;
            return lrq.s_invoke(handler);
          }
      }
    catch (gnuForwardRequest f)
      {
        try
          {
            return ((ObjectImpl) f.forward_reference)._invoke(f.forward_reference._request(
                operation,
                true
              )
            );
          }
        catch (RemarshalException e)
          {
            // Never thrown in this place by Classpath implementation.
            throw new NO_IMPLEMENT();
          }
      }
  }

  public void releaseReply(org.omg.CORBA.Object target, InputStream input)
  {
    release(target);
  }
}