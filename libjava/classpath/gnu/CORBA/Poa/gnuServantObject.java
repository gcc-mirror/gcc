/* gnuServantObject.java --
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

import gnu.CORBA.GIOP.ReplyHeader;
import gnu.CORBA.IorDelegate;
import gnu.CORBA.IorObject;
import gnu.CORBA.Interceptor.gnuServerRequestInfo;
import gnu.CORBA.typecodes.RecordTypeCode;
import gnu.CORBA.IOR;
import gnu.CORBA.IorProvider;
import gnu.CORBA.Minor;
import gnu.CORBA.ObjectCreator;
import gnu.CORBA.Unexpected;
import gnu.CORBA.ResponseHandlerImpl;
import gnu.CORBA.StreamHolder;

import org.omg.CORBA.Any;
import org.omg.CORBA.BAD_OPERATION;
import org.omg.CORBA.BAD_PARAM;
import org.omg.CORBA.CompletionStatus;
import org.omg.CORBA.OBJECT_NOT_EXIST;
import org.omg.CORBA.OBJ_ADAPTER;
import org.omg.CORBA.ORB;
import org.omg.CORBA.SystemException;
import org.omg.CORBA.TCKind;
import org.omg.CORBA.TRANSIENT;
import org.omg.CORBA.UserException;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.InvokeHandler;
import org.omg.CORBA.portable.ObjectImpl;
import org.omg.CORBA.portable.OutputStream;
import org.omg.CORBA.portable.ResponseHandler;
import org.omg.PortableInterceptor.ForwardRequest;
import org.omg.PortableInterceptor.ServerRequestInterceptorOperations;
import org.omg.PortableServer.CurrentOperations;
import org.omg.PortableServer.DynamicImplementation;
import org.omg.PortableServer.ImplicitActivationPolicyValue;
import org.omg.PortableServer.POA;
import org.omg.PortableServer.POAManager;
import org.omg.PortableServer.POAManagerPackage.State;
import org.omg.PortableServer.Servant;
import org.omg.PortableServer.ServantLocatorPackage.CookieHolder;
import org.omg.PortableServer.ServantRetentionPolicyValue;
import org.omg.PortableServer.portable.Delegate;

import java.io.IOException;

import java.util.Arrays;

/**
 * Represents a CORBA object, being locally served by the associated servant.
 * The calls to the object are forwarded to the calls to the servant.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class gnuServantObject extends ObjectImpl
  implements org.omg.CORBA.Object,
    InvokeHandler,
    CurrentOperations,
    IorProvider
{
  /**
   * The associated servant that must also implement the {@link InvokeHandler}
       * interface. This value can be temporary null if the object was created using
   * POA.create_reference or POA.create_reference_with_id, private to force
   * always to use {@link setServant}.
   */
  private Servant servant;

  /**
   * The Id of this object.
   */
  public final byte[] Id;

  /**
   * The poa that takes care about this object.
   */
  public final gnuPOA poa;

  /**
   * The POA manager, used to control the work of this object.
   */
  public final POAManager manager;

  /**
   * The orb.
   */
  public final ORB_1_4 orb;

  /**
   * The object repository ids, if they were specified separately. Normally, the
   * ids are requested from the servant.
   */
  public final String[] repository_ids;

  /**
   * Create an object with no connected servant. The servant must be set later.
   *
   * @param a_repository_ids an array of repository ids, can be null (then ids
   * will be requested from the servant).
   * @param an_id the object id.
   * @param a_poa the POA.
   */
  public gnuServantObject(String[] a_repository_ids, byte[] an_id,
    gnuPOA a_poa, ORB_1_4 an_orb
  )
  {
    repository_ids = a_repository_ids;
    Id = an_id;
    manager = a_poa.the_POAManager();
    poa = a_poa;
    orb = an_orb;
  }
  
  /**
   * Get the IOR as it would be for this object.
   */
  public IOR getIor()
  {
    return orb.getLocalIor(this);    
  }

  /**
   * Create a servant object, associated with the passed servant.
   *
   * @param a_servant a servant, serving this object.
   * @param an_id an Object Id for this object.
   *
   * @throws BAD_PARAM if the passed servant is not an {@link InvokeHandler}.
   */
  public gnuServantObject(Servant a_servant, byte[] an_id, ORB_1_4 an_orb,
    gnuPOA a_poa
  )
  {
    Id = an_id;
    setServant(a_servant);
    poa = a_poa;
    if (poa != null)
      {
        manager = poa.the_POAManager();
      }
    else
      {
        manager = null;
      }
    repository_ids = null;
    orb = an_orb;
  }

  /**
   * Set a servant, if it has not been previously set.
   *
   * @param a_servant a servant to set, can be null to indicate the necessity
   * for the subsequent activation.
   *
   * @throws BAD_PARAM if the passed servant is not an {@link InvokeHandler} or
   * {@link DynamicImplementation} and also not null.
   */
  public void setServant(Servant a_servant)
  {
    if (a_servant != null &&
      !(a_servant instanceof InvokeHandler) &&
      !(a_servant instanceof DynamicImplementation)
    )
      {
        throw new BAD_PARAM("Must be either InvokeHandler or " +
          "DynamicImplementation, but is " + a_servant
        );
      }
    servant = a_servant;
  }

  /**
   * Returns the associated servant.
   */
  public Servant getServant()
  {
    return servant;
  }

  /**
   * Return the associated invocation handler.
   */
  public InvokeHandler getHandler(String operation, CookieHolder cookie,
    boolean forwarding_allowed
  ) throws gnuForwardRequest
  {
    if (servant != null)
      {
        return servantToHandler(servant);
      }
    else
      {
        // Use servant locator to locate the servant.
        if (poa.servant_locator != null)
          {
            try
              {
                servant =
                  poa.servant_locator.preinvoke(Id, poa, operation, cookie);
                return servantToHandler(servant);
              }
            catch (org.omg.PortableServer.ForwardRequest forw_ex)
              {
                if (forwarding_allowed)
                  {
                    throw new gnuForwardRequest(forw_ex.forward_reference);
                  }
                else
                  {
                    servant =
                      ForwardedServant.create(forw_ex.forward_reference);
                    return servantToHandler(servant);
                  }
              }
          }
        else
        // Use servant activator to locate the servant.
        if (poa.applies(ImplicitActivationPolicyValue.IMPLICIT_ACTIVATION) &&
          poa.applies(ServantRetentionPolicyValue.RETAIN)
        )
          {
            try
              {
                poa.activate_object_with_id(Id, servant, forwarding_allowed);
                servant = poa.id_to_servant(Id);
                return servantToHandler(servant);
              }
            catch (gnuForwardRequest forwarded)
              {
                throw forwarded;
              }
            catch (Exception ex)
              {
                BAD_OPERATION bad =
                  new BAD_OPERATION("Unable to activate", Minor.Activation,
                    CompletionStatus.COMPLETED_NO
                  );
                bad.initCause(ex);
                throw bad;
              }
          }
        else if (poa.default_servant != null)
          {
            servant = poa.default_servant;
            return servantToHandler(servant);
          }

        // No servant and no servant manager - throw exception.
        else
          {
            throw new BAD_OPERATION("Unable to activate", Minor.Activation,
              CompletionStatus.COMPLETED_NO
            );
          }
      }
  }

  /**
   * Convert the servant to invocation handler.
   */
  public InvokeHandler servantToHandler(Servant a_servant)
  {
    if (a_servant instanceof InvokeHandler)
      {
        return (InvokeHandler) a_servant;
      }
    else if (a_servant instanceof DynamicImplementation)
      {
        return new DynamicImpHandler((DynamicImplementation) a_servant);
      }
    else
      {
        throw new BAD_OPERATION(a_servant +
          " must be either InvokeHandler or " + "POA DynamicImplementation"
        );
      }
  }

  /**
   * Create a servant object, associated with the passed servant. Requests the
   * object id from the servant. Depending on the policies of the servants POA,
   * the calls are eithe not synchronized or synchronized on POA or ORB.
   *
   * @param a_servant a servant, serving this object.
   * @param an_id an Object Id for this object.
   */
  public gnuServantObject(Servant a_servant, gnuPOA a_poa)
  {
    this(a_servant, a_servant._object_id(), (ORB_1_4) a_servant._orb(), a_poa);
  }

  /**
   * Delegates call to servant, passing the poa and Id.
   */
  public String[] _ids()
  {
    if (repository_ids == null)
      {
        return getServant()._all_interfaces(poa, Id);
      }
    else
      {
        return repository_ids;
      }
  }

  /**
   * Gets a string representation.
   */
  public String toString()
  {
    StringBuffer b = new StringBuffer("Servant object (");
    for (int i = 0; i < Id.length; i++)
      {
        b.append(Integer.toHexString(Id [ i ] & 0xFF));
        b.append(' ');
      }
    b.append(')');
    return b.toString();
  }

  /**
   * Always returns true.
   */
  public boolean _is_local()
  {
    return true;
  }

  /**
   * Check if this object could be named by the given repository id.
   *
   * @param idl_id the repository id to check.
   *
   * @return true if it is one of the possible repository ids of this object.
   */
  public boolean _is_a(String idl_id)
  {
    String[] maybe = _ids();
    for (int i = 0; i < maybe.length; i++)
      {
        if (maybe [ i ].equals(idl_id))
          {
            return true;
          }
      }
    return false;
  }

  /**
   * Get an ORB, associated with the servant of this object.
   *
   * @return
   */
  public ORB _orb()
  {
    return getServant()._orb();
  }

  /**
   * Handle the invocation (delegates to servant).
   *
   * @throws TRANSIENT minor 0x535503e9 if the POA is in discarding mode.
   * @throws OBJ_ADAPTER minor 0x535503ea if the POA is inactivated.
   * @throws OBJECT_NOT_EXISTS minor 0x535503ec if this object is inactivated.
   *
   * @specnote see {@link POAManagerOperations} for specnotes about the minor
   * codes.
   */
  public OutputStream _invoke(String method, InputStream input,
    ResponseHandler r_handler
  ) throws SystemException
  {
    boolean intercept = false;
    ServerRequestInterceptorOperations interceptor = null;
    gnuServerRequestInfo info = null;
    ResponseHandlerImpl i_handler = null;

    try
      {
        if (orb.iServer != null &&
          r_handler instanceof ResponseHandlerImpl
        )
          {
            interceptor = orb.iServer;

            i_handler = (ResponseHandlerImpl) r_handler;

            info =
              new gnuServerRequestInfo(this, i_handler.request_header,
                i_handler.reply_header
              );
            intercept = true;

            interceptor.receive_request_service_contexts(info);
          }

        try
          {
            CookieHolder cookie = null;
            AOM.Obj self = poa.aom.get(Id);

            if (poa.servant_locator != null)
              {
                // If the servant locator is in use, it is always responsible
                // for providing the servant.
                self.servant = servant = null;
                cookie = new CookieHolder();
              }
            else if (self != null && self.isDeactiveted())
              {
                if (poa.applies(
                    ImplicitActivationPolicyValue.IMPLICIT_ACTIVATION
                  ) &&
                  poa.servant_activator != null
                )
                  {
                    // Reset the servant, forcing the subsequent activation.
                    servant = null;
                  }
                else
                  {
                    throw new OBJECT_NOT_EXIST("Object deactivated",
                      0x535503ec, CompletionStatus.COMPLETED_NO
                    );
                  }
              }

            InvokeHandler handler = getHandler(method, cookie, true);

            Delegate d = null;

            try
              {
                d = servant._get_delegate();
                orb.currents.put(Thread.currentThread(), this);
              }
            catch (Exception ex)
              {
                // In some cases exception is thrown if the delegate is not set.
              }
            if (d instanceof ServantDelegateImpl)
              {
                // If the delegate is already set, check maybe we can
                // reuse the existing instance.
                if (((ServantDelegateImpl) d).object != this)
                  {
                    servant._set_delegate(new ServantDelegateImpl(servant, poa, Id));
                  }
              }
            else
              {
                servant._set_delegate(new ServantDelegateImpl(servant, poa, Id));
              }

            try
              {
                switch (manager.get_state().value())
                  {
                    case State._ACTIVE :

                      OutputStream rt;
                      try
                        {
                          if (intercept)
                            {
                              interceptor.receive_request(info);
                            }

                          rt = handler._invoke(method, input, r_handler);

                          if (intercept)
                            {
                              // Handler is casted into i_handler.
                              if (i_handler.isExceptionReply())
                                {
                                  info.m_reply_header.reply_status =
                                    ReplyHeader.USER_EXCEPTION;

                                  // Make Any, holding the user exception.
                                  Any a = orb.create_any();
                                  OutputStream buf = i_handler.getBuffer();
                                  InputStream in = buf.create_input_stream();
                                  String uex_idl = "unknown";
                                  try
                                    {
                                      in.mark(Integer.MAX_VALUE);
                                      uex_idl = in.read_string();
                                      in.reset();
                                    }
                                  catch (IOException e)
                                    {
                                      throw new Unexpected(e);
                                    }

                                  try
                                    {
                                      UserException exception =
                                        ObjectCreator.readUserException(uex_idl,
                                          in
                                        );

                                      ObjectCreator.insertWithHelper(a,
                                        exception
                                      );
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
                                      r.setName(ObjectCreator.getDefaultName(
                                          uex_idl
                                        )
                                      );
                                    }

                                  info.m_usr_exception = a;
                                  interceptor.send_exception(info);
                                }
                              else
                                {
                                  info.m_reply_header.reply_status =
                                    ReplyHeader.NO_EXCEPTION;
                                  interceptor.send_reply(info);
                                }
                            }
                        }
                      catch (SystemException sys_ex)
                        {
                          if (intercept)
                            {
                              info.m_reply_header.reply_status =
                                ReplyHeader.SYSTEM_EXCEPTION;
                              info.m_sys_exception = sys_ex;
                              interceptor.send_exception(info);
                            }
                          throw sys_ex;
                        }

                      return rt;

                    case State._HOLDING :

                      // The holding mode is implemented
                      // relying on the holding capabilites of the network
                      // support (if any).
                      // TODO FIXME in more recent CORBA applications, the
                      // client
                      // ORB can free the connection and wait for a server side
                      // notification about the completed request. Implement
                      // this
                      // as soon as JDK specification would allow bidirectional
                      // policy.
                      int sleep = 5;
                      int max = 500;

                      // Wait till the state will be switched into some other
                      // mode.
                      while (manager.get_state().value() == State._HOLDING)
                        {
                          try
                            {
                              Thread.sleep(sleep);
                              if (sleep < max)
                                {
                                  sleep = max;
                                }
                            }
                          catch (InterruptedException ex)
                            {
                            }
                        }

                      // Handle another mode.
                      return _invoke(method, input, r_handler);

                    case State._DISCARDING :
                      throw new TRANSIENT("Discarding mode", 0x535503e9,
                        CompletionStatus.COMPLETED_NO
                      );

                    case State._INACTIVE :
                      throw new OBJ_ADAPTER("POA deactivated", 0x535503ea,
                        CompletionStatus.COMPLETED_NO
                      );

                    default :
                      throw new InternalError(); // No more states.
                  }
              }
            finally
              {
                if (poa.servant_locator != null)
                  {
                    poa.servant_locator.postinvoke(Id, poa, method,
                      cookie.value, servant
                    );
                    servant = null;
                  }
              }
          }
        finally
          {
            orb.currents.remove(Thread.currentThread());
          }
      }
    catch (ForwardRequest fex)
      {
        // May be thrown by interceptor.
        if (intercept)
          {
            Forwarding:
            while (true)
              {
                info.m_reply_header.reply_status =
                  ReplyHeader.LOCATION_FORWARD;
                info.m_forward_reference = fex.forward;
                try
                  {
                    interceptor.send_other(info);
                    break Forwarding;
                  }
                catch (ForwardRequest fex2)
                  {
                    info.m_forward_reference = fex2.forward;
                    fex.forward = info.m_forward_reference;
                  }
              }
          }
        throw new gnuForwardRequest(fex.forward);
      }
    catch (gnuForwardRequest fex)
      {
        // May be thrown during activation.
        if (intercept)
          {
            Forwarding:
            while (true)
              {
                info.m_reply_header.reply_status =
                  ReplyHeader.LOCATION_FORWARD;
                info.m_forward_reference = fex.forward_reference;
                try
                  {
                    interceptor.send_other(info);
                    break Forwarding;
                  }
                catch (ForwardRequest fex2)
                  {
                    info.m_forward_reference = fex2.forward;
                    fex.forward_reference = (ObjectImpl) fex2.forward;
                  }
              }
          }
        throw fex;
      }
  }

  /**
   * Compare with another object for equality, comparing the object keys.
   */
  public boolean equals(java.lang.Object other)
  {
    if (other instanceof gnuServantObject)
      {
        gnuServantObject o = (gnuServantObject) other;

        return Arrays.equals(o.Id, Id);
      }
    else
      {
        return false;
      }
  }

  /**
   * Get the hash code, based on the object key.
   */
  public int hashCode()
  {
    long s = 0;
    int v = 1;
    for (int i = 0; i < Id.length; i++)
      {
        s += Id [ i ] * v;
        if (s > Integer.MAX_VALUE)
          {
            s = s % Integer.MAX_VALUE;
            v = 1;
          }
        v = v * 8;
      }
    return (int) (s % Integer.MAX_VALUE);
  }

  /**
   * Get the object id.
   */
  public byte[] get_object_id()
  {
    return Id;
  }

  /**
   * Get POA.
   */
  public POA get_POA()
  {
    return poa;
  }

  /**
   * Returns without action.
   */
  public void _release()
  {
  }

  /**
   * Returns without action.
   */
  public void _releaseReply(InputStream stream)
  {
  }

  /**
   * Checks if this object is equivalent to another instance. These objects are
   * assumed equal if they are connected to the same orb and poa under the same
   * Id, regardless of they delegates.
   *
   * @param another instance to check.
   * @return
   */
  public boolean _is_equivalent(org.omg.CORBA.Object other)
  {
    if (other instanceof gnuServantObject)
      {
        gnuServantObject g = (gnuServantObject) other;
        return orb == g.orb && poa == g.poa && Arrays.equals(Id, g.Id);
      }
    else if (other instanceof IorObject)
      {
        IorObject ir = ((IorObject) other);
        try
          {
            IorDelegate ird = (IorDelegate) ir._get_delegate();
            byte[] ior_id = poa.idFormIor(ird.getIor().key);
            if (ior_id != null && Arrays.equals(ior_id, Id))
              {
                return true;
              }
            else
              {
                return false;
              }
          }
        catch (Exception ex)
          {
            // Non - typical delegate or very specific subclass of
            // IOR_constructed_object.
            return super._is_equivalent(other);
          }
      }
    return super._is_equivalent(other);
  }
}