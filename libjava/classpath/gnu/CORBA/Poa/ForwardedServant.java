/* ForwardedServant.java --
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

import gnu.CORBA.IOR;
import gnu.CORBA.IorDelegate;
import gnu.CORBA.IorObject;
import gnu.CORBA.Minor;

import org.omg.CORBA.BAD_PARAM;
import org.omg.CORBA.CompletionStatus;
import org.omg.CORBA.MARSHAL;
import org.omg.CORBA.ORB;
import org.omg.CORBA.SystemException;
import org.omg.CORBA.portable.ApplicationException;
import org.omg.CORBA.portable.Delegate;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.InvokeHandler;
import org.omg.CORBA.portable.ObjectImpl;
import org.omg.CORBA.portable.OutputStream;
import org.omg.CORBA.portable.RemarshalException;
import org.omg.CORBA.portable.ResponseHandler;
import org.omg.PortableServer.POA;
import org.omg.PortableServer.Servant;

import java.io.IOException;

/**
 * A "virtual servant", delegating all invocation to the wrapped
 * object (usually remote). Used in cases when it is necessary to
 * handle the request forwarding.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class ForwardedServant
  extends Servant
  implements InvokeHandler
{
  /**
   * The reference object, handling requests.
   */
  public final ObjectImpl ref;

  /**
   * Create an instance, forwarding requests to the given object.
   */
  ForwardedServant(ObjectImpl a_ref)
  {
    ref = a_ref;
  }

  /**
   * Create an instance of the forwarded servant.
   *
   * @param a_ref a reference where request should be forwarded.
   *
   * @return a created forwarded servant or null if the parameter
   * forwards request to itself. Returning null will force to find
   * a right servant in one of many possible ways, depending on
   * policies.
   */
  public static Servant create(org.omg.CORBA.Object a_ref)
  {
    try
      {
        ObjectImpl fto = (ObjectImpl) a_ref;

        // Check maybe the remote side forwarded back to our local object.
        if (fto instanceof IorObject)
          {
            IorObject iref = (IorObject) fto;

            // Check maybe the IOR is local.
            ORB t_orb = iref._orb();
            if (t_orb instanceof ORB_1_4)
              {
                ORB_1_4 orb = (ORB_1_4) t_orb;
                Delegate d = iref._get_delegate();
                if (d instanceof IorDelegate)
                  {
                    IorDelegate ird = (IorDelegate) iref._get_delegate();
                    IOR ior = ird.getIor();
                    if (orb.LOCAL_HOST.equalsIgnoreCase(ior.Internet.host))
                      {
                        AOM.Obj rx = orb.rootPOA.findIorKey(ior.key);
                        if (rx != null)
                          {
                            if (rx.object == fto ||
                                rx.object._is_equivalent(fto)
                               )
                              return rx.primary_servant;
                            else
                              fto = (ObjectImpl) rx.object;
                          }
                      }
                  }
              }
          }
        return new ForwardedServant(fto);
      }
    catch (ClassCastException ex)
      {
        throw new BAD_PARAM("ObjectImpl required but " + a_ref + " passed ",
                            0x5005, CompletionStatus.COMPLETED_NO
                           );
      }
  }

  /**
   * Forward the call to the wrapped object.
   */
  public OutputStream _invoke(String method, InputStream input,
                              ResponseHandler handler
                             )
                       throws SystemException
  {
    org.omg.CORBA.portable.InputStream in = null;
    org.omg.CORBA.portable.OutputStream out = null;
    try
      {
        try
          {
            out = ref._request(method, true);

            // Transfer request information.
            int b;
            while ((b = input.read()) >= 0)
              {
                out.write(b);
              }
            in = ref._invoke(out);

            // Read the returned data.
            out = handler.createReply();
            while ((b = in.read()) >= 0)
              {
                out.write(b);
              }
          }
        catch (IOException io_ex)
          {
            MARSHAL m = new MARSHAL();
            m.minor = Minor.Forwarding;
            m.initCause(io_ex);
            throw m;
          }
      }
    catch (ApplicationException ex)
      {
        in = ex.getInputStream();

        String _id = ex.getId();
        throw new MARSHAL(_id, 5101, CompletionStatus.COMPLETED_NO);
      }
    catch (RemarshalException remarsh)
      {
        _invoke(method, input, handler);
      }
    finally
      {
        ref._releaseReply(in);
      }
    return out;
  }

  /**
   * Delegates to the wrapped object.
   */
  public String[] _all_interfaces(POA poa, byte[] key)
  {
    return ref._ids();
  }
}
