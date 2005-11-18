/* _GameManagerImpl_Tie.java --
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


package gnu.classpath.examples.CORBA.swing.x5;

import java.rmi.Remote;

import javax.rmi.PortableRemoteObject;
import javax.rmi.CORBA.Tie;

import org.omg.CORBA.BAD_OPERATION;
import org.omg.CORBA.ORB;
import org.omg.CORBA.SystemException;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.OutputStream;
import org.omg.CORBA.portable.ResponseHandler;
import org.omg.CORBA.portable.UnknownException;
import org.omg.PortableServer.Servant;

/**
 * Normally generated with rmic compiler, this class represents the GameManager
 * Tie on the client side. The Game Manager methods contain the code for remote
 * invocation.
 * 
 * This class is normally generated with rmic from the {@link GameManagerImpl}:
 * 
 * <pre>
 *   rmic -iiop -poa -keep gnu.classpath.examples.CORBA.swing.x5.GameManagerImpl
 * </pre>
 * 
 * (the compiled package must be present in the current folder).
 * 
 * In this example the class was manually edited and commented for better
 * understanding of functionality.
 * 
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org) 
 */
public class _GameManagerImpl_Tie
  extends Servant
  implements Tie
{
  /**
   * The target, where remote invocations are forwarded.
   */
  private GameManagerImpl target = null;

  /**
   * The GameManager repository Id.
   */
  private static final String[] _type_ids = 
  { "RMI:gnu.classpath.examples.CORBA.swing.x5.GameManager:0000000000000000" };

  /**
   * Set the target where the remote invocations are forwarded.
   */
  public void setTarget(Remote a_target)
  {
    this.target = (GameManagerImpl) a_target;
  }

  /**
   * Get the target where the remote invocations are forwarded.
   */
  public Remote getTarget()
  {
    return target;
  }

  /**
   * Get the CORBA object for that this Tie is currently serving the request.
   * The same tie may serve multiple requests for the different objects in
   * parallel threads.
   */
  public org.omg.CORBA.Object thisObject()
  {
    return _this_object();
  }

  /**
   * Deactivate this object.
   */
  public void deactivate()
  {
    try
      {
        _poa().deactivate_object(_poa().servant_to_id(this));
      }
    catch (org.omg.PortableServer.POAPackage.WrongPolicy exception)
      {
      }
    catch (org.omg.PortableServer.POAPackage.ObjectNotActive exception)
      {
      }
    catch (org.omg.PortableServer.POAPackage.ServantNotActive exception)
      {
      }
  }

  /**
   * Get the ORB for this tie.
   */
  public ORB orb()
  {
    return _orb();
  }

  /**
   * Set the ORB for this tie.
   */
  public void orb(ORB orb)
  {
    try
      {
        ((org.omg.CORBA_2_3.ORB) orb).set_delegate(this);
      }
    catch (ClassCastException e)
      {
        throw new org.omg.CORBA.BAD_PARAM(
          "POA Servant requires an instance of org.omg.CORBA_2_3.ORB");
      }
  }

  /**
   * Return all interfaces, supported by this method.
   */
  public String[] _all_interfaces(org.omg.PortableServer.POA poa,
    byte[] objectId)
  {
    return _type_ids;
  }

  /**
   * This method is invoked by CORBA system to handle the remote invocation.
   * 
   * @param method the name of the method being invoked.
   * @param _in the stream to read the method parameters.
   * @param reply the responsed handler that can create the output stream to
   * write the parameters being returned.
   */
  public OutputStream _invoke(String method, InputStream _in,
    ResponseHandler reply)
    throws SystemException
  {
    try
      {
        org.omg.CORBA_2_3.portable.InputStream in = 
          (org.omg.CORBA_2_3.portable.InputStream) _in;
        if (method.equals("requestTheGame"))
          {
            Player p = (Player) PortableRemoteObject.narrow(
              in.read_Object(), Player.class);
            target.requestTheGame(p);

            OutputStream out = reply.createReply();
            return out;
          }
        else if (method.equals("unregister"))
          {
            Player p = (Player) PortableRemoteObject.narrow(
              in.read_Object(), Player.class);
            target.unregister(p);

            OutputStream out = reply.createReply();
            return out;
          }
        else
          throw new BAD_OPERATION();
      }
    catch (SystemException ex)
      {
        throw ex;
      }
    catch (Throwable ex)
      {
        ex.printStackTrace();
        throw new UnknownException(ex);
      }
  }
}