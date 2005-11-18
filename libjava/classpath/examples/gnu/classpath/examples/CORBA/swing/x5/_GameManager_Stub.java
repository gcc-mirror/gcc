/* _GameManager_Stub.java --
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

import java.rmi.RemoteException;
import java.rmi.UnexpectedException;

import javax.rmi.CORBA.Stub;
import javax.rmi.CORBA.Util;

import org.omg.CORBA.SystemException;
import org.omg.CORBA.portable.ApplicationException;
import org.omg.CORBA.portable.OutputStream;
import org.omg.CORBA.portable.RemarshalException;
import org.omg.CORBA.portable.ServantObject;

/**
 * Normally generated with rmic compiler, this class represents the GameManager
 * Stub on the client side. The Game Manager methods contain the code for
 * remote invocation.
 * 
 * This class is normally generated with rmic from the {@link GameManagerImpl}:
 * <pre>
 * rmic -iiop -poa -keep gnu.classpath.examples.CORBA.swing.x5.GameManagerImpl
 * </pre>
 * (the compiled package must be present in the current folder).
 * 
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org) 
 */
public class _GameManager_Stub extends Stub implements GameManager
{
  /** 
   * Use serialVersionUID for interoperability. 
   */
  private static final long serialVersionUID = 1;
  
  private static final String[] _type_ids =
    { "RMI:gnu.classpath.examples.CORBA.swing.x5.GameManager:0000000000000000" };

  public String[] _ids()
  {
    return _type_ids;
  }
  
  /**
   * Notify the manager that the player is no longer willing to play and
   * should be removed from the queue.
   */
  public void unregister(Player p)
    throws RemoteException
  {
    if (!Util.isLocal(this))
      {
        try
          {
            org.omg.CORBA.portable.InputStream in = null;
            try
              {
                OutputStream out = _request("unregister", true);
                Util.writeRemoteObject(out, p);
                _invoke(out);
              }
            catch (ApplicationException ex)
              {
                in = ex.getInputStream();

                String id = in.read_string();
                throw new UnexpectedException(id);
              }
            catch (RemarshalException ex)
              {
                unregister(p);
              }
            finally
              {
                _releaseReply(in);
              }
          }
        catch (SystemException ex)
          {
            throw Util.mapSystemException(ex);
          }
      }
    else
      {
        ServantObject so =
          _servant_preinvoke("requestTheGame", GameManager.class);
        if (so == null)
          {
            unregister(p);
            return;
          }
        try
          {
            ((GameManager) so.servant).unregister(p);
          }
        catch (Throwable ex)
          {
            Throwable exCopy = (Throwable) Util.copyObject(ex, _orb());
            throw Util.wrapException(exCopy);
          }
        finally
          {
            _servant_postinvoke(so);
          }
      }
  }
  
  /**
   * The method that the user should invoke.
   */
  public void requestTheGame(Player arg0) throws RemoteException
  {
    if (!Util.isLocal(this))
      {
        try
          {
            org.omg.CORBA.portable.InputStream in = null;
            try
              {
                OutputStream out = _request("requestTheGame", true);
                Util.writeRemoteObject(out, arg0);
                _invoke(out);
              }
            catch (ApplicationException ex)
              {
                in = ex.getInputStream();

                String id = in.read_string();
                throw new UnexpectedException(id);
              }
            catch (RemarshalException ex)
              {
                requestTheGame(arg0);
              }
            finally
              {
                _releaseReply(in);
              }
          }
        catch (SystemException ex)
          {
            throw Util.mapSystemException(ex);
          }
      }
    else
      {
        ServantObject so =
          _servant_preinvoke("requestTheGame", GameManager.class);
        if (so == null)
          {
            requestTheGame(arg0);
            return;
          }
        try
          {
            Player arg0Copy = (Player) Util.copyObject(arg0, _orb());
            ((GameManager) so.servant).requestTheGame(arg0Copy);
          }
        catch (Throwable ex)
          {
            Throwable exCopy = (Throwable) Util.copyObject(ex, _orb());
            throw Util.wrapException(exCopy);
          }
        finally
          {
            _servant_postinvoke(so);
          }
      }
  }
}