/* _Player_Stub.java --
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

import java.awt.Point;
import java.io.Serializable;
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
 * Generate with rmic, command line
 * rmic -iiop -poa -keep gnu.classpath.examples.CORBA.swing.x5.PlayerImpl
 * (the compiled package must be present in the current folder).
 *
 * This class is normally generated with rmic from the {@link GameManagerImpl}:
 * <pre>
 * rmic -iiop -poa -keep gnu.classpath.examples.CORBA.swing.x5.GameManagerImpl
 * </pre>
 * (the compiled package must be present in the current folder).
 *
 * In this example the class was manually edited and commented for better
 * understanding of functionality.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class _Player_Stub extends Stub implements Player
{
  /**
   * Use serialVersionUID for interoperability.
   */
  private static final long serialVersionUID = 1;

  private static final String[] _type_ids =
    { "RMI:gnu.classpath.examples.CORBA.swing.x5.Player:0000000000000000" };

  public String[] _ids()
  {
    return _type_ids;
  }

  public boolean start_game(Player arg0, boolean arg1)
    throws RemoteException
  {
    if (!Util.isLocal(this))
      {
        try
          {
            org.omg.CORBA.portable.InputStream in = null;
            try
              {
                OutputStream out = _request("start_game", true);
                Util.writeRemoteObject(out, arg0);
                out.write_boolean(arg1);
                in = _invoke(out);
                return in.read_boolean();
              }
            catch (ApplicationException ex)
              {
                in = ex.getInputStream();

                String id = in.read_string();
                throw new UnexpectedException(id);
              }
            catch (RemarshalException ex)
              {
                return start_game(arg0, arg1);
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
        ServantObject so = _servant_preinvoke("start_game", Player.class);
        if (so == null)
          {
            return start_game(arg0, arg1);
          }
        try
          {
            Player arg0Copy = (Player) Util.copyObject(arg0, _orb());
            return ((Player) so.servant).start_game(arg0Copy, arg1);
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

  public int get_current_state() throws RemoteException
  {
    if (!Util.isLocal(this))
      {
        try
          {
            org.omg.CORBA.portable.InputStream in = null;
            try
              {
                OutputStream out = _request("_get_J_current_state", true);
                in = _invoke(out);
                return in.read_long();
              }
            catch (ApplicationException ex)
              {
                in = ex.getInputStream();

                String id = in.read_string();
                throw new UnexpectedException(id);
              }
            catch (RemarshalException ex)
              {
                return get_current_state();
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
          _servant_preinvoke("_get_J_current_state", Player.class);
        if (so == null)
          {
            return get_current_state();
          }
        try
          {
            return ((Player) so.servant).get_current_state();
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

  public void receive_chat(byte arg0, String arg1) throws RemoteException
  {
    if (!Util.isLocal(this))
      {
        try
          {
            org.omg.CORBA_2_3.portable.InputStream in = null;
            try
              {
                org.omg.CORBA_2_3.portable.OutputStream out =
                  (org.omg.CORBA_2_3.portable.OutputStream) _request("receive_chat",
                    true
                  );
                out.write_octet(arg0);
                out.write_value(arg1, String.class);
                _invoke(out);
              }
            catch (ApplicationException ex)
              {
                in =
                  (org.omg.CORBA_2_3.portable.InputStream) ex.getInputStream();

                String id = in.read_string();
                throw new UnexpectedException(id);
              }
            catch (RemarshalException ex)
              {
                receive_chat(arg0, arg1);
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
        ServantObject so = _servant_preinvoke("receive_chat", Player.class);
        if (so == null)
          {
            receive_chat(arg0, arg1);
            return;
          }
        try
          {
            ((Player) so.servant).receive_chat(arg0, arg1);
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

  public void disconnect() throws RemoteException
  {
    if (!Util.isLocal(this))
      {
        try
          {
            org.omg.CORBA.portable.InputStream in = null;
            try
              {
                OutputStream out = _request("disconnect", true);
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
                disconnect();
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
        ServantObject so = _servant_preinvoke("disconnect", Player.class);
        if (so == null)
          {
            disconnect();
            return;
          }
        try
          {
            ((Player) so.servant).disconnect();
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

  public void receive_move(int arg0, int arg1, Point[] arg2)
    throws RemoteException
  {
    if (!Util.isLocal(this))
      {
        try
          {
            org.omg.CORBA_2_3.portable.InputStream in = null;
            try
              {
                org.omg.CORBA_2_3.portable.OutputStream out =
                  (org.omg.CORBA_2_3.portable.OutputStream) _request("receive_move",
                    true
                  );
                out.write_long(arg0);
                out.write_long(arg1);
                out.write_value(cast_array(arg2), Point[].class);
                _invoke(out);
              }
            catch (ApplicationException ex)
              {
                in =
                  (org.omg.CORBA_2_3.portable.InputStream) ex.getInputStream();

                String id = in.read_string();
                throw new UnexpectedException(id);
              }
            catch (RemarshalException ex)
              {
                receive_move(arg0, arg1, arg2);
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
        ServantObject so = _servant_preinvoke("receive_move", Player.class);
        if (so == null)
          {
            receive_move(arg0, arg1, arg2);
            return;
          }
        try
          {
            Point[] arg2Copy = (Point[]) Util.copyObject(arg2, _orb());
            ((Player) so.servant).receive_move(arg0, arg1, arg2Copy);
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

  // This method is required as a work-around for
  // a bug in the JDK 1.1.6 verifier.
  private Serializable cast_array(Object obj)
  {
    return (Serializable) obj;
  }
}
