/* _PlayerImpl_Tie.java --
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
 * Generate with rmic, command line
 * rmic -iiop -poa -keep gnu.classpath.examples.CORBA.swing.x5.PlayerImpl
 * (the compiled package must be present in the current folder).
 * 
 * This class is normally generated with rmic or grmic from the 
 * {@link PlayerImpl}. See tools/gnu/classpath/tools/giop/README.
 * 
 * In this example the class was manually edited and commented for better 
 * understanding of functionality.
 * 
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org) 
 */
public class _PlayerImpl_Tie extends Servant implements Tie
{
  private PlayerImpl target = null;
  private static final String[] _type_ids =
    { "RMI:gnu.classpath.examples.CORBA.swing.x5.Player:0000000000000000" };

  public void setTarget(Remote a_target)
  {
    this.target = (PlayerImpl) a_target;
  }

  public Remote getTarget()
  {
    return target;
  }

  public org.omg.CORBA.Object thisObject()
  {
    return _this_object();
  }

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

  public ORB orb()
  {
    return _orb();
  }

  public void orb(ORB orb)
  {
    try
      {
        ((org.omg.CORBA_2_3.ORB) orb).set_delegate(this);
      }
    catch (ClassCastException e)
      {
        throw new org.omg.CORBA.BAD_PARAM(
          "POA Servant requires an instance of org.omg.CORBA_2_3.ORB"
        );
      }
  }

  public String[] _all_interfaces(org.omg.PortableServer.POA poa,
    byte[] objectId
  )
  {
    return _type_ids;
  }

  public OutputStream _invoke(String method, InputStream _in,
    ResponseHandler reply
  ) throws SystemException
  {
    try
      {
        org.omg.CORBA_2_3.portable.InputStream in =
          (org.omg.CORBA_2_3.portable.InputStream) _in;
        switch (method.charAt(9))
          {
            case 101 :
              if (method.equals("start_game"))
                {
                  Player arg0 =
                    (Player) PortableRemoteObject.narrow(in.read_Object(),
                      Player.class
                    );
                  boolean arg1 = in.read_boolean();
                  boolean result = target.start_game(arg0, arg1);
                  OutputStream out = reply.createReply();
                  out.write_boolean(result);
                  return out;
                }

            case 104 :
              if (method.equals("receive_chat"))
                {
                  byte arg0 = in.read_octet();
                  String arg1 = (String) in.read_value(String.class);
                  target.receive_chat(arg0, arg1);

                  OutputStream out = reply.createReply();
                  return out;
                }

            case 111 :
              if (method.equals("receive_move"))
                {
                  int arg0 = in.read_long();
                  int arg1 = in.read_long();
                  Point[] arg2 = (Point[]) in.read_value(Point[].class);
                  target.receive_move(arg0, arg1, arg2);

                  OutputStream out = reply.createReply();
                  return out;
                }

            case 114 :
              if (method.equals("_get_J_current_state"))
                {
                  int result = target.get_current_state();
                  OutputStream out = reply.createReply();
                  out.write_long(result);
                  return out;
                }

            case 116 :
              if (method.equals("disconnect"))
                {
                  target.disconnect();

                  OutputStream out = reply.createReply();
                  return out;
                }
          }
        throw new BAD_OPERATION("No such method: '"+method+"'");
      }
    catch (SystemException ex)
      {
        throw ex;
      }
    catch (Throwable ex)
      {
        throw new UnknownException(ex);
      }
  }
}