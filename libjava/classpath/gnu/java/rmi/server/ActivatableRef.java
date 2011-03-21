/* ActivatableRef.java -- Activatable server reference
   Copyright (C) 2006 Free Software Foundation, Inc.

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


package gnu.java.rmi.server;

import java.io.IOException;
import java.io.ObjectInput;
import java.io.ObjectOutput;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.rmi.Remote;
import java.rmi.RemoteException;
import java.rmi.activation.ActivationException;
import java.rmi.activation.ActivationID;
import java.rmi.server.ObjID;
import java.rmi.server.RMIClientSocketFactory;
import java.rmi.server.RemoteObject;
import java.rmi.server.RemoteObjectInvocationHandler;
import java.rmi.server.RemoteRef;

/**
 * The activatable reference works like UnicastRef, but if the remote object
 * appears to be not accessible, it tries to reactivate it before reporting
 * any errors. Apart the fields of the UnicastRef, the activatable reference
 * contains the ActivationID that is used for this activation.
 *
 * @author Audrius Meskauskas (Audriusa@Bioinformatics.org)
 */
public class ActivatableRef extends UnicastRef
{
  /**
   * Use serial version UID for iteroperability
   */
  private static final long serialVersionUID = 1;

  /**
   * The activation id.
   */
  ActivationID actId;

  /**
   * Delegate call to the superclass.
   */
  public ActivatableRef()
  {
    super();
  }

  /**
   * Delegate call to the superclass.
   */
  public ActivatableRef(ObjID objid, String host, int port,
                        RMIClientSocketFactory csf)
  {
    super(objid, host, port, csf);
  }

  /**
   * Delegate call to the superclass.
   */
  public ActivatableRef(ObjID objid)
  {
    super(objid);
  }

  /**
   * Get the referencing class.
   */
  public String getRefClass(ObjectOutput out)
  {
    return "ActivatableRef";
  }

  /**
   * Read the content from the input stream.
   */
  public void readExternal(ObjectInput in) throws IOException,
      ClassNotFoundException
  {
    actId = (ActivationID) in.readObject();
    String type = in.readUTF();
    // XXX handle type.equals("") (null reference)
    super.readExternal(in);
  }

  /**
   * Write the content to the output stream.
   */
  public void writeExternal(ObjectOutput out) throws IOException
  {
    out.writeObject(actId);
    // XXX write a "" if the "nested" reference is a null reference
    out.writeUTF("UnicastRef2");
    super.writeExternal(out);
  }

  /**
   * Invoke the remote method on the given object and try to activate the object
   * if it is not reacheable with the current manager.
   */
  protected Object invokeCommon(Remote obj, Method method, Object[] params,
                                int opnum, long hash) throws Exception
  {
    UnicastConnection conn;
    try
      {
        conn = manager.getConnection();
      }
    catch (IOException e1)
      {
        // Connection failed: try to activate.
        Remote reactivated = actId.activate(false);

        if (reactivated instanceof RemoteObject)
          {
            RemoteRef ref = ((RemoteObject) reactivated).getRef();
            manager = ((UnicastRef) ref).manager;
          }
        else if (Proxy.isProxyClass(reactivated.getClass()))
          {
            RemoteObjectInvocationHandler hander =
              (RemoteObjectInvocationHandler)
                Proxy.getInvocationHandler(reactivated);

            RemoteRef ref = hander.getRef();
            manager = ((UnicastRef) ref).manager;
          }
        else
          throw new ActivationException("Activating into unsupported class "
                                        + reactivated.getClass());

        try
          {
            conn = manager.getConnection();
          }
        catch (IOException e2)
          {
            throw new RemoteException("connection failed to host: "
                                      + manager.serverName, e1);
          }
      }
    return invokeCommon(conn, obj, method, params, opnum, hash);
  }
}
