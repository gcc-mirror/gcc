/* UnicastServer.java --
   Copyright (c) 1996, 1997, 1998, 1999, 2002, 2004
   Free Software Foundation, Inc.

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

import gnu.java.rmi.dgc.DGCImpl;
import gnu.java.util.WeakIdentityHashMap;

import java.io.DataOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.rmi.NoSuchObjectException;
import java.rmi.Remote;
import java.rmi.RemoteException;
import java.rmi.ServerError;
import java.rmi.activation.ActivationException;
import java.rmi.activation.ActivationID;
import java.rmi.server.ObjID;
import java.rmi.server.UID;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Map;
import java.util.WeakHashMap;

public class UnicastServer
    implements ProtocolConstants
{

  /**
   * Mapping OBJID to server ref by .equals().
   */
  static private Map objects = Collections.synchronizedMap(new WeakHashMap());

  /**
   * Mapping obj itself to server ref by identity.
   */
  static private Map refcache = Collections.synchronizedMap(new WeakIdentityHashMap());
  
  /**
   * Mapping the registered activatable objects into they server references.
   */
  public static Map actIds = new Hashtable();
  
  /**
   * The reference to the local distributed garbage collector. 
   */
  static private DGCImpl dgc;
  
  /**
   * Connect this server reference to the server, allowing the local
   * implementation, associated with this object, to receive remote calls.
   * 
   * @param obj the server reference, encloses the (usually local) remote
   *          object.
   */
  public static void exportObject(UnicastServerRef obj)
  {
    startDGC();
    objects.put(obj.objid, obj);
    refcache.put(obj.myself, obj);
    obj.manager.startServer();
  }
  
  /**
   * Register the activatable object into the table of the activatable
   * objects.
   */
  public static void registerActivatable(ActivatableServerRef ref)
  {
    actIds.put(ref.actId, ref);
  }
  
  /**
   * Export tha activatable object. The object id is placed into the map,
   * but the object itself not. This is enough to deliver call to
   * the ref.incomingMessageCall where the object will be instantiated,
   * if not present.
   */
  public static void exportActivatableObject(ActivatableServerRef ref)
  {
    startDGC();
    objects.put(ref.objid, ref);
    ref.manager.startServer();
    actIds.put(ref.actId, ref);
  }
    
  
  /**
   * Get the activatable server reference that is handling activation of the
   * given activation id.
   */
  public static ActivatableServerRef getActivatableRef(ActivationID id)
      throws ActivationException
  {
    ActivatableServerRef ref = (ActivatableServerRef) actIds.get(id);
    if (ref == null)
      throw new ActivationException(id + " was not registered with this server");
    return ref;
  }
  
  /**
   * Unregister the previously registered activatable server reference.
   */
  public static void unregisterActivatable(ActivationID id)
  {
     actIds.remove(id);    
  }
  
  // FIX ME: I haven't handle force parameter
  /**
   * Remove the given server reference. The remote object, associated with
   * this reference, will no longer receive remote calls via this server.
   */
  public static boolean unexportObject(UnicastServerRef obj, boolean force)
  {
    objects.remove(obj.objid);
    refcache.remove(obj.myself);
    obj.manager.stopServer();
    
    if (obj instanceof ActivatableServerRef)
      {
        ActivationID id = ((ActivatableServerRef) obj).actId;
        unregisterActivatable(id);
      }
    return true;
  }
  
  /**
   * Get the exported reference of the given Remote. The identity map is used,
   * the non-null value will only be returned if exactly the passed remote
   * is part of the registered UnicastServerRef. 
   * 
   * @param remote the Remote that is connected to this server via 
   * {@link UnicastServerRef}.
   * 
   * @return the UnicastServerRef that is used to connect the passed
   * remote with this server or null, if this Remote is not connected
   * to this server.
   */
  public static UnicastServerRef getExportedRef(Remote remote)
  {
    return (UnicastServerRef) refcache.get(remote);
  }

  /**
   * Get the server references to the object, previously exported via this
   * server. As the identity map is scanned, more than one reference may match
   * this Id.
   * 
   * @param id the id of the exported object
   * @return the server reference to this object, null if none.
   */
  public static Collection getExported(Object id)
  {
    synchronized (objects)
      {
        ArrayList list = new ArrayList();
        Iterator iter = objects.entrySet().iterator();
        Map.Entry e;
        Object key;
        while (iter.hasNext())
          {
            e = (Map.Entry) iter.next();
            key = e.getKey();
            if (key != null && key.equals(id))
              list.add(e.getValue());
          }
        return list;
      }
  }

  private static synchronized void startDGC()
  {
    if (dgc == null)
      {
        try
          {
            dgc = new DGCImpl();
            // Changed DGCImpl to inherit UnicastServerRef directly
            // ((UnicastServerRef)dgc.getRef()).exportObject(dgc);
            dgc.exportObject(dgc);
          }
        catch (RemoteException e)
          {
            e.printStackTrace();
          }
      }
  }

  public static void dispatch(UnicastConnection conn) throws Exception
  {
    switch (conn.getDataInputStream().readUnsignedByte())
      {
      case MESSAGE_CALL:
        incomingMessageCall(conn);
        break;
      case MESSAGE_PING:
        // jdk sends a ping before each method call -> answer it!
        DataOutputStream out = conn.getDataOutputStream();
        out.writeByte(MESSAGE_PING_ACK);
        out.flush();
        break;
      default:
        throw new Exception("bad method type");
      }
  }
  
  /**
   * This method is invoked when the remote call is received. The method
   * dispatches the call to the responsible object, connected to this 
   * server via UnicastServerReference.
   */
  private static void incomingMessageCall(UnicastConnection conn)
      throws IOException
  {
    ObjectInputStream in = conn.startObjectInputStream(); // (re)start
                                                          // ObjectInputStream

    ObjID objid = ObjID.read(in);
    int method = in.readInt();
    long hash = in.readLong();

    // System.out.println("ObjID: " + objid + ", method: " + method + ", hash: "
    // + hash);

    // Use the objid to locate the relevant UnicastServerRef
    UnicastServerRef uref = (UnicastServerRef) objects.get(objid);
    Object returnval;
    int returncode = RETURN_ACK;
    // returnval is from Method.invoke(), so we must check the return class to
    // see
    // if it's primitive type
    Class returncls = null;
    if (uref != null)
      {
        try
          {
            // Dispatch the call to it.
            returnval = uref.incomingMessageCall(conn, method, hash);
            returncls = uref.getMethodReturnType(method, hash);
          }
        catch (Exception e)
          {
            returnval = e;
            returncode = RETURN_NACK;
          }
        catch (Error e)
          {
            returnval = new ServerError(
              "Server error, ObjID: " + objid + 
              ", method: " + method + ", hash: "+ hash, e);
            returncode = RETURN_NACK;
          }
      }
    else
      {
        returnval = new NoSuchObjectException("ObjID: " + objid);
        returncode = RETURN_NACK;
      }

    conn.getDataOutputStream().writeByte(MESSAGE_CALL_ACK);

    ObjectOutputStream out = conn.startObjectOutputStream(); // (re)start
                                                              // ObjectOutputStream

    out.writeByte(returncode);
    (new UID()).write(out);

    // System.out.println("returnval=" + returnval + " returncls=" + returncls);

    if (returnval != null && returncls != null)
      ((RMIObjectOutputStream) out).writeValue(returnval, returncls);

    // 1.1/1.2 void return type detection:
    else if (! (returnval instanceof RMIVoidValue || returncls == Void.TYPE))
      out.writeObject(returnval);

    out.flush();
  }

}
