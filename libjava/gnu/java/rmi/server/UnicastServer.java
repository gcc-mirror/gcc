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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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

import java.io.DataOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.rmi.NoSuchObjectException;
import java.rmi.Remote;
import java.rmi.RemoteException;
import java.rmi.ServerError;
import java.rmi.server.ObjID;
import java.rmi.server.UID;
import java.util.Hashtable;

public class UnicastServer
	implements ProtocolConstants {

static private Hashtable objects = new Hashtable();  //mapping OBJID to server ref
static private Hashtable refcache = new Hashtable(); //mapping obj itself to server ref
static private DGCImpl dgc;

public static void exportObject(UnicastServerRef obj) {
	startDGC();
	objects.put(obj.objid, obj);
	refcache.put(obj.myself, obj);
	obj.manager.startServer();
}

// FIX ME: I haven't handle force parameter
public static boolean unexportObject(UnicastServerRef obj, boolean force) {
	objects.remove(obj.objid);
	refcache.remove(obj.myself);
	obj.manager.stopServer();
	return true;
}

public static UnicastServerRef getExportedRef(Remote remote){
    return (UnicastServerRef)refcache.get(remote);
}

private static synchronized void startDGC() {
	if (dgc == null) {
		try {
			dgc = new DGCImpl();
			// Changed DGCImpl to inherit UnicastServerRef directly
			//((UnicastServerRef)dgc.getRef()).exportObject(dgc);
			dgc.exportObject(dgc);
		}
		catch (RemoteException e) {
			e.printStackTrace();
		}
	}
}

public static void dispatch(UnicastConnection conn) throws Exception {
	switch (conn.getDataInputStream().readUnsignedByte()) {
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

private static void incomingMessageCall(UnicastConnection conn) throws IOException {
	ObjectInputStream in = conn.startObjectInputStream();  // (re)start ObjectInputStream

	ObjID objid = ObjID.read(in);
	int method = in.readInt();
	long hash = in.readLong();

//System.out.println("ObjID: " + objid + ", method: " + method + ", hash: " + hash);

	// Use the objid to locate the relevant UnicastServerRef
	UnicastServerRef uref = (UnicastServerRef)objects.get(objid);
	Object returnval;
	int returncode = RETURN_ACK;
	// returnval is from Method.invoke(), so we must check the return class to see
	// if it's primitive type
	Class returncls = null;
	if (uref != null) {
		try {
			// Dispatch the call to it.
			returnval = uref.incomingMessageCall(conn, method, hash);
			returncls = uref.getMethodReturnType(method, hash);
		}
		catch (Exception e) {
			returnval = e;
			returncode = RETURN_NACK;
		}
                catch (Error e) {
			returnval = new ServerError ("An Error is thrown while processing the invocation on the server", e);
			returncode = RETURN_NACK;
                }
	}
	else {
		returnval = new NoSuchObjectException("");
		returncode = RETURN_NACK;
	}

	conn.getDataOutputStream().writeByte(MESSAGE_CALL_ACK);

	ObjectOutputStream out = conn.startObjectOutputStream();   // (re)start ObjectOutputStream

	out.writeByte(returncode);
	(new UID()).write(out);

	//System.out.println("returnval=" + returnval + " returncls=" + returncls);

	if(returnval != null && returncls != null)
	    ((RMIObjectOutputStream)out).writeValue(returnval, returncls);

	// 1.1/1.2 void return type detection:
	else if (!(returnval instanceof RMIVoidValue || returncls == Void.TYPE)) 
	    out.writeObject(returnval);

	out.flush();
}

}
