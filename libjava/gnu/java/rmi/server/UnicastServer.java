/*
  Copyright (c) 1996, 1997, 1998, 1999 Free Software Foundation, Inc.

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

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License.
 */

package gnu.java.rmi.server;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.IOException;
import java.net.InetAddress;
import java.util.Hashtable;
import java.net.UnknownHostException;
import java.rmi.server.ObjID;
import java.rmi.server.UnicastRemoteObject;
import java.rmi.server.UID;
import java.rmi.server.RemoteRef;
import java.rmi.RemoteException;
import java.rmi.NoSuchObjectException;
import gnu.java.rmi.dgc.DGCImpl;

public class UnicastServer
	implements ProtocolConstants {

static private Hashtable objects = new Hashtable();
static private DGCImpl dgc;

public static void exportObject(UnicastServerRef obj) {
	startDGC();
	objects.put(obj.objid, obj);
	obj.manager.startServer();
}

private static synchronized void startDGC() {
	if (dgc == null) {
		try {
			dgc = new DGCImpl();
			((UnicastServerRef)dgc.getRef()).exportObject(dgc);
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
	default:
		throw new Exception("bad method type");
	}
}

private static void incomingMessageCall(UnicastConnection conn) throws IOException {
	ObjectInputStream in = conn.getObjectInputStream();

	ObjID objid = ObjID.read(in);
	int method = in.readInt();
	long hash = in.readLong();

//System.out.println("ObjID: " + objid + ", method: " + method + ", hash: " + hash);

	// Use the objid to locate the relevant UnicastServerRef
	UnicastServerRef uref = (UnicastServerRef)objects.get(objid);
	Object returnval;
	int returncode = RETURN_ACK;
	if (uref != null) {
		try {
			// Dispatch the call to it.
			returnval = uref.incomingMessageCall(conn, method, hash);
		}
		catch (Exception e) {
			returnval = e;
			returncode = RETURN_NACK;
		}
	}
	else {
		returnval = new NoSuchObjectException("");
		returncode = RETURN_NACK;
	}

	conn.getDataOutputStream().writeByte(MESSAGE_CALL_ACK);

	ObjectOutputStream out = conn.getObjectOutputStream();

	out.writeByte(returncode);
	(new UID()).write(out);
	out.writeObject(returnval);

	out.flush();
}

}
