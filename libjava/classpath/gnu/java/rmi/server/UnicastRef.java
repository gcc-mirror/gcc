/* UnicastRef.java --
   Copyright (c) 1996, 1997, 1998, 1999, 2002, 2005
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

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.ObjectInput;
import java.io.ObjectInputStream;
import java.io.ObjectOutput;
import java.io.ObjectOutputStream;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.rmi.ConnectException;
import java.rmi.Remote;
import java.rmi.RemoteException;
import java.rmi.server.ObjID;
import java.rmi.server.Operation;
import java.rmi.server.RMIClientSocketFactory;
import java.rmi.server.RemoteCall;
import java.rmi.server.RemoteObject;
import java.rmi.server.RemoteRef;
import java.rmi.server.UID;

public class UnicastRef
	implements RemoteRef, ProtocolConstants {

public ObjID objid;
UnicastConnectionManager manager;

/**
 * Used by serialization, and let subclass capable of having default constructor
 */
// must be public otherwise java.rmi.RemoteObject cannot instantiate this class
// -- iP
public UnicastRef() {
}

public UnicastRef(ObjID objid, String host, int port, RMIClientSocketFactory csf) {
	this(objid);
	manager = UnicastConnectionManager.getInstance(host, port, csf);
}

public UnicastRef(ObjID objid) {
	this.objid = objid;
}

public Object invoke(Remote obj, Method method, Object[] params, long opnum) throws Exception {
    // Check if client and server are in the same VM, then local call can be used to
    // replace remote call, but it's somewhat violating remote semantic.
    Object svrobj = manager.serverobj;
    
    // Make sure that the server object is compatible. It could be loaded from a different
    // classloader --iP
    if(svrobj != null && method.getDeclaringClass().isInstance(svrobj)){
        //local call
		Object ret = null;
		try{
		    ret = method.invoke(svrobj, params);
		}catch(InvocationTargetException e){
		    throw (Exception)e.getTargetException();
		}
		//System.out.println("\n\n ***** local call: " + method + "\nreturn: " + ret + "\n\n");
		return ret;
	}
	//System.out.println("***************** remote call:" + manager.serverPort);
	return (invokeCommon(obj, method, params, -1, opnum));
}

private Object invokeCommon(Remote obj, Method method, Object[] params, int opnum, long hash) throws Exception {
	UnicastConnection conn;
	try {
		conn = manager.getConnection();
	}
	catch (IOException e1) {
		throw new RemoteException("connection failed to host: " + manager.serverName, e1);
	}

	ObjectOutputStream out;
	DataOutputStream dout;
	try {
		dout = conn.getDataOutputStream();
		dout.writeByte(MESSAGE_CALL);

		out = conn.startObjectOutputStream(); // (re)start ObjectOutputStream
		
		objid.write(out);
		out.writeInt(opnum);
		out.writeLong(hash);
		
		// must handle primitive class and their wrapper classes
		Class clss[] = method.getParameterTypes();
	    for(int i = 0; i < clss.length; i++)
	        ((RMIObjectOutputStream)out).writeValue(params[i], clss[i]);

		out.flush();
	}
	catch (IOException e2) {
		throw new RemoteException("call failed: ", e2);
	}

	int returncode;
	Object returnval;
	DataInputStream din;
	ObjectInputStream in;
	UID ack;
	try {
		din = conn.getDataInputStream();
		
		if ((returncode = din.readUnsignedByte()) != MESSAGE_CALL_ACK) {
		    conn.disconnect();
			throw new RemoteException("Call not acked:" + returncode);
		}

		in = conn.startObjectInputStream(); // (re)start ObjectInputStream
		returncode = in.readUnsignedByte();
		ack = UID.read(in);

		Class cls = method.getReturnType();

	if (returncode == RETURN_NACK) { 
	    returnval = in.readObject();  // get Exception

        } else if(cls == Void.TYPE) { 
            returnval = null;
            // in.readObject() // not required! returntype 'void' means no field is returned.
        } else {
            returnval = ((RMIObjectInputStream)in).readValue(cls); // get returnvalue
	}
	} catch (IOException e3) {
	    //for debug: e3.printStackTrace();
		throw new RemoteException("call return failed: ", e3);
	}

    /* if DGCAck is necessary??
    //According to RMI wire protocol, send a DGCAck 
    // to indicate receiving return value
    dout.writeByte(MESSAGE_DGCACK);
    ack.write(dout);
    out.flush();
    */
    
	manager.discardConnection(conn);

	if (returncode != RETURN_ACK && returnval != null) {
		if (returncode == RETURN_NACK) throw (Exception)returnval;
		else throw new RemoteException("unexpected returncode: " + returncode);
	}

	return (returnval);
}

/**
 * @deprecated
 */
public RemoteCall newCall(RemoteObject obj, Operation[] op, int opnum, long hash) throws RemoteException {
    UnicastConnection conn;
    
	try {
		conn = manager.getConnection();
	}
	catch (IOException e1) {
		throw new ConnectException("connection failed to host: " + manager.serverName, e1);
	}

    //obj: useless?

	return (new UnicastRemoteCall(conn, objid, opnum, hash));
}

/**
 * @deprecated
 */
public void invoke(RemoteCall call) throws Exception {
	UnicastRemoteCall c = (UnicastRemoteCall)call;
	call.executeCall();
}

/**
 * @deprecated
 */
public void done(RemoteCall call) throws RemoteException {
	UnicastRemoteCall c = (UnicastRemoteCall)call;
	try{
	    c.done();
	} catch(IOException e){}
    UnicastConnection conn = c.getConnection();
	manager.discardConnection(conn);
}

public void writeExternal(ObjectOutput out) throws IOException {
	if (manager == null) {
		throw new IOException("no connection");
	}
	manager.write(out);
	objid.write(out);
	// This byte is somewhat confusing when interoperating with JDK
	out.writeByte(0); //RETURN_ACK);
}

public void readExternal(ObjectInput in) throws IOException, ClassNotFoundException {
	manager = UnicastConnectionManager.read(in);
	objid = ObjID.read(in);
	byte ack = in.readByte();
	// This byte is somewhat confusing when interoperating with JDK
	if (ack != RETURN_ACK && ack != 0/*jdk ack value*/) {
		throw new IOException("no ack found");
	}
}

public boolean remoteEquals(RemoteRef ref) {
	throw new Error("Not implemented");
}

public int remoteHashCode() {
	throw new Error("Not implemented");
}

public String getRefClass(ObjectOutput out) {
	return ("UnicastRef");
}

public String remoteToString() {
	throw new Error("Not implemented");
}

public void dump(UnicastConnection conn) {
	try {
		DataInputStream din = conn.getDataInputStream();
		for (;;) {
			int b = din.readUnsignedByte();
			System.out.print(Integer.toHexString(b));
			if (b >= 32 && b < 128) {
				System.out.print(": " + (char)b);
			}
			System.out.println();
		}
	}
	catch (IOException _) {
	}
}

}
