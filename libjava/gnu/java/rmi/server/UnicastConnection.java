/* UnicastConnection.java --
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

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.net.Socket;
import java.rmi.RemoteException;

public class UnicastConnection 
	implements Runnable, ProtocolConstants {

UnicastConnectionManager manager;
Socket sock;
DataInputStream din;
DataOutputStream dout;
ObjectInputStream oin;
ObjectOutputStream oout;

// reviveTime and expireTime make UnicastConnection pool-able
long reviveTime = 0;
long expireTime = Long.MAX_VALUE;

UnicastConnection(UnicastConnectionManager man, Socket sock) {
	this.manager = man;
	this.sock = sock;
}

void acceptConnection() throws IOException {
//System.out.println("Accepting connection on " + sock);
    //Use BufferedXXXStream would be more efficient
	din = new DataInputStream(new BufferedInputStream(sock.getInputStream()));
	dout = new DataOutputStream(new BufferedOutputStream(sock.getOutputStream()));

	int sig = din.readInt();
	if (sig != PROTOCOL_HEADER) {
		throw new IOException("bad protocol header");
	}
	short ver = din.readShort();
	if (ver != PROTOCOL_VERSION) {
		throw new IOException("bad protocol version");
	}
	int protocol = din.readUnsignedByte();
	if (protocol != SINGLE_OP_PROTOCOL) {
		// Send an ACK
		dout.writeByte(PROTOCOL_ACK);

		// Send my hostname and port
		dout.writeUTF(manager.serverName);
		dout.writeInt(manager.serverPort);
		dout.flush();

		// Read their hostname and port
		String rhost = din.readUTF();
		int rport = din.readInt();
	}
	// Okay, ready to roll ...
}

void makeConnection(int protocol) throws IOException {
    //Use BufferedXXXStream would be more efficient
	din = new DataInputStream(new BufferedInputStream(sock.getInputStream()));

	dout = new DataOutputStream(new BufferedOutputStream(sock.getOutputStream()));

	// Send header
	dout.writeInt(PROTOCOL_HEADER);
	dout.writeShort(PROTOCOL_VERSION);
	dout.writeByte(protocol);
    dout.flush();
    
	if (protocol != SINGLE_OP_PROTOCOL) {
		// Get back ack.
		int ack = din.readUnsignedByte();
		if (ack != PROTOCOL_ACK) {
			throw new RemoteException("Unsupported protocol");
		}

		// Read in host and port
		String dicard_rhost = din.readUTF();
		int discard_rport = din.readInt();

		// Send them my endpoint
		dout.writeUTF(manager.serverName);
		dout.writeInt(manager.serverPort);
		dout.flush();
	}
	// Okay, ready to roll ...
}

DataInputStream getDataInputStream() throws IOException {
	return (din);
}

DataOutputStream getDataOutputStream() throws IOException {
	return (dout);
}

/*
*
* get ObjectInputStream for reading more objects
*
*/
ObjectInputStream getObjectInputStream() throws IOException {
	if (oin == null) {
		throw new IOException("no ObjectInputtream for reading more objects");
	}
	return (oin);
}

/**
*
* starts ObjectInputStream.
*
*/
ObjectInputStream startObjectInputStream() throws IOException {
	return (oin = new RMIObjectInputStream(din));
}

/**
*
* get ObjectOutputStream for sending more objects
*
*/
ObjectOutputStream getObjectOutputStream() throws IOException {
	if (oout == null) {
		throw new IOException("no ObjectOutputStream for sending more objects");
	} 
	return (oout);
}

/**
*
* starts ObjectOutputStream.
*
*/
ObjectOutputStream startObjectOutputStream() throws IOException {
	return (oout = new RMIObjectOutputStream(dout));
} 

void disconnect() {
	try {
	    if(oout != null)
	        oout.close();
        sock.close();
	}
	catch (IOException _) {
    }

	oin = null;
    oout = null;
	din = null;
	dout = null;
	sock = null;
}

public static final long CONNECTION_TIMEOUT = 10000L;

static boolean isExpired(UnicastConnection conn, long l){
    if (l <= conn.expireTime )
        return false;
    return true;
}

static void resetTime(UnicastConnection conn){
    long l = System.currentTimeMillis();
    conn.reviveTime = l;
    conn.expireTime = l + CONNECTION_TIMEOUT;
}

/**
 * We run connects on the server. Dispatch it then discard it.
 */
public void run() {
    do{
	try {
		UnicastServer.dispatch(this);
            //don't discardConnection explicitly, only when
            //  exception happens or the connection's expireTime 
            //  comes
        } catch (Exception e ){
		manager.discardConnection(this);
            break;
	}
    }while(true);
}


}
