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

import java.lang.Runnable;
import java.net.Socket;
import java.net.ServerSocket;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.ObjectOutput;
import java.io.ObjectInput;
import java.io.IOException;
import java.rmi.RemoteException;

public class UnicastConnection 
	implements Runnable, ProtocolConstants {

UnicastConnectionManager manager;
Socket sock;
DataInputStream din;
DataOutputStream dout;
ObjectInputStream oin;
ObjectOutputStream oout;

UnicastConnection(UnicastConnectionManager man, Socket sock) {
	this.manager = man;
	this.sock = sock;
}

void acceptConnection() throws IOException {
//System.out.println("Accepting connection on " + lport);
	din = new DataInputStream(sock.getInputStream());
	dout = new DataOutputStream(sock.getOutputStream());

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

		// Read their hostname and port
		String rhost = din.readUTF();
		int rport = din.readInt();
	}
	// Okay, ready to roll ...
}

void makeConnection(int protocol) throws IOException {
	dout = new DataOutputStream(sock.getOutputStream());
	din = new DataInputStream(sock.getInputStream());

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
	}
	// Okay, ready to roll ...
}

DataInputStream getDataInputStream() throws IOException {
	return (din);
}

DataOutputStream getDataOutputStream() throws IOException {
	return (dout);
}

ObjectInputStream getObjectInputStream() throws IOException {
	if (oin == null) {
		oin = new RMIObjectInputStream(din, manager);
	}
	return (oin);
}

ObjectOutputStream getObjectOutputStream() throws IOException {
	if (oout == null) {
		oout = new RMIObjectOutputStream(dout);
	}
	return (oout);
}

void disconnect() {
	oin = null;
	oout = null;
	try {
		sock.close();
	}
	catch (IOException _) {
	}
	din = null;
	dout = null;
	sock = null;
}

/**
 * We run connects on the server. Dispatch it then discard it.
 */
public void run() {
	try {
		UnicastServer.dispatch(this);
		manager.discardConnection(this);
	}
	catch (Exception e) {
		e.printStackTrace();
	}
}

}
