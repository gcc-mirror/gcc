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

import java.rmi.server.RMISocketFactory;
import java.rmi.server.RMIServerSocketFactory;
import java.rmi.server.RMIClientSocketFactory;
import java.rmi.RemoteException;
import gnu.java.rmi.server.UnicastConnection;
import java.util.Hashtable;
import java.net.Socket;
import java.net.ServerSocket;
import java.io.IOException;
import java.io.ObjectOutput;
import java.io.ObjectInput;
import java.lang.Thread;
import java.lang.Runnable;
import java.net.InetAddress;
import java.net.UnknownHostException;

public class UnicastConnectionManager
	implements Runnable, ProtocolConstants {

private static String localhost;
private static Hashtable servers = new Hashtable();

private Thread serverThread;
private ServerSocket ssock;
String serverName;
int serverPort;
private RMIServerSocketFactory serverFactory;
private RMIClientSocketFactory clientFactory;

static {
        try {
                localhost = InetAddress.getLocalHost().getHostName();
        }
        catch (UnknownHostException _) {
                localhost = "localhost";
        }
}

private UnicastConnectionManager(String host, int port, RMIClientSocketFactory csf) {
	ssock = null;
	serverName = host;
	serverPort = port;
	serverFactory = null;
	clientFactory = csf;
}

private UnicastConnectionManager(int port, RMIServerSocketFactory ssf) {
	try {
		ssock = ssf.createServerSocket(port);
		serverPort = ssock.getLocalPort();
	}
	catch (IOException _) {
		try {
			ssock = ssf.createServerSocket(0);
			serverPort = ssock.getLocalPort();
		}
		catch (IOException __) {
			ssock = null;
			serverPort = 0;
		}
	}
	serverName = localhost;
	serverFactory = ssf;
	clientFactory = null;
}

/**
 * Return a client connection manager which will connect to the given
 * host/port.
 */
public static synchronized UnicastConnectionManager getInstance(String host, int port, RMIClientSocketFactory csf) {
//System.out.println("getInstance: " + host + "," + port + "," + csf);
	if (csf == null) {
		csf = RMISocketFactory.getSocketFactory();
	}
	TripleKey key = new TripleKey(host, port, csf);
	UnicastConnectionManager man = (UnicastConnectionManager)servers.get(key);
	if (man == null) {
		man = new UnicastConnectionManager(host, port, csf);
		servers.put(key, man);
	}
	return (man);
}

/**
 * Return a server connection manager which will accept connection on the
 * given port.
 */
public static synchronized UnicastConnectionManager getInstance(int port, RMIServerSocketFactory ssf) {
//System.out.println("getInstance: " + port + "," + ssf);
	if (ssf == null) {
		ssf = RMISocketFactory.getSocketFactory();
	}
	TripleKey key = new TripleKey(localhost, port, ssf);
	UnicastConnectionManager man = (UnicastConnectionManager)servers.get(key);
	if (man == null) {
		man = new UnicastConnectionManager(port, ssf);
		// The provided port might not be the set port.
		key.port = man.serverPort;
		servers.put(key, man);
	}
	return (man);
}

/**
 * Get a connection from this manager.
 */
public UnicastConnection getConnection() throws IOException {
	if (ssock == null) {
		return (getClientConnection());
	}
	else {
		return (getServerConnection());
	}
}

/**
 * Accept a connection to this server.
 */
private UnicastConnection getServerConnection() throws IOException {
	Socket sock = ssock.accept();
	UnicastConnection conn = new UnicastConnection(this, sock);
	conn.acceptConnection();
//System.out.println("Server connection " + conn);
	return (conn);
}

/**
 * Make a conection from this client to the server.
 */
private UnicastConnection getClientConnection() throws IOException {
	Socket sock = clientFactory.createSocket(serverName, serverPort);
	UnicastConnection conn = new UnicastConnection(this, sock);
	conn.makeConnection(DEFAULT_PROTOCOL);
//System.out.println("Client connection " + conn);
	return (conn);
}

/**
 * Discard a connection when we're done with it - maybe it can be
 * recycled.
 */
public void discardConnection(UnicastConnection conn) {
//System.out.println("Discarding connection " + conn);
	conn.disconnect();
}

/**
 * Start a server on this manager if it's a server socket and we've not
 * already got one running.
 */
public void startServer() {
	synchronized(this) {
		if (ssock == null || serverThread != null) {
			return;
		}
		serverThread = new Thread(this);
	}
	serverThread.start();
}

/**
 * Server thread for connection manager.
 */
public void run() {
	for (;;) {
		try {
//System.out.println("Waiting for connection on " + serverPort);
			UnicastConnection conn = getServerConnection();
			(new Thread(conn)).start();
		}
		catch (Exception e) {
			e.printStackTrace();
		}
	}
}

/**
 * Serialization routine.
 */
void write(ObjectOutput out) throws IOException {
        out.writeUTF(serverName);
        out.writeInt(serverPort);
}

/**
 * Serialization routine.
 */
static UnicastConnectionManager read(ObjectInput in) throws IOException {
        String host = in.readUTF();
        int port = in.readInt();
	RMIClientSocketFactory csf = ((RMIObjectInputStream)in).manager.clientFactory;
        return (getInstance(host, port, csf));
}

}

/**
 * This is use as the hashkey for the client/server connections.
 */
class TripleKey {

String host;
int port;
Object other;

TripleKey(String host, int port, Object other) {
	this.host = host;
	this.port = port;
	this.other = other;
}

/**
 * Hash code just include the host and other - we ignore the port since
 * this has unusual matching behaviour.
 */
public int hashCode() {
	return (host.hashCode() ^ other.hashCode());
}

public boolean equals(Object obj) {
	if (obj instanceof TripleKey) {
		TripleKey other = (TripleKey)obj;
		if (this.host.equals(other.host) &&
		    this.other == other.other &&
		    (this.port == other.port || this.port == 0 || other.port == 0)) {
			return (true);
		}
	}
	return (false);
}

}
