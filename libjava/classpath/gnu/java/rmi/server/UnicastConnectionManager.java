/* UnicastConnectionManager.java --
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

import java.io.IOException;
import java.io.ObjectInput;
import java.io.ObjectOutput;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.UnknownHostException;
import java.rmi.RemoteException;
import java.rmi.server.RMIClientSocketFactory;
import java.rmi.server.RMIServerSocketFactory;
import java.rmi.server.RMISocketFactory;
import java.util.ArrayList;
import java.util.ConcurrentModificationException;
import java.util.Hashtable;
import java.util.Iterator;

public class UnicastConnectionManager
        implements Runnable, ProtocolConstants {

private static String localhost;
// use different maps for server/client type UnicastConnectionManager
private static Hashtable servers = new Hashtable();
// Package-private to avoid trampolines.
static Hashtable clients = new Hashtable();
ArrayList connections; //client connection pool

// make serverThread volatile for poll
private volatile Thread serverThread;
private ServerSocket ssock;
String serverName;
int serverPort;

// Package-private to avoid a trampoline.
static Thread scavenger;

// If client and server are in the same VM, serverobj represents server
Object serverobj;

private static RMISocketFactory defaultSocketFactory = RMISocketFactory.getSocketFactory();
private RMIServerSocketFactory serverFactory;
private RMIClientSocketFactory clientFactory;

// The following is for debug
private static int ncsock = 0;    //count of client socket
private static int nssock = 0;    //count of server socket
private static int ncmanager = 0; //count of client manager
private static int nsmanager = 0; //count of server manager

private static final boolean debug = false;

private static final Object GLOBAL_LOCK = new Object();

static {
        try {
                //Use host address instead of host name to avoid name resolving issues
                //localhost = InetAddress.getLocalHost().getHostName();
                localhost = InetAddress.getLocalHost().getHostAddress();
        }
        catch (UnknownHostException _) {
                localhost = "localhost";
        }


}

//Only one scavenger thread running globally
private static void startScavenger(){
    scavenger = new Thread(new Runnable(){
        public void run(){
            if (debug) System.out.println("************* start scavenger.");
            boolean liveon = true;
            while (liveon){
                // Sleep for the expire timeout
                try{
                    Thread.sleep(UnicastConnection.CONNECTION_TIMEOUT);
                }catch(InterruptedException _ie){
                    break;
                }
                liveon = false;
                // Scavenge all clients' connections that're expired
                Iterator iter = clients.values().iterator();
                long l = System.currentTimeMillis();
                try{
                    while(iter.hasNext()){
                        UnicastConnectionManager man = (UnicastConnectionManager)iter.next();
                        ArrayList conns = man.connections;
                        synchronized(conns) { // is the lock a little coarser?
                            for (int last = conns.size() - 1;
                                 last >= 0;
                                 --last)
                            {
                                UnicastConnection conn = (UnicastConnection)conns.get(last);
                                if (UnicastConnection.isExpired(conn, l)){
                                    conns.remove(last);
                                    conn.disconnect();
                                    conn = null;
                                }else
                                    liveon = true; //there're still live connections
                            }
                        }
                    }
                }catch(ConcurrentModificationException cme) {
                    // handle it lazily
                    liveon = true;
                }
            }
            scavenger = null;
            if (debug) System.out.println("************* exit scavenger.");
        }
    });
    // As it is used for client connection, we may put this thread
    // in daemon state to prevent the VM from blocking when exiting.
    scavenger.setDaemon(true);
    scavenger.start();
}

/**
  * Client UnicastConnectionManager constructor
  */
private UnicastConnectionManager(String host, int port, RMIClientSocketFactory csf) {
        ssock = null;
        serverName = host;
        serverPort = port;
        serverFactory = null;
        clientFactory = csf;
    connections = new ArrayList();
}

/**
  * Server UnicastConnectionManager constructor
  */
private UnicastConnectionManager(int port, RMIServerSocketFactory ssf) throws RemoteException {

        try {
                ssock = ssf.createServerSocket(port);
                serverPort = ssock.getLocalPort();
        }
        catch (IOException ioex) {
                ssock = null;
                serverPort = 0;
                throw new java.rmi.server.ExportException("can not create Server Socket on port " + port,ioex);
        }
        // Note that for compatibility the serverName is "localhost",
        // not UnicastConnectionManager.localhost, which is the name
        // of the local box.  A server listening on localhost:port is
        // listening on the loopback interface, 127.0.0.1, but
        // UnicastConnectionManager.localhost is an externally
        // accessible IP address.
        serverName = "localhost";
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
        csf = defaultSocketFactory;
        }
        // change host name to host address to avoid name resolving issues
        try{
        host = InetAddress.getByName(host).getHostAddress();
    }catch(Exception _){}

        TripleKey key = new TripleKey(host, port, csf);
        UnicastConnectionManager man = (UnicastConnectionManager)clients.get(key);
        if (man == null) {
                man = new UnicastConnectionManager(host, port, csf);
        if (debug) {
            ncmanager++;
            System.out.println("\n\n ====== " + ncmanager + " client managers.\n\n");
        }
                clients.put(key, man);

        // Detect if client and server are in the same VM, i.e., their keys are equal
        UnicastConnectionManager svrman = (UnicastConnectionManager)servers.get(key);
        if(svrman != null){ // server and client are in the same VM
            man.serverobj = svrman.serverobj;
        }
        }
        return (man);
}

/**
 * Return a server connection manager which will accept connection on the
 * given port.
 */
public static synchronized UnicastConnectionManager getInstance(int port, RMIServerSocketFactory ssf) throws RemoteException {
//System.out.println("getInstance: " + port + "," + ssf);
        if (ssf == null) {
        ssf = defaultSocketFactory;
        }
        TripleKey key = new TripleKey(localhost, port, ssf);
        UnicastConnectionManager man = (UnicastConnectionManager)servers.get(key);
        if (man == null) {
                man = new UnicastConnectionManager(port, ssf);
        if (debug) {
            nsmanager++;
            System.out.println("\n\n ****** " + nsmanager + " server managers.\n\n");
        }
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
    sock.setTcpNoDelay(true); //??
        UnicastConnection conn = new UnicastConnection(this, sock);
        conn.acceptConnection();
    if (debug){
        nssock++;
        System.out.println("\n\n ****** " + nssock + " server socks.\n\n");
    }
    //System.out.println("Server connection " + sock);
        return (conn);
}

/**
 * Make a conection from this client to the server.
 */
private UnicastConnection getClientConnection() throws IOException {
    ArrayList conns = connections;
    UnicastConnection conn;

    synchronized(conns) {
        int nconn = conns.size() - 1;

        // if there're free connections in connection pool
        if(nconn >= 0) {
            conn = (UnicastConnection)conns.get(nconn);
            //Should we check if conn is alive using Ping??
            conns.remove(nconn);

            // Check if the connection is already expired
            long l = System.currentTimeMillis();
            if (!UnicastConnection.isExpired(conn, l)){
                return conn;
            }else {
                conn.disconnect();
                conn = null;
            }
        }
    }

        Socket sock = clientFactory.createSocket(serverName, serverPort);
    conn = new UnicastConnection(this, sock);
        conn.makeConnection(DEFAULT_PROTOCOL);

    if (debug) {
        ncsock++;
        System.out.println("\n\n ====== " + ncsock + " client socks.\n\n");
    }

        return (conn);
}

/**
 * Get the string representation, describing the connection.
 */
public String toString()
{
  return serverName+":"+serverPort+" ("+serverobj+")";
}

/**
 * Discard a connection when we're done with it - maybe it can be
 * recycled.
 */
public void discardConnection(UnicastConnection conn) {
//System.out.println("Discarding connection " + conn);
    //conn.disconnect();
    if (ssock != null) //server connection
        conn.disconnect();
    else {
        // To client connection, we'd like to return back to pool
        UnicastConnection.resetTime(conn);
        //Ensure there're only one scavenger globally
        synchronized(GLOBAL_LOCK) {
            connections.add(conn); //borrow this lock to garantee thread safety
            if (scavenger == null)
                startScavenger();
        }
    }
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
        // The following is not necessary when java.lang.Thread's constructor do this.
        // serverThread.setContextClassLoader(Thread.currentThread().getContextClassLoader());
        }
        serverThread.start();
}

/**
 * Stop a server on this manager
 */
public void stopServer() {
    synchronized(this) {
        if(serverThread != null){
            serverThread = null;
            try{
                ssock.close();
            }catch(Exception _){}
        }
    }
}

/**
 * Server thread for connection manager.
 */
public void run() {
        for (;serverThread != null;) { // if serverThread==null, then exit thread
                try {
//System.out.println("Waiting for connection on " + serverPort);
                        UnicastConnection conn = getServerConnection();

                        // get address of remote host for the RMIIncomingThread object
                        String remoteHost = null;
                        if (conn.sock != null) {
                                remoteHost = conn.sock.getInetAddress().getHostAddress();
                        }

                        // use a thread pool to improve performance
            //ConnectionRunnerPool.dispatchConnection(conn);
            (new RMIIncomingThread(conn, remoteHost)).start();
//         (new Thread(conn)).start();
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
        //RMIClientSocketFactory csf = ((RMIObjectInputStream)in).manager.clientFactory;
        //return (getInstance(host, port, csf));
        return (getInstance(host, port, null));
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
            (this.port == other.port /* || this.port == 0 || other.port == 0*/)) {
                        return (true);
                }
        }
        return (false);
}

  /**
   * Get the string representation, describing the connection.
   */
  public String toString()
  {
    return host+":"+port+" ("+other+")";
  }

}
