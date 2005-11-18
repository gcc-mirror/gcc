/* OrbFunctional.java --
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


package gnu.CORBA;

import gnu.CORBA.CDR.UnknownExceptionCtxHandler;
import gnu.CORBA.CDR.BufferredCdrInput;
import gnu.CORBA.CDR.BufferedCdrOutput;
import gnu.CORBA.GIOP.CloseMessage;
import gnu.CORBA.GIOP.ErrorMessage;
import gnu.CORBA.GIOP.MessageHeader;
import gnu.CORBA.GIOP.ReplyHeader;
import gnu.CORBA.GIOP.RequestHeader;
import gnu.CORBA.NamingService.NameParser;
import gnu.CORBA.NamingService.NamingServiceTransient;
import gnu.CORBA.Poa.gnuForwardRequest;
import gnu.CORBA.interfaces.SocketFactory;

import org.omg.CORBA.BAD_OPERATION;
import org.omg.CORBA.BAD_PARAM;
import org.omg.CORBA.CompletionStatus;
import org.omg.CORBA.MARSHAL;
import org.omg.CORBA.NO_RESOURCES;
import org.omg.CORBA.OBJECT_NOT_EXIST;
import org.omg.CORBA.Request;
import org.omg.CORBA.SystemException;
import org.omg.CORBA.UNKNOWN;
import org.omg.CORBA.WrongTransaction;
import org.omg.CORBA.ORBPackage.InvalidName;
import org.omg.CORBA.portable.Delegate;
import org.omg.CORBA.portable.InvokeHandler;
import org.omg.CORBA.portable.ObjectImpl;
import org.omg.CORBA.portable.UnknownException;
import org.omg.CosNaming.NamingContextExt;
import org.omg.CosNaming.NamingContextExtHelper;

import java.applet.Applet;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketException;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Map;
import java.util.Properties;
import java.util.Random;
import java.util.StringTokenizer;
import java.util.TreeMap;

/**
 * The ORB implementation, capable to handle remote invocations on the
 * registered object. This class implements all features, required till the jdk
 * 1.3 inclusive, but does not support the POA that appears since 1.4. The POA
 * is supported by {@link gnu.CORBA.Poa.ORB_1_4}.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class OrbFunctional extends OrbRestricted
{
  /**
   * A server, responsible for listening on requests on some local port. The ORB
   * may listen on multiple ports and process the requests in separate threads.
   * Normally the server takes one port per object being served.
   */
  protected class portServer
    extends Thread
  {
    /**
     * The number of the currently running parallel threads.
     */
    int running_threads;

    /**
     * The port on that this portServer is listening for requests.
     */
    int s_port;

    /**
     * The server socket of this portServer.
     */
    ServerSocket service;

    /**
     * True if the serving node must shutdown due call of the close_now().
     */
    boolean terminated;

    /**
     * Create a new portServer, serving on specific port.
     */
    portServer(int _port)
    {
      s_port = _port;
      setDaemon(true);
      try
        {
          service = socketFactory.createServerSocket(s_port);
        }
      catch (IOException ex)
        {
          BAD_OPERATION bad = new BAD_OPERATION(
            "Unable to open the server socket at " + s_port);
          bad.minor = Minor.Socket;
          bad.initCause(ex);
          throw bad;
        }
    }

    /**
     * Enter the serving loop (get request/process it). All portServer normally
     * terminate thy threads when the OrbFunctional.running is set to false.
     */
    public void run()
    {
      while (running)
        {
          try
            {
              tick();
            }
          catch (SocketException ex)
            {
              // May be thrown when the service is closed by
              // the close_now().
              if (terminated)
                return;
            }
          catch (Exception iex)
            {
              // Wait. Do not terminate the
              // service due potentially transient error.
              try
                {
                  Thread.sleep(TWAIT_SERVER_ERROR_PAUSE);
                }
              catch (InterruptedException ex)
                {
                }
            }
        }
    }

    /**
     * Perform a single serving step.
     * 
     * @throws java.lang.Exception
     */
    void tick()
      throws Exception
    {
      serve(this, service);
    }

    /**
     * Forcibly close the server socket and mark this port as free.
     */
    public void close_now()
    {
      try
        {
          terminated = true;
          service.close();
        }
      catch (Exception ex)
        {
          // This may happen if the service has not been opened or
          // cannot be closed. Return without action.
        }
    }

    /**
     * If the thread is no longer in use, close the socket (if opened).
     */
    protected void finalize()
    {
      close_now();
    }
  }

  /**
   * A server, responsible for listening on requests on some local port and
   * serving multiple requests (probably to the different objects) on the same
   * thread.
   */
  protected class sharedPortServer extends portServer
  {
    /**
     * Create a new portServer, serving on specific port.
     */
    sharedPortServer(int _port)
    {
      super(_port);
    }

    /**
     * Perform a single serving step.
     *
     * @throws java.lang.Exception
     */
    void tick() throws Exception
    {
      Socket request = service.accept();
      serveStep(request, false);
    }
  }

  /**
   * The default value where the first instance of this ORB will start looking
   * for a free port.
   */
  public static int DEFAULT_INITIAL_PORT = 1126;
  
  /**
   * When trying to open the socket on a random port, start of the interval to
   * try.
   */
  public static int RANDOM_PORT_FROM = 1024;
  
  /**
   * When trying to open the socket on a random port, end of the interval to
   * try.
   */
  public static int RANDOM_PORT_TO = 4024;
  
  /**
   * The number of attempts to try when opening random port.
   */
  public static int RANDOM_PORT_ATTEMPTS = 64;

  /**
   * The property of port, on that this ORB is listening for requests from
   * clients. This class supports one port per ORB only.
   */
  public static final String LISTEN_ON = "gnu.classpath.CORBA.ListenOn";

  /**
   * The property, defining the IOR of the intial reference to resolve.
   */
  public static final String REFERENCE = "org.omg.CORBA.ORBInitRef";

  /**
   * The property, defining the port on that the default name service is
   * running.
   */
  public static final String NS_PORT = "org.omg.CORBA.ORBInitialPort";

  /**
   * The property, defining the host on that the default name service is
   * running.
   */
  public static final String NS_HOST = "org.omg.CORBA.ORBInitialHost";

  /**
   * The string, defining the naming service initial reference.
   */
  public static final String NAME_SERVICE = "NameService";
  
  /**
   * Defines the ORB ID that is accessible by IOR interceptors.
   */
  public static final String ORB_ID = "org.omg.CORBA.ORBid";
  
  
  /**
   * Defines the SERVER ID that is accessible by IOR interceptors.
   */
  public static final String SERVER_ID = "org.omg.CORBA.ServerId";
  
  /**
   * The if the client has once opened a socket, it should start sending the
   * message header in a given time. Otherwise the server will close the socket.
   * This prevents server hang when the client opens the socket, but does not
   * send any message, usually due crash on the client side.
   */
  public static String START_READING_MESSAGE =
    "gnu.classpath.CORBA.TOUT_START_READING_MESSAGE";

  /**
   * If the client has started to send the request message, the socket time out
   * changes to the specified value.
   */
  public static String WHILE_READING =
    "gnu.classpath.CORBA.TOUT_WHILE_READING";

  /**
   * If the message body is received, the time out changes to the specifice
   * value. This must be longer, as includes time, required to process the
   * received task. We make it 40 minutes.
   */
  public static String AFTER_RECEIVING =
    "gnu.classpath.CORBA.TOUT_AFTER_RECEIVING";
  
  /**
   * The server waits for this duration after the potentially transient error
   * during its servicing cycle.
   */
  public static String SERVER_ERROR_PAUSE =
    "gnu.classpath.CORBA.SERVER_ERROR_PAUSE";

  /**
   * The address of the local host.
   */
  public final String LOCAL_HOST;

  /**
   * The if the client has once opened a socket, it should start sending the
   * message header in a given time. Otherwise the server will close the socket.
   * This prevents server hang when the client opens the socket, but does not
   * send any message, usually due crash on the client side.
   */
  public int TOUT_START_READING_MESSAGE = 20 * 1000;

  // (Here and below, we use * to make the meaning of the constant clearler).

  /**
   * If the client has started to send the request message, the socket time out
   * changes to the specified value.
   */
  public int TOUT_WHILE_READING = 2 * 60 * 1000;

  /**
   * If the message body is received, the time out changes to the specifice
   * value. This must be longer, as includes time, required to process the
   * received task. We make it 40 minutes.
   */
  public int TOUT_AFTER_RECEIVING = 40 * 60 * 1000;
  
  /**
   * The server waits for this duration after the potentially transient error
   * during its servicing cycle.
   */
  public int TWAIT_SERVER_ERROR_PAUSE = 5000;

  /**
   * Some clients tend to submit multiple requests over the same socket. The
   * server waits for the next request on the same socket for the duration,
   * specified below. In additions, the request of this implementation also
   * waits for the same duration before closing the socket. The default time is
   * seven seconds.
   */
  public static int TANDEM_REQUESTS = 7000;
  
  /**
   * The Id of this ORB.
   */
  public String orb_id = "orb_"+hashCode();
  
  /**
   * The Id of this Server. This field is defined static to ensure it has
   * the same value over all ORB's in this machine.
   */
  public static String server_id = "server_"+OrbFunctional.class.hashCode(); 

  /**
   * The map of the already conncted objects.
   */
  protected final Connected_objects connected_objects =
    new Connected_objects();

  /**
   * The maximal CORBA version, supported by this ORB. The default value 0 means
   * that the ORB will not check the request version while trying to respond.
   */
  protected Version max_version;

  /**
   * Setting this value to false causes the ORB to shutdown after the latest
   * serving operation is complete.
   */
  protected boolean running;

  /**
   * The map of the initial references.
   */
  protected Map initial_references = new TreeMap();

  /**
   * The currently active portServers.
   */
  protected ArrayList portServers = new ArrayList();

  /**
   * The host, on that the name service is expected to be running.
   */
  private String ns_host;

  /**
   * Probably free port, under that the ORB will try listening for remote
   * requests first. When the new object is connected, this port is used first,
   * then it is incremented by 1, etc. If the given port is not available, up to
   * 20 subsequent values are tried and then the parameterless server socket
   * contructor is called. The constant is shared between multiple instances of
   * this ORB.
   */
  private static int Port = DEFAULT_INITIAL_PORT;

  /**
   * The port, on that the name service is expected to be running.
   */
  private int ns_port = 900;
  
  /**
   * The name parser.
   */
  NameParser nameParser = new NameParser();

  /**
   * The instance, stored in this field, handles the asynchronous dynamic
   * invocations.
   */
  protected Asynchron asynchron = new Asynchron();

  /**
   * The list of the freed ports. The ORB reuses ports, when possible.
   */
  protected LinkedList freed_ports = new LinkedList();

  /**
   * Maps a single-threaded POAs to they sharedPortServants.
   */
  protected Hashtable identities = new Hashtable();

  /**
   * The maximal allowed number of the currently running parallel threads per
   * object. For security reasons, this is made private and unchangeable. After
   * exceeding this limit, the NO_RESOURCES is thrown back to the client.
   */
  private int MAX_RUNNING_THREADS = 256;
  
  /**
   * The producer of the client and server sockets for this ORB.
   */
  public SocketFactory socketFactory = DefaultSocketFactory.Singleton;

  /**
   * Create the instance of the Functional ORB.
   */
  public OrbFunctional()
  {
    try
      {
        LOCAL_HOST = ns_host = InetAddress.getLocalHost().getHostAddress();
        initial_references.put("CodecFactory", new gnuCodecFactory(this));
      }
    catch (UnknownHostException ex)
      {
        BAD_OPERATION bad =
          new BAD_OPERATION("Unable to open the server socket.");
        bad.initCause(ex);
        throw bad;
      }
  }

  /**
   * If the max version is assigned, the orb replies with the error message if
   * the request version is above the supported 1.2 version. This behavior is
   * recommended by OMG, but not all implementations respond that error message
   * by re-sending the request, encoded in the older version.
   */
  public void setMaxVersion(Version max_supported)
  {
    max_version = max_supported;
  }

  /**
   * Get the maximal supported GIOP version or null if the version is not
   * checked.
   */
  public Version getMaxVersion()
  {
    return max_version;
  }

  /**
   * Get the currently free port, starting from the initially set port and going
   * up max 20 steps, then trying to bind into any free address.
   * 
   * @return the currently available free port.
   * 
   * @throws NO_RESOURCES if the server socked cannot be opened on the local
   * host.
   */
  public int getFreePort()
    throws BAD_OPERATION
  {
    ServerSocket s;
    int a_port;

    try
      {
        // If there are some previously freed ports, use them first.
        if (!freed_ports.isEmpty())
          {
            Integer free = (Integer) freed_ports.getLast();
            freed_ports.removeLast();
            s = socketFactory.createServerSocket(free.intValue());
            s.close();
            return free.intValue();
          }
      }
    catch (Exception ex)
      {
        // This may be thrown if the request for the new port has arrived
        // before the current service is completly shutdown.
        // OK then, use a new port.
      }

    for (a_port = Port; a_port < Port + 20; a_port++)
      {
        try
          {
            s = socketFactory.createServerSocket(a_port);
            s.close();
            Port = a_port + 1;
            return a_port;
          }
        catch (IOException ex)
          {
            // Repeat the loop if this exception has been thrown.
          }
      }

    Random rand = new Random();
    // Try any random port in the interval RANDOM_PORT_FROM.RANDOM_PORT_TO.
    int range = RANDOM_PORT_TO - RANDOM_PORT_FROM;
    IOException ioex = null;
    for (int i = 0; i < RANDOM_PORT_ATTEMPTS; i++)
      {
        try
          {
            a_port = RANDOM_PORT_FROM + rand.nextInt(range);
            s = socketFactory.createServerSocket(a_port);
            s.close();
            return a_port;
          }
        catch (IOException ex)
          {
            // Repeat the loop if this exception has been thrown.
            ioex = ex;
          }
      }

    NO_RESOURCES bad = new NO_RESOURCES("Unable to open the server socket.");
    bad.minor = Minor.Ports;
    if (ioex != null)
      bad.initCause(ioex);
    throw bad;
  }

  /**
   * Set the port, on that the server is listening for the client requests. If
   * only one object is connected to the orb, the server will be try listening
   * on this port first. It the port is busy, or if more objects are connected,
   * the subsequent object will receive a larger port values, skipping
   * unavailable ports, if required. The change applies globally.
   * 
   * @param a_Port a port, on that the server is listening for requests.
   */
  public static void setPort(int a_Port)
  {
    Port = a_Port;
  }

  /**
   * Connect the given CORBA object to this ORB. After the object is connected,
   * it starts receiving remote invocations via this ORB.
   *
   * The ORB tries to connect the object to the port, that has been previously
   * set by {@link setPort(int)}. On failure, it tries 20 subsequent larger
   * values and then calls the parameterless server socked constructor to get
   * any free local port. If this fails, the {@link NO_RESOURCES} is thrown.
   *
   * @param object the object, must implement the {@link InvokeHandler})
   * interface.
   *
   * @throws BAD_PARAM if the object does not implement the
   * {@link InvokeHandler}).
   */
  public void connect(org.omg.CORBA.Object object)
  {
    int a_port = getFreePort();

    Connected_objects.cObject ref = connected_objects.add(object, a_port);
    IOR ior = createIOR(ref);
    prepareObject(object, ior);
    if (running)
      startService(ior);
  }

  /**
   * Connect the given CORBA object to this ORB, explicitly specifying the
   * object key.
   *
   * The ORB tries to connect the object to the port, that has been previously
   * set by {@link setPort(int)}. On failure, it tries 20 subsequent larger
   * values and then calls the parameterless server socked constructor to get
   * any free local port. If this fails, the {@link NO_RESOURCES} is thrown.
   *
   * @param object the object, must implement the {@link InvokeHandler})
   * interface.
   * @param key the object key, usually used to identify the object from remote
   * side.
   *
   * @throws BAD_PARAM if the object does not implement the
   * {@link InvokeHandler}).
   */
  public void connect(org.omg.CORBA.Object object, byte[] key)
  {
    int a_port = getFreePort();

    Connected_objects.cObject ref =
      connected_objects.add(key, object, a_port, null);
    IOR ior = createIOR(ref);
    prepareObject(object, ior);
    if (running)
      startService(ior);
  }

  /**
   * Connect the given CORBA object to this ORB, explicitly specifying the
   * object key and the identity of the thread (and port), where the object must
   * be served. The identity is normally the POA.
   *
   * The new port server will be started only if there is no one already running
   * for the same identity. Otherwise, the task of the existing port server will
   * be widened, including duty to serve the given object. All objects,
   * connected to a single identity by this method, will process they requests
   * subsequently in the same thread. The method is used when the expected
   * number of the objects is too large to have a single port and thread per
   * object. This method is used by POAs, having a single thread policy.
   *
   * @param object the object, must implement the {@link InvokeHandler})
   * interface.
   * @param key the object key, usually used to identify the object from remote
   * side.
   * @param port the port, where the object must be connected.
   *
   * @throws BAD_PARAM if the object does not implement the
   * {@link InvokeHandler}).
   */
  public void connect_1_thread(org.omg.CORBA.Object object, byte[] key,
    java.lang.Object identity
  )
  {
    sharedPortServer shared = (sharedPortServer) identities.get(identity);
    if (shared == null)
      {
        int a_port = getFreePort();
        shared = new sharedPortServer(a_port);
        identities.put(identity, shared);
        if (running)
          {
            portServers.add(shared);
            shared.start();
          }
      }

    Connected_objects.cObject ref =
      connected_objects.add(key, object, shared.s_port, identity);
    IOR ior = createIOR(ref);
    prepareObject(object, ior);
  }

  /**
   * Start the service on the given port of this IOR.
   *
   * @param ior the ior (only Internet.port is used).
   */
  public void startService(IOR ior)
  {
    portServer p = new portServer(ior.Internet.port);
    portServers.add(p);
    p.start();
  }

  /**
   * Destroy this server, releasing the occupied resources.
   */
  public void destroy()
  {
    portServer p;
    for (int i = 0; i < portServers.size(); i++)
      {
        p = (portServer) portServers.get(i);
        p.close_now();
      }
    super.destroy();
  }

  /**
   * Disconnect the given CORBA object from this ORB. The object will be no
   * longer receiving the remote invocations. In response to the remote
   * invocation on this object, the ORB will send the exception
   * {@link OBJECT_NOT_EXIST}. The object, however, is not destroyed and can
   * receive the local invocations.
   *
   * @param object the object to disconnect.
   */
  public void disconnect(org.omg.CORBA.Object object)
  {
    Connected_objects.cObject rmKey = null;

    // Handle the case when it is possible to get the object key.
    // Handle the case when the object is known, but not local.
    if (object instanceof ObjectImpl)
      {
        Delegate delegate = ((ObjectImpl) object)._get_delegate();
        if (delegate instanceof SimpleDelegate)
          {
            byte[] key = ((SimpleDelegate) delegate).getIor().key;
            rmKey = connected_objects.get(key);
          }
      }

    // Try to find and disconned the object that is not an instance of the
    // object implementation.
    if (rmKey == null)
      rmKey = connected_objects.getKey(object);
    if (rmKey != null)
      {
        // Find and stop the corresponding portServer.
        portServer p;
        StopService:
        for (int i = 0; i < portServers.size(); i++)
          {
            p = (portServer) portServers.get(i);
            if (p.s_port == rmKey.port && !(p instanceof sharedPortServer))
              {
                p.close_now();
                freed_ports.addFirst(new Integer(rmKey.port));
                break StopService;
              }
            connected_objects.remove(rmKey.key);
          }
      }
  }

  /**
   * Notifies ORB that the shared service indentity (usually POA) is destroyed.
   * The matching shared port server is terminated and the identity table entry
   * is deleted. If this identity is not known for this ORB, the method returns
   * without action.
   *
   * @param identity the identity that has been destroyed.
   */
  public void identityDestroyed(java.lang.Object identity)
  {
    if (identity == null)
      return;

    sharedPortServer ise = (sharedPortServer) identities.get(identity);
    if (ise != null)
      {
        synchronized (connected_objects)
          {
            ise.close_now();
            identities.remove(identity);

            Connected_objects.cObject obj;
            Map.Entry m;
            Iterator iter = connected_objects.entrySet().iterator();
            while (iter.hasNext())
              {
                m = (Map.Entry) iter.next();
                obj = (Connected_objects.cObject) m.getValue();
                if (obj.identity == identity)
                  iter.remove();
              }
          }
      }
  }

  /**
   * Find the local object, connected to this ORB.
   *
   * @param ior the ior of the potentially local object.
   *
   * @return the local object, represented by the given IOR, or null if this is
   * not a local connected object.
   */
  public org.omg.CORBA.Object find_local_object(IOR ior)
  {
    // Must be the same host.
    if (!ior.Internet.host.equals(LOCAL_HOST))
      return null;

    return find_connected_object(ior.key, ior.Internet.port);
  }

  /**
   * List the initially available CORBA objects (services).
   *
   * @return a list of services.
   *
   * @see resolve_initial_references(String)
   */
  public String[] list_initial_services()
  {
    String[] refs = new String[ initial_references.size() ];
    int p = 0;

    Iterator iter = initial_references.keySet().iterator();
    while (iter.hasNext())
      {
        refs [ p++ ] = (String) iter.next();
      }
    return refs;
  }

  /**
   * Get the IOR reference string for the given object. The string embeds
   * information about the object repository Id, its access key and the server
   * internet address and port. With this information, the object can be found
   * by another ORB, possibly located on remote computer.
   *
   * @param the CORBA object
   * @return the object IOR representation.
   *
   * @throws BAD_PARAM if the object has not been previously connected to this
   * ORB.
   *
   * @throws BAD_OPERATION in the unlikely case if the local host address cannot
   * be resolved.
   *
   * @see string_to_object(String)
   */
  public String object_to_string(org.omg.CORBA.Object forObject)
  {
    // Handle the case when the object is known, but not local.
    if (forObject instanceof ObjectImpl)
      {
        Delegate delegate = ((ObjectImpl) forObject)._get_delegate();
        if (delegate instanceof SimpleDelegate)
          return ((SimpleDelegate) delegate).getIor().toStringifiedReference();
      }

    // Handle the case when the object is local.
    Connected_objects.cObject rec = connected_objects.getKey(forObject);

    if (rec == null)
      throw new BAD_PARAM("The object " + forObject +
        " has not been previously connected to this ORB"
      );

    IOR ior = createIOR(rec);

    return ior.toStringifiedReference();
  }

  /**
   * Get the local IOR for the given object, null if the object is not local.
   */
  public IOR getLocalIor(org.omg.CORBA.Object forObject)
  {
    Connected_objects.cObject rec = connected_objects.getKey(forObject);
    if (rec == null)
      return null;
    else
      return createIOR(rec);
  }

  /**
   * Find and return the easily accessible CORBA object, addressed by name.
   *
   * @param name the object name.
   * @return the object
   *
   * @throws org.omg.CORBA.ORBPackage.InvalidName if the given name is not
   * associated with the known object.
   */
  public org.omg.CORBA.Object resolve_initial_references(String name)
    throws InvalidName
  {
    org.omg.CORBA.Object object = null;
    try
      {
        object = (org.omg.CORBA.Object) initial_references.get(name);
        if (object == null && name.equals(NAME_SERVICE))
          {
            object = getDefaultNameService();
            if (object != null)
              initial_references.put(NAME_SERVICE, object);
          }
      }
    catch (Exception ex)
      {
        InvalidName err = new InvalidName(name);
        err.initCause(ex);
        throw err;
      }
    if (object != null)
      return object;
    else
      throw new InvalidName("Not found: '" + name + "'");
  }

  /**
   * Start the ORBs main working cycle (receive invocation - invoke on the local
   * object - send response - wait for another invocation).
   *
   * The method only returns after calling {@link #shutdown(boolean)}.
   */
  public void run()
  {
    running = true;

    // Instantiate the port server for each socket.
    Iterator iter = connected_objects.entrySet().iterator();
    Map.Entry m;
    Connected_objects.cObject obj;

    while (iter.hasNext())
      {
        m = (Map.Entry) iter.next();
        obj = (Connected_objects.cObject) m.getValue();

        portServer subserver;

        if (obj.identity == null)
          {
            subserver = new portServer(obj.port);
            portServers.add(subserver);
          }
        else
          subserver = (portServer) identities.get(obj.identity);
        
        if (!subserver.isAlive())
          {
            // Reuse the current thread for the last portServer.
            if (!iter.hasNext())
              {
                // Discard the iterator, eliminating lock checks.
                iter = null;
                subserver.run();
                return;
              }
            else
              subserver.start();
          }
      }
  }
  
  /**
   * Start the server in a new thread, if not already running. This method is
   * used to ensure that the objects being transfered will be served from the 
   * remote side, if required. If the ORB is started using this method, it
   * starts as a daemon thread.
   */
  public void ensureRunning()
  {
    final OrbFunctional THIS = this;
    
    if (!running)
      {
        Thread t = new Thread()
        {
          public void run()
          {
            THIS.run();
          }
        };
        t.setDaemon(true);
        t.start();
      }
  }

  /**
   * Shutdown the ORB server.
   *
   * @param wait_for_completion if true, the current thread is suspended until
   * the shutdown process is complete.
   */
  public void shutdown(boolean wait_for_completion)
  {
    super.shutdown(wait_for_completion);
    running = false;

    if (!wait_for_completion)
      {
        for (int i = 0; i < portServers.size(); i++)
          {
            portServer p = (portServer) portServers.get(i);
            p.close_now();
          }
      }
  }

  /**
   * Find and return the CORBA object, addressed by the given IOR string
   * representation. The object can (an usually is) located on a remote
   * computer, possibly running a different (not necessary java) CORBA
   * implementation.
   * 
   * @param ior the object IOR representation string.
   * 
   * @return the found CORBA object.
   * @see object_to_string(org.omg.CORBA.Object)
   */
  public org.omg.CORBA.Object string_to_object(String an_ior)
  {
    return nameParser.corbaloc(an_ior, this);
  }
  
  /**
   * Convert ior reference to CORBA object.
   */
  public org.omg.CORBA.Object ior_to_object(IOR ior)
  {
    org.omg.CORBA.Object object = find_local_object(ior);
    if (object == null)
      {
        ObjectImpl impl = StubLocator.search(this, ior);
        try
          {
            if (impl._get_delegate() == null)
              impl._set_delegate(new IorDelegate(this, ior));
          }
        catch (BAD_OPERATION ex)
          {
            // Some colaborants may throw this exception
            // in response to the attempt to get the unset delegate.
            impl._set_delegate(new IorDelegate(this, ior));
          }

        object = impl;
        // TODO remove commented out code below.
        // connected_objects.add(ior.key, impl, ior.Internet.port, null);
      }
    return object;
  }

  /**
   * Get the default naming service for the case when there no NameService
   * entries.
   */
  protected org.omg.CORBA.Object getDefaultNameService()
  {
    if (initial_references.containsKey(NAME_SERVICE))
      return (org.omg.CORBA.Object) initial_references.get(NAME_SERVICE);

    IOR ior = new IOR();
    ior.Id = NamingContextExtHelper.id();
    ior.Internet.host = ns_host;
    ior.Internet.port = ns_port;
    ior.key = NamingServiceTransient.getDefaultKey();

    IorObject iorc = new IorObject(this, ior);
    NamingContextExt namer = NamingContextExtHelper.narrow(iorc);
    initial_references.put(NAME_SERVICE, namer);
    return namer;
  }

  /**
   * Find and return the object, that must be previously connected to this ORB.
   * Return null if no such object is available.
   * 
   * @param key the object key.
   * @param port the port where the object is connected.
   * 
   * @return the connected object, null if none.
   */
  protected org.omg.CORBA.Object find_connected_object(byte[] key, int port)
  {
    Connected_objects.cObject ref = connected_objects.get(key);
    if (ref == null)
      return null;
    if (port >= 0 && ref.port != port)
      return null;
    else
      return ref.object;
  }

  /**
   * Set the ORB parameters. This method is normally called from
   * {@link #init(Applet, Properties)}.
   * 
   * @param app the current applet.
   * 
   * @param props application specific properties, passed as the second
   * parameter in {@link #init(Applet, Properties)}. Can be <code>null</code>.
   */
  protected void set_parameters(Applet app, Properties props)
  {
    useProperties(props);

    String[][] para = app.getParameterInfo();
    if (para != null)
      {
        for (int i = 0; i < para.length; i++)
          {
            if (para[i][0].equals(LISTEN_ON))
              Port = Integer.parseInt(para[i][1]);
            if (para[i][0].equals(REFERENCE))
              {
                StringTokenizer st = new StringTokenizer(para[i][1], "=");
                initial_references.put(st.nextToken(),
                  string_to_object(st.nextToken()));
              }

            if (para[i][0].equals(ORB_ID))
              orb_id = para[i][1];

            if (para[i][0].equals(SERVER_ID))
              server_id = para[i][1];

            if (para[i][0].equals(NS_HOST))
              ns_host = para[i][1];
            if (para[i][0].equals(START_READING_MESSAGE))
              TOUT_START_READING_MESSAGE = Integer.parseInt(para[i][1]);
            if (para[i][0].equals(WHILE_READING))
              TOUT_WHILE_READING = Integer.parseInt(para[i][1]);
            if (para[i][0].equals(AFTER_RECEIVING))
              TOUT_AFTER_RECEIVING = Integer.parseInt(para[i][1]);
            try
              {
                if (para[i][0].equals(NS_PORT))
                  ns_port = Integer.parseInt(para[i][1]);
              }
            catch (NumberFormatException ex)
              {
                BAD_PARAM bad = new BAD_PARAM("Invalid " + NS_PORT
                  + "property, unable to parse '" + props.getProperty(NS_PORT)
                  + "'");
                bad.initCause(ex);
                throw bad;
              }
          }
      }
  }

  /**
   * Set the ORB parameters. This method is normally called from
   * {@link #init(String[], Properties)}.
   * 
   * @param para the parameters, that were passed as the parameters to the
   * <code>main(String[] args)</code> method of the current standalone
   * application.
   * 
   * @param props application specific properties that were passed as a second
   * parameter in {@link init(String[], Properties)}). Can be <code>null</code>.
   */
  protected void set_parameters(String[] para, Properties props)
  {
    if (para.length > 1)
      {
        for (int i = 0; i < para.length - 1; i++)
          {
            if (para[i].endsWith("ListenOn"))
              Port = Integer.parseInt(para[i + 1]);
            if (para[i].endsWith("ORBInitRef"))
              {
                StringTokenizer st = new StringTokenizer(para[i + 1], "=");
                initial_references.put(st.nextToken(),
                  string_to_object(st.nextToken()));
              }

            if (para[i].endsWith("ORBInitialHost"))
              ns_host = para[i + 1];

            if (para[i].endsWith("ServerId"))
              server_id = para[i++];
            else if (para[i].endsWith("ORBid"))
              orb_id = para[i++];

            try
              {
                if (para[i].endsWith("ORBInitialPort"))
                  ns_port = Integer.parseInt(para[i + 1]);
              }
            catch (NumberFormatException ex)
              {
                throw new BAD_PARAM("Invalid " + para[i]
                  + "parameter, unable to parse '"
                  + props.getProperty(para[i + 1]) + "'");
              }
          }
      }

    useProperties(props);
  }

  /**
   * Create IOR for the given object references.
   */
  protected IOR createIOR(Connected_objects.cObject ref)
    throws BAD_OPERATION
  {
    IOR ior = new IOR();
    ior.key = ref.key;
    ior.Internet.port = ref.port;

    if (ref.object instanceof ObjectImpl)
      {
        ObjectImpl imp = (ObjectImpl) ref.object;
        if (imp._ids().length > 0)
          ior.Id = imp._ids() [ 0 ];
      }
    if (ior.Id == null)
      ior.Id = ref.object.getClass().getName();
    try
      {
        ior.Internet.host = InetAddress.getLocalHost().getHostAddress();
        ior.Internet.port = ref.port;
      }
    catch (UnknownHostException ex)
      {
        throw new BAD_OPERATION("Cannot resolve the local host address");
      }
    return ior;
  }

  /**
   * Prepare object for connecting it to this ORB.
   *
   * @param object the object being connected.
   *
   * @throws BAD_PARAM if the object does not implement the
   * {@link InvokeHandler}).
   */
  protected void prepareObject(org.omg.CORBA.Object object, IOR ior)
    throws BAD_PARAM
  {
    /*
     * if (!(object instanceof InvokeHandler)) throw new
     * BAD_PARAM(object.getClass().getName() + " does not implement
     * InvokeHandler. " );
     */

    // If no delegate is set, set the default delegate.
    if (object instanceof ObjectImpl)
      {
        ObjectImpl impl = (ObjectImpl) object;
        try
          {
            if (impl._get_delegate() == null)
              impl._set_delegate(new SimpleDelegate(this, ior));
          }
        catch (BAD_OPERATION ex)
          {
            // Some colaborants may throw this exception.
            impl._set_delegate(new SimpleDelegate(this, ior));
          }
      }
  }

  /**
   * Write the response message.
   *
   * @param net_out the stream to write response into
   * @param msh_request the request message header
   * @param rh_request the request header
   * @param handler the invocation handler that has been used to invoke the
   * operation
   * @param sysEx the system exception, thrown during the invocation, null if
   * none.
   *
   * @throws IOException
   */
  private void respond_to_client(OutputStream net_out,
    MessageHeader msh_request, RequestHeader rh_request,
    ResponseHandlerImpl handler, SystemException sysEx
  ) throws IOException
  {
    // Set the reply header properties.
    ReplyHeader reply = handler.reply_header;

    if (sysEx != null)
      reply.reply_status = ReplyHeader.SYSTEM_EXCEPTION;
    else if (handler.isExceptionReply())
      reply.reply_status = ReplyHeader.USER_EXCEPTION;
    else
      reply.reply_status = ReplyHeader.NO_EXCEPTION;
    reply.request_id = rh_request.request_id;

    BufferedCdrOutput out =
      new BufferedCdrOutput(50 + handler.getBuffer().buffer.size());
    out.setOrb(this);

    out.setOffset(msh_request.getHeaderSize());

    reply.write(out);

    if (msh_request.version.since_inclusive(1, 2))
      {
        out.align(8);

        // Write the reply data from the handler. The handler data already
        // include the necessary heading zeroes for alignment.
      }
    handler.getBuffer().buffer.writeTo(out);

    MessageHeader msh_reply = new MessageHeader();

    msh_reply.version = msh_request.version;
    msh_reply.message_type = MessageHeader.REPLY;
    msh_reply.message_size = out.buffer.size();

    // Write the reply.
    msh_reply.write(net_out);
    out.buffer.writeTo(net_out);
    net_out.flush();
  }

  /**
   * Forward request to another target, as indicated by the passed exception.
   */
  private void forward_request(OutputStream net_out,
    MessageHeader msh_request, RequestHeader rh_request, gnuForwardRequest info
  ) throws IOException
  {
    MessageHeader msh_forward = new MessageHeader();
    msh_forward.version = msh_request.version;

    ReplyHeader rh_forward = msh_forward.create_reply_header();
    msh_forward.message_type = MessageHeader.REPLY;
    rh_forward.reply_status = info.forwarding_code;
    rh_forward.request_id = rh_request.request_id;

    // The forwarding code is either LOCATION_FORWARD or LOCATION_FORWARD_PERM.
    BufferedCdrOutput out = new BufferedCdrOutput();
    out.setOrb(this);
    out.setOffset(msh_forward.getHeaderSize());

    rh_forward.write(out);

    if (msh_forward.version.since_inclusive(1, 2))
      out.align(8);
    out.write_Object(info.forward_reference);

    msh_forward.message_size = out.buffer.size();

    // Write the forwarding instruction.
    msh_forward.write(net_out);
    out.buffer.writeTo(net_out);
    net_out.flush();
  }

  /**
   * Contains a single servicing task.
   *
   * Normally, each task matches a single remote invocation. However under
   * frequent tandem submissions the same task may span over several
   * invocations.
   *
   * @param serverSocket the ORB server socket.
   *
   * @throws MARSHAL
   * @throws IOException
   */
  void serve(final portServer p, ServerSocket serverSocket)
    throws MARSHAL, IOException
  {
    final Socket service;
    service = serverSocket.accept();

    // Tell the server there are no more resources.
    if (p.running_threads >= MAX_RUNNING_THREADS)
      {
        serveStep(service, true);
        return;
      }

    new Thread()
      {
        public void run()
        {
          try
            {
              synchronized (p)
                {
                  p.running_threads++;
                }
              serveStep(service, false);
            }
          finally
            {
              synchronized (p)
                {
                  p.running_threads--;
                }
            }
        }
      }.start();
  }

  /**
   * A single servicing step, when the client socket is alrady open.
   * 
   * Normally, each task matches a single remote invocation. However under
   * frequent tandem submissions the same task may span over several
   * invocations.
   * 
   * @param service the opened client socket.
   * @param no_resources if true, the "NO RESOURCES" exception is thrown to the
   * client.
   */
  void serveStep(Socket service, boolean no_resources)
  {
    try
      {
        Serving: while (true)
          {
            InputStream in = service.getInputStream();
            service.setSoTimeout(TOUT_START_READING_MESSAGE);

            MessageHeader msh_request = new MessageHeader();

            try
              {
                msh_request.read(in);
              }
            catch (MARSHAL ex)
              {
                // This exception may be thrown due closing the connection.
                return;
              }

            if (max_version != null)
              {
                if (!msh_request.version.until_inclusive(max_version.major,
                  max_version.minor))
                  {
                    OutputStream out = service.getOutputStream();
                    new ErrorMessage(max_version).write(out);
                    return;
                  }
              }

            byte[] r = msh_request.readMessage(in, service, TOUT_WHILE_READING,
              TOUT_AFTER_RECEIVING);

            if (msh_request.message_type == MessageHeader.REQUEST)
              {
                RequestHeader rh_request;

                BufferredCdrInput cin = new BufferredCdrInput(r);
                cin.setOrb(this);
                cin.setVersion(msh_request.version);
                cin.setOffset(msh_request.getHeaderSize());
                cin.setBigEndian(msh_request.isBigEndian());

                rh_request = msh_request.create_request_header();

                // Read header and auto set the charset.
                rh_request.read(cin);

                // in 1.2 and higher, align the current position at
                // 8 octet boundary.
                if (msh_request.version.since_inclusive(1, 2))
                  {
                    cin.align(8);

                    // find the target object.
                  }

                InvokeHandler target = (InvokeHandler) find_connected_object(
                  rh_request.object_key, -1);

                // Prepare the reply header. This must be done in advance,
                // as the size must be known for handler to set alignments
                // correctly.
                ReplyHeader rh_reply = msh_request.create_reply_header();

                // TODO log errors about not existing objects and methods.
                ResponseHandlerImpl handler = new ResponseHandlerImpl(
                  this, msh_request, rh_reply, rh_request);

                SystemException sysEx = null;

                try
                  {
                    if (no_resources)
                      {
                        NO_RESOURCES no = new NO_RESOURCES("Too many parallel calls");
                        no.minor = Minor.Threads;
                        throw no;
                      }
                    if (target == null)
                      throw new OBJECT_NOT_EXIST();
                    target._invoke(rh_request.operation, cin, handler);
                  }
                catch (gnuForwardRequest forwarded)
                  {
                    OutputStream sou = service.getOutputStream();
                    forward_request(sou, msh_request, rh_request, forwarded);
                    if (service != null && !service.isClosed())
                      {
                        // Wait for the subsequent invocations on the
                        // same socket for the TANDEM_REQUEST duration.
                        service.setSoTimeout(TANDEM_REQUESTS);
                        continue Serving;
                      }
                  }
                catch (UnknownException uex)
                  {
                    sysEx = new UNKNOWN("Unknown", 2,
                      CompletionStatus.COMPLETED_MAYBE);
                    sysEx.initCause(uex.originalEx);

                    org.omg.CORBA.portable.OutputStream ech = handler.createExceptionReply();

                    rh_reply.service_context = UnknownExceptionCtxHandler.addExceptionContext(
                      rh_reply.service_context, uex.originalEx, ech);

                    ObjectCreator.writeSystemException(ech, sysEx);
                  }
                catch (SystemException ex)
                  {
                    sysEx = ex;
                    
                    org.omg.CORBA.portable.OutputStream ech = handler.createExceptionReply();
                    
                    rh_reply.service_context = UnknownExceptionCtxHandler.addExceptionContext(
                      rh_reply.service_context, ex, ech);
                    
                    ObjectCreator.writeSystemException(ech, ex);
                  }
                catch (Exception except)
                  {
                    // This should never happen under normal operation and
                    // can only indicate errors in user object implementation.
                    // We inform the user.
                    except.printStackTrace();

                    sysEx = new UNKNOWN("Unknown", 2,
                      CompletionStatus.COMPLETED_MAYBE);
                    sysEx.initCause(except);

                    org.omg.CORBA.portable.OutputStream ech = handler.createExceptionReply();

                    rh_reply.service_context = UnknownExceptionCtxHandler.addExceptionContext(
                      rh_reply.service_context, except, ech);

                    ObjectCreator.writeSystemException(ech, sysEx);
                  }

                // Write the response.
                if (rh_request.isResponseExpected())
                  {
                    OutputStream sou = service.getOutputStream();
                    respond_to_client(sou, msh_request, rh_request, handler,
                      sysEx);
                  }
              }
            else if (msh_request.message_type == MessageHeader.CLOSE_CONNECTION
              || msh_request.message_type == MessageHeader.MESSAGE_ERROR)
              {
                CloseMessage.close(service.getOutputStream());
                service.close();
                return;
              }

            if (service != null && !service.isClosed())

              // Wait for the subsequent invocations on the
              // same socket for the TANDEM_REQUEST duration.
              service.setSoTimeout(TANDEM_REQUESTS);
            else
              return;
          }
      }
    catch (SocketException ex)
      {
        // OK.
        return;
      }
    catch (IOException ioex)
      {
        // Network error, probably transient.
        // TODO log it.
        return;
      }
    finally
      {
        try 
          {
            if (service!=null && !service.isClosed())
              service.close();
          }
        catch (IOException ioex)
          {
            // OK.
          }
      }
  }
  
  /**
   * Set the ORB parameters from the properties that were accumulated
   * from several locations.
   */
  protected void useProperties(Properties props)
  {
    if (props != null)
      {
        if (props.containsKey(LISTEN_ON))
          Port = Integer.parseInt(props.getProperty(LISTEN_ON));
        if (props.containsKey(NS_HOST))
          ns_host = props.getProperty(NS_HOST);
        try
          {
            if (props.containsKey(NS_PORT))
              ns_port = Integer.parseInt(props.getProperty(NS_PORT));
            if (props.containsKey(START_READING_MESSAGE))
              TOUT_START_READING_MESSAGE =
                Integer.parseInt(props.getProperty(START_READING_MESSAGE));
            if (props.containsKey(WHILE_READING))
              TOUT_WHILE_READING =
                Integer.parseInt(props.getProperty(WHILE_READING));
            if (props.containsKey(AFTER_RECEIVING))
              TOUT_AFTER_RECEIVING =
                Integer.parseInt(props.getProperty(AFTER_RECEIVING));
            if (props.containsKey(SERVER_ERROR_PAUSE))
              TWAIT_SERVER_ERROR_PAUSE = 
                Integer.parseInt(props.getProperty(SERVER_ERROR_PAUSE));
          }
        catch (NumberFormatException ex)
          {
            throw new BAD_PARAM("Invalid " + NS_PORT +
              "property, unable to parse '" + props.getProperty(NS_PORT) +
              "'"
            );
          }
        
        if (props.containsKey(SocketFactory.PROPERTY))
          {
            String factory = null;
            try
              {
                factory = props.getProperty(SocketFactory.PROPERTY);
                if (factory!=null)
                  socketFactory = (SocketFactory) 
                    ObjectCreator.forName(factory).newInstance();
              }
            catch (Exception ex)
              {
                BAD_PARAM p = new BAD_PARAM("Bad socket factory "+factory);
                p.initCause(ex);
                throw p;
              }
          }
        
        if (props.containsKey(ORB_ID))
          orb_id = props.getProperty(ORB_ID);
        
        if (props.containsKey(SERVER_ID))
          server_id = props.getProperty(SERVER_ID);
        
        Enumeration en = props.elements();
        while (en.hasMoreElements())
          {
            String item = (String) en.nextElement();
            if (item.equals(REFERENCE))
              initial_references.put(item,
                string_to_object(props.getProperty(item))
              );
          }
      }
  }

  /**
   * Get the next instance with a response being received. If all currently sent
   * responses not yet processed, this method pauses till at least one of them
   * is complete. If there are no requests currently sent, the method pauses
   * till some request is submitted and the response is received. This strategy
   * is identical to the one accepted by Suns 1.4 ORB implementation.
   *
   * The returned response is removed from the list of the currently submitted
   * responses and is never returned again.
   *
   * @return the previously sent request that now contains the received
   * response.
   *
   * @throws WrongTransaction If the method was called from the transaction
   * scope different than the one, used to send the request. The exception can
   * be raised only if the request is implicitly associated with some particular
   * transaction.
   */
  public Request get_next_response() throws org.omg.CORBA.WrongTransaction
  {
    return asynchron.get_next_response();
  }

  /**
   * Find if any of the requests that have been previously sent with
   * {@link #send_multiple_requests_deferred}, have a response yet.
   *
   * @return true if there is at least one response to the previously sent
   * request, false otherwise.
   */
  public boolean poll_next_response()
  {
    return asynchron.poll_next_response();
  }

  /**
   * Send multiple prepared requests expecting to get a reply. All requests are
   * send in parallel, each in its own separate thread. When the reply arrives,
   * it is stored in the agreed fields of the corresponing request data
   * structure. If this method is called repeatedly, the new requests are added
   * to the set of the currently sent requests, but the old set is not
   * discarded.
   *
   * @param requests the prepared array of requests.
   *
   * @see #poll_next_response()
   * @see #get_next_response()
   * @see Request#send_deferred()
   */
  public void send_multiple_requests_deferred(Request[] requests)
  {
    asynchron.send_multiple_requests_deferred(requests);
  }

  /**
   * Send multiple prepared requests one way, do not caring about the answer.
   * The messages, containing requests, will be marked, indicating that the
   * sender is not expecting to get a reply.
   *
   * @param requests the prepared array of requests.
   *
   * @see Request#send_oneway()
   */
  public void send_multiple_requests_oneway(Request[] requests)
  {
    asynchron.send_multiple_requests_oneway(requests);
  }

  /**
   * Set the flag, forcing all server threads to terminate.
   */
  protected void finalize() throws java.lang.Throwable
  {
    running = false;
    super.finalize();
  }
}