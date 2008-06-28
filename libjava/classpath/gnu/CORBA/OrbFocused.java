/* OrbFocused.java --
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

import gnu.CORBA.Poa.ORB_1_4;

import org.omg.CORBA.BAD_INV_ORDER;
import org.omg.CORBA.BAD_OPERATION;
import org.omg.CORBA.BAD_PARAM;
import org.omg.CORBA.CompletionStatus;
import org.omg.CORBA.NO_RESOURCES;
import org.omg.CORBA.portable.InvokeHandler;

import java.applet.Applet;
import java.net.ServerSocket;
import java.util.Iterator;
import java.util.Properties;
import java.util.Random;
import java.util.StringTokenizer;

/**
 * This class implements the ORB that uses a single port or the restricted port
 * range for all its objects. It is required to for use together with various
 * firewalls that does not allow opening multiple randomly selected ports, as
 * the defauld CORBA implementation used to do. The firewal must be configured
 * to allow CORBA to work on one fixed port or (for better performance) on a
 * small fixed range of ports. This does not restrict the maximal number of the
 * connected objects as the objects can share the same port.
 * 
 * The used port or the used port range can be specified via property
 * gnu.CORBA.ListenerPort. The value of this property is a single port or range
 * of ports, boundary values (inclusive) being separeted by dash (for instance,
 * "1245-1250").
 * 
 * It is possible to instantiate multiple instances of the focused ORBs and
 * combine them with the ordinary ORBs. If you instantiate several instances of
 * the focused ORBs on the same host, they used port sets should not overlap.
 * 
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class OrbFocused
  extends ORB_1_4
{
  /**
   * The name of the fixed port range property. The presence of this property
   * indicates that the default focused ORB must be used.
   */
  public static final String LISTENER_PORT = "gnu.CORBA.ListenerPort";

  /**
   * The start of the range of the available ports, inclusive.
   */
  int m_ports_from = -1;

  /**
   * The end of the range of the available ports, inclusive.
   */
  int m_ports_to = -1;

  /**
   * The requests to port are served in parallel threads.
   */
  static final int PARALLEL = 0;

  /**
   * The requests to port are served in the same thread.
   */
  static final int SEQUENTIAL = 1;

  /**
   * The random number generator to get a random port in the valid range.
   */
  Random m_random = new Random();

  /**
   * Parse the "gnu.CORBA.ListenerPort" property and initialize the valid port
   * set range.
   */
  public void setPortRange(String property)
  {
    int from, to;
    try
      {
        StringTokenizer st = new StringTokenizer(property, " -");
        if (st.countTokens() == 1)
          from = to = Integer.parseInt(st.nextToken());
        else
          {
            from = Integer.parseInt(st.nextToken());
            to = Integer.parseInt(st.nextToken());
            m_random = new Random();
          }
        setPortRange(from, to);
      }
    catch (Exception ex)
      {
        throw new BAD_PARAM("Unable to parse port range '" + property + "'");
      }
  }

  /**
   * Set the port range.
   * 
   * @param from - start of the port range, inclusive.
   * @param to - end of the port range, inclusive.
   */
  public void setPortRange(int from, int to)
  {
    if (from < 0 || to < 0 || to < from)
      throw new BAD_PARAM("Invalid range");
    m_ports_from = from;
    m_ports_to = to;
  }

  /**
   * Get the port from the previously specified usage range.
   */
  int getPortFromRange(int attempt)
  {
    if (m_ports_from == m_ports_to)
      return m_ports_from;
    else if (m_ports_to - m_ports_from < RANDOM_PORT_ATTEMPTS)
      return m_ports_from + (attempt % (m_ports_to - m_ports_from + 1));
    else
      return m_random.nextInt(m_ports_to - m_ports_from + 1) + m_ports_from;
  }

  /**
   * Get the shared port server where the new object can be added. This may
   * result reusing the existing server or instantiating a new server. If the
   * new server is instantiated and the ORB is already running, the server is
   * started.
   */
  protected portServer getPortServer(int type)
  {
    portServer p;

    int n;
    if (m_ports_from < m_ports_to)
      n = RANDOM_PORT_ATTEMPTS;
    else
      n = 1;

    Ports: for (int a = 0; a < n; a++)
      {
        int port = getPortFromRange(a);
        for (int i = 0; i < portServers.size(); i++)
          {
            p = (portServer) portServers.get(i);
            if (p.s_port == port)
              {
                return p;
              }
          }
        // The server is not yet instantiated. Instantiate.
        try
          {
            // Check if the port is ok:
            ServerSocket s = socketFactory.createServerSocket(port);
            s.close();

            portServer shared;

            switch (type)
              {
                case PARALLEL:
                  shared = new portServer(port);
                  break;

                case SEQUENTIAL:
                  shared = new sharedPortServer(port);
                  break;

                default:
                  throw new InternalError("Invalid server type " + type);
              }

            portServers.add(shared);

            if (running)
              shared.start();

            return shared;
          }
        catch (Exception ex)
          {
            // Port is taken or probably other problems.
            continue Ports;
          }
      }
    throw new NO_RESOURCES("No free port available at " + m_ports_from + "-"
      + m_ports_to, Minor.Ports, CompletionStatus.COMPLETED_NO);
  }

  /**
   * Start the ORBs main working cycle (receive invocation - invoke on the local
   * object - send response - wait for another invocation).
   * 
   * The method only returns after calling {@link #shutdown(boolean)}.
   */
  public void run()
  {
    if (m_ports_from < 0 || m_ports_to < 0)
      throw new BAD_INV_ORDER("For " + getClass().getName() + " "
        + LISTENER_PORT + " property must be set");

    running = true;

    // Start all port servers. In the current subclass, the portServers
    // collection must be already filled in.
    Iterator iter = portServers.iterator();

    while (iter.hasNext())
      {
        portServer subserver = (portServer) iter.next();

        if (!subserver.isAlive())
          {
            // Reuse the current thread for the last portServer.
            if (!iter.hasNext())
              {
                // Discard the iterator.
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
   * Get free port from the allowed range. This method instantiates the port
   * server for the returned port.
   */
  public int getFreePort()
    throws BAD_OPERATION
  {
    portServer s = getPortServer(PARALLEL);
    return s.s_port;
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
    java.lang.Object identity)
  {
    sharedPortServer shared = (sharedPortServer) identities.get(identity);
    if (shared == null)
      {
        shared = (sharedPortServer) getPortServer(SEQUENTIAL);
        identities.put(identity, shared);
        if (running)
          {
            shared.start();
          }
      }

    Connected_objects.cObject ref = connected_objects.add(key, object,
      shared.s_port, identity);
    IOR ior = createIOR(ref);
    prepareObject(object, ior);
  }

  /**
   * In this type of ORB, the service is started by {@link #getPortServer}. The
   * method below is not in use and should return without action.
   */
  public void startService(IOR ior)
  {
  }

  /**
   * Set parameters (additionally search for the port range property).
   */
  protected void set_parameters(Applet applet, Properties props)
  {
    super.set_parameters(applet, props);
    String lp = applet.getParameter(LISTENER_PORT);
    if (lp != null)
      setPortRange(lp);
  }

  /**
   * Set parameters (additionally search for the port range property).
   */
  protected void set_parameters(String[] args, Properties props)
  {
    super.set_parameters(args, props);
    String lp = null;

    String lpKey = "-" + LISTENER_PORT;

    if (args != null)
      if (args.length >= 2)
        {
          for (int i = 0; i < args.length - 1; i++)
            if (args[i].equals(lpKey))
              lp = args[i + 1];
        }

    if (lp != null)
      setPortRange(lp);

  }

  /**
   * Additionally set the port range property, when applicable.
   */
  protected void useProperties(Properties props)
  {
    super.useProperties(props);
    String lp = props.getProperty(LISTENER_PORT);
    if (lp != null)
      setPortRange(lp);
  }

}
