/* Jdwp.java -- Virtual machine to JDWP back-end programming interface
   Copyright (C) 2005 Free Software Foundation

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
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */


package gnu.classpath.jdwp;

import gnu.classpath.jdwp.event.Event;
import gnu.classpath.jdwp.event.EventManager;
import gnu.classpath.jdwp.event.EventRequest;
import gnu.classpath.jdwp.id.ThreadId;
import gnu.classpath.jdwp.processor.PacketProcessor;
import gnu.classpath.jdwp.transport.ITransport;
import gnu.classpath.jdwp.transport.JdwpConnection;
import gnu.classpath.jdwp.transport.TransportException;
import gnu.classpath.jdwp.transport.TransportFactory;

import java.io.IOException;
import java.security.AccessController;
import java.util.HashMap;

/**
 * Main interface from the virtual machine to the JDWP back-end.
 *
 * @author Keith Seitz (keiths@redhat.com)
 */
public class Jdwp
  extends Thread
{
  // The single instance of the back-end
  private static Jdwp _instance = null;

  /**
   * Are we debugging?
   */
  public static boolean isDebugging = false;

  // Packet processor
  private PacketProcessor _packetProcessor;
  private Thread _ppThread;

  // JDWP configuration properties
  private HashMap _properties;

  // The suspend property of the configure string
  // (-Xrunjdwp:..suspend=<boolean>)
  private static final String _PROPERTY_SUSPEND = "suspend";

  // User's main application thread
  private Thread _mainThread;

  // Connection to debugger
  private JdwpConnection _connection;

  // Are we shutting down the current session?
  private boolean _shutdown;

  // A thread group for the JDWP threads
  private ThreadGroup _group;

  /**
   * constructor
   */
  public Jdwp ()
  {
    _shutdown = false;
    isDebugging = true;
    _instance = this;
  }

  /**
   * Returns the JDWP back-end, creating an instance of it
   * if one does not already exist.
   */
  public static Jdwp getDefault ()
  {
    return _instance;
  }

  /**
   * Should the virtual machine suspend on startup?
   */
  public static boolean suspendOnStartup ()
  {
    Jdwp jdwp = getDefault ();
    if (jdwp != null)
      {
	String suspend = (String) jdwp._properties.get (_PROPERTY_SUSPEND);
	if (suspend != null && suspend.equals ("y"))
	  return true;
      }

    return false;
  }

  /**
   * Configures the back-end
   *
   * @param configArgs  a string of configury options
   * @param mainThread  the main application thread
   */
  public void configure (String configArgs, Thread mainThread)
  {
    _mainThread = mainThread;
    _processConfigury (configArgs);
  }

  // A helper function to initialize the transport layer
  private void _doInitialization ()
    throws TransportException
  {
    _group = new ThreadGroup ("JDWP threads");
    // initialize transport
    ITransport transport = TransportFactory.newInstance (_properties);
    _connection = new JdwpConnection (_group, transport);
    _connection.initialize ();
    _connection.start ();

    // Create processor
    _packetProcessor = new PacketProcessor (_connection);
    _ppThread = new Thread (_group, new Runnable ()
      {
	public void run ()
	{
	  AccessController.doPrivileged (_packetProcessor);
	}
      });
    _ppThread.start ();
  }

  /**
   * Shutdown the JDWP back-end
   *
   * NOTE: This does not quite work properly. See notes in 
   * run() on this subject (catch of InterruptedException).
   */
  public void shutdown ()
  {
    if (!_shutdown)
      {
	_packetProcessor.shutdown ();
	_ppThread.interrupt ();
	_connection.shutdown ();
	_shutdown = true;
	isDebugging = false;

	/* FIXME: probably need to check state of user's
	   program -- if it is suspended, we need to either
	   resume or kill them. */

	interrupt ();
      }
  }

  /**
   * Notify the debugger of an event. This method should not
   * be called if debugging is not active (but it would not
   * cause any harm). Places where event notifications occur
   * should check isDebugging before doing anything.
   *
   * The event is filtered through the event manager before being
   * sent.
   *
   * FIXME: Probably need logic to send multiple events
   * @param event the event to report
   */
  public static void notify (Event event)
  {
    Jdwp jdwp = getDefault ();
    if (jdwp != null)
      {
	EventManager em = EventManager.getDefault ();
	EventRequest request = em.getEventRequest (event);
	if (request != null)
	  sendEvent (request, event);
      }
  }
  
  /**
   * Sends the event to the debugger.
   *
   * This method bypasses the event manager's filtering.
   *
   * @param  request  the debugger request for the event
   * @param  event    the event to send
   */
  public static void sendEvent (EventRequest request, Event event)
  {
    Jdwp jdwp = getDefault ();
    if (jdwp != null)
      {
	try
	  {
	    // !! May need to implement send queue?
	    synchronized (jdwp._connection)
	      {
		jdwp._connection.sendEvent (request, event);
	      }
	    
	    // Follow suspend policy
	    jdwp._enforceSuspendPolicy (request.getSuspendPolicy ());
	  }
	catch (IOException ie)
	  {
	    System.out.println ("Jdwp.notify: caught exception: " + ie);
	  }
      }
  }

  // Helper function to enforce suspend policies on event notification
  private void _enforceSuspendPolicy (byte suspendPolicy)
  {
    switch (suspendPolicy)
      {
      case EventRequest.SUSPEND_NONE:
	// do nothing
	break;

      case EventRequest.SUSPEND_THREAD:
	VMVirtualMachine.suspendThread (this);
	break;

      case EventRequest.SUSPEND_ALL:
	VMVirtualMachine.suspendAllThreads ();
	break;
      }
  }

  public void run ()
  {
    try
      {
	_doInitialization ();

	_mainThread.start ();

	_mainThread.join ();
      }
    catch (InterruptedException ie)
      {
	/* Shutting down. If we're in server mode, we should
	   prepare for a new connection. Otherwise, we should
	   simply exit. */
	// FIXME
      }
    catch (Throwable t)
      {
	System.out.println ("Exception in JDWP back-end: " + t);
	System.exit (1);
      }
  }

  // A helper function to process the configure string "-Xrunjdwp:..."
  private void _processConfigury (String configString)
  {
    // Loop through configuration arguments looking for a
    // transport name
    _properties = new HashMap ();
    String[] options = configString.split (",");
    for (int i = 0; i < options.length; ++i)
      {
	String[] property = options[i].split ("=");
	if (property.length == 2)
	  _properties.put (property[0], property[1]);
	// ignore malformed options
      }
  }
}
