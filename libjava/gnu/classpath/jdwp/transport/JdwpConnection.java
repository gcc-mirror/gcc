/* JdwpConnection.java -- A JDWP-speaking connection
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


package gnu.classpath.jdwp.transport;

import gnu.classpath.jdwp.Jdwp;
import gnu.classpath.jdwp.event.Event;
import gnu.classpath.jdwp.event.EventRequest;

import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;

/**
 * A connection via some transport to some JDWP-speaking entity.
 * This is also a thread which handles all communications to/from
 * the debugger. While access to the transport layer may be accessed by
 * several threads, start-up and initialization should not be allowed
 * to occur more than once.
 *
 * <p>This class is also a thread that is responsible for pulling
 * packets off the wire and sticking them in a queue for packet
 * processing threads.
 * 
 * @author Keith Seitz (keiths@redhat.com)
 */
public class JdwpConnection
  extends Thread
{
  // The JDWP handshake
  private static final byte[] _HANDSHAKE = {'J', 'D', 'W', 'P', '-', 'H', 'a',
					    'n', 'd', 's', 'h', 'a', 'k', 'e'};

  // Transport method
  private ITransport _transport;

  // Command queue
  private ArrayList _commandQueue;

  // Shutdown flag
  private boolean _shutdown;

  // Input stream from transport
  private DataInputStream _inStream;

  // Output stream from transprot
  private DataOutputStream _outStream;

  // A buffer used to construct the packet data
  private ByteArrayOutputStream _bytes;

  // A DataOutputStream for the byte buffer
  private DataOutputStream _doStream;

  /**
   * Creates a new <code>JdwpConnection</code> instance
   *
   * @param transport  the transport to use for communications
   */
  public JdwpConnection (ITransport transport)
  {
    _transport = transport;
    _commandQueue = new ArrayList ();
    _shutdown = false;
    _bytes = new ByteArrayOutputStream ();
    _doStream = new DataOutputStream (_bytes);
  }

  /**
   * Initializes the connection, including connecting
   * to socket or shared memory endpoint
   *
   * @throws TransportException if initialization fails
   */
  public void initialize ()
    throws TransportException
  {
    // Initialize transport (connect socket, e.g.)
    _transport.initialize ();

    // Do handshake
    try
      {
	_inStream = new DataInputStream (_transport.getInputStream ());
	_outStream = new DataOutputStream (_transport.getOutputStream ());
	_doHandshake ();
      }
    catch (IOException ioe)
      {
	throw new TransportException (ioe);
      }
  }

  /* Does the JDWP handshake -- this should not need synchronization
     because this is called by VM startup code, i.e., no packet
     processing threads have started yet. */
  private void _doHandshake ()
    throws IOException
  {
    // According to the spec, the handshake is always initiated by
    // the debugger, regardless of whether the JVM is in client mode or
    // server mode.

    // Wait for handshake from debugger
    byte[] hshake = new byte[_HANDSHAKE.length];
    _inStream.readFully (hshake, 0, _HANDSHAKE.length);

    if (Arrays.equals (hshake, _HANDSHAKE))
      {
	// Send reply handshake
	_outStream.write (_HANDSHAKE, 0, _HANDSHAKE.length);
	return;
      }
    else
      {
	throw new IOException ("invalid JDWP handshake (\"" + hshake + "\")");
      }
  }

  /**
   * Main run method for the thread. This thread loops waiting for
   * packets to be read via the connection. When a packet is complete
   * and ready for processing, it places the packet in a queue that can
   * be accessed via <code>getPacket</code>
   */
  public void run ()
  {
    while (!_shutdown)
      {
	try
	  {
	    _readOnePacket ();
	  }
	catch (IOException ioe)
	  {
	    /* IOException can occur for two reasons:
	       1. Lost connection with the other side
	       2. Transport was shutdown
	       In either case, we make sure that all of the
	       back-end gets shutdown. */
	    Jdwp.getInstance().shutdown ();
	  }
	catch (Throwable t)
	  {
	    System.out.println ("JdwpConnection.run: caught an exception: "
				+ t);
	    // Just keep going
	  }
      }
  }

  // Reads a single packet from the connection, adding it to the packet
  // queue when a complete packet is ready.
  private void _readOnePacket ()
    throws IOException
  {
    byte[] data = null;

    // Read in the packet
    int length = _inStream.readInt ();
    if (length < 11)
      {
	throw new IOException ("JDWP packet length < 11 (" 
			       + length + ")");
      }

    data = new byte[length];
    data[0] = (byte) (length >>> 24);
    data[1] = (byte) (length >>> 16);
    data[2] = (byte) (length >>> 8);
    data[3] = (byte) length;
    _inStream.readFully (data, 4, length - 4);

    JdwpPacket packet = JdwpPacket.fromBytes (data);
    if (packet != null)
      {
	synchronized (_commandQueue)
	  {
	    _commandQueue.add (packet);
	    _commandQueue.notifyAll ();
	  }
      }
  }

  /**
   * Returns a packet from the queue of ready packets
   *
   * @returns  a <code>JdwpPacket</code> ready for processing
   *           <code>null</code> when shutting down
   */
  public JdwpPacket getPacket ()
  {
    synchronized (_commandQueue)
      {
	while (_commandQueue.isEmpty ())
	  {
	    try
	      {
		_commandQueue.wait ();
	      }
	    catch (InterruptedException ie)
	      {
		/* PacketProcessor is interrupted
		   when shutting down */
		return null;
	      }
	  }

	return (JdwpPacket) _commandQueue.remove (0);
      }
  }

  /**
   * Send a packet to the debugger
   *
   * @param pkt a <code>JdwpPacket</code> to send
   * @throws IOException
   */
  public void sendPacket (JdwpPacket pkt)
    throws IOException
  {
    pkt.write (_outStream);
  }

  /**
   * Send an event notification to the debugger
   *
   * @param request  the debugger request that wanted this event
   * @param event    the event
   * @throws IOException
   */
  public void sendEvent (EventRequest request, Event event)
    throws IOException
  {
    JdwpPacket pkt;

    synchronized (_bytes)
      {
	_bytes.reset ();
	pkt = event.toPacket (_doStream, request);
	pkt.setData (_bytes.toByteArray ());
      }

    sendPacket (pkt);
  }

  /**
   * Shutdown the connection
   */
  public void shutdown ()
  {
    if (!_shutdown)
      {
	_transport.shutdown ();
	_shutdown = true;
	interrupt ();
      }
  }
}
