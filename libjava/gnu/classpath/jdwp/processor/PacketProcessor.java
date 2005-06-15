/* PacketProcessor.java -- a thread which processes command packets
   from the debugger
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
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */


package gnu.classpath.jdwp.processor;

import gnu.classpath.jdwp.Jdwp;
import gnu.classpath.jdwp.event.Event;
import gnu.classpath.jdwp.exception.JdwpException;
import gnu.classpath.jdwp.transport.JdwpConnection;
import gnu.classpath.jdwp.transport.JdwpCommandPacket;
import gnu.classpath.jdwp.transport.JdwpPacket;
import gnu.classpath.jdwp.transport.JdwpReplyPacket;

import java.io.IOException;

/**
 * This class is responsible for processing packets from the
 * debugger. It waits for an available packet from the connection
 * ({@link gnu.classpath.jdwp.transport.JdwpConnection}) and then
 * processes the packet and any reply.
 *
 * @author Keith Seitz (keiths@redhat.com)
 */
public class PacketProcessor
  extends Thread
{
  // The connection to the debugger
  private JdwpConnection _connection;
  
  // Shutdown this thread?
  private boolean _shutdown;

  /**
   * Constructs a new <code>PacketProcessor</code> object
   * Connection must be validated before getting here!
   *
   * @param con  the connection
   */
  public PacketProcessor (JdwpConnection con)
  {
    _connection = con;
    _shutdown = false;
  }

  /**
   * Main run routine for this thread. Will loop getting packets
   * from the connection and processing them.
   */
  public void run ()
  {
    while (!_shutdown)
      {
	_processOnePacket ();
      }
  }

  /**
   * Shutdown the packet processor
   */
  public void shutdown ()
  {
    _shutdown = true;
    interrupt ();
  }

  // Helper function which actually does all the work of waiting
  // for a packet and getting it processed.
  private void _processOnePacket ()
  {
    JdwpPacket pkt = _connection.getPacket ();
    if (pkt instanceof JdwpReplyPacket)
      {
	// We're not supposed to get these from the debugger!
	// Drop it on the floor
	return;
      }

    if (pkt != null)
      {
	JdwpReplyPacket reply;
	try
	  {
	    // !! process packet here !!
	    reply = new JdwpReplyPacket (pkt, (short) 0);
	  }
	catch (JdwpException ex)
	  {
	    reply = new JdwpReplyPacket (pkt, ex.getErrorCode ());
	  }

	try
	  {
	    _connection.sendPacket (reply);
	  }
	catch (IOException ioe)
	  {
	    // Not much we can do...
	  }
      }
  }
}
