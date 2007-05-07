/* Event.java -- a base class for all event types
   Copyright (C) 2005, 2007 Free Software Foundation

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


package gnu.classpath.jdwp.event;

import gnu.classpath.jdwp.JdwpConstants;
import gnu.classpath.jdwp.transport.JdwpCommandPacket;
import gnu.classpath.jdwp.transport.JdwpPacket;

import java.io.DataOutputStream;
import java.io.IOException;

/**
 * This class is a base class for all VM->debugger event
 * notifications.
 *
 * @author Keith Seitz  (keiths@redhat.com)
 */
public abstract class Event
{
  /**
   * The class of the object in which the event occurred
   */
  public static final int EVENT_CLASS = 1;
  
  /**
   * The thread where the event occurred
   */
  public static final int EVENT_THREAD = 2;
  
  /**
   * The location where an event occurred
   */
  public static final int EVENT_LOCATION = 3;
  
  /**
   * The instance of the class where the event occurred
   */
  public static final int EVENT_INSTANCE = 4;
  
  /**
   * The field acted on by an event
   */
  public static final int EVENT_FIELD = 5;

  /**
   * The class of the exception for ExceptionEvent
   */
  public static final int EVENT_EXCEPTION_CLASS = 6;

  /**
   * Whether this exception was caught (only valid for ExceptionEvents)
   */
  public static final int EVENT_EXCEPTION_CAUGHT = 7;
  
  // The kind of event represented by this event
  private byte _eventKind;

  /**
   * Constructs an <code>Event</code> of the given kind
   *
   * @param kind  the type of event
   */
  public Event (byte kind)
  {
    _eventKind = kind;
  }

  /**
   * Returns the event type of this event
   *
   * @returns  the event kind
   */
  public byte getEventKind ()
  {
    return _eventKind;
  }

  /**
   * Abstract function used by implementing classes to fill in the
   * event-specific data. Note that request ID is automatically added
   * by this class (since it appears in all event notifications).
   *
   * @param outStream  the stream to which to write data
   */
  protected abstract void _writeData (DataOutputStream outStream)
    throws IOException;

  /**
   * Returns a specific filtering parameter for this event. For example,
   * most events may be filtered by thread. Consequently, a call to this
   * method with <code>ThreadId.class</code> should return a
   * <code>Thread</code>.
   *
   * @param type  the type of parameter to return
   * @returns the parameter (not the ID) or <code>null</code> if none is
   *          is defined for this event
   */
  public abstract Object getParameter (int type);

  /**
   * Converts the events into to a single JDWP Event.COMPOSITE packet
   *
   * @param dos     the stream to which to write data
   * @param events  the events to package into the packet
   * @param requests the corresponding event requests
   * @param suspendPolicy the suspend policy enforced by the VM
   * @returns a <code>JdwpPacket</code> of the events
   */
  public static JdwpPacket toPacket (DataOutputStream dos,
				     Event[] events,
				     EventRequest[] requests,
				     byte suspendPolicy)
  {
    JdwpPacket pkt;
    try
      {
	dos.writeByte (suspendPolicy);
	dos.writeInt (events.length);
	for (int i = 0; i < events.length; ++i)
	  _toData (dos, events[i], requests[i]);

	pkt
	  = new JdwpCommandPacket (JdwpConstants.CommandSet.Event.CS_VALUE,
				   JdwpConstants.CommandSet.Event.COMPOSITE);
      }
    catch (IOException ioe)
      {
	pkt = null;
      }

    return pkt;
  }

  // Helper function for toPacket
  private static void _toData (DataOutputStream dos, Event event,
			       EventRequest request)
    throws IOException
  {
    dos.writeByte (event._eventKind);
    dos.writeInt (request.getId ());
    event._writeData (dos);
  }
}
