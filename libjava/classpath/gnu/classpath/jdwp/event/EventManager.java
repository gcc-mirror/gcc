/* EventManager.java -- event management and notification infrastructure
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


package gnu.classpath.jdwp.event;

import gnu.classpath.jdwp.VMVirtualMachine;
import gnu.classpath.jdwp.exception.InvalidEventTypeException;

import java.util.Collection;
import java.util.Hashtable;
import java.util.Iterator;

/**
 * Manages event requests and filters event notifications.
 *
 * The purpose of this class is actually two-fold:
 * 
 * 1) Maintain a list of event requests from the debugger
 * 2) Filter event notifications from the VM
 * 
 * If an event request arrives from the debugger, the back-end will
 * call {@link #reqestEvent}, which will first check for a valid event.
 * If it is valid, <code>EventManager</code> will record the request
 * internally and register the event with the virtual machine, which may
 * choose to handle the request itself (as is likely the case with
 * breakpoints and other execution-related events), or it may decide to
 * allow the <code>EventManager</code> to handle notifications and all
 * filtering (which is convenient for other events such as class (un)loading).
 * 
 * @author Keith Seitz  (keiths@redhat.com)
 */
public class EventManager
{
  // Single instance
  private static EventManager _instance = new EventManager ();

  // maps event (EVENT_*) to lists of EventRequests
  private Hashtable _requests = null;

  /**
   * Returns an instance of the event manager
   *
   * @return the event manager
   */
  public static EventManager getDefault ()
  {
    return _instance;
  }

  // Private constructs a new <code>EventManager</code>
  private EventManager ()
  {
    _requests = new Hashtable ();

    // Add lists for all the event types
    _requests.put (new Byte (EventRequest.EVENT_SINGLE_STEP),
		   new Hashtable ());
    _requests.put (new Byte (EventRequest.EVENT_BREAKPOINT),
		   new Hashtable ());
    _requests.put (new Byte (EventRequest.EVENT_FRAME_POP),
		   new Hashtable ());
    _requests.put (new Byte (EventRequest.EVENT_EXCEPTION),
		   new Hashtable ());
    _requests.put (new Byte (EventRequest.EVENT_USER_DEFINED),
		   new Hashtable ());
    _requests.put (new Byte (EventRequest.EVENT_THREAD_START),
		   new Hashtable ());
    _requests.put (new Byte (EventRequest.EVENT_THREAD_END),
		   new Hashtable ());
    _requests.put (new Byte (EventRequest.EVENT_CLASS_PREPARE),
		   new Hashtable ());
    _requests.put (new Byte (EventRequest.EVENT_CLASS_UNLOAD),
		   new Hashtable ());
    _requests.put (new Byte (EventRequest.EVENT_CLASS_LOAD),
		   new Hashtable ());
    _requests.put (new Byte (EventRequest.EVENT_FIELD_ACCESS),
		   new Hashtable ());
    _requests.put (new Byte (EventRequest.EVENT_FIELD_MODIFY),
		   new Hashtable ());
    _requests.put (new Byte (EventRequest.EVENT_METHOD_ENTRY),
		   new Hashtable ());
    _requests.put (new Byte (EventRequest.EVENT_METHOD_EXIT),
		   new Hashtable ());
    _requests.put (new Byte (EventRequest.EVENT_VM_INIT),
		   new Hashtable ());
    _requests.put (new Byte (EventRequest.EVENT_VM_DEATH),
		   new Hashtable ());

    // Add auto-generated event notifications
    // only two: VM_INIT, VM_DEATH
    try
      {
	requestEvent (new EventRequest (0,
					EventRequest.EVENT_VM_INIT,
					EventRequest.SUSPEND_NONE));
	requestEvent (new EventRequest (0,
					EventRequest.EVENT_VM_DEATH,
					EventRequest.SUSPEND_NONE));
      }
    catch (InvalidEventTypeException e)
      {
	// This can't happen
      }
  }

  /**
   * Returns a request for the given event. This method will only
   * be used if the <code>EventManager</code> is handling event filtering.
   *
   * @param  event  the event
   * @return request that was interested in this event
   *         or <code>null</code> if none (and event should not be sent)
   * @throws IllegalArgumentException for invalid event kind
   */
  public EventRequest getEventRequest (Event event)
  {
    EventRequest interestedRequest = null;
    Hashtable requests;
    Byte kind = new Byte (event.getEventKind ());
    requests = (Hashtable) _requests.get (kind);
    if (requests == null)
      {
	// Did not get a valid event type
	throw new IllegalArgumentException ("invalid event kind: " + kind);
      }
    boolean match = false;

    // Loop through the requests. Must look at ALL requests in order
    // to evaluate all filters (think count filter).
    // TODO: What if multiple matches? Spec isn't so clear on this.
    Iterator rIter = requests.values().iterator ();
    while (rIter.hasNext ())
      {
	EventRequest request = (EventRequest) rIter.next ();
	if (request.matches (event))
	  interestedRequest = request;
      }

    return interestedRequest;
  }

  /**
   * Requests monitoring of an event.
   *
   * The debugger registers for event notification through
   * an event filter. If no event filter is specified for an event
   * in the VM, it is assumed that the debugger is not interested in
   * receiving notifications of this event.
   *
   * The virtual machine will be notified of the request.
   *
   * @param request  the request to monitor
   * @throws InvalidEventTypeException for invalid event kind
   */
  public void requestEvent (EventRequest request)
    throws InvalidEventTypeException
  {
    // Add request to request list
    Hashtable requests;
    Byte kind = new Byte (request.getEventKind ());
    requests = (Hashtable) _requests.get (kind);
    if (requests == null)
      {
	// Did not get a valid event type
	throw new InvalidEventTypeException (request.getEventKind ());
      }

    // Register the event with the VM
    VMVirtualMachine.registerEvent (request);
    requests.put (new Integer (request.getId ()), request);
  }

  /**
   * Deletes the given request from the management table
   *
   * @param  kind  the event kind
   * @param  id    the ID of the request to delete
   * @throws IllegalArgumentException for invalid event kind
   */
  public void deleteRequest (byte kind, int id)
  {
    Hashtable requests;
    requests = (Hashtable) _requests.get (new Byte (kind));
    if (requests == null)
      {
	// Did not get a valid event type
	throw new IllegalArgumentException ("invalid event kind: " + kind);
      }

    Integer iid = new Integer (id);
    EventRequest request = (EventRequest) requests.get (iid);
    if (request != null)
      {
	VMVirtualMachine.unregisterEvent (request);
	requests.remove (iid);
      }
  }

  /**
   * Clears all the requests for a given event
   *
   * @param  kind  the event kind
   * @throws IllegalArgumentException for invalid event kind
   */
  public void clearRequests (byte kind)
  {
    Hashtable requests = (Hashtable) _requests.get (new Byte (kind));
    if (requests == null)
      {
	// Did not get a valid event type
	throw new IllegalArgumentException ("invalid event kind: " + kind);
      }

    VMVirtualMachine.clearEvents (kind);
    requests.clear ();
  }

  /**
   * Returns a given event request for an event
   *
   * @param  kind  the kind of event for the request
   * @param  id    the integer request id to return
   * @return  the request for the given event kind with the given id
   *          (or <code>null</code> if not found)
   * @throws IllegalArgumentException for invalid event kind
   */
  public EventRequest getRequest (byte kind, int id)
  {
    Hashtable requests = (Hashtable) _requests.get (new Byte (kind));
    if (requests == null)
      {
	// Did not get a valid event type
	throw new IllegalArgumentException ("invalid event kind: " + kind);
      }

    return (EventRequest) requests.get (new Integer (id));
  }

  /**
   * Returns all requests of the given event kind
   *
   * @param  kind  the event kind
   * @returns a <code>Collection</code> of all the registered requests
   * @throws IllegalArgumentException for invalid event kind
   */
  public Collection getRequests (byte kind)
  {
    Hashtable requests = (Hashtable) _requests.get (new Byte (kind));
    if (requests == null)
      {
	// Did not get a valid event type
	throw new IllegalArgumentException ("invalid event kind: " + kind);
      }
    
    return requests.values ();
  }
}
