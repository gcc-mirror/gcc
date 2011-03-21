/* EventRequest.java -- an event request from the debugger
   Copyright (C) 2005, 2006 Free Software Foundation

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
import gnu.classpath.jdwp.event.filters.*;
import gnu.classpath.jdwp.exception.JdwpIllegalArgumentException;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;

/**
 * A class which represents a request by the debugger for an event
 * in the VM. <code>EventRequest</code>s usually have event filters
 * associated with them, which allow the debugger to specify conditions
 * under which the notification should be sent (specific thread, specific
 * class, ignore count, etc).
 *
 * @author Keith Seitz  (keiths@redhat.com)
 */
public class EventRequest
{
  /*
   * Event types
   */

  /**
   * Single step event
   */
  public static final byte EVENT_SINGLE_STEP =
    JdwpConstants.EventKind.SINGLE_STEP;

  /**
   * Breakpoint event
   */
  public static final byte EVENT_BREAKPOINT =
    JdwpConstants.EventKind.BREAKPOINT;

  /**
   * Frame pop event
   */
  public static final byte EVENT_FRAME_POP =
    JdwpConstants.EventKind.FRAME_POP;

  /**
   * Exception event
   */
  public static final byte EVENT_EXCEPTION =
    JdwpConstants.EventKind.EXCEPTION;

  /**
   * User-defined event
   */
  public static final byte EVENT_USER_DEFINED =
    JdwpConstants.EventKind.USER_DEFINED;

  /**
   * Thread start event
   */
  public static final byte EVENT_THREAD_START =
    JdwpConstants.EventKind.THREAD_START;

  /**
   * Thread end/death event
   */
  public static final byte EVENT_THREAD_END =
    JdwpConstants.EventKind.THREAD_END;

  /**
   * Class prepare event
   */
  public static final byte EVENT_CLASS_PREPARE =
    JdwpConstants.EventKind.CLASS_PREPARE;

  /**
   * Class unload event
   */
  public static final byte EVENT_CLASS_UNLOAD =
    JdwpConstants.EventKind.CLASS_UNLOAD;

  /**
   * Class load event
   */
  public static final byte EVENT_CLASS_LOAD =
    JdwpConstants.EventKind.CLASS_LOAD;

  /**
   * Field access event
   */
  public static final byte EVENT_FIELD_ACCESS =
    JdwpConstants.EventKind.FIELD_ACCESS;

  /**
   * Field modify event
   */
  public static final byte EVENT_FIELD_MODIFY =
    JdwpConstants.EventKind.FIELD_MODIFICATION;

  /**
   * Method entry event
   */
  public static final byte EVENT_METHOD_ENTRY =
    JdwpConstants.EventKind.METHOD_ENTRY;

  /**
   * Method exit event
   */
  public static final byte EVENT_METHOD_EXIT =
    JdwpConstants.EventKind.METHOD_EXIT;

  /**
   * Virtual machine initialization/start
   */
  public static final byte EVENT_VM_INIT =
    JdwpConstants.EventKind.VM_INIT;

  /**
   * Virutal machine death
   */
  public static final byte EVENT_VM_DEATH =
    JdwpConstants.EventKind.VM_DEATH;


  /*
   * Suspend policies
   */

  /**
   * Do not suspend any threads
   */
  public static final byte SUSPEND_NONE =
    JdwpConstants.SuspendPolicy.NONE;

  /**
   * Suspend the thread in which the event occurred
   */
  public static final byte SUSPEND_THREAD =
    JdwpConstants.SuspendPolicy.EVENT_THREAD;

  /**
   * Suspend all threads
   */
  public static final byte SUSPEND_ALL =
    JdwpConstants.SuspendPolicy.ALL;

  // ID of last EventRequest
  private static int _last_id = 0;
  private static Object _idLock = new Object ();

  // A list of filters
  private LinkedList _filters;

  // The ID of this request
  private int _id;

  // The suspend policy to enforce when this event occurs
  private byte _suspendPolicy;

  // Kind of event requested
  private byte _kind;

  /**
   * Construct a new <code>EventRequest</code>
   *
   * @param kind           the kind of event requested
   * @param suspendPolicy  how to suspend threads when event occurs
   */
  public EventRequest (byte kind, byte suspendPolicy)
  {
    _filters = new LinkedList ();
    synchronized (_idLock)
      {
        _id = ++_last_id;
      }
    _kind = kind;
    _suspendPolicy = suspendPolicy;
  }

  /**
   * Construct a new <code>EventRequest</code> with the given ID
   *
   * @param id             the id of the request to create
   * @param kind           the kind of event requested
   * @param suspendPolicy  how to suspend threads when event occurs
   */
  public EventRequest (int id, byte kind, byte suspendPolicy)
  {
    _filters = new LinkedList ();
    _kind = kind;
    _suspendPolicy = suspendPolicy;
  }

  /**
   * Creates a new event filter, adding it to this request
   *
   * @param  filter  the filter to add
   * @throws JdwpIllegalArgumentException if an invalid or illegal filter
   *         is added to the request
   */
  public void addFilter (IEventFilter filter)
    throws JdwpIllegalArgumentException
  {
    // Check validity of filter for this request
    boolean valid = true;

    Class clazz = filter.getClass ();
    if (clazz == ClassExcludeFilter.class)
      {
        if (_kind == EVENT_THREAD_START
            || _kind == EVENT_THREAD_END)
          valid = false;
      }
    else if (clazz == ClassMatchFilter.class)
      {
        if (_kind == EVENT_THREAD_START
            || _kind == EVENT_THREAD_END)
          valid = false;
      }
    else if (clazz == ClassOnlyFilter.class)
      {
        if (_kind == EVENT_CLASS_UNLOAD
            || _kind == EVENT_THREAD_START
            || _kind == EVENT_THREAD_END)
          valid = false;
      }
    else if (clazz == ConditionalFilter.class)
      {
        // JDWP 1.4 does not say much about this
      }
    else if (clazz == CountFilter.class)
      {
        // may be used with any event
      }
    else if (clazz == ExceptionOnlyFilter.class)
      {
        if (_kind != EVENT_EXCEPTION)
          valid = false;
      }
    else if (clazz == FieldOnlyFilter.class)
      {
        if (_kind != EVENT_FIELD_ACCESS
            && _kind != EVENT_FIELD_MODIFY)
          valid = false;
      }
    else if (clazz == InstanceOnlyFilter.class)
      {
        if (_kind == EVENT_CLASS_PREPARE
            || _kind == EVENT_CLASS_UNLOAD
            || _kind == EVENT_THREAD_START
            || _kind == EVENT_THREAD_END)
          valid = false;
      }
    else if (clazz == LocationOnlyFilter.class)
      {
        if (_kind != EVENT_BREAKPOINT
            && _kind != EVENT_FIELD_ACCESS
            && _kind != EVENT_FIELD_MODIFY
            && _kind != EVENT_SINGLE_STEP
            && _kind != EVENT_EXCEPTION)
          valid = false;
      }
    else if (clazz == StepFilter.class)
      {
        if (_kind != EVENT_SINGLE_STEP)
          valid = false;
      }
    else if (clazz == ThreadOnlyFilter.class)
      {
        if (_kind == EVENT_CLASS_UNLOAD)
          valid = false;
      }

    if (!valid)
      {
        String msg = ("cannot use " + filter.getClass ().getName ()
                      + " with class unload events");
        throw new JdwpIllegalArgumentException (msg);
      }

    // Add filter to list
    _filters.add (filter);
  }

  /**
   * Returns the filters attached to this request
   */
  public Collection getFilters ()
  {
    return _filters;
  }

  /**
   * Returns the suspend policy for this request
   */
  public byte getSuspendPolicy ()
  {
    return _suspendPolicy;
  }

  /**
   * Returns the request id of this request
   */
  public int getId ()
  {
    return _id;
  }

  /**
   * Sets the id of the request (used for auto-generated events)
   */
  public void setId (int id)
  {
    _id = id;
  }

  /**
   * Returns the kind of event for this request
   */
  public byte getEventKind ()
  {
    return _kind;
  }

  /**
   * Determines whether the given event matches this request
   *
   * @param  theEvent  the event to compare to
   */
  public boolean matches (Event theEvent)
  {
    boolean matches = true;

    // Loop through filters; all must match
    // Note that we must allow EVERY filter to evaluate. This way
    // things like CountFilter will work.
    Iterator iter = _filters.iterator ();
    while (iter.hasNext ())
      {
        IEventFilter filter = (IEventFilter) iter.next ();
        if (!filter.matches (theEvent))
          matches = false;
      }

    return matches;
  }
}
