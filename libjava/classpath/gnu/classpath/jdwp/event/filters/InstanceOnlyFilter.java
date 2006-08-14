/* InstanceOnlyFilter.java -- filter on instance
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


package gnu.classpath.jdwp.event.filters;

import gnu.classpath.jdwp.event.Event;
import gnu.classpath.jdwp.exception.InvalidObjectException;
import gnu.classpath.jdwp.id.ObjectId;

/**
 * Restricts reported events to those whose active 'this' object is the
 * given object. Match value is the null object for static methods.
 * 
 * This modifier can be used with any event kind except class prepare,
 * class unload, thread start, and thread end. Introduced in JDWP version 1.4.
 *
 * @author Keith Seitz  (keiths@redhat.com)
 */
public class InstanceOnlyFilter
  implements IEventFilter
{
  private ObjectId _instance;

  /**
   * Constructs a new <code>InstanceOnlyFilter</code>.
   *
   * @param  oid  the object to which to restrict events (may be null)
   * @throws InvalidObjectException if Object is invalid
   */
  public InstanceOnlyFilter (ObjectId oid)
    throws InvalidObjectException
  {
    if (oid != null && oid.getReference().get () == null)
      throw new InvalidObjectException (oid.getId ());

    _instance = oid;
  }

  /**
   * Returns the instance to which to restrict events
   *
   * @return the object's ID
   */
  public ObjectId getInstance ()
  {
    return _instance;
  }

  /**
   * Does the given event match the filter?
   *
   * @param event  the <code>Event</code> to scrutinize
   */
  public boolean matches (Event event)
  {
    Object eventInstance = event.getParameter (Event.EVENT_INSTANCE);
    if (eventInstance != null)
      {
	Object myInstance = _instance.getReference().get ();
	return ((myInstance != null) && (myInstance == eventInstance));
      }

    return false;
  }
}
