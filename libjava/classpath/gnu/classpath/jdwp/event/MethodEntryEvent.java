/* MethodEntryEvent.java -- an event specifying that a method has been invoked
 Copyright (C) 2006 Free Software Foundation, Inc.

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


package gnu.classpath.jdwp.event;

import gnu.classpath.jdwp.JdwpConstants;
import gnu.classpath.jdwp.VMIdManager;
import gnu.classpath.jdwp.id.ThreadId;
import gnu.classpath.jdwp.util.Location;

import java.io.DataOutputStream;
import java.io.IOException;

/**
 * Notification from the VM that that a method has been invoked
 * 
 * @author Kyle Galloway (kgallowa@redhat.com)
 */
public class MethodEntryEvent
    extends Event
{
  // The thread where the event occurred
  private Thread _thread;

  // the location where the event occurred
  private Location _location;

  //object instance
  private Object _instance;

  /**
   * Constructs a new <code>MethodEntryEvent</code>
   * 
   * @param thread the thread where the exception occurred
   * @param location the location single stepped to
   * @param instance instance from which the method was called
   */
  public MethodEntryEvent(Thread thread, Location location, Object instance)
  {
    super(JdwpConstants.EventKind.METHOD_ENTRY);
    _thread = thread;
    _location = location;
    _instance = instance;
  }

  /**
   * Returns a specific filtering parameter for this event. Valid types are
   * thread and location
   * 
   * @param type the type of parameter desired
   * @returns the desired parameter or null
   */
  public Object getParameter(int type)
  {
    if (type == EVENT_THREAD)
      return _thread;
    else if (type == EVENT_LOCATION)
      return _location;
    else if (type == EVENT_INSTANCE)
      return _instance;
    else if (type == EVENT_CLASS)
      return _instance.getClass();

    return null;
  }

  /**
   * Writes the event to the given stream
   * 
   * @param outStream the output stream to write the event to
   * @throws IOException
   */
  protected void _writeData(DataOutputStream outStream) 
    throws IOException
  {
    VMIdManager idm = VMIdManager.getDefault();
    ThreadId tid = (ThreadId) idm.getObjectId(_thread);

    tid.write(outStream);
    _location.write(outStream);
  }

}
