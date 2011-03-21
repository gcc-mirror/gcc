/* ClassPrepareEvent.java -- An event specifying that a class has been
   prepared by the virtual machine
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

import gnu.classpath.jdwp.JdwpConstants;
import gnu.classpath.jdwp.VMIdManager;
import gnu.classpath.jdwp.id.ReferenceTypeId;
import gnu.classpath.jdwp.id.ThreadId;
import gnu.classpath.jdwp.util.JdwpString;
import gnu.classpath.jdwp.util.Signature;

import java.io.DataOutputStream;
import java.io.IOException;

/**
 * "Notification of a class prepare in the target VM. See the JVM
 * specification for a definition of class preparation. Class prepare
 * events are not generated for primtiive classes (for example,
 * <code>java.lang.Integer.TYPE</code>)." -- JDWP 1.4.2
 *
 * @author Keith Seitz  (keiths@redhat.com)
 */
public class ClassPrepareEvent
  extends Event
{
  // The thread in which this event occurred
  private Thread _thread;

  // The class that was prepared
  private Class _class;

  // Prepare flags
  private int _status;

  /**
   * Class has been verified
   */
  public static final int STATUS_VERIFIED
      = JdwpConstants.ClassStatus.VERIFIED;

  /**
   * Class has been prepared
   */
  public static final int STATUS_PREPARED
      = JdwpConstants.ClassStatus.PREPARED;

  /**
   * Class has been initialized
   */
  public static final int STATUS_INITIALIZED
      = JdwpConstants.ClassStatus.INITIALIZED;

  /**
   * Error preparing class
   */
  public static final int STATUS_ERROR
      = JdwpConstants.ClassStatus.ERROR;

  /**
   * Constructs a new <code>ClassPrepareEvent</code>
   *
   * @param thread  thread in which event occurred
   * @param clazz   class which was prepared
   * @param flags   prepare status flags
   */
  public ClassPrepareEvent (Thread thread, Class clazz, int flags)
  {
    super (JdwpConstants.EventKind.CLASS_PREPARE);
    _thread = thread;
    _class = clazz;
    _status = flags;
  }

  /**
   * Returns a specific filtering parameter for this event.
   * Valid types are thread and class.
   *
   * @param type  the type of parameter desired
   * @returns the desired parameter or <code>null</code>
   */
  public Object getParameter (int type)
  {
    if (type == EVENT_THREAD)
      return _thread;
    else if (type == EVENT_CLASS)
      return _class;

    return null;
  }

  /**
   * Writes the event to the given stream
   *
   * @param outStream  the output stream to write the event to
   */
  protected void _writeData (DataOutputStream outStream)
    throws IOException
  {
    VMIdManager idm = VMIdManager.getDefault();
    ThreadId tid = (ThreadId) idm.getObjectId (_thread);
    ReferenceTypeId rid = idm.getReferenceTypeId (_class);

    tid.write (outStream);
    rid.writeTagged (outStream);
    JdwpString.writeString (outStream,
                            Signature.computeClassSignature (_class));
    outStream.writeInt (_status);
  }
}
