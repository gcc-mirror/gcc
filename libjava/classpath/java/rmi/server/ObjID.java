/* ObjID.java -- Unique object id with respect to the given host.
   Copyright (c) 1996, 1997, 1998, 1999, 2004, 2006
   Free Software Foundation, Inc.

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


package java.rmi.server;

import java.io.DataInput;
import java.io.DataOutput;
import java.io.IOException;
import java.io.ObjectInput;
import java.io.ObjectOutput;
import java.io.Serializable;

/**
 * Represents the object identifier, unique for the host that generated it.
 * The ObjID contains inside the integer object identifier that, if needed,
 * may indicated that this is a reference to one of the well known objects
 * on that host (registry, activator or dgc) and the {@link UID} that
 * ensures uniqueness.
 */
public final class ObjID
    implements Serializable
{

  /**
   * Use serial version uid for interoperability.
   */
  static final long serialVersionUID = - 6386392263968365220L;

  /**
   * The object counter, which value is assigned when creating the ordinary
   * objects without the known object id. The counter is incremented each time
   * the new ObjID is constructed.
   */
  private static long next = 0x8000000000000000L;

  /**
   * The object to put the lock on when incrementing {@link #next}
   */
  private static final Object lock = ObjID.class;

  /**
   * Defines the ID of the naming service.
   */
  public static final int REGISTRY_ID = 0;

  /**
   * Defines the ID of the activator.
   */
  public static final int ACTIVATOR_ID = 1;

  /**
   * Defines the ID of the distributed garbage collector.
   */
  public static final int DGC_ID = 2;

  /**
   * The object Id (either well-known value or the value of the incrementing
   * object counter.
   */
  long objNum;

  /**
   * The object unique identifier, generated individually for each object.
   */
  UID space;

  /**
   * Create the new object id, unique for this host.
   */
  public ObjID()
  {
    synchronized (lock)
      {
        objNum = next++;
      }
    space = new UID();
  }

  /**
   * Create the new object id defining the well known remotely accessible
   * object, present in this host. The well - known objects are:
   * <ul>
   * <li>{@link #REGISTRY_ID} - RMI naming service.</li>
   * <li>{@link #ACTIVATOR_ID} - activator</li>
   * <li>{@link #DGC_ID} - distributed garbage collector (grants lease
   * durations to keep the object before it is garbage collected.</li>
   * </ul>
   *
   * @param id the well known object id, one of the above.
   */
  public ObjID(int id)
  {
    objNum = (long) id;
    space = new UID((short) 0);
  }

  /**
   * Write object id as long, then the object {@link UID}.
   */
  public void write(ObjectOutput out) throws IOException
  {
    DataOutput dout = (DataOutput) out;
    dout.writeLong(objNum);
    space.write(dout);
  }

  /**
   * Read object id (as long), then the object {@link UID}.
   */
  public static ObjID read(ObjectInput in) throws IOException
  {
    DataInput din = (DataInput) in;
    ObjID id = new ObjID();
    id.objNum = din.readLong();
    id.space = UID.read(din);
    return (id);
  }

  /**
   * Get the hashcode.
   */
  public int hashCode()
  {
    return space == null ? (int) objNum : space.hashCode() ^ (int) objNum;
  }

  /**
   * Compare for equality.
   */
  public boolean equals(Object obj)
  {
    if (obj instanceof ObjID)
      {
        ObjID that = (ObjID) obj;
        return that.objNum == objNum && eq(that.space, space);
      }
    else
      return false;
  }

  /**
   * Compare by .equals if both a and b are not null, compare directly if at
   * least one of them is null.
   */
  static final boolean eq(Object a, Object b)
  {
    if (a == null || b == null)
      return a == b;
    else
      return a.equals(b);
  }

  /**
   * Get the string representation.
   */
  public String toString()
  {
    return (objNum + ":" + space);
  }

}
