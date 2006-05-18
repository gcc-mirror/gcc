/* JdwpId.java -- base class for all object ID types
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


package gnu.classpath.jdwp.id;

import java.io.DataOutputStream;
import java.io.IOException;
import java.lang.ref.SoftReference;

/**
 * A baseclass for all object types reported to the debugger
 *
 * @author Keith Seitz  <keiths@redhat.com>
 */
public abstract class JdwpId
{
  /**
   * The size of an ID. The default is 8 bytes (a long).
   */
   public static final int SIZE = 8;

  /**
   * ID assigned to this object
   */
  protected long _id;

  /**
   * Tag of ID's type (see {@link gnu.classpath.jdwp.JdwpConstants.Tag})
   * for object-like IDs or the type tag (see {@link
   * gnu.classpath.jdwp.JdwpConstants.TypeTag}) for reference type IDs.
   */
  private byte _tag;

  /**
   * The object/class represented by this Id
   */
  protected SoftReference _reference;

  /**
   * Constructs an empty <code>JdwpId</code>
   */
  public JdwpId (byte tag)
  {
    _tag = tag;
  }

  /**
   * Sets the id for this object reference
   */
  public void setId (long id)
  {
    _id = id;
  }

  /**
   * Returns the id for this object reference
   */
  public long getId ()
  {
    return _id;
  }

  /**
   * Gets the object/class reference for this ID
   *
   * @returns a refernce to the object or class
   */
  public SoftReference getReference ()
  {
    return _reference;
  }

  /**
   * Sets the object/class reference for this ID
   *
   * @param ref a refernce to the object or class
   */
  public void setReference (SoftReference ref)
  {
    _reference = ref;
  }

  /**
   * Compares two object ids for equality. Two object ids
   * are equal if they point to the same type and contain to
   * the same id number. 
   */
  public boolean equals (JdwpId id)
  {
    return (id.getId () == getId ());
  }

  /**
   * Writes the contents of this type to the <code>DataOutputStream</code>
   * @param  outStream    the <code>DataOutputStream</code> to use
   * @throws IOException  when an error occurs on the <code>OutputStream</code>
   */
  public abstract void write (DataOutputStream outStream)
    throws IOException;

  /**
   * Writes the contents of this type to the output stream, preceded
   * by a one-byte tag for tagged object IDs or type tag for
   * reference type IDs.
   *
   * @param  outStream    the <code>DataOutputStream</code> to use
   * @throws IOException  when an error occurs on the <code>OutputStream</code>
   */
  public void writeTagged (DataOutputStream outStream)
    throws IOException
  {
    outStream.writeByte (_tag);
    write (outStream);
  }
}
