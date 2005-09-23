/* Location.java -- class to read/write JDWP locations
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
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */


package gnu.classpath.jdwp.util;

import gnu.classpath.jdwp.VMIdManager;
import gnu.classpath.jdwp.exception.JdwpException;
import gnu.classpath.jdwp.id.ClassReferenceTypeId;
import gnu.classpath.jdwp.id.ObjectId;

import java.io.DataOutputStream;
import java.io.IOException;
import java.lang.reflect.Method;
import java.nio.ByteBuffer;

/**
 * A class to read/write JDWP locations.
 * 
 * @author Aaron Luchko <aluchko@redhat.com>
 */
public class Location
{

  private ClassReferenceTypeId crti;

  private int index;

  private byte tag;

  private ObjectId mid;

  /**
   * Create a location with the given parameters.
   * 
   * @param tag the type of construct the location is in
   * @param clazz the class the location is in
   * @param meth the Method
   * @param index location in the method
   * @throws JdwpException
   */
  public Location(byte tag, Class clazz, Method meth, int index)
      throws JdwpException
  {
    this.tag = tag;
    this.crti = 
      (ClassReferenceTypeId) VMIdManager.getDefault().getReferenceTypeId(clazz);
    this.mid = VMIdManager.getDefault().getObjectId(meth);
    this.index = index;
  }

  /**
   * Read a location from the given bytebuffer, consists of a TAG (byte),
   * followed by a ReferenceTypeId, a MethodId and an index (int).
   * 
   * @param bb this holds the location
   * @throws IOException
   * @throws JdwpException
   */
  public Location(ByteBuffer bb) throws IOException, JdwpException
  {
    this.tag = bb.get();
    this.crti = 
      (ClassReferenceTypeId) VMIdManager.getDefault().readReferenceTypeId(bb);
    this.mid = VMIdManager.getDefault().readObjectId(bb);
    this.index = bb.getInt();
  }

  /**
   * Write the given location to an output stream.
   * 
   * @param os stream to write to
   * @throws IOException
   */
  public void write(DataOutputStream os) throws IOException
  {
    os.writeByte(tag);
    crti.write(os);
    mid.write(os);
    os.writeInt(index);
  }
}
