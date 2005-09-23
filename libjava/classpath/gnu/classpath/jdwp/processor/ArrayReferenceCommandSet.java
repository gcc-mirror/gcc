/* ArrayReferenceCommandSet.java -- class to implement the Array
   Reference Command Set
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


package gnu.classpath.jdwp.processor;

import gnu.classpath.jdwp.JdwpConstants;
import gnu.classpath.jdwp.exception.InvalidObjectException;
import gnu.classpath.jdwp.exception.JdwpException;
import gnu.classpath.jdwp.exception.JdwpInternalErrorException;
import gnu.classpath.jdwp.exception.NotImplementedException;
import gnu.classpath.jdwp.id.ObjectId;
import gnu.classpath.jdwp.util.Value;

import java.io.DataOutputStream;
import java.io.IOException;
import java.lang.reflect.Array;
import java.nio.ByteBuffer;

/**
 * A class representing the ArrayReference Command Set.
 * 
 * @author Aaron Luchko <aluchko@redhat.com>
 */
public class ArrayReferenceCommandSet
  extends CommandSet
{
  public boolean runCommand(ByteBuffer bb, DataOutputStream os, byte command)
    throws JdwpException
  {
    try
      {
        switch (command)
          {
          case JdwpConstants.CommandSet.ArrayReference.LENGTH:
            executeLength(bb, os);
            break;
          case JdwpConstants.CommandSet.ArrayReference.GET_VALUES:
            executeGetValues(bb, os);
            break;
          case JdwpConstants.CommandSet.ArrayReference.SET_VALUES:
            executeSetValues(bb, os);
            break;
          default:
            throw new NotImplementedException("Command " + command +
              " not found in Array Reference Command Set.");
          }
      }
    catch (IOException ex)
      {
        // The DataOutputStream we're using isn't talking to a socket at all
        // So if we throw an IOException we're in serious trouble
        throw new JdwpInternalErrorException(ex);
      }
    return true;
  }

  private void executeLength(ByteBuffer bb, DataOutputStream os)
    throws InvalidObjectException, IOException
  {
    ObjectId oid = idMan.readObjectId(bb);
    Object array = oid.getObject();
    os.writeInt(Array.getLength(array));
  }

  private void executeGetValues(ByteBuffer bb, DataOutputStream os)
    throws JdwpException, IOException
  {
    ObjectId oid = idMan.readObjectId(bb);
    Object array = oid.getObject();
    int first = bb.getInt();
    int length = bb.getInt();

    // We need to write out the byte signifying the type of array first
    Class clazz = array.getClass().getComponentType();

    // Uugh, this is a little ugly but it's the only time we deal with
    // arrayregions
    if (clazz == byte.class)
      os.writeByte(JdwpConstants.Tag.BYTE);
    else if (clazz == char.class)
      os.writeByte(JdwpConstants.Tag.CHAR);
    else if (clazz == float.class)
      os.writeByte(JdwpConstants.Tag.FLOAT);
    else if (clazz == double.class)
      os.writeByte(JdwpConstants.Tag.DOUBLE);
    else if (clazz == int.class)
      os.writeByte(JdwpConstants.Tag.BYTE);
    else if (clazz == long.class)
      os.writeByte(JdwpConstants.Tag.LONG);
    else if (clazz == short.class)
      os.writeByte(JdwpConstants.Tag.SHORT);
    else if (clazz == void.class)
      os.writeByte(JdwpConstants.Tag.VOID);
    else if (clazz == boolean.class)
      os.writeByte(JdwpConstants.Tag.BOOLEAN);
    else if (clazz.isArray())
      os.writeByte(JdwpConstants.Tag.ARRAY);
    else if (String.class.isAssignableFrom(clazz))
      os.writeByte(JdwpConstants.Tag.STRING);
    else if (Thread.class.isAssignableFrom(clazz))
      os.writeByte(JdwpConstants.Tag.THREAD);
    else if (ThreadGroup.class.isAssignableFrom(clazz))
      os.writeByte(JdwpConstants.Tag.THREAD_GROUP);
    else if (ClassLoader.class.isAssignableFrom(clazz))
      os.writeByte(JdwpConstants.Tag.CLASS_LOADER);
    else if (Class.class.isAssignableFrom(clazz))
      os.writeByte(JdwpConstants.Tag.CLASS_OBJECT);
    else
      os.writeByte(JdwpConstants.Tag.OBJECT);

    // Write all the values, primitives should be untagged and Objects must be
    // tagged
    for (int i = first; i < first + length; i++)
      {
        Object value = Array.get(array, i);
        if (clazz.isPrimitive())
          Value.writeUntaggedValue(os, value);
        else
          Value.writeTaggedValue(os, value);
      }
  }

  private void executeSetValues(ByteBuffer bb, DataOutputStream os)
    throws IOException, JdwpException
  {
    ObjectId oid = idMan.readObjectId(bb);
    Object array = oid.getObject();
    int first = bb.getInt();
    int length = bb.getInt();
    Class type = array.getClass().getComponentType();
    for (int i = first; i < first + length; i++)
      {
        Object value = Value.getUntaggedObj(bb, type);
        Array.set(array, i, value);
      }
  }
}
