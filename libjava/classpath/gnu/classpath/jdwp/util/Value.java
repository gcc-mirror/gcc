/* Value.java -- class to read/write JDWP tagged and untagged values
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

import gnu.classpath.jdwp.JdwpConstants;
import gnu.classpath.jdwp.VMIdManager;
import gnu.classpath.jdwp.exception.InvalidFieldException;
import gnu.classpath.jdwp.exception.JdwpException;
import gnu.classpath.jdwp.exception.JdwpInternalErrorException;
import gnu.classpath.jdwp.exception.NotImplementedException;
import gnu.classpath.jdwp.id.ObjectId;

import java.io.DataOutputStream;
import java.io.IOException;
import java.nio.ByteBuffer;

/**
 * A class to read/write JDWP tagged and untagged values.
 * 
 * @author Aaron Luchko <aluchko@redhat.com>
 */
public class Value
{
  /**
   * Will write the given object as an untagged value to the DataOutputStream.
   * 
   * @param os write the value here
   * @param obj the Object to write
   * @throws IOException
   * @throws InvalidFieldException
   */  
  public static void writeUntaggedValue(DataOutputStream os, Object obj)
    throws JdwpException, IOException
  {
    writeValue(os, obj, false);
  }
  
  /**
   * Will write the given object as a tagged value to the DataOutputStream.
   * 
   * @param os write the value here
   * @param obj the Object to write
   * @throws IOException
   * @throws InvalidFieldException
   */
  public static void writeTaggedValue(DataOutputStream os, Object obj)
    throws JdwpException, IOException
  {
    writeValue(os, obj, true);
  }
  
  /**
   * Will write the given object as either a value or an untagged value to the
   * DataOutputStream.
   * 
   * @param os write the value here
   * @param obj the Object to write
   * @param tagged true if the value is tagged, false otherwise
   * @throws IOException
   * @throws InvalidFieldException
   */
  private static void writeValue(DataOutputStream os, Object obj,
                                boolean tagged)
    throws IOException, JdwpException
  {
    Class clazz = obj.getClass();
    if (clazz.isPrimitive())
      {
        if (clazz == byte.class)
          {
            if (tagged)
              os.writeByte(JdwpConstants.Tag.BYTE);
            os.writeByte(((Byte) obj).byteValue());
          }
        else if (clazz == char.class)
          {
            if (tagged)
              os.writeByte(JdwpConstants.Tag.CHAR);
            os.writeChar(((Character) obj).charValue());
          }
        else if (clazz == float.class)
          {
            if (tagged)
              os.writeByte(JdwpConstants.Tag.FLOAT);
            os.writeFloat(((Float) obj).floatValue());
          }
        else if (clazz == double.class)
          {
            if (tagged)
              os.writeByte(JdwpConstants.Tag.DOUBLE);
            os.writeDouble(((Double) obj).doubleValue());
          }
        else if (clazz == int.class)
          {
            if (tagged)
              os.writeByte(JdwpConstants.Tag.BYTE);
            os.writeInt(((Integer) obj).intValue());
          }
        else if (clazz == long.class)
          {
            if (tagged)
              os.writeByte(JdwpConstants.Tag.LONG);
            os.writeLong(((Long) obj).longValue());
          }
        else if (clazz == short.class)
          {
            if (tagged)
              os.writeByte(JdwpConstants.Tag.SHORT);
            os.writeInt(((Short) obj).shortValue());
          }
        else if (clazz == void.class)
          { // A 'void' has no data
            if (tagged)
              os.writeByte(JdwpConstants.Tag.VOID);
          }
        else if (clazz == boolean.class)
          {
            if (tagged)
              os.writeByte(JdwpConstants.Tag.BOOLEAN);
            os.writeBoolean(((Boolean) obj).booleanValue());
          }
        else
          { // This shouldn't be possible
            throw new JdwpInternalErrorException(
              "Field has invalid primitive!");
          }
      }
    else
      {
        // Object is an Object, not a primitive type wrapped in an object
        // Write the appropriate tag
        if (tagged)
          {
            if (clazz.isArray())
              os.writeByte(JdwpConstants.Tag.ARRAY);
            else if (obj instanceof String)
              os.writeByte(JdwpConstants.Tag.STRING);
            else if (obj instanceof Thread)
              os.writeByte(JdwpConstants.Tag.THREAD);
            else if (obj instanceof ThreadGroup)
              os.writeByte(JdwpConstants.Tag.THREAD_GROUP);
            else if (obj instanceof ClassLoader)
              os.writeByte(JdwpConstants.Tag.CLASS_LOADER);
            else if (obj instanceof Class)
              os.writeByte(JdwpConstants.Tag.CLASS_OBJECT);
            else
              os.writeByte(JdwpConstants.Tag.OBJECT);
          }
        ObjectId oid = VMIdManager.getDefault().getObjectId(obj);
        oid.write(os);
      }
  }

  /**
   * Reads the appropriate object for the tagged value contained in the 
   * ByteBuffer.
   * 
   * @param bb contains the Object
   * @return The Object referenced by the value
   * @throws JdwpException
   * @throws IOException
   */
  public static Object getObj(ByteBuffer bb)
    throws JdwpException, IOException
  {
    return getUntaggedObj(bb, bb.get());
  }

  /**
   * Reads the an object of the given Class from the untagged value contained
   * in the ByteBuffer.
   * 
   * @param bb contains the Object
   * @param type corresponds to the TAG of value to be read 
   * @return
   * @throws JdwpException
   * @throws IOException
   */
  public static Object getUntaggedObj(ByteBuffer bb, Class type)
    throws JdwpException, IOException
  {
    if (type.isPrimitive())
      {
        if (type == byte.class)
          return new Byte(bb.get());
        else if (type == char.class)
          return new Character(bb.getChar());
        else if (type == float.class)
          return new Float(bb.getFloat());
        else if (type == double.class)
          return new Double(bb.getDouble());
        else if (type == int.class)
          return new Integer(bb.getInt());
        else if (type == long.class)
          return new Long(bb.getLong());
        else if (type == short.class)
          return new Short(bb.getShort());
        else if (type == boolean.class)
          return (bb.get() == 0) ? new Boolean(false) : new Boolean(true);
        else if (type == void.class)
          return new byte[0];
        else
          { // This shouldn't be possible
            throw new JdwpInternalErrorException(
              "Field has invalid primitive!");
          }
      }
    else
      {
        // Field is an object
        ObjectId oid = VMIdManager.getDefault().readObjectId(bb);
        return oid.getObject();
      }
  }

  /**
   * Reads the an object of the given Class from the untagged value contained
   * in the ByteBuffer.
   * 
   * @param bb contains the Object
   * @param tag TAG of the Value to be read
   * @return the object
   * @throws JdwpException
   * @throws IOException
   */
  public static Object getUntaggedObj(ByteBuffer bb, byte tag)
    throws JdwpException, IOException
  {
    switch (tag)
      {
      case JdwpConstants.Tag.BYTE:
        return new Byte(bb.get());
      case JdwpConstants.Tag.CHAR:
        return new Character(bb.getChar());
      case JdwpConstants.Tag.FLOAT:
        return new Float(bb.getFloat());
      case JdwpConstants.Tag.DOUBLE:
        return new Double(bb.getDouble());
      case JdwpConstants.Tag.INT:
        return new Integer(bb.getInt());
      case JdwpConstants.Tag.LONG:
        return new Long(bb.getLong());
      case JdwpConstants.Tag.SHORT:
        return new Short(bb.getShort());
      case JdwpConstants.Tag.VOID:
        return new byte[0];
      case JdwpConstants.Tag.BOOLEAN:
        return (bb.get() == 0) ? new Boolean(false) : new Boolean(true);
      case JdwpConstants.Tag.STRING:
        return JdwpString.readString(bb);
      case JdwpConstants.Tag.ARRAY:
      case JdwpConstants.Tag.THREAD:
      case JdwpConstants.Tag.OBJECT:
      case JdwpConstants.Tag.THREAD_GROUP:
      case JdwpConstants.Tag.CLASS_LOADER:
      case JdwpConstants.Tag.CLASS_OBJECT:
        // All these cases are ObjectIds
        ObjectId oid = VMIdManager.getDefault().readObjectId(bb);
        return oid.getObject();
      default:
        throw new NotImplementedException("Tag " + tag
                                          + " is not implemented.");
      }
  }
}
