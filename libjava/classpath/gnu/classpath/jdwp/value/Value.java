/* Value.java -- base class of JDWP values
   Copyright (C) 2007 Free Software Foundation

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


package gnu.classpath.jdwp.value;

import gnu.classpath.jdwp.exception.InvalidClassException;
import gnu.classpath.jdwp.exception.InvalidObjectException;
import gnu.classpath.jdwp.exception.InvalidTagException;
import gnu.classpath.jdwp.exception.JdwpInternalErrorException;

import java.io.DataOutputStream;
import java.io.IOException;
import java.nio.ByteBuffer;

/**
 * Superclass for all JDWP Values.
 * 
 * @author Kyle Galloway <kgallowa@redhat.com>
 */
public abstract class Value
{
  // A Tag representing the type of this value
  private byte _tag;
  
  /**
   * Create a new value of type tag.
   * 
   * @param tag the type of the value
   */ 
  protected Value(byte tag)
  {
    _tag = tag;
  }
  
  /**
   * Get the tag for this Value
   * 
   * @return the byte tag of this Value
   */
  public byte getTag()
  {
    return _tag;
  }
  
  /**
   * Calls the dervied classes writeValue method to write its value to the 
   * DataOutputStream.
   * 
   * @param os write the value here
   * @throws IOException
   */  
  public void writeUntagged(DataOutputStream os)
    throws IOException
  {
    write(os);
  }
  
  /**
   * Will write the given object as a tagged value to the DataOutputStream.
   * 
   * @param os write the value here
   * @param obj the Object to write
   * @throws IOException
   */
  public void writeTagged(DataOutputStream os)
    throws IOException
  {
    os.write (_tag);
    write(os);
  }
  
  /**
   * This method must write the value to the DataOutputStream in a manner
   * appropriate for the type of the value.
   * 
   * @param os DataOutputStream to write to
   * @throws IOException
   */
  protected abstract void write(DataOutputStream os)
    throws IOException;
  
  /**
   * Returns an object representing this type
   * 
   * @return an Object represntation of this value
   */
  protected abstract Object getObject();
    
  /**
   * Get an untagged object from the ByteBuffer
   * 
   * @param bb the ByteBuffer to extract the value from
   * @param type a Class representing the type
   * @return an Object from the ByteBuffer of the type of the Class parameter
   * @throws JdwpInternalErrorException
   * @throws InvalidObjectException
   */
  public static Object getUntaggedObject(ByteBuffer bb, Class type)
    throws JdwpInternalErrorException, InvalidObjectException, InvalidClassException
  {
    Value val = ValueFactory.createFromUntagged(bb, type);
    return val.getObject();
  }
  
  /**
   * Get an untagged object from the ByteBuffer
   * 
   * @param bb the ByteBuffer to extract the value from
   * @param tag a byte tag representing the type
   * @return an Object from the ByteBuffer of the type of the Class parameter
   * @throws JdwpInternalErrorException
   * @throws InvalidObjectException
   */
  public static Object getTaggedObject(ByteBuffer bb)
    throws JdwpInternalErrorException, InvalidObjectException, InvalidTagException
  {
    Value val = ValueFactory.createFromTagged(bb);
    return val.getObject();
  }
}
