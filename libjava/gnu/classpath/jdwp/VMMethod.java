/* VMMethod.java -- a method in a virtual machine
   Copyright (C) 2006, 2007 Free Software Foundation, Inc.

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


package gnu.classpath.jdwp;

import java.io.DataOutputStream;
import java.io.IOException;
import java.nio.ByteBuffer;

import gnu.classpath.jdwp.exception.JdwpException;
import gnu.classpath.jdwp.util.LineTable;
import gnu.classpath.jdwp.util.VariableTable;

/**
 * This class is really an amalgamation of two classes: one class
 * represents a virtual machine method and the other represents
 * the JDWP back-end's ID for the method.
 *
 * @author Keith Seitz  (keiths@redhat.com)
 */
public class VMMethod
{
  /**
   * Returns the size of a JDWP method ID
   * @see gnu.classpath.jdwp.id.JdwpId#SIZE
   */
   public static final int SIZE = 8;

  // The class in which this method is declared
  private Class _class;

  // The method's ID
  private long _methodId;

  /**
   * Constructs a new VMMethod object. This constructor is protected
   * so that only the factory methods of VMVirtualMachine can be used
   * to create VMMethods.
   *
   * @param klass the method's containing class
   * @param id    method identifier, e.g., jmethodID
   * @see gnu.classpath.jdwp.VMVirtualMachine#getAllClassMethods
   * @see gnu.classpath.jdwp.VMVirtualMachine#getClassMethod
   */
  protected VMMethod(Class klass, long id)
  {
    _class = klass;
    _methodId = id;
  }

  /**
   * Returns the internal method ID for this method
   */
  public long getId()
  {
    return _methodId;
  }

  /**
   * Returns the method's declaring class
   */
  public Class getDeclaringClass()
  {
    return _class;
  }

  /**
   * Returns the name of this method
   */
  public native String getName();

  /**
   * Returns the signature of this method
   */
  public native String getSignature();

  /**
   * Returns the method's modifier flags
   */
  public native int getModifiers();

  /**
   * "Returns line number information for the method, if present. The line
   * table maps source line numbers to the initial code index of the line.
   * The line table is ordered by code index (from lowest to highest). The
   * line number information is constant unless a new class definition is
   * installed using RedefineClasses."
   *
   * @return the line table
   * @throws JdwpException
   */
  public native LineTable getLineTable()
    throws JdwpException;

  /**
   * "Returns variable information for the method. The variable table
   * includes arguments and locals declared within the method. For instance
   * methods, the "this" reference is included in the table. Also, synthetic
   * variables may be present."
   *
   * @return the variable table
   * @throws JdwpException
   */
  public native VariableTable getVariableTable()
    throws JdwpException;

  /**
   * Returns a string representation of this method (not
   * required but nice for debugging).
   */
  public String toString()
  {
    return getDeclaringClass().getName() + "." + getName();
  }

  /**
   * Writes the method's ID to the output stream
   *
   * @param ostream the output stream to which to write
   * @throws IOException for any errors writing to the stream
   * @see gnu.classpath.jdwp.id.JdwpId#write
   */
  public void writeId(DataOutputStream ostream)
    throws IOException
  {
    ostream.writeLong(getId());
  }

  /**
   * Returns a VMMethod from the ID in the byte buffer
   *
   * @param klass the method's declaring class
   * @param bb    a ByteBuffer containing the method's ID
   * @throws JdwpException for any errors creating the method
   * @throws IOException for any errors reading from the buffer
   */
  public static VMMethod readId(Class klass, ByteBuffer bb)
    throws JdwpException, IOException
  {
    return VMVirtualMachine.getClassMethod(klass, bb.getLong());
  }

  public boolean equals(Object obj)
  {
    if (obj instanceof VMMethod)
      {
	VMMethod m = (VMMethod) obj;
	return (getId() == m.getId());
      }

    return false;
  }
}
