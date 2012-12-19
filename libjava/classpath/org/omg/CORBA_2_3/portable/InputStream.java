/* InputStream.java --
   Copyright (C) 2005, 2006 Free Software Foundation, Inc.

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


package org.omg.CORBA_2_3.portable;

import gnu.CORBA.CDR.Vio;

import org.omg.CORBA.CustomMarshal;
import org.omg.CORBA.portable.BoxedValueHelper;
import org.omg.CORBA.portable.StreamableValue;

import java.io.Serializable;

/**
 * This class defines a new CDR input stream methods, added since
 * CORBA 2.3.
 *
 * This class is abstract; no direct instances can be instantiated.
 * Also, up till v 1.4 inclusive there are no methods that would
 * return it, and only one unimplemented interface,
 * {@link org.omg.CORBA.portable.ValueFactory }, needs it as a parameter.
 *
 * However since 1.3 all methods, declared as returning an
 * org.omg.CORBA.portable.InputStream actually return the instance of this
 * derived class and the new methods are accessible after the casting
 * operation.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public abstract class InputStream
  extends org.omg.CORBA.portable.InputStream
{
  /**
   * Read the abstract interface. An abstract interface can be either
   * CORBA value type or CORBA object and is returned as an abstract
   * java.lang.Object.
   *
   * As specified in OMG specification, this reads a single
   * boolean and then delegates either to {@link #read_Object()} (for false)
   * or to {@link #read_value()} (for true).
   *
   * @return an abstract interface, unmarshaled from the stream.
   */
  public Object read_abstract_interface()
  {
    boolean isObject = read_boolean();

    if (isObject)
      return read_Object();
    else
      return read_value();
  }

  /**
   * Read the abstract interface, corresponding to the passed type.
   * An abstract interface can be either CORBA value type or CORBA
   * object and is returned as an abstract java.lang.Object.
   *
   * As specified in OMG specification, this reads a single
   * boolean and then delegates either to {@link #read_Object(Class)} (for false)
   * or to {@link #read_value(Class)} (for true).
   *
   * @param clz a base class for the abstract interface.
   *
   * @return an abstract interface, unmarshaled from the stream
   */
  @SuppressWarnings("rawtypes") // Needed for API compatibility
  public Object read_abstract_interface(Class clz)
  {
    boolean isValue = read_boolean();

    if (isValue)
      return read_value(clz);
    else
      return read_Object(clz);
  }

  /**
   * Read a value type structure, extracting the repository id
   * from the input stream itself. The repository id is optional
   * in the value type record, but it  must be present for this
   * method to succeed. The {@link OutputStream} of this
   * implementation always stores the repository id.
   *
   * The casts the streams ORB into a CORBA 2.3 ORB and then
   * searched for a suitable value factory, where it delegates
   * the functionality.
   *
   * If you know the exact class or can create an unitialised instance
   * of the value type, it is recommended (faster) to use
   * {@link #read_value(Class)} or {@link #read_value(Serializable)}
   * instead.
   *
   * @return an value type structure, unmarshaled from the stream
   */
  public Serializable read_value()
  {
    return Vio.read(this);
  }

  /**
   * Read a value type structure, corresponing to the passed type.
   * As the type is known, the repository Id in the input stream is
   * optional an not required. The codebase, if present, is also ignored.
   *
   * The passed class must implement either {@link CustomMarshal}
   * for the user-defined reading operations or {@link StreamableValue}
   * for the standard (generated by IDL compiler) reading operations.
   * Also, it must have the parameterless constructor to create a new
   * instance.
   *
   * @param clz a base class for a value type.
   *
   * @return an value type structure, unmarshaled from the stream
   */
  @SuppressWarnings("rawtypes") // Needed for API compatibility
  public Serializable read_value(Class clz)
  {
    return Vio.read(this, clz);
  }

  /**
   * Read a value type structure content, when the unitialised
   * instance is passed as a parameter. It is a fastest method to read
   * a value type.
   *
   * As the type is known, the repository Id in the input stream is
   * optional an not required. The codebase, if present, is also ignored.
   *
   * The passed instance must implement either {@link CustomMarshal}
   * for the user-defined reading operations or {@link StreamableValue}
   * for the standard (generated by IDL compiler) reading operations.
   *
   * @param unitialised_value the unitialised value.
   *
   * @return same value, filled in by the stream content.
   */
  public Serializable read_value(Serializable unitialised_value)
  {
    return (Serializable) Vio.read(this, unitialised_value, null);
  }

  /**
   * Read a value type structure, having the given repository id.
   * The casts the streams ORB into a CORBA 2.3 ORB and then
   * searched for a suitable value factory, where it delegates
   * the functionality.
   *
   * If you know the exact class or can create an unitialised instance
   * of the value type, it is recommended (faster) to use
   * {@link #read_value(Class)} or {@link #read_value(Serializable)}
   * instead.
   *
   * @param repository_id a repository id of the value type.
   *
   * @return an value type structure, unmarshaled from the stream
   */
  public Serializable read_value(String repository_id)
  {
    return Vio.read(this, repository_id);
  }

  /**
   * Use the provided boxed value helper to read the value.
   *
   * @param helper a helper for reading the value from the stream.
   *
   * @return an value type structure, unmarshaled from the stream.
   */
  public Serializable read_value(BoxedValueHelper helper)
  {
    return Vio.read(this, helper);
  }
}
