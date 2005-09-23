/* Any.java --
   Copyright (C) 2005 Free Software Foundation, Inc.

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


package org.omg.CORBA;

import java.io.Serializable;

import org.omg.CORBA.portable.IDLEntity;

/**
 * A container that can store a value of either user defined or
 * primitive IDL type.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public abstract class Any
  implements Serializable, IDLEntity
{
  /**
   * Using v 1.4 serialVersionUID for interoperability.
   */
  private static final long serialVersionUID = 1217179597823814463L;

  /**
   * Creates an input stream from that this Any object's value can be
   * read (unmarshalled).
   */
  public abstract org.omg.CORBA.portable.InputStream create_input_stream();

  /**
   * Creates an output stream into which this <code>Any</code> object's
   * value can be written (marshalled).
   *
   * @return the newly created output stream.
   */
  public abstract org.omg.CORBA.portable.OutputStream create_output_stream();

  /**
   * Compare this <code>Any</code> with another <code>Any</code>.
   *
   * @param other the other instance to compare with.
   *
   * @return true if both values and value typecodes are equal,
   * false otherwise.
   */
  public abstract boolean equal(Any other);

  /**
   * Extract the CORBA <code>Object</code> from this <code>Any</code>
   * @throws BAD_OPERATION  if this instance contains value other
   * than <code>Object</code> or the value has not been set.
   */
  public abstract org.omg.CORBA.Object extract_Object()
                                               throws BAD_OPERATION;

  /**
   * Extract the CORBA <code>Principal</code> from this <code>Any</code>
   *
   * @throws NO_IMPLEMENT, always.
   *
   * @deprecated by CORBA 2.2.
   */
  public Principal extract_Principal()
  {
    throw new NO_IMPLEMENT();
  }

  /**
   * Extract an arbitrary {@link org.omg.CORBA.portable.Streamable } from
   * this <code>Any</code>.
   *
   * @throws BAD_INV_ORDER if the caller has invoked operations in the
   * wrong order.
   *
   * @throws NO_IMPLEMENT, always (override to get functionality).
   */
  public org.omg.CORBA.portable.Streamable extract_Streamable()
    throws org.omg.CORBA.BAD_INV_ORDER
  {
    throw new NO_IMPLEMENT();
  }

  /**
   * Extract the TypeCode from this <code>Any</code> value field.
   *
   * @throws BAD_OPERATION  if this instance contains value other
   * than <code>TypeCode</code> or the value has not been set.
   */
  public abstract TypeCode extract_TypeCode()
                                     throws BAD_OPERATION;

  /**
   * Extract the CORBA <code>Value</code> from this <code>Any</code>
   * @throws BAD_OPERATION  if this instance contains value other
   * than <code>Value</code> or the value has not been set.
   */
  public abstract java.io.Serializable extract_Value()
                                              throws BAD_OPERATION;

  /**
   * Extract another <code>Any</code> from this <code>Any</code>.
   *
   * @throws BAD_OPERATION  if this instance contains value other
   * than <code>any</code> or the value has not been set.
   */
  public abstract Any extract_any()
                           throws BAD_OPERATION;

  /**
   * Extract the CORBA <code>boolean</code> from this <code>Any</code>.
   *
   * @throws BAD_OPERATION  if this instance contains value other
   * than <code>boolean</code> or the value has not been set.
   */
  public abstract boolean extract_boolean()
                                   throws BAD_OPERATION;

  /**
   * Extract the CORBA <code>char</code> from this <code>Any</code>.
   *
   * @throws BAD_OPERATION  if this instance contains value other
   * than <code>char</code> or the value has not been set.
   */
  public abstract char extract_char()
                             throws BAD_OPERATION;

  /**
   * Extract the CORBA <code>double</code> from this <code>Any</code>.
   *
   * @throws BAD_OPERATION  if this instance contains value other
   * than <code>double</code> or the value has not been set.
   */
  public abstract double extract_double()
                                 throws BAD_OPERATION;

  /**
   * Extract the CORBA <code>fixed</code> from this <code>Any</code>.
   *
   * @throws BAD_OPERATION  if this instance contains value other
   * than <code>fixed</code> or the value has not been set.
   *
   * @throws NO_IMPLEMENT, always (override to get functionality).
   */
  public java.math.BigDecimal extract_fixed()
                                     throws BAD_OPERATION
  {
    throw new NO_IMPLEMENT();
  }

  /**
   * Extract the CORBA <code>float</code> from this <code>Any</code>.
   *
   * @throws BAD_OPERATION  if this instance contains value other
   * than <code>float</code> or the value has not been set.
   */
  public abstract float extract_float()
                               throws BAD_OPERATION;

  /**
   * Extract the CORBA <code>long</code> from this <code>Any</code>.
   *
   * @throws BAD_OPERATION  if this instance contains value other
   * than <code>long</code> or the value has not been set.
   */
  public abstract int extract_long()
                            throws BAD_OPERATION;

  /**
   * Extract the CORBA <code>long long</code> from this <code>Any</code>.
   *
   * @throws BAD_OPERATION  if this instance contains value other
   * than <code>longlong</code> or the value has not been set.
   */
  public abstract long extract_longlong()
                                 throws BAD_OPERATION;

  /**
   * Extract the CORBA <code>octet</code> from this <code>Any</code>.
   *
   * @throws BAD_OPERATION  if this instance contains value other
   * than <code>octet</code> or the value has not been set.
   */
  public abstract byte extract_octet()
                              throws BAD_OPERATION;

  /**
   * Extract the CORBA <code>short</code> from this <code>Any</code>.
   *
   * @throws BAD_OPERATION  if this instance contains value other
   * than <code>short</code> or the value has not been set.
   */
  public abstract short extract_short()
                               throws BAD_OPERATION;

  /**
   * Extract the CORBA <code>string</code> from this <code>Any</code>.
   *
   * @throws BAD_OPERATION  if this instance contains value other
   * than <code>string</code> or the value has not been set.
   */
  public abstract String extract_string()
                                 throws BAD_OPERATION;

  /**
   * Extract the CORBA unsigned <code>long</code> from this <code>Any</code>
   * @throws BAD_OPERATION  if this instance contains value other
   * than unsigned <code>long</code> or the value has not been set.
   */
  public abstract int extract_ulong()
                             throws BAD_OPERATION;

  /**
   * Extract the CORBA unsigned <code>long long</code> from this
   * <code>Any</code>.
   *
   * @throws BAD_OPERATION  if this instance contains value other
   * than unsigned <code>long long</code> or the value has not been set.
   */
  public abstract long extract_ulonglong()
                                  throws BAD_OPERATION;

  /**
   * Extract the CORBA unsigned <code>short</code> from this <code>Any</code>
   * @throws BAD_OPERATION  if this instance contains value other
   * than unsigned <code>short</code> or the value has not been set.
   */
  public abstract short extract_ushort()
                                throws BAD_OPERATION;

  /**
   * Extract the CORBA <code>wchar</code> from this <code>Any</code>
   * @throws BAD_OPERATION  if this instance contains value other
   * than <code>wchar</code> or the value has not been set.
   */
  public abstract char extract_wchar()
                              throws BAD_OPERATION;

  /**
   * Extract the CORBA <code>wstring</code> from this <code>Any</code>
   * @throws BAD_OPERATION  if this instance contains value other
   * than <code>wstring</code> or the value has not been set.
   */
  public abstract String extract_wstring()
                                  throws BAD_OPERATION;

  /**
   * Insert the CORBA <code>Object</code> into this <code>Any</code>
   */
  public abstract void insert_Object(org.omg.CORBA.Object x, TypeCode typecode);

  /**
   * Insert the CORBA <code>Object</code> into this <code>Any</code>
   */
  public abstract void insert_Object(org.omg.CORBA.Object x);

  /**
   * Insert the CORBA <code>Principal</code> into this <code>Any</code>.
   * @deprecated by CORBA 2.2.
   */
  public void insert_Principal(Principal x)
  {
    throw new NO_IMPLEMENT();
  }

  /**
   * Insert the CORBA <code>Streamable</code> into this <code>Any</code>
   */
  public void insert_Streamable(org.omg.CORBA.portable.Streamable x)
  {
    throw new NO_IMPLEMENT();
  }

  /**
   * Insert the CORBA <code>TypeCode</code> into this <code>Any</code>
   * value field.
   */
  public abstract void insert_TypeCode(TypeCode typecode);

  /**
   * Insert the CORBA <code>Value</code> into this <code>Any</code>.
   *
   * The type of the Any should be set (by {@link #type(TypeCode)})
   * before inserting the value.
   */
  public abstract void insert_Value(Serializable x, TypeCode typecode);

  /**
   * Insert the CORBA <code>Value</code> into this <code>Any</code>.
   *
   * The type of the Any should be set (by {@link #type(TypeCode)})
   * before inserting the value.
   */
  public abstract void insert_Value(Serializable x);

  /**
   * Insert the CORBA <code>any</code> into this <code>Any</code>
   */
  public abstract void insert_any(Any x);

  /**
   * Insert the CORBA <code>boolean</code> into this <code>Any</code>
   */
  public abstract void insert_boolean(boolean x);

  /**
   * Insert the CORBA <code>char</code> into this <code>Any</code>
   */
  public abstract void insert_char(char x);

  /**
   * Insert the CORBA <code>double</code> into this <code>Any</code>
   */
  public abstract void insert_double(double x);

  /**
   * Insert the CORBA <code>fixed</code> into this <code>Any</code>
   */
  public void insert_fixed(java.math.BigDecimal x, TypeCode typecode)
  {
    throw new NO_IMPLEMENT();
  }

  /**
   * Insert the CORBA <code>fixed</code> into this <code>Any</code>
   */
  public void insert_fixed(java.math.BigDecimal x)
  {
    throw new NO_IMPLEMENT();
  }

  /**
   * Insert the CORBA <code>float</code> into this <code>Any</code>
   */
  public abstract void insert_float(float x);

  /**
   * Insert the CORBA <code>long</code> into this <code>Any</code>
   */
  public abstract void insert_long(int x);

  /**
   * Insert the CORBA <code>longlong</code> into this <code>Any</code>
   */
  public abstract void insert_longlong(long x);

  /**
   * Insert the CORBA <code>octet</code> into this <code>Any</code>
   */
  public abstract void insert_octet(byte x);

  /**
   * Insert the CORBA <code>short</code> into this <code>Any</code>
   */
  public abstract void insert_short(short x);

  /**
   * Insert the CORBA <code>string</code> into this <code>Any</code>
   */
  public abstract void insert_string(String x);

  /**
   * Insert the CORBA <code>ulong</code> into this <code>Any</code>
   */
  public abstract void insert_ulong(int x);

  /**
   * Insert the CORBA <code>ulonglong</code> into this <code>Any</code>
   */
  public abstract void insert_ulonglong(long x);

  /**
   * Insert the CORBA <code>ushort</code> into this <code>Any</code>
   */
  public abstract void insert_ushort(short x);

  /**
   * Insert the CORBA <code>wchar</code> into this <code>Any</code>
   */
  public abstract void insert_wchar(char x);

  /**
   * Insert the CORBA <code>wstring</code> into this <code>Any</code>
   */
  public abstract void insert_wstring(String x);

  /**
   * Read the value into this <code>Any</code> from the given input stream.
   *
   * @param input a CORBA stream to read from.
   * @param type a TypeCode of the object being read.
   *
   * @throws org.omg.CORBA.MARSHAL if the given TypeCode does not match
   * the TypeCode of the object, found in the stream.
   */
  public abstract void read_value(org.omg.CORBA.portable.InputStream input,
                                  TypeCode type
                                 )
                           throws MARSHAL;

  /**
   * Set the type of the object, stored in this <code>Any</code>, to the
   * given TypeCode. Clear the value.
   *
   * @param valueTypeCode the type of the object that is expected to be stored
   * in this <code>any</code>.
   */
  public abstract void type(TypeCode valueTypeCode);

  /**
   * Returns the TypeCode of the object, stored in this <code>Any</code>
   * @return the TypeCode
   */
  public abstract TypeCode type();

  /**
   * Writes out the value (without the typecode of the value), stored in
   * this <code>Any</code>.
   *
   * @param output the CORBA stream to write into.
   *
   * @throws NullPointerException if the value of this <code>Any</code>
   * has not been set.
   */
  public abstract void write_value(org.omg.CORBA.portable.OutputStream output);
}
