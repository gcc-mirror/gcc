/* gnuAny.java --
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


package gnu.CORBA;

import gnu.CORBA.CDR.cdrBufInput;
import gnu.CORBA.CDR.cdrBufOutput;

import org.omg.CORBA.Any;
import org.omg.CORBA.AnyHolder;
import org.omg.CORBA.BAD_OPERATION;
import org.omg.CORBA.BooleanHolder;
import org.omg.CORBA.CharHolder;
import org.omg.CORBA.DoubleHolder;
import org.omg.CORBA.FixedHolder;
import org.omg.CORBA.FloatHolder;
import org.omg.CORBA.IntHolder;
import org.omg.CORBA.LongHolder;
import org.omg.CORBA.MARSHAL;
import org.omg.CORBA.ORB;
import org.omg.CORBA.ObjectHolder;
import org.omg.CORBA.Principal;
import org.omg.CORBA.PrincipalHolder;
import org.omg.CORBA.ShortHolder;
import org.omg.CORBA.StringHolder;
import org.omg.CORBA.TCKind;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.TypeCodeHolder;
import org.omg.CORBA.TypeCodePackage.BadKind;
import org.omg.CORBA.ValueBaseHolder;
import org.omg.CORBA.portable.Streamable;

import java.io.IOException;
import java.io.Serializable;

import java.lang.reflect.Field;

import java.math.BigDecimal;

import java.util.Arrays;

/**
 * The implementation of {@link Any}.
 *
 * For performance reasonse, the inserted values are not cloned.
 * If the value object allows modifications (like {@link Streamable}),
 * these subsequent alterations are reflected by the instance of
 * this gnuAny, and the gnuAny alterations are reflected by the
 * returned value. If it is required to have the uncoupled value,
 * it must be requested from the copy of the current instance.
 * The {@link gnuAny} can be simply cloned by the provided
 * {@link Clone()} method.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class gnuAny
  extends Any
{
  /**
   * The value, returned by {@link #type()} if the value has been
   * not intialized.
   */
  protected static final TypeCode nullType =
    new primitiveTypeCode(TCKind.tk_null);

  /**
   * The Streamable, representing the value, held by this gnuAny.
   */
  protected Streamable has;

  /**
   * The complete typecode of the Streamable, if explicitly set.
   */
  protected TypeCode typecode;

  /**
   * The typecode kind of the Streamable, if explicitly set.
   */
  protected int xKind = -1;

  /**
   * The associated ORB.
   */
  private ORB orb;

  /**
   * Set the associated orb.
   */
  public void setOrb(ORB an_orb)
  {
    orb = an_orb;
  }

  /**
   * Creates a deep copy of this gnuAny, writing to and subsequently
   * reading from from the byte buffer.
   *
   * @return the uncoupled gnuAny with all fields set to identical
   * values.
   */
  public gnuAny Clone()
  {
    cdrBufOutput out = new cdrBufOutput();
    out.setOrb(orb);
    out.write_any(this);

    cdrBufInput in = new cdrBufInput(out.buffer.toByteArray());
    in.setOrb(orb);
    return (gnuAny) in.read_any();
  }

  /**
   * Create the buffered CDR input stream, containing the
   * value, stored inside of this {@link Any}.
   */
  public org.omg.CORBA.portable.InputStream create_input_stream()
  {
    if (has instanceof universalHolder)
      {
        universalHolder u = (universalHolder) has;
        return u.getInputStream();
      }
    else
      {
        cdrBufOutput out = new cdrBufOutput();
        out.setOrb(orb);
        write_value(out);

        cdrBufInput in = new cdrBufInput(out.buffer.toByteArray());
        in.setOrb(orb);
        return in;
      }
  }

  /**
   * Create the buffered CDR output stream (empty).
   */
  public org.omg.CORBA.portable.OutputStream create_output_stream()
  {
    cdrBufOutput stream = new cdrBufOutput();
    stream.setOrb(orb);
    return stream;
  }

  /**
   * Compare two Any's for equality.
   * @param other the other Any to compare.
   */
  public boolean equal(Any other)
  {
    if (other == this)
      return true;
    if (type().kind() != other.type().kind())
      return false;

    if (has != null && other instanceof gnuAny)
      if (has.equals(((gnuAny) other).has))
        return true;

    cdrBufOutput a = new cdrBufOutput();
    a.setOrb(orb);
    write_value(a);

    cdrBufOutput b = new cdrBufOutput();
    b.setOrb(orb);
    other.write_value(b);

    byte[] ba = a.buffer.toByteArray();
    byte[] bb = b.buffer.toByteArray();

    return Arrays.equals(ba, bb);
  }

  /**
   * Delegates functionality to {@link #equal(Any)}.
   */
  public boolean equals(java.lang.Object other)
  {
    if (other == this)
      return true;
    if (!(other instanceof Any))
      return false;

    return equal((Any) other);
  }

  /**
   * Extract the previously stored object.
   */
  public org.omg.CORBA.Object extract_Object()
  {
    try
      {
        return ((ObjectHolder) has).value;
      }
    catch (ClassCastException ex)
      {
        throw new BAD_OPERATION();
      }
  }

  /**
   * Extract the previously inserted CORBA <code>Principal</code>/
   * @return the previously inserted value.
   *
   * @throws org.omg.CORBA.BAD_OPERATION if the holder contains something
   * else than Principal.
   *
   * @deprecated by CORBA 2.2.
   */
  public Principal extract_Principal()
  {
    check(TCKind._tk_Principal);
    return ((PrincipalHolder) has).value;
  }

  /**
   * Return the value, encapsulated in a suitable holder.
   * This implementation returns the direct reference,
   * so the alterations on the returned streamable are
   * directly reflected to the content of this {@link Any}.
   */
  public Streamable extract_Streamable()
  {
    return has;
  }

  public TypeCode extract_TypeCode()
                            throws BAD_OPERATION
  {
    check(TCKind._tk_TypeCode);
    return ((TypeCodeHolder) has).value;
  }

  /**
   * Extract the stored value type.
   *
   * @return the previously stored value type.
   *
   * @throws BAD_OPERATION if the Any contains something different.
   *
   * @see org.omg.CORBA.portable.ValueBase
   */
  public Serializable extract_Value()
                             throws BAD_OPERATION
  {
    try
      {
        if (has instanceof ValueBaseHolder)
          return ((ValueBaseHolder) has).value;
        else
          {
            // Normally, ValueBase holder must be an instance of the
            // ValueBaseHolder. However some IDL compilers probably
            // have a bug, do not deriving this way. The the only
            // way to access the wrapped value is via reflection.
            Field f = has.getClass().getField("value");
            return (Serializable) f.get(has);
          }
      }
    catch (Exception ex)
      {
        return new BAD_OPERATION("Value type expected");
      }
  }

  /** {@inheritDoc} */
  public Any extract_any()
                  throws BAD_OPERATION
  {
    check(TCKind._tk_any);
    return ((AnyHolder) has).value;
  }

  /** {@inheritDoc} */
  public boolean extract_boolean()
                          throws BAD_OPERATION
  {
    check(TCKind._tk_boolean);
    return ((BooleanHolder) has).value;
  }

  /** {@inheritDoc} */
  public char extract_char()
                    throws BAD_OPERATION
  {
    check(TCKind._tk_char);
    return ((CharHolder) has).value;
  }

  /** {@inheritDoc} */
  public double extract_double()
                        throws BAD_OPERATION
  {
    check(TCKind._tk_double);
    return ((DoubleHolder) has).value;
  }

  /**
   * Extract the previously inserted CORBA <code>fixed</code>/
   * @return the previously inserted value.
   *
   * @throws org.omg.CORBA.BAD_OPERATION if the holder contains something
   * else than BigDecimal.
   */
  public BigDecimal extract_fixed()
                           throws org.omg.CORBA.BAD_OPERATION
  {
    check(TCKind._tk_fixed);
    return ((FixedHolder) has).value;
  }

  /** {@inheritDoc} */
  public float extract_float()
                      throws BAD_OPERATION
  {
    check(TCKind._tk_float);
    return ((FloatHolder) has).value;
  }

  /** {@inheritDoc} */
  public int extract_long()
                   throws BAD_OPERATION
  {
    // CORBA long = java int.
    check(TCKind._tk_long);
    return ((IntHolder) has).value;
  }

  /** {@inheritDoc} */
  public long extract_longlong()
                        throws BAD_OPERATION
  {
    check(TCKind._tk_longlong);
    return ((LongHolder) has).value;
  }

  /** {@inheritDoc} */
  public byte extract_octet()
                     throws BAD_OPERATION
  {
    // ShortHolder holds also octets.
    check(TCKind._tk_octet);
    return (byte) ((OctetHolder) has).value;
  }

  /** {@inheritDoc} */
  public short extract_short()
                      throws BAD_OPERATION
  {
    check(TCKind._tk_short);
    return ((ShortHolder) has).value;
  }

  /** {@inheritDoc} */
  public String extract_string()
                        throws BAD_OPERATION
  {
    check(TCKind._tk_string);
    return ((StringHolder) has).value;
  }

  /** {@inheritDoc} */
  public int extract_ulong()
                    throws BAD_OPERATION
  {
    // IntHolder also holds ulongs.
    check(TCKind._tk_ulong);
    return ((IntHolder) has).value;
  }

  /** {@inheritDoc} */
  public long extract_ulonglong()
                         throws BAD_OPERATION
  {
    // LongHolder also holds ulonglong
    check(TCKind._tk_ulonglong);
    return ((LongHolder) has).value;
  }

  /** {@inheritDoc} */
  public short extract_ushort()
                       throws BAD_OPERATION
  {
    // ShortHolder also holds ushorts.
    check(TCKind._tk_ushort);
    return ((ShortHolder) has).value;
  }

  /** {@inheritDoc} */
  public char extract_wchar()
                     throws BAD_OPERATION
  {
    check(TCKind._tk_wchar);
    return ((WCharHolder) has).value;
  }

  /** {@inheritDoc} */
  public String extract_wstring()
                         throws BAD_OPERATION
  {
    // StringHolder also holds wstrings.
    check(TCKind._tk_wstring);
    return ((WStringHolder) has).value;
  }

  /**
   * Inserts the CORBA object and sets the typecode to the given type.
   */
  public void insert_Object(org.omg.CORBA.Object x, TypeCode typecode)
  {
    has = new ObjectHolder(x);
    type(typecode);
  }

  /**
   * Inserts the CORBA object.
   */
  public void insert_Object(org.omg.CORBA.Object x)
  {
    has = new ObjectHolder(x);
  }

  /**
   * Insert the CORBA Principal.
   * This implementation uses direct assignment, so the later
   * alterations of that BigDecimal are reflected on the
   * content of this {@link Any}.
   *
   * @deprecated by CORBA 2.2.
   */
  public void insert_Principal(Principal x)
  {
    resetTypes();
    if (has instanceof PrincipalHolder)
      ((PrincipalHolder) has).value = x;
    else
      has = new PrincipalHolder(x);
  }

  /**
   * Sets the value to the value, encapsulated in this holder.
   * This implementation uses direct assignment, so the later
   * alterations of that streamable are reflected on the
   * content of this {@link Any}.
   */
  public void insert_Streamable(Streamable x)
  {
    resetTypes();
    has = x;
  }

  /**
   * Insert the typecode into this Any
   * @param typecode the typecode to insert.
   */
  public void insert_TypeCode(TypeCode typecode)
  {
    resetTypes();
    if (has instanceof TypeCodeHolder)
      ((TypeCodeHolder) has).value = typecode;
    else
      has = new TypeCodeHolder(typecode);
  }

  /** {@inheritDoc} */
  public void insert_Value(Serializable x, TypeCode typecode)
  {
    type(typecode);
    insert_Value(x);
  }

  /** {@inheritDoc} */
  public void insert_Value(Serializable x)
  {
    resetTypes();
    if (has instanceof ValueBaseHolder)
      ((ValueBaseHolder) has).value = x;
    else
      has = new ValueBaseHolder(x);
  }

  /**
  * Insert another {@link Any} into this {@link Any}.
  * This implementation uses direct assignment, so the later
  * alterations of that {@link Any} are reflected on the
  * content of this {@link Any}.
  */
  public void insert_any(Any an_any)
  {
    resetTypes();
    if (has instanceof AnyHolder)
      ((AnyHolder) has).value = an_any;
    else
      has = new AnyHolder(an_any);
  }

  /** {@inheritDoc} */
  public void insert_boolean(boolean x)
  {
    resetTypes();
    if (has instanceof BooleanHolder)
      ((BooleanHolder) has).value = x;
    else
      has = new BooleanHolder(x);
  }

  /** {@inheritDoc} */
  public void insert_char(char x)
  {
    resetTypes();
    if (has instanceof CharHolder)
      ((CharHolder) has).value = x;
    else
      has = new CharHolder(x);
  }

  /** {@inheritDoc} */
  public void insert_double(double x)
  {
    resetTypes();
    if (has instanceof DoubleHolder)
      ((DoubleHolder) has).value = x;
    else
      has = new DoubleHolder(x);
  }

  /**
   * Inserts the CORBA <code>fixed</code>, setting the typecode
   * explicitly.
   * This implementation uses direct assignment, so the later
   * alterations of that BigDecimal are reflected on the
   * content of this {@link Any}.
   */
  public void insert_fixed(BigDecimal x, TypeCode x_typecode)
  {
    resetTypes();
    insert_fixed(x);
    typecode = x_typecode;
  }

  /**
   * Inserts the CORBA <code>fixed</code>, setting the typecode
   * by example of the currently passed value.
   * This implementation uses direct assignment, so the later
   * alterations of that BigDecimal are reflected on the
   * content of this {@link Any}, including the typecode.
   */
  public void insert_fixed(BigDecimal x)
  {
    resetTypes();
    if (has instanceof FixedHolder)
      ((FixedHolder) has).value = x;
    else
      has = new FixedHolder(x);
  }

  /** {@inheritDoc} */
  public void insert_float(float x)
  {
    resetTypes();
    if (has instanceof FloatHolder)
      ((FloatHolder) has).value = x;
    else
      has = new FloatHolder(x);
  }

  /** {@inheritDoc} */
  public void insert_long(int x)
  {
    resetTypes();
    if (has instanceof IntHolder)
      ((IntHolder) has).value = x;
    else
      has = new IntHolder(x);
  }

  /** {@inheritDoc} */
  public void insert_longlong(long x)
  {
    resetTypes();
    if (has instanceof LongHolder)
      ((LongHolder) has).value = x;
    else
      has = new LongHolder(x);
  }

  /** {@inheritDoc} */
  public void insert_octet(byte x)
  {
    resetTypes();
    if (has instanceof OctetHolder)
      ((OctetHolder) has).value = x;
    else
      has = new OctetHolder(x);
  }

  /** {@inheritDoc} */
  public void insert_short(short x)
  {
    resetTypes();
    if (has instanceof ShortHolder)
      ((ShortHolder) has).value = x;
    else
      has = new ShortHolder(x);
  }

  /** {@inheritDoc} */
  public void insert_string(String x)
  {
    resetTypes();
    if (has instanceof StringHolder)
      ((StringHolder) has).value = x;
    else
      has = new StringHolder(x);

    typecode = new stringTypeCode(TCKind.tk_string);
  }

  /** {@inheritDoc} */
  public void insert_ulong(int x)
  {
    resetTypes();
    if (has instanceof IntHolder)
      ((IntHolder) has).value = x;
    else
      has = new IntHolder(x);
    xKind = TCKind._tk_ulong;
  }

  /** {@inheritDoc} */
  public void insert_ulonglong(long x)
  {
    resetTypes();
    if (has instanceof LongHolder)
      ((LongHolder) has).value = x;
    else
      has = new LongHolder(x);
    xKind = TCKind._tk_ulonglong;
  }

  /** {@inheritDoc} */
  public void insert_ushort(short x)
  {
    resetTypes();
    if (has instanceof ShortHolder)
      ((ShortHolder) has).value = x;
    else
      has = new ShortHolder(x);
    xKind = TCKind._tk_ushort;
  }

  /** {@inheritDoc} */
  public void insert_wchar(char x)
  {
    resetTypes();
    if (has instanceof WCharHolder)
      ((WCharHolder) has).value = x;
    else
      has = new WCharHolder(x);
  }

  /** {@inheritDoc} */
  public void insert_wstring(String x)
  {
    resetTypes();
    if (has instanceof WStringHolder)
      ((WStringHolder) has).value = x;
    else
      has = new WStringHolder(x);
  }

  /**
   * Return the associated orb.
   */
  public ORB orb()
  {
    return orb;
  }

  /**
   * Read the value of the given type from the given stream.
   *
   * @param input a stream to read from.
   * @param a_type a typecode of the value to read.
   */
  public void read_value(org.omg.CORBA.portable.InputStream input,
                         TypeCode a_type
                        )
                  throws MARSHAL
  {
    try
      {
        int kind = a_type.kind().value();

        // Fixed needs special handling.
        if (kind == TCKind._tk_fixed)
          {
            BigDecimal dec = BigDecimalHelper.read(input, a_type.fixed_scale());
            has = new FixedHolder(dec);
          }
        else
          {
            has = holderFactory.createHolder(a_type);
            if (has == null)
              {
                // Use the Universal Holder that reads till the end of stream.
                // This works with the extract/insert pair of the typical
                // Helper.
                cdrBufOutput buffer = new cdrBufOutput();
                buffer.setOrb(orb);
                has = new universalHolder(buffer);
              }
          }
        type(a_type);
        has._read(input);
      }
    catch (BadKind ex)
      {
        throw new MARSHAL("Bad kind: " + ex.getMessage());
      }
    catch (IOException ex)
      {
        throw new MARSHAL("IO exception: " + ex.getMessage());
      }
  }

  /** {@inheritDoc} */
  public TypeCode type()
  {
    if (typecode != null)
      return typecode;
    else if (xKind >= 0)
      {
        typecode = new primitiveTypeCode(TCKind.from_int(xKind));
        return typecode;
      }
    else
      return has != null ? has._type() : nullType;
  }

  /**
   * Explicitly set the typecode of the value to the given type.
   *
   * @param valueTypeCode the typecode of the value.
   */
  public void type(TypeCode valueTypeCode)
  {
    xKind = valueTypeCode.kind().value();
    typecode = valueTypeCode;
  }

  /** {@inheritDoc} */
  public void write_value(org.omg.CORBA.portable.OutputStream output)
  {
    if (has != null)
      has._write(output);
  }

  /**
   * Check if the current value if the value of the given kind.
   * @param kind a kind to check.
   * @throws BAD_OPERATION if the value is not set of is different kind.
   */
  protected void check(int kind)
                throws BAD_OPERATION
  {
    if (has == null)
      throw new BAD_OPERATION("value not set");

    if (xKind >= 0)
      {
        if (xKind != kind)
          throw new BAD_OPERATION("Extracting " + typeNamer.nameIt(kind) +
                                  " when stored " + typeNamer.nameIt(xKind)
                                 );
      }
    else
      {
        if (type().kind().value() != kind)
          throw new BAD_OPERATION("Extracting " + typeNamer.nameIt(kind) +
                                  " stored " + typeNamer.nameIt(type())
                                 );
      }
  }

  /**
   * Clear the additional type information before reusing this instance.
   */
  private final void resetTypes()
  {
    typecode = null;
    xKind = -1;
  }
}