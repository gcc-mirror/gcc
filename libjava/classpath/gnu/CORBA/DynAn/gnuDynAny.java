/* gnuDynAny.java --
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


package gnu.CORBA.DynAn;

import gnu.CORBA.CDR.BufferedCdrOutput;
import gnu.CORBA.OctetHolder;
import gnu.CORBA.Unexpected;
import gnu.CORBA.WCharHolder;
import gnu.CORBA.WStringHolder;
import gnu.CORBA.HolderLocator;
import gnu.CORBA.TypeKindNamer;
import gnu.CORBA.GeneralHolder;

import org.omg.CORBA.Any;
import org.omg.CORBA.AnyHolder;
import org.omg.CORBA.BooleanHolder;
import org.omg.CORBA.CharHolder;
import org.omg.CORBA.DoubleHolder;
import org.omg.CORBA.FloatHolder;
import org.omg.CORBA.IntHolder;
import org.omg.CORBA.LongHolder;
import org.omg.CORBA.MARSHAL;
import org.omg.CORBA.ORB;
import org.omg.CORBA.Object;
import org.omg.CORBA.ObjectHolder;
import org.omg.CORBA.ShortHolder;
import org.omg.CORBA.StringHolder;
import org.omg.CORBA.TCKind;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.TypeCodeHolder;
import org.omg.CORBA.TypeCodePackage.BadKind;
import org.omg.CORBA.ValueBaseHolder;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.OutputStream;
import org.omg.CORBA.portable.Streamable;
import org.omg.DynamicAny.DynAny;
import org.omg.DynamicAny.DynAnyPackage.InvalidValue;
import org.omg.DynamicAny.DynAnyPackage.TypeMismatch;

import java.io.IOException;
import java.io.Serializable;

import java.util.Arrays;

/**
 * The primitive dynamic Any holds the value basic final_type that cannot be
 * traversed.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class gnuDynAny extends AbstractAny implements DynAny, Serializable
{
  /**
   * Use serialVersionUID for interoperability.
   */
  private static final long serialVersionUID = 1;

  /**
   * The enclosed Streamable, holding the actual value.
   */
  protected Streamable holder;

  /**
   * Create DynAny providing the holder.
   *
   * @param a_holder
   */
  public gnuDynAny(Streamable aHolder, TypeCode oType, TypeCode aType,
    gnuDynAnyFactory aFactory, ORB anOrb
  )
  {
    super(oType, aType, aFactory, anOrb);
    holder = aHolder;
  }

  /**
   * Assign the contents of the given {@link DynAny} to this DynAny.
   *
   * @param from the source to assign from.
   */
  public void assign(DynAny from) throws TypeMismatch
  {
    checkType(official_type, from.type());

    if (from instanceof gnuDynAny)
      holder = ((gnuDynAny) from).holder;
    else
      holder = from.to_any().extract_Streamable();
    valueChanged();
  }

  /**
   * Create a copy of this {@link DynAny} via buffer read/write.
   */
  public DynAny copy()
  {
    if (holder != null)
      {
        BufferedCdrOutput buffer = new BufferedCdrOutput();
        holder._write(buffer);

        gnuDynAny other;
        try
          {
            other =
              new gnuDynAny((Streamable) (holder.getClass().newInstance()),
                official_type, final_type, factory, orb
              );
          }
        catch (Exception e)
          {
            // Holder must have parameterless constructor.
            throw new Unexpected(e);
          }
        other.holder._read(buffer.create_input_stream());
        return other;
      }
    else
      {
        return new gnuDynAny(null, official_type, final_type, factory, orb);
      }
  }

  /**
   * Always returns <code>null</code>.
   *
   * @return <code>null</code>, always.
   */
  public DynAny current_component() throws TypeMismatch
  {
    throw new TypeMismatch("Not applicable for " +
      TypeKindNamer.nameIt(final_type)
    );
  }

  /**
   * Returns without action, leaving all work to the garbage collector.
   */
  public void destroy()
  {
  }

  /**
   * Takes the passed parameter as the enclosed {@link Any} reference.
   *
   * @param an_any the {@link Any} that will be used as an enclosed reference.
   *
   * @throws TypeMismatch if the final_type of the passed Any is not the same as
   * the final_type, currently stored in this Any.
   */
  public void from_any(Any an_any) throws TypeMismatch, InvalidValue
  {
    checkType(official_type, an_any.type());

    Streamable a_holder = an_any.extract_Streamable();
    if (a_holder == null)
      {
        throw new InvalidValue(ISNULL);
      }
    else if (a_holder instanceof GeneralHolder)
      {
        holder = HolderLocator.createHolder(official_type);
        if (holder == null)
          holder = HolderLocator.createHolder(final_type);

        if (holder == null)
          holder = ((GeneralHolder) a_holder).Clone();
        else
          {
            InputStream in = an_any.create_input_stream();
            holder._read(in);
            try
              {
                in.close();
              }
            catch (IOException ex)
              {
                throw new Unexpected(ex);
              }
          }
      }
    else
      {
        try
          {
            InputStream in = an_any.create_input_stream();
            holder = (Streamable) a_holder.getClass().newInstance();
            holder._read(in);
            in.close();
          }
        catch (Exception ex)
          {
            TypeMismatch t = new TypeMismatch();
            t.initCause(ex);
            throw t;
          }
      }
    valueChanged();
  }

  /**
   * Return the second (enclosed any) that is stored in the wrapped Any.
   */
  public Any get_any() throws TypeMismatch
  {
    try
      {
        return ((AnyHolder) holder).value;
      }
    catch (ClassCastException cex)
      {
        TypeMismatch m = new TypeMismatch();
        m.initCause(cex);
        throw m;
      }
  }

  /** {@inheritDoc} */
  public boolean get_boolean() throws TypeMismatch
  {
    try
      {
        return ((BooleanHolder) holder).value;
      }
    catch (ClassCastException cex)
      {
        TypeMismatch m = new TypeMismatch();
        m.initCause(cex);
        throw m;
      }
  }

  /** {@inheritDoc} */
  public char get_char() throws TypeMismatch
  {
    try
      {
        return ((CharHolder) holder).value;
      }
    catch (ClassCastException cex)
      {
        TypeMismatch m = new TypeMismatch();
        m.initCause(cex);
        throw m;
      }
  }

  /** {@inheritDoc} */
  public double get_double() throws TypeMismatch
  {
    try
      {
        return ((DoubleHolder) holder).value;
      }
    catch (ClassCastException cex)
      {
        TypeMismatch m = new TypeMismatch();
        m.initCause(cex);
        throw m;
      }
  }

  /** {@inheritDoc} */
  public float get_float() throws TypeMismatch
  {
    try
      {
        return ((FloatHolder) holder).value;
      }
    catch (ClassCastException cex)
      {
        TypeMismatch m = new TypeMismatch();
        m.initCause(cex);
        throw m;
      }
  }

  /** {@inheritDoc} */
  public int get_long() throws TypeMismatch
  {
    try
      {
        return ((IntHolder) holder).value;
      }
    catch (ClassCastException cex)
      {
        TypeMismatch m = new TypeMismatch();
        m.initCause(cex);
        throw m;
      }
  }

  /** {@inheritDoc} */
  public long get_longlong() throws TypeMismatch
  {
    try
      {
        return ((LongHolder) holder).value;
      }
    catch (ClassCastException cex)
      {
        TypeMismatch m = new TypeMismatch();
        m.initCause(cex);
        throw m;
      }
  }

  /** {@inheritDoc} */
  public byte get_octet() throws TypeMismatch
  {
    try
      {
        return ((OctetHolder) holder).value;
      }
    catch (ClassCastException cex)
      {
        TypeMismatch m = new TypeMismatch();
        m.initCause(cex);
        throw m;
      }
  }

  /** {@inheritDoc} */
  public Object get_reference() throws TypeMismatch
  {
    try
      {
        return ((ObjectHolder) holder).value;
      }
    catch (ClassCastException cex)
      {
        TypeMismatch m = new TypeMismatch();
        m.initCause(cex);
        throw m;
      }
  }

  /** {@inheritDoc} */
  public short get_short() throws TypeMismatch
  {
    try
      {
        return ((ShortHolder) holder).value;
      }
    catch (ClassCastException cex)
      {
        TypeMismatch m = new TypeMismatch();
        m.initCause(cex);
        throw m;
      }
  }

  /** {@inheritDoc} */
  public String get_string() throws TypeMismatch
  {
    try
      {
        return ((StringHolder) holder).value;
      }
    catch (ClassCastException cex)
      {
        TypeMismatch m = new TypeMismatch();
        m.initCause(cex);
        throw m;
      }
  }

  /** {@inheritDoc} */
  public TypeCode get_typecode() throws TypeMismatch
  {
    try
      {
        return ((TypeCodeHolder) holder).value;
      }
    catch (ClassCastException cex)
      {
        TypeMismatch m = new TypeMismatch();
        m.initCause(cex);
        throw m;
      }
  }

  /** {@inheritDoc} */
  public int get_ulong() throws TypeMismatch
  {
    check(TCKind.tk_ulong);
    return get_long();
  }

  /** {@inheritDoc} */
  public long get_ulonglong() throws TypeMismatch
  {
    check(TCKind.tk_ulonglong);
    return get_longlong();
  }

  /** {@inheritDoc} */
  public short get_ushort() throws TypeMismatch
  {
    check(TCKind.tk_ushort);
    return get_short();
  }

  /** {@inheritDoc} */
  public Serializable get_val() throws TypeMismatch
  {
    try
      {
        return ((ValueBaseHolder) holder).value;
      }
    catch (ClassCastException cex)
      {
        TypeMismatch m = new TypeMismatch();
        m.initCause(cex);
        throw m;
      }
  }

  /** {@inheritDoc} */
  public char get_wchar() throws TypeMismatch
  {
    try
      {
        return ((WCharHolder) holder).value;
      }
    catch (ClassCastException cex)
      {
        TypeMismatch m = new TypeMismatch();
        m.initCause(cex);
        throw m;
      }
  }

  /** {@inheritDoc} */
  public String get_wstring() throws TypeMismatch
  {
    try
      {
        return ((WStringHolder) holder).value;
      }
    catch (ClassCastException cex)
      {
        TypeMismatch m = new TypeMismatch();
        m.initCause(cex);
        throw m;
      }
  }

  /** {@inheritDoc} */
  public void insert_any(Any a_x) throws TypeMismatch, InvalidValue
  {
    try
      {
        if (a_x.type().kind().value() == TCKind._tk_null)
          ((AnyHolder) holder).value = a_x;
        else
          {
            OutputStream buf = a_x.create_output_stream();
            buf.write_any(a_x);
            holder._read(buf.create_input_stream());
            buf.close();
          }
        valueChanged();
      }
    catch (ClassCastException cex)
      {
        TypeMismatch t = new TypeMismatch();
        t.initCause(cex);
        throw t;
      }
    catch (MARSHAL m)
      {
        InvalidValue v = new InvalidValue();
        v.initCause(m);
        throw v;
      }
    catch (IOException ex)
      {
        throw new Unexpected(ex);
      }
  }

  /** {@inheritDoc} */
  public void insert_boolean(boolean a_x) throws InvalidValue, TypeMismatch
  {
    try
      {
        ((BooleanHolder) holder).value = a_x;
        valueChanged();
      }
    catch (ClassCastException cex)
      {
        TypeMismatch t = new TypeMismatch();
        t.initCause(cex);
        throw t;
      }
  }

  /** {@inheritDoc} */
  public void insert_char(char a_x) throws InvalidValue, TypeMismatch
  {
    try
      {
        ((CharHolder) holder).value = a_x;
        valueChanged();
      }
    catch (ClassCastException cex)
      {
        TypeMismatch t = new TypeMismatch();
        t.initCause(cex);
        throw t;
      }
  }

  /** {@inheritDoc} */
  public void insert_double(double a_x) throws InvalidValue, TypeMismatch
  {
    try
      {
        ((DoubleHolder) holder).value = a_x;
        valueChanged();
      }
    catch (ClassCastException cex)
      {
        TypeMismatch t = new TypeMismatch();
        t.initCause(cex);
        throw t;
      }
  }

  /** {@inheritDoc} */
  public void insert_float(float a_x) throws InvalidValue, TypeMismatch
  {
    try
      {
        ((FloatHolder) holder).value = a_x;
        valueChanged();
      }
    catch (ClassCastException cex)
      {
        TypeMismatch t = new TypeMismatch();
        t.initCause(cex);
        throw t;
      }
  }

  /** {@inheritDoc} */
  public void insert_long(int a_x) throws InvalidValue, TypeMismatch
  {
    try
      {
        ((IntHolder) holder).value = a_x;
        valueChanged();
      }
    catch (ClassCastException cex)
      {
        TypeMismatch t = new TypeMismatch();
        t.initCause(cex);
        throw t;
      }
  }

  /** {@inheritDoc} */
  public void insert_longlong(long a_x) throws InvalidValue, TypeMismatch
  {
    try
      {
        ((LongHolder) holder).value = a_x;
        valueChanged();
      }
    catch (ClassCastException cex)
      {
        TypeMismatch t = new TypeMismatch();
        t.initCause(cex);
        throw t;
      }
  }

  /** {@inheritDoc} */
  public void insert_octet(byte a_x) throws InvalidValue, TypeMismatch
  {
    try
      {
        ((OctetHolder) holder).value = a_x;
        valueChanged();
      }
    catch (ClassCastException cex)
      {
        TypeMismatch t = new TypeMismatch();
        t.initCause(cex);
        throw t;
      }
  }

  /** {@inheritDoc} */
  public void insert_reference(Object a_x) throws InvalidValue, TypeMismatch
  {
    try
      {
        ((ObjectHolder) holder).value = a_x;
        valueChanged();
      }
    catch (ClassCastException cex)
      {
        TypeMismatch t = new TypeMismatch();
        t.initCause(cex);
        throw t;
      }
  }

  /** {@inheritDoc} */
  public void insert_short(short a_x) throws InvalidValue, TypeMismatch
  {
    try
      {
        ((ShortHolder) holder).value = a_x;
        valueChanged();
      }
    catch (ClassCastException cex)
      {
        TypeMismatch t = new TypeMismatch();
        t.initCause(cex);
        throw t;
      }
  }

  /** {@inheritDoc} */
  public void insert_string(String a_x) throws InvalidValue, TypeMismatch
  {
    try
      {
        if (a_x != null &&
          final_type.length() > 0 &&
          a_x.length() > final_type.length()
        )
          throw new InvalidValue(a_x.length() + " exceeds bound, " +
            final_type.length()
          );
        ((StringHolder) holder).value = a_x;
        valueChanged();
      }
    catch (ClassCastException cex)
      {
        TypeMismatch t = new TypeMismatch();
        t.initCause(cex);
        throw t;
      }
    catch (BadKind e)
      {
        TypeMismatch t = new TypeMismatch();
        t.initCause(e);
        throw t;
      }
  }

  /** {@inheritDoc} */
  public void insert_typecode(TypeCode a_x) throws InvalidValue, TypeMismatch
  {
    try
      {
        ((TypeCodeHolder) holder).value = a_x;
        valueChanged();
      }
    catch (ClassCastException cex)
      {
        TypeMismatch t = new TypeMismatch();
        t.initCause(cex);
        throw t;
      }
  }

  /** {@inheritDoc} */
  public void insert_ulong(int a_x) throws InvalidValue, TypeMismatch
  {
    try
      {
        ((IntHolder) holder).value = a_x;
        valueChanged();
      }
    catch (ClassCastException cex)
      {
        TypeMismatch t = new TypeMismatch();
        t.initCause(cex);
        throw t;
      }
  }

  /** {@inheritDoc} */
  public void insert_ulonglong(long a_x) throws InvalidValue, TypeMismatch
  {
    try
      {
        ((LongHolder) holder).value = a_x;
        valueChanged();
      }
    catch (ClassCastException cex)
      {
        TypeMismatch t = new TypeMismatch();
        t.initCause(cex);
        throw t;
      }
  }

  /** {@inheritDoc} */
  public void insert_ushort(short a_x) throws InvalidValue, TypeMismatch
  {
    try
      {
        ((ShortHolder) holder).value = a_x;
        valueChanged();
      }
    catch (ClassCastException cex)
      {
        TypeMismatch t = new TypeMismatch();
        t.initCause(cex);
        throw t;
      }
  }

  /** {@inheritDoc} */
  public void insert_val(Serializable a_x) throws InvalidValue, TypeMismatch
  {
    try
      {
        ((ValueBaseHolder) holder).value = a_x;
        valueChanged();
      }
    catch (ClassCastException cex)
      {
        TypeMismatch t = new TypeMismatch();
        t.initCause(cex);
        throw t;
      }
  }

  /** {@inheritDoc} */
  public void insert_wchar(char a_x) throws InvalidValue, TypeMismatch
  {
    try
      {
        ((WCharHolder) holder).value = a_x;
        valueChanged();
      }
    catch (ClassCastException cex)
      {
        TypeMismatch t = new TypeMismatch();
        t.initCause(cex);
        throw t;
      }
  }

  /** {@inheritDoc} */
  public void insert_wstring(String a_x) throws InvalidValue, TypeMismatch
  {
    try
      {
        if (a_x != null &&
          final_type.length() > 0 &&
          a_x.length() > type().length()
        )
          throw new InvalidValue(a_x.length() + " exceeds bound, " +
            final_type.length()
          );
        ((WStringHolder) holder).value = a_x;
        valueChanged();
      }
    catch (ClassCastException cex)
      {
        TypeMismatch t = new TypeMismatch();
        t.initCause(cex);
        throw t;
      }
    catch (BadKind e)
      {
        TypeMismatch t = new TypeMismatch();
        t.initCause(e);
        throw t;
      }
  }

  /**
   * The objects, enclosed inside this class, have only one component (self).
   *
   * @return false, always (no other action).
   */
  public boolean next()
  {
    return false;
  }

  /**
   * Returns without action.
   */
  public void rewind()
  {
  }

  /**
   * This objects, stored in this wrapper, never have multiple internal
   * components to seek.
   *
   * @return false, always (no other action).
   */
  public boolean seek(int p)
  {
    return false;
  }

  /**
   * Returns the enclosed {@link Any}.
   *
   * @return the enclosed {@link Any}.
   */
  public Any to_any()
  {
    Any a = createAny();
    a.insert_Streamable(holder);
    a.type(official_type);
    return a;
  }

  /** {@inheritDoc} */
  public TypeCode type()
  {
    return official_type;
  }

  /**
   * Compute hashcode in a trivial way.
   */
  protected int getHashCodeSimple(int maximum)
  {
    int h = super.hashCode() / 2;
    if (h < 0)
      h = -h;
    return h % maximum;
  }

  /**
   * Inserts Any, contained in the parameter, into Any, contained in this
   * DynAny.
   */
  public void insert_dyn_any(DynAny d) throws TypeMismatch, InvalidValue
  {
    check(d.type().kind());

    Any a = d.to_any();
    holder = a.extract_Streamable();
    valueChanged();
  }

  /**
   * Checks for equality. The DynAnys are equal if the stored Anys are equal.
   */
  public boolean equal(DynAny other)
  {
    if (other instanceof AbstractAny)
      {
        if (other instanceof gnuDynAny)
          {
            gnuDynAny x = (gnuDynAny) other;

            if (!x.holder.getClass().equals(holder.getClass()))
              return false;

            BufferedCdrOutput b1 = new BufferedCdrOutput();
            x.holder._write(b1);

            BufferedCdrOutput b2 = new BufferedCdrOutput(b1.buffer.size() + 10);
            holder._write(b2);

            return Arrays.equals(b1.buffer.toByteArray(),
              b2.buffer.toByteArray()
            );
          }
        else
          return false;
      }
    if (other == null)
      return false;
    else if (other.component_count() != component_count() ||
      !official_type.equal(other.type())
    )
      return false;
    else
      return other.to_any().equal(to_any());
  }

  /**
   * This final_type has no components.
   *
   * @return 0, always.
   */
  public int component_count()
  {
    return 0;
  }

  public DynAny get_dyn_any() throws TypeMismatch, InvalidValue
  {
    return new gnuDynAny(holder, official_type, final_type, factory, orb);
  }

  private void check(TCKind t) throws TypeMismatch
  {
    if (t.value() != final_type.kind().value())
      throw new TypeMismatch(t.value() + "!=" + final_type.kind().value());
  }
}