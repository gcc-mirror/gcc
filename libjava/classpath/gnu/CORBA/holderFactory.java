/* holderFactory.java --
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

import org.omg.CORBA.AnyHolder;
import org.omg.CORBA.AnySeqHolder;
import org.omg.CORBA.BooleanHolder;
import org.omg.CORBA.BooleanSeqHolder;
import org.omg.CORBA.CharHolder;
import org.omg.CORBA.CharSeqHolder;
import org.omg.CORBA.DoubleHolder;
import org.omg.CORBA.DoubleSeqHolder;
import org.omg.CORBA.FixedHolder;
import org.omg.CORBA.FloatHolder;
import org.omg.CORBA.FloatSeqHolder;
import org.omg.CORBA.IntHolder;
import org.omg.CORBA.LongHolder;
import org.omg.CORBA.LongLongSeqHolder;
import org.omg.CORBA.LongSeqHolder;
import org.omg.CORBA.OctetSeqHolder;
import org.omg.CORBA.PrincipalHolder;
import org.omg.CORBA.ShortHolder;
import org.omg.CORBA.ShortSeqHolder;
import org.omg.CORBA.StringHolder;
import org.omg.CORBA.StringSeqHolder;
import org.omg.CORBA.TCKind;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.TypeCodeHolder;
import org.omg.CORBA.ULongLongSeqHolder;
import org.omg.CORBA.ULongSeqHolder;
import org.omg.CORBA.UShortSeqHolder;
import org.omg.CORBA.WCharSeqHolder;
import org.omg.CORBA.WStringSeqHolder;
import org.omg.CORBA.portable.Streamable;

/**
 * Creates the suitable holder for storing the value of the given
 * type.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class holderFactory
{
  /**
   * The array, sufficiently large to use any {@link TCKind}._tk* constant
   * as an index.
   */
  private static final Class[] holders;
  private static final Class[] seqHolders;

  static
  {
    holders = new Class[ 32 ];
    holders [ TCKind._tk_Principal ] = PrincipalHolder.class;
    holders [ TCKind._tk_TypeCode ] = TypeCodeHolder.class;
    holders [ TCKind._tk_any ] = AnyHolder.class;
    holders [ TCKind._tk_boolean ] = BooleanHolder.class;
    holders [ TCKind._tk_char ] = CharHolder.class;
    holders [ TCKind._tk_double ] = DoubleHolder.class;
    holders [ TCKind._tk_float ] = FloatHolder.class;
    holders [ TCKind._tk_fixed ] = FixedHolder.class;
    holders [ TCKind._tk_long ] = IntHolder.class;
    holders [ TCKind._tk_longdouble ] = DoubleHolder.class;
    holders [ TCKind._tk_longlong ] = LongHolder.class;
    holders [ TCKind._tk_octet ] = OctetHolder.class;
    holders [ TCKind._tk_short ] = ShortHolder.class;
    holders [ TCKind._tk_string ] = StringHolder.class;
    holders [ TCKind._tk_ulong ] = IntHolder.class;
    holders [ TCKind._tk_ulonglong ] = LongHolder.class;
    holders [ TCKind._tk_ushort ] = ShortHolder.class;
    holders [ TCKind._tk_wchar ] = WCharHolder.class;
    holders [ TCKind._tk_wstring ] = WStringHolder.class;

    seqHolders = new Class[ 32 ];

    seqHolders [ TCKind._tk_ulonglong ] = ULongLongSeqHolder.class;
    seqHolders [ TCKind._tk_short ] = ShortSeqHolder.class;
    seqHolders [ TCKind._tk_octet ] = OctetSeqHolder.class;
    seqHolders [ TCKind._tk_any ] = AnySeqHolder.class;
    seqHolders [ TCKind._tk_long ] = LongSeqHolder.class;
    seqHolders [ TCKind._tk_longlong ] = LongLongSeqHolder.class;
    seqHolders [ TCKind._tk_float ] = FloatSeqHolder.class;
    seqHolders [ TCKind._tk_double ] = DoubleSeqHolder.class;
    seqHolders [ TCKind._tk_char ] = CharSeqHolder.class;
    seqHolders [ TCKind._tk_boolean ] = BooleanSeqHolder.class;
    seqHolders [ TCKind._tk_wchar ] = WCharSeqHolder.class;
    seqHolders [ TCKind._tk_ushort ] = UShortSeqHolder.class;
    seqHolders [ TCKind._tk_ulong ] = ULongSeqHolder.class;
    seqHolders [ TCKind._tk_string ] = StringSeqHolder.class;
    seqHolders [ TCKind._tk_wstring ] = WStringSeqHolder.class;
  }

  /**
   * Create a holder for storing the value of the given built-in type.
   * This function returns the defined holders for the built-in primitive
   * types and they sequences.
   *
   * @param t the typecode
   *
   * @return an instance of the corresponding built-in holder of null
   * if no such is defined for this type. The holder is created with a
   * parameterless constructor.
   */
  public static Streamable createHolder(TypeCode t)
  {
    try
      {
        int kind = t.kind().value();
        int componentKind;

        Streamable holder = null;
        Streamable component;

        if (kind < holders.length && holders [ kind ] != null)
          holder = (Streamable) holders [ kind ].newInstance();

        if (holder != null)
          return holder;

        switch (kind)
          {
            case TCKind._tk_sequence :
              componentKind = t.content_type().kind().value();
              if (componentKind < seqHolders.length)
                return (Streamable) seqHolders [ componentKind ].newInstance();
              break;

            default :
              break;
          }
      }
    catch (Exception ex)
      {
        throw new Unexpected(ex);
      }

    try
      {
        Object ox = ObjectCreator.createObject(t.id(), "Holder");
        return (Streamable) ox;
      }
    catch (Exception ex)
      {
        return null;
      }
  }
}
