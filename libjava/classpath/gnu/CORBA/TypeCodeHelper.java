/* TypeCodeHelper.java --
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

import gnu.CORBA.typecodes.FixedTypeCode;
import gnu.CORBA.typecodes.GeneralTypeCode;
import gnu.CORBA.typecodes.ArrayTypeCode;
import gnu.CORBA.typecodes.PrimitiveTypeCode;
import gnu.CORBA.typecodes.RecordTypeCode;
import gnu.CORBA.typecodes.StringTypeCode;

import org.omg.CORBA.TCKind;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.TypeCodePackage.BadKind;
import org.omg.CORBA.TypeCodePackage.Bounds;

/**
 * Reads and writes the TypeCodes usind common data representation.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class TypeCodeHelper
{
  /**
   * Read the CORBA {@link TypeCode}. First, the TypeCode kind
   * is read as four byte long. Then, if needed, the additional
   * parameters are loaded following CORBA specification.
   *
   * @param in a stream to read from.
   */
  public static TypeCode read(org.omg.CORBA.portable.InputStream in)
                       throws BadKind, Bounds
  {
    TCKind kind = TCKind.from_int(in.read_long());
    TypeCode rt;
    GeneralTypeCode g;
    RecordTypeCode r;
    RecordTypeCode.Field f;
    StringTypeCode s;
    int n;

    switch (kind.value())
      {
        case TCKind._tk_sequence :
        case TCKind._tk_array :

          ArrayTypeCode p = new ArrayTypeCode(kind);
          p.setLength(in.read_long());
          rt = p;
          break;

        case TCKind._tk_string :
        case TCKind._tk_wstring :
          s = new StringTypeCode(kind);
          s.setLength(in.read_long());
          rt = s;
          break;

        case TCKind._tk_fixed :

          FixedTypeCode fx = new FixedTypeCode();
          fx.setDigits(in.read_short());
          fx.setScale(in.read_short());
          rt = fx;
          break;

        case TCKind._tk_objref :
        case TCKind._tk_native :
        case TCKind._tk_abstract_interface :
          g = new GeneralTypeCode(kind);
          g.setId(in.read_string());
          g.setName(in.read_string());
          rt = g;
          break;

        case TCKind._tk_alias :
        case TCKind._tk_value_box :
          g = new GeneralTypeCode(kind);
          g.setId(in.read_string());
          g.setName(in.read_string());
          g.setContentType(in.read_TypeCode());
          rt = g;
          break;

        case TCKind._tk_struct :
        case TCKind._tk_except :
          r = new RecordTypeCode(kind);
          r.setId(in.read_string());
          r.setName(in.read_string());

          n = in.read_long();

          for (int i = 0; i < n; i++)
            {
              f = r.field();
              f.name = in.read_string();
              f.type = in.read_TypeCode();
            }
          rt = r;
          break;

        case TCKind._tk_enum :
          r = new RecordTypeCode(kind);
          r.setId(in.read_string());
          r.setName(in.read_string());

          n = in.read_long();

          for (int i = 0; i < n; i++)
            {
              f = r.field();
              f.name = in.read_string();
            }
          rt = r;
          break;

        case TCKind._tk_union :
          r = new RecordTypeCode(kind);
          r.setId(in.read_string());
          r.setName(in.read_string());
          r.setDiscriminator_type(in.read_TypeCode());
          r.setDefaultIndex(in.read_long());

          n = in.read_long();

          for (int i = 0; i < n; i++)
            {
              f = r.field();
              f.label = in.read_any();
              f.name = in.read_string();
              f.type = in.read_TypeCode();
            }
          rt = r;

          break;

        case TCKind._tk_value :
          r = new RecordTypeCode(kind);
          r.setId(in.read_string());
          r.setName(in.read_string());
          r.setTypeModifier(in.read_short());
          r.setConcreteBase_type(in.read_TypeCode());

          n = in.read_long();

          for (int i = 0; i < n; i++)
            {
              f = r.field();
              f.name = in.read_string();
              f.type = in.read_TypeCode();
              f.visibility = in.read_short();
            }
          rt = r;
          break;

        default :
          rt = new PrimitiveTypeCode(kind);
      }
    return rt;
  }

  /**
   * Write the CORBA {@link TypeCode}. First, the TypeCode kind
   * is written as four byte long. Then, if needed, the additional
   * parameters are stored following CORBA specification.
   *
   * @param out a stream to write into.
   * @param x a {@link TypeCode} to write.
   */
  public static void write(org.omg.CORBA.portable.OutputStream out, TypeCode x)
                    throws BadKind, Bounds
  {
    out.write_long(x.kind().value());

    switch (x.kind().value())
      {
        case TCKind._tk_string :
        case TCKind._tk_wstring :
          out.write_long(x.length());
          break;

        case TCKind._tk_sequence :
        case TCKind._tk_array :
          write(out, x.content_type());
          out.write_long(x.length());
          break;

        case TCKind._tk_fixed :
          out.write_short(x.fixed_digits());
          out.write_short(x.fixed_scale());
          break;

        case TCKind._tk_objref :
        case TCKind._tk_native :
        case TCKind._tk_abstract_interface :
          out.write_string(x.id());
          out.write_string(x.name());
          break;

        case TCKind._tk_alias :
        case TCKind._tk_value_box :
          out.write_string(x.id());
          out.write_string(x.name());
          write(out, x.content_type());
          break;

        case TCKind._tk_struct :
        case TCKind._tk_except :
          out.write_string(x.id());
          out.write_string(x.name());

          out.write_long(x.member_count());

          for (int i = 0; i < x.member_count(); i++)
            {
              out.write_string(x.member_name(i));
              write(out, x.member_type(i));
            }
          break;

        case TCKind._tk_enum :
          out.write_string(x.id());
          out.write_string(x.name());

          out.write_long(x.member_count());

          for (int i = 0; i < x.member_count(); i++)
            {
              out.write_string(x.member_name(i));
            }
          break;

        case TCKind._tk_union :
          out.write_string(x.id());
          out.write_string(x.name());

          write(out, x.discriminator_type());
          out.write_long(x.default_index());

          out.write_long(x.member_count());

          for (int i = 0; i < x.member_count(); i++)
            {
              out.write_any(x.member_label(i));
              out.write_string(x.member_name(i));
              write(out, x.member_type(i));
            }
          break;

        case TCKind._tk_value :
          out.write_string(x.id());
          out.write_string(x.name());
          out.write_short(x.type_modifier());
          write(out, x.concrete_base_type());

          out.write_long(x.member_count());

          for (int i = 0; i < x.member_count(); i++)
            {
              out.write_string(x.member_name(i));
              write(out, x.member_type(i));
              out.write_short(x.member_visibility(i));
            }
          break;

        default :}
  }
}
