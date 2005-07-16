/* ServiceDetailHelper.java --
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

import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.OutputStream;
import gnu.CORBA.*;

/**
 * The helper operations on the Service Detail.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public abstract class ServiceDetailHelper
{
  /**
   * The service detail repository id.
   */
  private static String _id = "IDL:omg.org/CORBA/ServiceDetail:1.0";

  /**
   * The cached typecode value, computed once.
   */
  private static TypeCode typeCode;

  /**
   * Extract the service detail info from the given {@link Any}
   *
   * @param a the Any to extract from.
   *
   * @return the extracted detail.
   *
   * @throws BAD_OPERATION if the parameter holds something different
   * from the ServiceDetail.
   */
  public static ServiceDetail extract(Any a)
  {
    try
      {
        return ((ServiceDetailHolder) a.extract_Streamable()).value;
      }
    catch (ClassCastException ex)
      {
        throw new BAD_OPERATION();
      }
  }

  /**
   * Get the service detail repository id.
   *
   * @return the service detail repository id,
   * <code>IDL:omg.org/CORBA/ServiceDetail:1.0</code>, always.
   */
  public static String id()
  {
    return _id;
  }

  /**
   * Insert the service detail into the given {@link Any}.
   *
   * @param a the Any to insert into.
   *
   * @param that the detail to insert.
   */
  public static void insert(Any a, ServiceDetail that)
  {
    a.insert_Streamable(new ServiceDetailHolder(that));
  }

  /**
   * Read the service detail information from the given CDR
   * intput stream. First reads the type, then the flexible
   * length byte sequence.
   *
   * @param istream a stram to read from.
   *
   * @return the loaded service detail.
   */
  public static ServiceDetail read(InputStream istream)
  {
    ServiceDetail value = new ServiceDetail();
    value.service_detail_type = istream.read_ulong();

    int l = istream.read_long();
    value.service_detail = new byte[ l ];
    istream.read_octet_array(value.service_detail, 0, l);
    return value;
  }

  /**
   * Get the typecode of the service detail, assuming to be it
   * a structure with the two fields.
   *
   * @return the newly created or cached typecode value.
   */
  public static TypeCode type()
  {
    if (typeCode == null)
      {
        ORB orb = ORB.init();

        StructMember[] members = new StructMember[ 2 ];

        TypeCode type =
          orb.create_alias_tc(_id, "ServiceDetailType",
                              orb.get_primitive_tc(TCKind.tk_ulong)
                             );
        members [ 0 ] = new StructMember("service_detail_type", type, null);

        TypeCode data =
          orb.create_sequence_tc(0, orb.get_primitive_tc(TCKind.tk_octet));
        members [ 1 ] = new StructMember("service_detail", data, null);

        typeCode = orb.create_struct_tc(id(), "ServiceDetail", members);
      }
    return typeCode;
  }

  /**
   * Write the service detail data to the given CDR output stream.
   * Writes the detail type first, then the detail type data
   * as the variable length byte sequence.
   *
   * @param ostream a stream to write into.
   * @param value a value to write.
   */
  public static void write(OutputStream ostream, ServiceDetail value)
  {
    ostream.write_ulong(value.service_detail_type);
    ostream.write_long(value.service_detail.length);
    ostream.write_octet_array(value.service_detail, 0,
                              value.service_detail.length
                             );
  }
}
