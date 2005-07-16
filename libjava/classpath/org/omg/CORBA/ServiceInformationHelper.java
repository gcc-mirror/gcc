/* ServiceInformationHelper.java --
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

/**
 * Helper operations for the service information.
 *
 * This class is part of the service information support, but the
 * 1.4 API specification states that this support should be not implemented.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public abstract class ServiceInformationHelper
{
  /**
   * The service information repository id.
   */
  private static String _id = "IDL:org/omg/CORBA/ServiceInformation:1.0";

  /**
   * The caches typecode value, computed once.
   */
  private static TypeCode typeCode;

  /**
   * Extract the service information from the given Any.
   */
  public static ServiceInformation extract(Any a)
  {
    return read(a.create_input_stream());
  }

  /**
   * Get the service information repositroy id.
   */
  public static String id()
  {
    return _id;
  }

  /**
   * Insert the service information into the given Any.
   */
  public static void insert(Any a, ServiceInformation that)
  {
    OutputStream out = a.create_output_stream();
    a.type(type());
    write(out, that);
    a.read_value(out.create_input_stream(), type());
  }

  /**
   * Read the service information from the given CDR input stream.
   */
  public static ServiceInformation read(InputStream istream)
  {
    ServiceInformation value = new ServiceInformation();
    value.service_options = LongSeqHelper.read(istream);

    int n = istream.read_ulong();
    value.service_details = new ServiceDetail[ n ];

    for (int i = 0; i < n; i++)
      value.service_details [ i ] = ServiceDetailHelper.read(istream);
    return value;
  }

  /**
   * Get the service information typecode.
   */
  public static TypeCode type()
  {
    if (typeCode == null)
      {
        ORB orb = ORB.init();

        StructMember[] members = new StructMember[ 2 ];
        TypeCode member;

        member =
          orb.create_alias_tc("IDL:omg.org/CORBA/ServiceOptionSeq:1.0",
                              "ServiceOptionSeq", LongSeqHelper.type()
                             );

        members [ 0 ] = new StructMember("service_options", member, null);

        member = ServiceDetailHelper.type();
        member = orb.create_sequence_tc(0, member);
        member =
          orb.create_alias_tc("IDL:omg.org/CORBA/ServiceDetailSeq:1.0",
                              "ServiceDetailSeq", member
                             );
        members [ 1 ] = new StructMember("service_details", member, null);

        typeCode =
          orb.create_struct_tc(ServiceInformationHelper.id(),
                               "ServiceInformation", members
                              );
      }
    return typeCode;
  }

  /**
   * Write the service information to the given CDR output stream.
   */
  public static void write(OutputStream ostream, ServiceInformation value)
  {
    LongSeqHelper.write(ostream, value.service_options);

    ostream.write_ulong(value.service_details.length);

    for (int i = 0; i < value.service_details.length; i++)
      ServiceDetailHelper.write(ostream, value.service_details [ i ]);
  }
}
