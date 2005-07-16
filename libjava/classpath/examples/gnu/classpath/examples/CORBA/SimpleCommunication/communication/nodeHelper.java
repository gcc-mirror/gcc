/* nodeHelper.java --
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


package gnu.classpath.examples.CORBA.SimpleCommunication.communication;

import org.omg.CORBA.Any;
import org.omg.CORBA.ORB;
import org.omg.CORBA.StructMember;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.OutputStream;

/**
 * This class is used for various helper operations around the
 * tree {@link} structure.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public abstract class nodeHelper
{
  /**
   * The node repository id, used to identify the structure.
   */
  private static String _id =
    "IDL:gnu/classpath/examples/CORBA/SimpleCommunication/communication/node:1.0";

  /**
   * Caches the typecode, allowing to compute it only once.
   */
  private static TypeCode typeCode;

  /**
   * This is used to handle the recursive object references in
   * CORBA - supported way. The tree node definition is recursive,
   * as the node contains the sequence of the nodes as its field.
   */
  private static boolean active;

  /**
   * Extract the tree node from the unversal CORBA wrapper, Any.
   */
  public static node extract(Any a)
  {
    return read(a.create_input_stream());
  }

  /**
   * Get the node string identifer.
   */
  public static String id()
  {
    return _id;
  }

  /**
   * Insert the node into the universal CORBA wrapper, Any.
   */
  public static void insert(Any a, node that)
  {
    OutputStream out = a.create_output_stream();
    a.type(type());
    write(out, that);
    a.read_value(out.create_input_stream(), type());
  }

  /**
   * Read the node from the common data reprentation (CDR) stream.
   */
  public static node read(InputStream istream)
  {
    node value = new node();
    value.name = istream.read_string();

    int _len0 = istream.read_long();
    value.children = new node[ _len0 ];
    for (int i = 0; i < value.children.length; ++i)
      value.children [ i ] = nodeHelper.read(istream);
    return value;
  }

  /**
   * Get the node type code definition.
   */
  public static synchronized TypeCode type()
  {
    // Compute the type code only once.
    if (typeCode == null)
      {
        synchronized (TypeCode.class)
          {
            if (typeCode == null)
              {
                // To avoid the infinite recursion loop, the
                // recursive reference is handled in specific way.
                if (active)
                  return ORB.init().create_recursive_tc(_id);
                active = true;

                // List all memebers of the node structure.
                StructMember[] members = new StructMember[ 2 ];
                TypeCode memberType;
                memberType = ORB.init().create_string_tc(0);
                members [ 0 ] = new StructMember("name", memberType, null);
                memberType = ORB.init().create_recursive_tc("");
                members [ 1 ] = new StructMember("children", memberType, null);
                typeCode =
                  ORB.init().create_struct_tc(nodeHelper.id(), "node", members);
                active = false;
              }
          }
      }
    return typeCode;
  }

  /**
   * Write the node into the common data reprentation (CDR) stream.
   */
  public static void write(OutputStream ostream, node value)
  {
    ostream.write_string(value.name);
    ostream.write_long(value.children.length);
    for (int i = 0; i < value.children.length; ++i)
      nodeHelper.write(ostream, value.children [ i ]);
  }
}
