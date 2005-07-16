/* ourUserExceptionHelper.java --
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
import org.omg.CORBA.TCKind;
import org.omg.CORBA.TypeCode;

/**
 * The class, providing various helper operations with our user
 * exception.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public abstract class ourUserExceptionHelper
{
  /**
   * The exception repository id. This name is also used to find the
   * mapping local CORBA class.
   */
  private static String _id =
    "IDL:gnu/classpath/examples/CORBA/SimpleCommunication/communication/ourUserException:1.0";

  /**
   * Get the exception repository id.
   */
  public static String id()
  {
    return _id;
  }

  /**
   * Extract the exception from the given Any where it might be
   * wrapped.
   */
  public static ourUserException extract(Any a)
  {
    return read(a.create_input_stream());
  }

  /**
   * Read the exception from the CDR stream.
   */
  public static ourUserException read(org.omg.CORBA.portable.InputStream istream)
  {
    ourUserException value = new ourUserException(0);

    // The repository ID is not used
    istream.read_string();
    value.ourField = istream.read_long();
    return value;
  }

  /**
   * Create the type code of this exception.
   */
  public static synchronized TypeCode type()
  {
    StructMember[] members = new StructMember[ 1 ];
    TypeCode member = null;
    member = ORB.init().get_primitive_tc(TCKind.tk_long);
    members [ 0 ] = new StructMember("ourField", member, null);
    return ORB.init().create_struct_tc(ourUserExceptionHelper.id(),
                                       "ourUserException", members
                                      );
  }

  /**
   * Write the exception into the CDR stream.
   */
  public static void write(org.omg.CORBA.portable.OutputStream ostream,
                           ourUserException value
                          )
  {
    ostream.write_string(id());
    ostream.write_long(value.ourField);
  }
}