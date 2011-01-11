/* GiopIo.java -- Generates GIOP input/output statements.
   Copyright (C) 2006 Free Software Foundation

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


package gnu.classpath.tools.rmic;

import java.rmi.Remote;

import org.omg.CORBA.portable.ObjectImpl;

/**
 * Generates the code for reading and writing data over GIOP stream.
 *
 * @author Audrius Meskauskas, Lithuania (audriusa@Bioinformatics.org)
 */
public class GiopIo
{
  /**
   * Get the statement for writing the variable of the given type to the GIOP ({@link org.omg.CORBA_2_3.portable.OutputStream) stream. The
   * stream is always named "out".
   *
   * @param c
   *          the class of the object being written
   * @param variable
   *          the variable, where the object value is stored
   * @param r
   *          the parent generator, used to name the class
   * @return the write statement.
   */
  public static String getWriteStatement(Class c, String variable, SourceGiopRmicCompiler r)
  {
    if (c.equals(boolean.class))
      return "out.write_boolean(" + variable + ");";
    if (c.equals(byte.class))
      return "out.write_octet(" + variable + ");";
    else if (c.equals(short.class))
      return "out.write_int(" + variable + ");";
    else if (c.equals(int.class))
      return "out.write_long(" + variable + ");";
    else if (c.equals(long.class))
      return "out.write_long_long(" + variable + ");";
    else if (c.equals(double.class))
      return "out.write_double(" + variable + ");";
    else if (c.equals(float.class))
      return "out.write_float(" + variable + ");";
    else if (c.equals(char.class))
      return "out.write_char(" + variable + ");";
    else if (Remote.class.isAssignableFrom(c))
      return "Util.writeRemoteObject(out, " + variable + ");";
    else if (ObjectImpl.class.isAssignableFrom(c))
      return "out.write_Object(" + variable + ");";
    else
      return "out.write_value(" + variable + ", " + r.name(c) + ".class);";
  }

  /**
   * Get the statement for reading the value of the given type from to the GIOP ({@link org.omg.CORBA_2_3.portable.InputStream) stream. The
   * stream is always named "in".
   *
   * @param c
   *          the class of the object being written
   * @param r
   *          the parent generator, used to name the class
   * @return the right side of the read statement.
   */
  public static String getReadStatement(Class c, SourceGiopRmicCompiler r)
  {
    if (c.equals(boolean.class))
      return "in.read_boolean();";
    else if (c.equals(byte.class))
      return "in.read_octet();";
    else if (c.equals(short.class))
      return "in.read_int();";
    else if (c.equals(int.class))
      return "in.read_long();";
    else if (c.equals(long.class))
      return "in.read_long_long();";
    else if (c.equals(double.class))
      return "in.read_double();";
    else if (c.equals(float.class))
      return "in.read_float();";
    else if (c.equals(char.class))
      return "in.read_char();";
    else if (Remote.class.isAssignableFrom(c))
      return "(" + r.name(c)
             + ") PortableRemoteObject.narrow(in.read_Object()," + r.name(c)
             + ".class);";
    else if (ObjectImpl.class.isAssignableFrom(c))
      return "in.read_Object();";
    else
      return "(" + r.name(c)
             + ") in.read_value(" + r.name(c) + ".class);";
  }

}
