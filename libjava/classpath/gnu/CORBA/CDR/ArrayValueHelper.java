/* ArrayValueHelper.java --
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


package gnu.CORBA.CDR;

import gnu.CORBA.ObjectCreator;

import org.omg.CORBA.BooleanSeqHelper;
import org.omg.CORBA.CharSeqHelper;
import org.omg.CORBA.DoubleSeqHelper;
import org.omg.CORBA.FloatSeqHelper;
import org.omg.CORBA.LongLongSeqHelper;
import org.omg.CORBA.LongSeqHelper;
import org.omg.CORBA.OctetSeqHelper;
import org.omg.CORBA.ShortSeqHelper;
import org.omg.CORBA.portable.BoxedValueHelper;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.OutputStream;

import java.io.Serializable;
import java.lang.reflect.Array;
import java.rmi.Remote;

import javax.rmi.CORBA.Util;
import javax.rmi.CORBA.ValueHandler;

/**
 * Writes arrays as a boxed value types. A single instance is used to write a
 * single array. This class is only used with RMI/IIOP, to handle array boxed
 * values.
 * 
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
class ArrayValueHelper
  implements BoxedValueHelper
{
  /**
   * The value handler (one for all instances).
   */
  static ValueHandler handler = Util.createValueHandler();

  /**
   * A class of the array being written.
   */
  Class arrayClass;

  /**
   * The array component class.
   */
  Class component;

  /**
   * The array component repository Id.
   */
  String componentId;

  /**
   * If true, the array members are written as objects rather than as values.
   * True for Remotes and CORBA objects.
   */
  boolean written_as_object()
  {
    return org.omg.CORBA.Object.class.isAssignableFrom(component)
      || Remote.class.isAssignableFrom(component);
  }

  /**
   * Creates the instance of the helper to write this specific array class.
   */
  ArrayValueHelper(Class an_arrayClass)
  {
    arrayClass = an_arrayClass;
  }

  /**
   * Get the array repository Id that will be the RMI repository id.
   */
  public String get_id()
  {
    return ObjectCreator.getRepositoryId(arrayClass);
  }

  /**
   * Read the array from the input stream.
   */
  public Serializable read_value(InputStream input)
  {
    if (input instanceof HeadlessInput)
      {
        ((HeadlessInput) input).subsequentCalls = true;
      }

    component = arrayClass.getComponentType();

    if (component.equals(byte.class))
      return OctetSeqHelper.read(input);
    else if (component.equals(String.class))
      {
        // String array is optimized because this may be frequent.
        String[] s = new String[input.read_long()];

        for (int i = 0; i < s.length; i++)
          s[i] = (String) Vio.read(input, Vio.m_StringValueHelper);
        return s;
      }
    else if (component.equals(int.class))
      return LongSeqHelper.read(input);
    else if (component.equals(long.class))
      return LongLongSeqHelper.read(input);
    else if (component.equals(double.class))
      return DoubleSeqHelper.read(input);
    else if (component.equals(float.class))
      return FloatSeqHelper.read(input);
    else if (component.equals(boolean.class))
      return BooleanSeqHelper.read(input);
    else if (component.equals(short.class))
      return ShortSeqHelper.read(input);
    else if (component.equals(char.class))
      return CharSeqHelper.read(input);
    else
      {
        // Read others, use reflection.
        int n = input.read_long();

        gnuValueStream s = null;

        Serializable array = (Serializable) Array.newInstance(component, n);
        if (written_as_object())
          for (int i = 0; i < n; i++)
            {
              gnuRuntime g;
              int position;
              if (input instanceof gnuValueStream)
                {
                  s = (gnuValueStream) input;
                  g = s.getRunTime();
                  position = s.getPosition();
                }
              else
                {
                  g = null;
                  position = -1;
                }

              if (input instanceof HeadlessInput)
                ((HeadlessInput) input).subsequentCalls = true;

              Object o = handler.readValue(input, position, component, null, g);
              Array.set(array, i, o);
            }
        else
          for (int i = 0; i < n; i++)
            Array.set(array, i, Vio.read(input, component));
        return array;
      }
  }

  /**
   * Write the array to the input stream.
   */
  public void write_value(OutputStream output, Serializable value)
  {
    if (output instanceof gnuValueStream)
      {
        gnuRuntime r = ((gnuValueStream) output).getRunTime();
        if (r != null)
          r.target = null;
      }

    if (value instanceof byte[])
      OctetSeqHelper.write(output, (byte[]) value);
    else if (value instanceof String[])
      {
        String[] s = (String[]) value;
        output.write_long(s.length);
        for (int i = 0; i < s.length; i++)
          Vio.write(output, s[i], Vio.m_StringValueHelper);
      }
    else if (value instanceof int[])
      LongSeqHelper.write(output, (int[]) value);
    else if (value instanceof long[])
      LongLongSeqHelper.write(output, (long[]) value);
    else if (value instanceof double[])
      DoubleSeqHelper.write(output, (double[]) value);
    else if (value instanceof float[])
      FloatSeqHelper.write(output, (float[]) value);
    else if (value instanceof boolean[])
      BooleanSeqHelper.write(output, (boolean[]) value);
    else if (value instanceof short[])
      ShortSeqHelper.write(output, (short[]) value);
    else if (value instanceof char[])
      CharSeqHelper.write(output, (char[]) value);
    else
      {
        // Write others, use reflection.
        component = arrayClass.getComponentType();

        int n = Array.getLength(value);
        output.write_long(n);
        if (written_as_object())
          for (int i = 0; i < n; i++)
            {
              Object o = Array.get(value, i);
              if (o == null)
                output.write_Object(null);
              else
                // CORBA objects have another notation.
                handler.writeValue(output, (Serializable) o);
            }
        else
          {
            for (int i = 0; i < n; i++)
              Vio.write(output, (Serializable) Array.get(value, i),
                component);
          }

      }
  }
}
