/* gnuValueHolder.java --
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

import gnu.CORBA.CDR.Vio;

import org.omg.CORBA.TypeCode;
import org.omg.CORBA.ValueBaseHolder;
import org.omg.CORBA.portable.BoxedValueHelper;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.OutputStream;

import java.io.Serializable;

/**
 * Boxed value holder that also remembers the value type and the value helper.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class gnuValueHolder
  extends ValueBaseHolder
{
  /**
   * The type code of the stored value.
   */
  TypeCode type;

  /**
   * The helper that could read and write fields of the boxed value.
   */
  transient BoxedValueHelper helper;

  /**
   * If true, the helper not available.
   */
  transient boolean helper_NA;

  /**
   * Create a new instance for the given value and given type.
   */
  public gnuValueHolder(Serializable value, TypeCode a_type)
  {
    super(value);
    type = a_type;
  }

  /**
   * Get the true type, as it was passed in the constructor.
   */
  public TypeCode _type()
  {
    return type;
  }

  /**
   * Write content to the output stream. Tries to locate the
   * corresponding helper class.
   */
  public void _write(OutputStream output)
  {
    findHelper();
    if (helper == null)
      super._write(output);
    else
      Vio.write(output, value, helper);
  }

  /**
   * Read, trying to locate helper, if possible.
   */
  public void _read(InputStream input)
  {
    findHelper();
    if (helper == null)
      super._read(input);
    else
      value = Vio.read(input, helper);
  }

  /**
   * Set the read and write methods.
   */
  void findHelper()
  {
    if (helper != null || helper_NA)
      return;
    try
      {
        Class helperClass =
          ObjectCreator.forName(ObjectCreator.toHelperName(type.id()));

        helper = (BoxedValueHelper) helperClass.newInstance();
      }
    catch (Exception ex)
      {
        helper_NA = true;
      }
  }
}