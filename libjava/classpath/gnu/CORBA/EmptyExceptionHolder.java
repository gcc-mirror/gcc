/* EmptyStructHolder.java --
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

import org.omg.CORBA.BAD_OPERATION;
import org.omg.CORBA.NO_IMPLEMENT;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.UNKNOWN;
import org.omg.CORBA.UnknownUserException;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.OutputStream;
import org.omg.CORBA.portable.Streamable;

/**
 * This holder can store any CORBA exception that has no user defined fields.
 * Only the repository ID is written when the method {@link #_write} is called.
 * The _read method is not supported for this holder.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class EmptyExceptionHolder
  implements Streamable
{
  /**
   * The wrapped exception.
   */
  public Throwable value;

  /**
   * The typecode of the wrapped exception.
   */
  public TypeCode typecode;

  /**
   * Create the exception holder, initialised to the given values.
   *
   * @param an_exception the wrapped exception.
   * @param an_id the exception repository id.
   */
  public EmptyExceptionHolder(Throwable an_exception, TypeCode a_typecode)
  {
    value = an_exception;
    typecode = a_typecode;
  }

  /**
   * Reads the exception from the input stream.
   *
   * The value field obtains the value of either the read exception or
   * the UNKNOWN if the repository ID does not match
   * the exception from the reachable code.
   */
  public void _read(InputStream input)
  {
    String id = input.read_string();
    Object ex = ObjectCreator.Idl2Object(id);
    if (ex == null)
      value = new UNKNOWN(id);
    else
      value = (Throwable) ex;
  }

  /**
   * Return the typecode of the stored exception.
   *
   * @return the value, passed as a_typecode in constructor.
   */
  public TypeCode _type()
  {
    return typecode;
  }

  /**
   * Write the exception into the give output stream. Writes the
   * repository id that is taken from the typecode. This method also
   * works when no helper class is available.
   *
   * @param output a stream to write into.
   *
   * @throws BAD_OPERATION if the value for the holder is not set or
   * the typecode cannot provide repository id.
   */
  public void _write(OutputStream output)
  {
    try
      {
        output.write_string(typecode.id());
      }
    catch (Exception ex)
      {
        BAD_OPERATION bad = new BAD_OPERATION();
        bad.minor = Minor.CDR;
        bad.initCause(ex);
        throw bad;
      }
  }
}