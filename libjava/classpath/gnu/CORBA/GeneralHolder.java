/* universalStreamable.java --
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

import gnu.CORBA.CDR.BufferredCdrInput;
import gnu.CORBA.CDR.BufferedCdrOutput;

import org.omg.CORBA.BAD_OPERATION;
import org.omg.CORBA.MARSHAL;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.OutputStream;
import org.omg.CORBA.portable.Streamable;

import java.io.IOException;

/**
 * This class holds the abstract binary data array of the Streamable
 * being stored. It is used to insert and extract into {@link Any} objects
 * that have no holder, but have the helper classes.
 * Encoding/decoding then must be performed by the helper. This class is
 * defined as package private because it is not idiot proof and
 * must be used with care.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class GeneralHolder
  implements Streamable
{
  /**
   * The binary data, stored inside this holder.
   */
  private BufferedCdrOutput value = new BufferedCdrOutput();

  /**
   * Create the universal holder that uses the given buffer to store the data.
   */
  public GeneralHolder(BufferedCdrOutput buffer)
  {
    value = buffer;
  }

  /**
   * Reads into the internal buffer till the end of stream is
   * reached. No alignment operations are performed. This operation
   * normally reads from the stream, where the single value,
   * stored using {@link #_write}, is written.
   *
   * @throws MARSHALL, if the IOException is thrown during the
   * stream operation.
   */
  public void _read(InputStream input)
  {
    try
      {
        if (input instanceof BufferredCdrInput)
          {
            BufferredCdrInput b = (BufferredCdrInput) input;
            value.write(b.buffer.getBuffer());
          }
        else
          {
            int c;

            do
              {
                c = input.read();
                if (c >= 0)
                  value.write(c);
              }
            while (c >= 0);
          }
      }
    catch (IOException ex)
      {
        MARSHAL t = new MARSHAL();
        t.minor = Minor.Any;
        t.initCause(ex);
        throw t;
      }
  }

  /**
   * The type is not known and cannot be returned.
   *
   * @throws BAD_OPERATION, always.
   */
  public TypeCode _type()
  {
    BAD_OPERATION bad = new BAD_OPERATION();
    bad.minor = Minor.Inappropriate;
    throw bad;
  }

  /**
   * Writes the binary data being stored into the given output
   * stream. This operation supposes that the current stream
   * position is 0 or satisfies the required alignments anyway.
   *
   * @throws MARSHAL if the IOExeption is thrown when writing the
   * byte array.
   */
  public void _write(OutputStream output)
  {
    try
      {
        value.buffer.writeTo(output);
      }
    catch (IOException ex)
      {
        MARSHAL t = new MARSHAL();
        t.minor = Minor.Any;
        t.initCause(ex);
        throw t;
      }
  }

  /**
   * Get the input stream that reads the fields of the stored value.
   */
  InputStream getInputStream()
  {
    return value.create_input_stream();
  }

  /**
   * Clone.
   */
  public GeneralHolder Clone()
  {
    try
      {
        BufferedCdrOutput nb = new BufferedCdrOutput(value.buffer.size());
        value.buffer.writeTo(nb);
        return new GeneralHolder(nb);
      }
    catch (IOException ex)
      {
        throw new Unexpected(ex);
      }
  }
}
