/* StreamHolder.java --
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

import org.omg.CORBA.MARSHAL;
import org.omg.CORBA.NO_IMPLEMENT;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.OutputStream;
import org.omg.CORBA.portable.Streamable;

import java.io.IOException;

/**
 * A holder that stores the input stream, from that the holder data
 * can be read. There is no way to write the data into this holder.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class StreamHolder
  implements Streamable
{
  /**
   * The stream, holding the data for this holder.
   */
  protected final InputStream stream;

  /**
   * Create a holder that will read from the given stream.
   *
   * @param a_stream a stream.
   */
  public StreamHolder(InputStream a_stream)
  {
    stream = a_stream;
  }

  /**
   * This method is not in use, should never be called.
   */
  public TypeCode _type()
  {
    throw new NO_IMPLEMENT();
  }

  /**
   * Writes the data from the stored stream into the provided
   * output stream till the end of the input stream is reached.
   *
   * @throws MARSHAL if the IOException is thrown during operation.
   */
  public void _write(OutputStream output)
  {
    try
      {
        int d = stream.read();

        while (d >= 0)
          {
            output.write(d);
            d = stream.read();
          }
      }
    catch (IOException ex)
      {
        MARSHAL m = new MARSHAL();
        m.initCause(ex);
        m.minor = Minor.CDR;
        throw m;
      }
  }

  /**
   * This method is not in use, should never be called.
   */
  public void _read(InputStream input)
  {
    throw new NO_IMPLEMENT();
  }

  /**
   * Get the input stream that has been passed in constructor.
   */
  InputStream getInputStream()
  {
    return stream;
  }
}