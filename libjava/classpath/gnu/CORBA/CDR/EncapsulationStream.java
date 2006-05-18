/* EncapsulationOutput.java --
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

import java.io.IOException;

/**
 * The encapsulated data, as they are defined by CORBA specification.
 * This includes the extra 0 byte (Big endian) in the beginning.
 * When written to the parent steam, the encapsulated data are preceeded
 * by the data length in bytes.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class EncapsulationStream
  extends AbstractCdrOutput
{
  /**
   * The Big Endian (most siginificant byte first flag).
   */
  public static final byte BIG_ENDIAN = 0;

  /**
   * The Little Endian (least siginificant byte first flag).
   */
  public static final byte LITTLE_ENDIAN = 1;

  /**
   * The byte buffer.
   */
  public final AligningOutput buffer;

  /**
   * The stream, where the data are being encapsulated.
   */
  public final org.omg.CORBA.portable.OutputStream parent;

  /**
   * Create the EncapsulationOutput with the given parent stream
   * and the specified encoding.
   */
  public EncapsulationStream(org.omg.CORBA.portable.OutputStream _parent,
                            boolean use_big_endian)
  {
    super();
    setBigEndian(use_big_endian);
    buffer = new AligningOutput();
    setOutputStream(buffer);
    parent = _parent;
    write(use_big_endian?BIG_ENDIAN:LITTLE_ENDIAN);
  }

  /**
   * Set the alignment offset, if the index of the first byte in the
   * stream is different from 0.
   */
  public void setOffset(int an_offset)
  {
    buffer.setOffset(an_offset);
  }

  /**
   * Align the curretn position at the given natural boundary.
   */
  public void align(int boundary)
  {
    buffer.align(boundary);
  }

  /**
   * Writes the content of the encapsulated output into the parent
   * buffer.
   */
  public void close()
  {
    try
      {
        parent.write_long(buffer.size());
        buffer.writeTo(parent);
      }
    catch (IOException ex)
      {
        InternalError err = new InternalError();
        err.initCause(ex);
        throw err;
      }
  }

  /**
   * Return the input stream that reads the previously written values.
   */
  public org.omg.CORBA.portable.InputStream create_input_stream()
  {
    BufferredCdrInput in = new BufferredCdrInput(buffer.toByteArray());
    in.setOrb(orb);

    in.setVersion(giop);
    in.setCodeSet(getCodeSet());

    return in;
  }

  /**
   * Resets (clears) the buffer.
   */
  public void reset()
  {
    buffer.reset();
    setOutputStream(buffer);
  }
}
