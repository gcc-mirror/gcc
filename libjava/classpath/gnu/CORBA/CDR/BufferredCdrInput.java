/* BufferredCdrInput.java --
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


/**
 * The CDR input stream that reads data from the byte buffer.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class BufferredCdrInput
  extends AbstractCdrInput 
  implements gnuValueStream
{
  /** 
   * Use serialVersionUID for interoperability. 
   */
  private static final long serialVersionUID = 1;
  
  /**
   * The byte array input stream to read data from.
   */
  public final AligningInput buffer;

  /**
   * Creates the CDR input stream that reads from the given buffer
   * array.
   *
   * @param a_buffer an array to read from.
   */
  public BufferredCdrInput(byte[] a_buffer)
  {
    buffer = new AligningInput(a_buffer);
    setInputStream(buffer);
  }

  /**
   * Set the alignment offset, if the index of the first byte in the
   * stream is different from 0.
   */
  public void setOffset(int offset)
  {
    buffer.setOffset(offset);
  }

  /**
   * Skip several bytes, aligning the internal pointer on the
   * selected boundary.
   */
  public void align(int alignment)
  {
    buffer.align(alignment);
  }

  /**
   * Mark the current position.
   * @param ahead
   */
  public synchronized void mark(int ahead)
  {
    buffer.mark(ahead);
  }

  /**
   * Checks if marking is supported.
   * @return
   */
  public boolean markSupported()
  {
    return buffer.markSupported();
  }

  /**
   * Resets the stream to the previously marked position.
   */
  public void reset()
  {
    buffer.reset();
    setInputStream(buffer);
  }
  
  /**
   * Get the current position in the buffer.
   * 
   * @return The position in the buffer, taking offset into consideration.
   */
  public int getPosition()
  {
    return buffer.getPosition();
  }
  
  /**
   * Jump to the given position, taking offset into consideration.
   */
  public void seek(int position)
  {
    buffer.seek(position);
    setInputStream(buffer);
  }
  
  /**
   * Get the associated RunTime.
   */
  public gnuRuntime getRunTime()
  {
    return runtime;
  }  
  
  /**
   * Replace the instance of RunTime.
   */
  public void setRunTime(gnuRuntime a_runtime)
  {
    runtime = a_runtime;
  }
  
}
