/* ProgressMonitorInputStream.java --
   Copyright (C) 2002, 2004, 2005, 2006, Free Software Foundation, Inc.

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


package javax.swing;

import java.awt.Component;

import java.io.FilterInputStream;
import java.io.InputStream;
import java.io.InterruptedIOException;
import java.io.IOException;

/**
 * An input stream with a {@link ProgressMonitor}.
 *
 * @author      Andrew Selkirk
 * @author  Robert Schuster (robertschuster@fsfe.org)
 * @status updated to 1.2
 * @since 1.2
 */
public class ProgressMonitorInputStream extends FilterInputStream
{

  /**
   * The monitor watching the progress of the input stream.
   */
  private ProgressMonitor monitor;

  /**
   * read
   */
  private int read;

  /**
   * Creates a new <code>ProgressMonitorInputStream</code>.
   *
   * @param component  the parent component for the progress monitor dialog.
   * @param message  the task description.
   * @param stream  the underlying input stream.
   */
  public ProgressMonitorInputStream(Component component, Object message,
                                    InputStream stream)
  {
    super(stream);

    int max = 0;

    try
      {
        max = stream.available();
      }
    catch ( IOException ioe )
      {
        // Behave like the JDK here.
      }

    monitor = new ProgressMonitor(component, message, null, 0, max);
  }

  /**
   * Resets the input stream to the point where {@link #mark(int)} was called.
   *
   * @exception IOException TODO
   */
  public void reset() throws IOException
  {
    super.reset();

    checkMonitorCanceled();

    // TODO: The docs says the monitor should be resetted. But to which
    // value? (mark is not overridden)
  }

  /**
   * Reads an unsigned byte from the input stream and returns it as an
   * <code>int</code> in the range of 0-255.  Returns -1 if the end of the
   * stream has been reached.  The progress monitor is updated.
   *
   * @return int
   *
   * @exception IOException if there is a problem reading the stream.
   */
  public int read() throws IOException
  {
    int t = super.read();

    monitor.setProgress(++read);

    checkMonitorCanceled();

    return t;
  }

  /**
   * Reads bytes from the input stream and stores them in the supplied array,
   * and updates the progress monitor (or closes it if the end of the stream
   * is reached).
   *
   * @param data  the data array for returning bytes read from the stream.
   *
   * @return The number of bytes read, or -1 if there are no more bytes in the
   *         stream.
   *
   * @throws IOException if there is a problem reading bytes from the stream.
   */
  public int read(byte[] data) throws IOException
  {
    int t = super.read(data);

    if ( t > 0 )
      {
        read += t;
        monitor.setProgress(read);

        checkMonitorCanceled();
      }
    else
      {
        monitor.close();
      }

    return t;
  }

  /**
   * Reads up to <code>length</code> bytes from the input stream and stores
   * them in the supplied array at the given index, and updates the progress
   * monitor (or closes it if the end of the stream is reached).
   *
   * @param data  the data array for returning bytes read from the stream.
   * @param offset  the offset into the array where the bytes should be written.
   * @param length  the maximum number of bytes to read from the stream.
   *
   * @return The number of bytes read, or -1 if there are no more bytes in the
   *         stream.
   *
   * @throws IOException if there is a problem reading bytes from the stream.
   */
  public int read(byte[] data, int offset, int length) throws IOException
  {
    int t = super.read(data, offset, length);

    if ( t > 0 )
      {
        read += t;
        monitor.setProgress(read);

        checkMonitorCanceled();
      }
    else
      {
        monitor.close();
      }

    return t;
  }

  /**
   * Skips the specified number of bytes and updates the
   * {@link ProgressMonitor}.
   *
   * @param length the number of bytes to skip.
   *
   * @return The actual number of bytes skipped.
   *
   * @throws IOException if there is a problem skipping bytes in the stream.
   */
  public long skip(long length) throws IOException
  {
    long t = super.skip(length);

    // 'read' may overflow here in rare situations.
    assert ( (long) read + t <= (long) Integer.MAX_VALUE );

    read += (int) t;

    monitor.setProgress(read);

    checkMonitorCanceled();

    return t;
  }

  /**
   * Closes the input stream and the associated {@link ProgressMonitor}.
   *
   * @throws IOException if there is a problem closing the input stream.
   */
  public void close() throws IOException
  {
    super.close();
    monitor.close();
  }

  /**
   * Returns the {@link ProgressMonitor} used by this input stream.
   *
   * @return The progress monitor.
   */
  public ProgressMonitor getProgressMonitor()
  {
    return monitor;
  }

  private void checkMonitorCanceled() throws InterruptedIOException
  {
    if (monitor.isCanceled())
      {
        throw new InterruptedIOException("ProgressMonitor was canceled");
      }
  }

}
