/* ProgressMonitorInputStream.java --
   Copyright (C) 2002, 2004, 2005 Free Software Foundation, Inc.

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
 * ProgressMonitorInputStream
 * @author	Andrew Selkirk
 * @author  Robert Schuster (robertschuster@fsfe.org)
 * @status updated to 1.2
 * @since 1.2
 */
public class ProgressMonitorInputStream extends FilterInputStream
{

  /**
   * monitor
   */
  private ProgressMonitor monitor;

  /**
   * read
   */
  private int read;

  /**
   * Constructor ProgressMonitorInputStream
   * @param component TODO
   * @param message TODO
   * @param stream TODO
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

    monitor = new ProgressMonitor(
      component, message, null, 0, max );
  }

  /**
   * reset
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
   * read
   * @exception IOException TODO
   * @returns int
   */
  public int read() throws IOException
  {
    int t = super.read();

    monitor.setProgress(++read);

    checkMonitorCanceled();

    return t;
  }

  /**
   * read
   * @param data TODO
   * @exception IOException TODO
   * @returns int
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
   * read
   * @param data TODO
   * @param offset TODO
   * @param length TODO
   * @exception IOException TODO
   * @returns int
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
   * skip
   * @param length TODO
   * @exception IOException TODO
   * @returns long
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
   * close
   * @exception IOException TODO
   */
  public void close() throws IOException
  {
    super.close();
    monitor.close();
  }

  /**
   * getProgressMonitor
   * @returns ProgressMonitor
   */
  public ProgressMonitor getProgressMonitor()
  {
    return monitor;
  }

  private void checkMonitorCanceled() throws InterruptedIOException
  {
    if ( monitor.isCanceled() )
      {
        throw new InterruptedIOException("ProgressMonitor was canceled");
      }
  }

}
