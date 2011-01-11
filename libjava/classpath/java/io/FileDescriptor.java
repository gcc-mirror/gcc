/* FileDescriptor.java -- Opaque file handle class
   Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004
   Free Software Foundation, Inc.

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


package java.io;

import gnu.java.nio.FileChannelImpl;

import java.nio.channels.ByteChannel;
import java.nio.channels.FileChannel;

/**
 * This class represents an opaque file handle as a Java class.  It should
 * be used only to pass to other methods that expect an object of this
 * type.  No system specific information can be obtained from this object.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @author Tom Tromey (tromey@cygnus.com)
 * @date September 24, 1998
 */
public final class FileDescriptor
{
  /**
   * A <code>FileDescriptor</code> representing the system standard input
   * stream.  This will usually be accessed through the
   * <code>System.in</code>variable.
   */
  public static final FileDescriptor in
  = new FileDescriptor (FileChannelImpl.in);

  /**
   * A <code>FileDescriptor</code> representing the system standard output
   * stream.  This will usually be accessed through the
   * <code>System.out</code>variable.
   */
  public static final FileDescriptor out
  = new FileDescriptor (FileChannelImpl.out);

  /**
   * A <code>FileDescriptor</code> representing the system standard error
   * stream.  This will usually be accessed through the
   * <code>System.err</code>variable.
   */
  public static final FileDescriptor err
  = new FileDescriptor (FileChannelImpl.err);

  final ByteChannel channel;

  /**
   * This method is used to initialize an invalid FileDescriptor object.
   */
  public FileDescriptor()
  {
    channel = null;
  }

  /**
   * This method is used to initialize a FileDescriptor object.
   */
  FileDescriptor(ByteChannel channel)
  {
    this.channel = channel;
  }


  /**
   * This method forces all data that has not yet been physically written to
   * the underlying storage medium associated with this
   * <code>FileDescriptor</code>
   * to be written out.  This method will not return until all data has
   * been fully written to the underlying device.  If the device does not
   * support this functionality or if an error occurs, then an exception
   * will be thrown.
   */
  public void sync () throws SyncFailedException
  {
    if (channel instanceof FileChannel)
      {
        try
          {
            ((FileChannel) channel).force(true);
          }
        catch (IOException ex)
          {
            if (ex instanceof SyncFailedException)
              throw (SyncFailedException) ex;
            else
              throw new SyncFailedException(ex.toString());
          }
      }
  }

  /**
   * This methods tests whether or not this object represents a valid open
   * native file handle.
   *
   * @return <code>true</code> if this object represents a valid
   * native file handle, <code>false</code> otherwise
   */
  public boolean valid ()
  {
    ByteChannel c = channel;
    return (c != null) && (c.isOpen());
  }
}
