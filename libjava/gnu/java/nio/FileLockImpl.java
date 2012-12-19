/* FileLockImpl.java -- FileLock associated with a FileChannelImpl.
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


package gnu.java.nio;

import gnu.java.nio.channels.FileChannelImpl;

import java.io.IOException;
import java.nio.channels.FileLock;

/**
 * A FileLock associated with a FileChannelImpl.
 *
 * @author Michael Koch
 * @since 1.4
 */
public final class FileLockImpl extends FileLock
{
  /**
   * Whether or not this lock is valid, false when channel is closed or
   * release has been explicitly called.
   */
  private boolean valid;

  public FileLockImpl (FileChannelImpl channel, long position,
                       long size, boolean shared)
  {
    super (channel, position, size, shared);
    valid = true;
  }

  /**
   * Releases this lock.
   */
  protected void finalize()
  {
    try
      {
	release();
      }
    catch (IOException e)
      {
	// Ignore this.
      }
  }
  
  /**
   * Whether or not this lock is valid, false when channel is closed or
   * release has been explicitly called.
   */
  public boolean isValid()
  {
    if (valid)
      valid = channel().isOpen();
    return valid;
  }

  public void close() throws Exception
  {
    release();
  }

  /**
   * Releases the lock if it is still valid. Marks this lock as invalid.
   */
  public void release() throws IOException
  {
    if (isValid())
      {
	valid = false;
	((FileChannelImpl) channel()).unlock(position(), size());
      }
  }
}
