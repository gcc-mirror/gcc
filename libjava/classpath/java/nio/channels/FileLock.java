/* FileLock.java --
   Copyright (C) 2002, 2005 Free Software Foundation, Inc.

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

package java.nio.channels;

import gnu.java.lang.CPStringBuilder;

import java.io.IOException;

/**
 * @since 1.4
 */
public abstract class FileLock
{
  private final FileChannel channel;
  private final long position;
  private final long size;
  private final boolean shared;

  /**
   * Initializes the file lock.
   *
   * @exception IllegalArgumentException If the preconditions on the parameters do not hold
   */
  protected FileLock(FileChannel channel, long position, long size,
                     boolean shared)
  {
    if (position < 0 || size < 0)
      throw new IllegalArgumentException();

    this.channel = channel;
    this.position = position;
    this.size = size;
    this.shared = shared;
  }

  /**
   * Tells whether or not this lock is valid.
   */
  public abstract boolean isValid();

  /**
   * Releases this lock.
   *
   * @exception IOException If an error occurs
   * @exception ClosedChannelException If the locked channel is no longer open.
   */
  public abstract void release() throws IOException;

  /**
   * Returns the file channel upon whose file this lock is held.
   */
  public final FileChannel channel()
  {
    return channel;
  }

  /**
   * Tells whether this lock is shared.
   */
  public final boolean isShared()
  {
    return shared;
  }

  /**
   * Tells whether or not this lock overlaps the given lock range.
   */
  public final boolean overlaps(long position, long size)
  {
    if (position > this.position + this.size)
      return false;

    if (position + size < this.position)
      return false;

    return true;
  }

  /**
   * Returns the position within the file of the first byte of the
   * locked region.
   */
  public final long position()
  {
    return position;
  }

  /**
   * Returns the size of the locked region in bytes.
   */
  public final long size()
  {
    return size;
  }

  /**
   * Returns a string describing the range, type, and validity of this lock.
   */
  public final String toString()
  {
    CPStringBuilder buf = new CPStringBuilder(getClass().getName());
    buf.append("[");
    buf.append(position);
    buf.append(":");
    buf.append(size);
    if (shared)
      buf.append(" shared");
    else
      buf.append(" exclusive");
    if (isValid())
      buf.append(" valid]");
    else
      buf.append(" invalid]");
    return buf.toString();
  }
}
