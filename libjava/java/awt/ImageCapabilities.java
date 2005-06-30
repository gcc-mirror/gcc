/* ImageCapabilities.java -- the capabilities of an image buffer
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


package java.awt;

/**
 * This class represents the capabilities of an image buffer.  An
 * image buffer may be backed by accelerated graphics resources.
 * Those resources may or may not be volatile.  This class is used to
 * describe these image buffer characteristics.
 */
public class ImageCapabilities implements Cloneable
{
  /**
   * Whether or not this the image buffer uses accelerated graphics
   * resources.
   */
  private final boolean accelerated;

  /**
   * Create a new image capability descriptor.
   *
   * @param accelerated true if the image buffer uses accelerated
   * graphics resources
   */
  public ImageCapabilities(boolean accelerated)
  {
    this.accelerated = accelerated;
  }

  /**
   * Returns whether or not the image buffer uses accelerated graphics
   * resources.
   *
   * @return true if the image buffer uses accelerated graphics
   * resources; false otherwise
   */
  public boolean isAccelerated()
  {
    return accelerated;
  }

  /**
   * Returns whether or not the image buffer's resources are volatile,
   * meaning that they can be reclaimed by the graphics system at any
   * time.
   *
   * @return true if the image buffer's resources are volatile; false
   * otherwise
   */
  public boolean isTrueVolatile()
  {
    return true;
  }

  /**
   * Clone this image capability descriptor.
   *
   * @return a clone of this image capability descriptor
   */
  public Object clone()
  {
    try
      {
        return super.clone();
      }
    catch (CloneNotSupportedException e)
      {
        throw (Error) new InternalError().initCause(e);
      }
  }
}
