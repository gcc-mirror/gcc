/* Kernel.java -- Java class for an image processing kernel
   Copyright (C) 2004 Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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


package java.awt.image;

/**
 * Kernel represents an image processing kernel.  It gets used to hold
 * convolution filters among other purposes.  It stores an array of float
 * values representing a 2-dimensional array in row-major order.
 *
 * @author <a href="mailto:jlquinn@optonline.net">Jerry Quinn</a>
 * @version 1.0
 */
public class Kernel implements Cloneable
{
  private final int width;
  private final int height;
  private final float[] data;

  /**
   * Creates a new <code>Kernel</code> instance.
   *
   * @param width The 2D width of data.
   * @param height The 2D height of data.
   * @param data The source data array.
   * @exception IllegalArgumentException if width * height < data.length.
   */
  public Kernel(int width, int height, float[] data)
    throws IllegalArgumentException
  {
    this.width = width;
    this.height = height;
    if (data.length < width * height || width < 0 || height < 0)
      throw new IllegalArgumentException();
    this.data = new float[width * height];
    System.arraycopy(data, 0, this.data, 0, width * height);
  }

  /**
   * Return the X origin: (width - 1) / 2
   */
  public final int getXOrigin()
  {
    return (width - 1) / 2;
  }

  /**
   * Return the Y origin: (height - 1) / 2
   */
  public final int getYOrigin()
  {
    return (height - 1) / 2;
  }

  /**
   * @return The kernel width.
   */
  public final int getWidth()
  {
    return width;
  }

  /**
   * @return The kernel height.
   */
  public final int getHeight()
  {
    return height;
  }

  /**
   * Return the kernel data.
   *
   * If data is null, allocates a new array and returns it.  Otherwise, the
   * kernel values are copied into data.
   *
   * @param data Array to copy values into, or null.
   * @return The array with copied values.
   * @exception IllegalArgumentException if data != null and too small.
   */
  public final float[] getKernelData(float[] data)
    throws IllegalArgumentException
  {
    if (data == null)
	return (float[])this.data.clone();

    if (data.length < this.data.length)
      throw new IllegalArgumentException();

    System.arraycopy(this.data, 0, data, 0, this.data.length);
    return data;
  }

  /**
   * @return a clone of this Kernel.
   */
  public Object clone()
  {
    return new Kernel(width, height, data);
  }
}
