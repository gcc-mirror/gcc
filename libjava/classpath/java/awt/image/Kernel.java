/* Kernel.java -- Java class for an image processing kernel
   Copyright (C) 2004, 2005, 2006,  Free Software Foundation, Inc.

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


package java.awt.image;

/**
 * Kernel represents an image processing kernel.  It gets used to hold
 * convolution filters among other purposes.  It stores an array of float
 * values representing a 2-dimensional array in row-major order.
 *
 * @author Jerry Quinn (jlquinn@optonline.net)
 */
public class Kernel implements Cloneable
{
  /** The kernel width. */
  private final int width;
  
  /** The kernel height. */
  private final int height;
  
  /** Internal storage for the kernel's values. */
  private final float[] data;

  /**
   * Creates a new <code>Kernel</code> instance with the specified dimensions
   * and values.  The first <code>width * height</code> values in the specified
   * <code>data</code> array are copied to internal storage.
   *
   * @param width  the kernel width.
   * @param height  the kernel height.
   * @param data  the source data array (<code>null</code> not permitted).
   * 
   * @throws IllegalArgumentException if <code>data.length</code> is less than
   *     <code>width * height</code>.
   * @throws IllegalArgumentException if <code>width</code> or 
   *      <code>height</code> is less than zero.
   * @throws NullPointerException if <code>data</code> is <code>null</code>.
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
   * Returns the x-origin for the kernel, which is calculated as 
   * <code>(width - 1) / 2</code>.
   * 
   * @return The x-origin for the kernel.
   */
  public final int getXOrigin()
  {
    return (width - 1) / 2;
  }

  /**
   * Returns the y-origin for the kernel, which is calculated as
   * <code>(height - 1) / 2</code>.
   * 
   * @return The y-origin for the kernel.
   */
  public final int getYOrigin()
  {
    return (height - 1) / 2;
  }

  /**
   * Returns the kernel width (as supplied to the constructor).
   * 
   * @return The kernel width.
   */
  public final int getWidth()
  {
    return width;
  }

  /**
   * Returns the kernel height (as supplied to the constructor).
   * 
   * @return The kernel height.
   */
  public final int getHeight()
  {
    return height;
  }

  /**
   * Returns an array containing a copy of the kernel data.  If the 
   * <code>data</code> argument is non-<code>null</code>, the kernel values
   * are copied into it and then <code>data</code> is returned as the result.
   * If the <code>data</code> argument is <code>null</code>, this method 
   * allocates a new array then populates and returns it.
   *
   * @param data  an array to copy the return values into (if 
   *     <code>null</code>, a new array is allocated).
   *     
   * @return The array with copied values.
   * 
   * @throws IllegalArgumentException if <code>data.length</code> is less than 
   *     the kernel's <code>width * height</code>.
   */
  public final float[] getKernelData(float[] data)
    throws IllegalArgumentException
  {
    if (data == null)
      return (float[]) this.data.clone();

    if (data.length < this.data.length)
      throw new IllegalArgumentException();

    System.arraycopy(this.data, 0, data, 0, this.data.length);
    return data;
  }

  /**
   * Returns a clone of this kernel.
   * 
   * @return a clone of this Kernel.
   */
  public Object clone()
  {
    try
      {
        return super.clone();
      }
    catch (CloneNotSupportedException e)
      {
        throw (Error) new InternalError().initCause(e); // Impossible
      }
  }
}
