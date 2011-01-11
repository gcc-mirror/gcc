/* Paper.java -- Information about a paper type.
   Copyright (C) 1999, 2006 Free Software Foundation, Inc.

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


package java.awt.print;

/**
 * This class describes a particular type of paper.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 */
public class Paper
  implements Cloneable
{
  // Height of the paper
  private double height;

  // Width of the paper
  private double width;

  // Upper left imageable X coordinate
  private double imageableX;

  // Upper left imageable Y coordinate
  private double imageableY;

  // Imageable width of the page
  private double imageableWidth;

  // Imageable height of the page
  private double imageableHeight;

  /**
   * This method creates a letter sized paper with one inch margins
   */
  public Paper()
  {
    width = 8.5 * 72;
    height = 11 * 72;
    imageableX = 72;
    imageableY = 72;
    imageableWidth = width - (2 * 72);
    imageableHeight = height - (2 * 72);
  }

  /**
   * This method returns the height of the paper in 1/72nds of an inch.
   *
   * @return The height of the paper in 1/72nds of an inch.
   */
  public double getHeight()
  {
    return height;
  }

  /**
   * Returns the width of the paper in 1/72nds of an inch.
   *
   * @return The width of the paper in 1/72nds of an inch.
   */
  public double getWidth()
  {
    return width;
  }

  /**
   * This method returns the X coordinate of the upper left hand corner of the
   * imageable area of the paper.
   *
   * @return The X coordinate of the upper left hand corner of the imageable
   *         area of the paper.
   */
  public double getImageableX()
  {
    return imageableX;
  }

  /**
   * This method returns the Y coordinate of the upper left hand corner of the
   * imageable area of the paper.
   *
   * @return The Y coordinate of the upper left hand corner of the imageable
   *         area of the paper.
   */
  public double getImageableY()
  {
    return imageableY;
  }

  /**
   * Returns the width of the imageable area of the paper.
   *
   * @return The width of the imageable area of the paper.
   */
  public double getImageableWidth()
  {
    return imageableWidth;
  }

  /**
   * Returns the height of the imageable area of the paper.
   *
   * @return The height of the imageable area of the paper.
   */
  public double getImageableHeight()
  {
    return imageableHeight;
  }

  /**
   * This method sets the size of the paper to the specified width and height,
   * which are specified in 1/72nds of an inch.
   *
   * @param width The width of the paper in 1/72nds of an inch.
   * @param height The height of the paper in 1/72nds of an inch.
   */
  public void setSize(double width, double height)
  {
    this.width = width;
    this.height = height;
  }

  /**
   * This method sets the imageable area of the paper by specifying the
   * coordinates of the upper left hand corner of that area, and its length
   * and height. All values are in 1/72nds of an inch.
   *
   * @param imageableX The X coordinate of the upper left hand corner of the
   *          imageable area, in 1/72nds of an inch.
   * @param imageableY The Y coordinate of the upper left hand corner of the
   *          imageable area, in 1/72nds of an inch.
   * @param imageableWidth The width of the imageable area of the paper, in
   *          1/72nds of an inch.
   * @param imageableHeight The heigth of the imageable area of the paper, in
   *          1/72nds of an inch.
   */
  public void setImageableArea(double imageableX, double imageableY,
      double imageableWidth, double imageableHeight)
  {
    this.imageableX = imageableX;
    this.imageableY = imageableY;
    this.imageableWidth = imageableWidth;
    this.imageableHeight = imageableHeight;
  }

  /**
   * This method creates a copy of this object.
   *
   * @return A copy of this object.
   */
  public Object clone()
  {
    try
      {
        return (super.clone());
      }
    catch (CloneNotSupportedException e)
      {
        return (null);
      }
  }

}
