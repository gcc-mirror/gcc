/* ImageGraphicAttribute.java
   Copyright (C) 2003 Free Software Foundation, Inc.

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


package java.awt.font;

import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.geom.Rectangle2D;

/**
 * This is an implementation of GraphicAttribute which draws images in a
 * TextLayout.
 * 
 * @author Lillian Angel
 * @author Michael Koch
 */
public final class ImageGraphicAttribute
    extends GraphicAttribute
{
  private Image image;
  private float originX;
  private float originY;

  /**
   * Constucts an instance from the specified Image. The origin is at (0, 0).
   * 
   * @param image - image to construct from.
   * @param alignment - the alignment
   */
  public ImageGraphicAttribute(Image image, int alignment)
  {
    this(image, alignment, 0, 0);
  }

  /**
   * Constucts an instance from the specified Image. The origin is at (originX,
   * originY).
   * 
   * @param image - image to construct from
   * @param alignment - the alignment
   * @param originX - x point of origin
   * @param originY - y point of origin
   */
  public ImageGraphicAttribute(Image image, int alignment, float originX,
                               float originY)
  {
    super(alignment);
    this.image = image;
    this.originX = originX;
    this.originY = originY;
  }

  /**
   * Draws the image at the specified location, relative to the
   * origin.
   * 
   * @param g - the graphics to use to render the image
   * @param x - the x location
   * @param y - the y location 
   */
  public void draw(Graphics2D g, float x, float y)
  {
    g.drawImage(image, (int) (x - originX), (int) (y - originY), null);
  }

  /**
   * Compares this to the specified Object
   * 
   * @param obj - the object to compare
   * @return true if the obj and this are equivalent
   */
  public boolean equals(Object obj)
  {
    if (! (obj instanceof ImageGraphicAttribute))
      return false;

    return equals((ImageGraphicAttribute) obj);
  }

  /**
   * Compares this to the ImageGraphicAttribute given, by
   * comparing all fields and values.
   * 
   * @param rhs - the ImageGraphicAttribute to compare
   * @return true if the object given is equivalent to this
   */
  public boolean equals(ImageGraphicAttribute rhs)
  {
    return ((this == rhs) || ((this.getAscent() == rhs.getAscent())
                              && (this.getAdvance() == rhs.getAdvance())
                              && (this.getAlignment() == rhs.getAlignment())
                              && (this.getBounds().equals(rhs.getBounds()))
                              && (this.getDescent() == rhs.getDescent())
                              && (this.hashCode() == rhs.hashCode())
                              && (this.image.equals(rhs.image))
                              && (this.originX == rhs.originX) 
                              && (this.originY == rhs.originY)));
  }

  /**
   * Returns distance from the origin to the right edge of the image of this.
   * 
   * @return the advance
   */
  public float getAdvance()
  {
    return Math.max(0, image.getWidth(null) - originX);
  }

  /**
   * Returns the the distance from the top of the image to the origin of this.
   * 
   * @return the ascent.
   */
  public float getAscent()
  {
    return Math.max(0, originY);
  }

  /**
   * Gets the bounds of the object rendered, relative to the position.
   * 
   * @return the bounds of the object rendered, relative to the position.
   */
  public Rectangle2D getBounds()
  {
    // This is equivalent to what Sun's JDK returns.
    // I am not entirely sure why the origin is negative.
    return new Rectangle2D.Float(- originX, - originY, image.getWidth(null),
                                 image.getHeight(null));
  }

  /**
   * Returns the distance from the origin to the bottom of the image.
   * 
   * @return the descent
   */
  public float getDescent()
  {
    return Math.max(0, image.getHeight(null) - originY);
  }

  /**
   * Gets the hash code for this image.
   * 
   * @return the hash code
   */
  public int hashCode()
  {
    return image.hashCode();
  }
}
