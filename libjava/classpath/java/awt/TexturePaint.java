/* TexturePaint.java --
   Copyright (C) 2002 Free Software Foundation, Inc.

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

import gnu.java.awt.java2d.TexturePaintContext;

import java.awt.geom.AffineTransform;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;
import java.awt.image.ColorModel;

/**
 * This class provides a way to fill a Shape with a texture that is
 * specified by a BufferedImage.
 */
public class TexturePaint implements Paint
{
  private final BufferedImage texture;
  private final Rectangle2D anchor;

  /**
   * Constructor.
   *
   * @param texture - the texture
   * @param anchor - the shape
   */
  public TexturePaint(BufferedImage texture, Rectangle2D anchor)
  {
    this.texture = texture;
    this.anchor = anchor;
  }

  /**
   * Gets the texture image.
   *
   * @return the texture
   */
  public BufferedImage getImage()
  {
    return texture;
  }

  /**
   * Gets the shape anchor.
   *
   * @return the shape anchor
   */
  public Rectangle2D getAnchorRect()
  {
    return anchor;
  }

  /**
   * Creates the context used to paint the texture.
   *
   * @param cm - the ColorModel that receives the Paint data. Used only as a hint.
   * @param deviceBounds - the device space being rendered.
   * @param userBounds - the user space being rendered
   * @param xform - the AffineTransform from user space into device space
   * @param hints - a RenderingHints object that is used to specify how the
   * pattern is rendered
   * @return the paint context used to paint the texture
   */
  public PaintContext createContext(ColorModel cm, Rectangle deviceBounds,
                                    Rectangle2D userBounds,
                                    AffineTransform xform, RenderingHints hints)
  {
    // TODO: Maybe add some hook for providing alternative/accelerated
    // implementations of this.
    return new TexturePaintContext(this, deviceBounds, userBounds, xform);
  }

  /**
   * Returns the transparency mode.
   *
   * @return the transparency mode.
   */
  public int getTransparency()
  {
    return texture.getTransparency();
  }
} // class TexturePaint
