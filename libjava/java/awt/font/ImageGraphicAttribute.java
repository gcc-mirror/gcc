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


package java.awt.font;

import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.geom.Rectangle2D;

/**
 * @author Michael Koch
 */
public final class ImageGraphicAttribute extends GraphicAttribute
{
  private Image image;
  
  public ImageGraphicAttribute (Image image, int alignment)
  {
    super (alignment);
    this.image = image;
  }

  public ImageGraphicAttribute (Image image, int alignment, float originX,
                                float originY)
  {
    super (alignment);
    this.image = image;
    
    throw new Error ("not implemented");
  }

  public void draw (Graphics2D graphics, float x, float y)
  {
    throw new Error ("not implemented");
  }

  public boolean equals (Object obj)
  {
    if (! (obj instanceof ImageGraphicAttribute))
      return false;

    return equals ((ImageGraphicAttribute) obj);
  }

  public boolean equals (ImageGraphicAttribute rhs)
  {
    throw new Error ("not implemented");
  }

  public float getAdvance ()
  {
    throw new Error ("not implemented");
  }

  public float getAscent ()
  {
    throw new Error ("not implemented");
  }

  public Rectangle2D getBounds () 
  {
    throw new Error ("not implemented");
  }

  public float getDescent ()
  {
    throw new Error ("not implemented");
  }

  public int hashCode ()
  {
    throw new Error ("not implemented");
  }
}
