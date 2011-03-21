/* FontRenderContext.java
   Copyright (C) 2002, 2003 Free Software Foundation, Inc.

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

import java.awt.geom.AffineTransform;

/**
 * @author Michael Koch
 */
public class FontRenderContext
{
  private AffineTransform affineTransform;
  private boolean isAntiAliased;
  private boolean usesFractionalMetrics;

  /**
   * Construct a new <code>FontRenderContext</code>.
   */
  protected FontRenderContext()
  {
    // Do nothing here.
  }

  /**
   * Construct a new <code>FontRenderContext</code>.
   */
  public FontRenderContext (AffineTransform tx, boolean isAntiAliased,
                            boolean usesFractionalMetrics)
  {
    if (tx != null
        && !tx.isIdentity ())
      {
        this.affineTransform = new AffineTransform (tx);
      }

    this.isAntiAliased = isAntiAliased;
    this.usesFractionalMetrics = usesFractionalMetrics;
  }

  public boolean equals (Object obj)
  {
    if (! (obj instanceof FontRenderContext))
      return false;

    return equals ((FontRenderContext) obj);
  }

  public boolean equals (FontRenderContext rhs)
  {
    if (rhs == null)
      return false;

    if (affineTransform == null && rhs.affineTransform != null
        || affineTransform != null && rhs.affineTransform == null)
      return false;

    return ((affineTransform == rhs.affineTransform
             || affineTransform.equals (rhs.getTransform ()))
            && isAntiAliased == rhs.isAntiAliased ()
            && usesFractionalMetrics == rhs.usesFractionalMetrics ());
  }


  /**
   * Retrieves the affine transform for scaling typographical points
   * to raster pixels.
   *
   * @return a clone of the transform object.
   */
  public AffineTransform getTransform ()
  {
    if (affineTransform == null)
      return new AffineTransform ();
    else
      return new AffineTransform (affineTransform);
  }


  /**
   * Returns the hash code of the font render context.
   */
  public int hashCode ()
  {
    int code = ( isAntiAliased ? 1 : 0 ) + ( usesFractionalMetrics ? 2 : 0 );

    if( affineTransform != null && !affineTransform.isIdentity() )
      code ^= affineTransform.hashCode();

    return code;
  }

  public boolean isAntiAliased ()
  {
    return isAntiAliased;
  }

  public boolean usesFractionalMetrics ()
  {
    return usesFractionalMetrics;
  }
}
