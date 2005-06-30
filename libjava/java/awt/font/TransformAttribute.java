/* TransformAttribute.java --
   Copyright (C) 2003, 2005  Free Software Foundation, Inc.

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
import java.io.Serializable;

/**
 * This class provides a mechanism for using an {@link AffineTransform} as
 * an <i>immutable</i> attribute (for example, in the 
 * {@link java.text.AttributedString} class).  Any transform passed to 
 * this class is copied before being stored, and any transform handed out
 * by this class is a copy of the stored transform.  In this way, it is 
 * not possible to modify the stored transform.
 * 
 * @author Michael Koch
 */
public final class TransformAttribute implements Serializable
{
  private static final long serialVersionUID = 3356247357827709530L;

  private AffineTransform affineTransform;
  
  /**
   * Creates a new attribute that contains a copy of the given transform.
   * 
   * @param transform  the transform (<code>null</code> not permitted).
   * 
   * @throws IllegalArgumentException if <code>transform</code> is 
   *         <code>null</code>.
   */
  public TransformAttribute (AffineTransform transform) 
  {
    if (transform == null)
      {
        throw new IllegalArgumentException("Null 'transform' not permitted.");
      }
    this.affineTransform = new AffineTransform (transform);
  }

  /**
   * Returns a copy of the transform contained by this attribute.
   * 
   * @return A copy of the transform.
   */
  public AffineTransform getTransform ()
  {
    return (AffineTransform) affineTransform.clone();
  }

  /**
   * Returns <code>true</code> if the transform contained by this attribute is
   * an identity transform, and <code>false</code> otherwise.
   * 
   * @return <code>true</code> if the transform contained by this attribute is
   * an identity transform, and <code>false</code> otherwise.
   * 
   * @since 1.4
   */
  public boolean isIdentity ()
  {
    return (affineTransform == null ? false : affineTransform.isIdentity ());
  }
}
