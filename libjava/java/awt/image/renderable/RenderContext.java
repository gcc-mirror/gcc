/* RenderContext.java -- 
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


package java.awt.image.renderable;

import java.awt.RenderingHints;
import java.awt.Shape;
import java.awt.geom.AffineTransform;

public class RenderContext implements Cloneable
{
  private AffineTransform xform;
  private Shape aoi;
  private RenderingHints hints;

  public RenderContext(AffineTransform xform, Shape aoi, RenderingHints hints)
  {
    this.xform = xform;
    this.aoi = aoi;
    this.hints = hints;
  }

  public RenderContext(AffineTransform xform)
  {
    this(xform, null, null);
  }

  public RenderContext(AffineTransform xform, RenderingHints hints)
  {
    this(xform, null, hints);
  }

  public RenderContext(AffineTransform xform, Shape aoi)
  {
    this(xform, aoi, null);
  }

  public RenderingHints getRenderingHints()
  {
    return hints;
  }

  public void setRenderingHints(RenderingHints hints)
  {
    this.hints = hints;
  }

  public void setTransform(AffineTransform xform)
  {
    this.xform = xform;
  }

  public void preConcatenateTransform(AffineTransform pre)
  {
    xform.preConcatenate(pre);
  }

  /** @deprecated Sun can't spell concatenate */
  public void preConcetenateTransform(AffineTransform pre)
  {
    preConcetenateTransform(pre);
  }

  public void concatenateTransform(AffineTransform post)
  {
    xform.concatenate(post);
  }

  /** @deprecated Sun can't spell concatenate */
  public void concetenateTransform(AffineTransform post)
  {
    concatenateTransform(post);
  }

  public AffineTransform getTransform()
  {
    return xform;
  }

  public void setAreaOfInterest(Shape aoi)
  {
    this.aoi = aoi;
  }

  public Shape getAreaOfInterest()
  {
    return aoi;
  }

  public Object clone()
  {
    try
      {
        RenderContext copy = (RenderContext) super.clone();
        if (xform != null)
          copy.xform = (AffineTransform) xform.clone();
        if (hints != null)
          copy.hints = (RenderingHints) hints.clone();
        return copy;
      }
    catch (CloneNotSupportedException e)
      {
        throw (Error) new InternalError().initCause(e); // impossible
      }
  }
} // class RenderContext
