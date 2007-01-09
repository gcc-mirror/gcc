/* RenderableImageOp.java -- 
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


package java.awt.image.renderable;

import java.awt.RenderingHints;
import java.awt.geom.AffineTransform;
import java.awt.image.RenderedImage;
import java.util.Vector;

public class RenderableImageOp implements RenderableImage
{
  private final ContextualRenderedImageFactory crif;
  private ParameterBlock block;

  public RenderableImageOp(ContextualRenderedImageFactory crif,
                           ParameterBlock block)
  {
    this.crif = crif;
    this.block = (ParameterBlock) block.clone();
  }

  public Vector<RenderableImage> getSources()
  {
    if (block.sources == null)
      return null;
    int size = block.sources.size();
    Vector v = new Vector();
    for (int i = 0; i < size; i++)
      {
        Object o = block.sources.get(i);
        if (o instanceof RenderableImage)
          v.add(o);
      }
    return v;
  }

  public Object getProperty(String name)
  {
    return crif.getProperty(block, name);
  }

  public String[] getPropertyNames()
  {
    return crif.getPropertyNames();
  }

  public boolean isDynamic()
  {
    return crif.isDynamic();
  }

  public float getWidth()
  {
    return (float) crif.getBounds2D(block).getWidth();
  }

  public float getHeight()
  {
    return (float) crif.getBounds2D(block).getHeight();
  }

  public float getMinX()
  {
    return (float) crif.getBounds2D(block).getX();
  }

  public float getMinY()
  {
    return (float) crif.getBounds2D(block).getY();
  }

  public ParameterBlock setParameterBlock(ParameterBlock block)
  {
    ParameterBlock result = this.block;
    this.block = (ParameterBlock) block.clone();
    return result;
  }

  public ParameterBlock getParameterBlock()
  {
    return block;
  }

  public RenderedImage createScaledRendering(int w, int h,
                                             RenderingHints hints)
  {
    if (w == 0)
      if (h == 0)
        throw new IllegalArgumentException();
      else
        w = Math.round(h * getWidth() / getHeight());
    if (h == 0)
      h = Math.round(w * getHeight() / getWidth());
    AffineTransform xform = AffineTransform.getScaleInstance(w * getWidth(),
                                                             h * getHeight());
    return createRendering(new RenderContext(xform, hints));
  }

  public RenderedImage createDefaultRendering()
  {
    return createRendering(new RenderContext(new AffineTransform()));
  }

  public RenderedImage createRendering(RenderContext context)
  {
    ParameterBlock copy = (ParameterBlock) block.clone();
    int i = block.sources.size();
    while (--i >= 0)
      {
        Object o = block.sources.get(i);
        if (o instanceof RenderableImage)
          {
            RenderableImage ri = (RenderableImage) o;
            RenderContext rc = crif.mapRenderContext(i, context, block, ri);
            copy.sources.set(i, ri.createRendering(rc));
          }
      }
    // Now copy.sources should be only RenderedImages.
    return crif.create(context, copy);
  }
} // class RenderableImageOp
