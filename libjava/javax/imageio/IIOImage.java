/* IIOImage.java --
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


package javax.imageio;

import java.awt.image.BufferedImage;
import java.awt.image.Raster;
import java.awt.image.RenderedImage;
import java.util.List;

import javax.imageio.metadata.IIOMetadata;

public class IIOImage
{
  protected RenderedImage image;
  protected IIOMetadata metadata;
  protected Raster raster;
  protected List thumbnails;
  
  public IIOImage (Raster raster, List thumbnails, IIOMetadata metadata)
  {
    if (raster == null)
      throw new IllegalArgumentException ("raster may not be null");
    
    this.raster = raster;
    this.thumbnails = thumbnails;
    this.metadata = metadata;
  }
  
  public IIOImage (RenderedImage image, List thumbnails, IIOMetadata metadata)
  {
    if (image == null)
      throw new IllegalArgumentException ("image may not be null");
    
    this.image = image;
    this.thumbnails = thumbnails;
    this.metadata = metadata;
  }

  public IIOMetadata getMetadata()
  {
    return metadata;
  }

  public int getNumThumbnails()
  {
    return thumbnails.size();
  }

  public Raster getRaster()
  {
    return raster;
  }

  public RenderedImage getRenderedImage()
  {
    return image;
  }

  public BufferedImage getThumbnail (int index)
  {
    return (BufferedImage) thumbnails.get (index);
  }

  public List getThumbnails()
  {
    return thumbnails;
  }

  public boolean hasRaster()
  {
    return raster != null;
  }

  public void setMetadata (IIOMetadata metadata)
  {
    this.metadata = metadata;
  }

  public void setRaster (Raster raster)
  {
    if (raster == null)
      throw new IllegalArgumentException ("raster may not be null");
    
    this.image = null;
    this.raster = raster;
  }

  public void setRenderedImage (RenderedImage image)
  {
    if (image == null)
      throw new IllegalArgumentException ("image may not be null");

    this.image = image;
    this.raster = null;
  }

  public void setThumbnails (List thumbnails)
  {
    this.thumbnails = thumbnails;
  }
  
} // class IIOParam
