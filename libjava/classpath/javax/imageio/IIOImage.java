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


package javax.imageio;

import java.awt.image.BufferedImage;
import java.awt.image.Raster;
import java.awt.image.RenderedImage;
import java.util.List;

import javax.imageio.metadata.IIOMetadata;

/**
 * IIOImage is a container class for components of an image file that
 * stores image data, image metadata and thumbnails.
 *
 * The image data can be either a RenderedImage or a Raster but not
 * both.  Image readers that produce IIOImages will always produce
 * BufferedImages from the RenderedImage field.  Image writers that
 * accept IIOImages will always accept RenderedImages and may
 * optionally accept Rasters.
 *
 * @author Thomas Fitzsimmons (fitzsim@redhat.com)
 */
public class IIOImage
{
  /**
   * Image data as a RenderedImage.  null if this IIOImage uses the
   * Raster representation.
   */
  protected RenderedImage image;

  /**
   * Image metadata.
   */
  protected IIOMetadata metadata;

  /**
   * Image data as a Raster.  null if this IIOImage uses the
   * RenderedImage representation.
   */
  protected Raster raster;

  /**
   * A list of BufferedImage thumbnails of this image.
   */
  protected List<? extends BufferedImage> thumbnails;

  /**
   * Construct an IIOImage containing raster image data, thumbnails
   * and metadata.
   *
   * @param raster image data
   * @param thumbnails a list of BufferedImage thumbnails or null
   * @param metadata image metadata or null
   *
   * @exception IllegalArgumentException if raster is null
   */
  public IIOImage (Raster raster, List<? extends BufferedImage> thumbnails,
                   IIOMetadata metadata)
  {
    if (raster == null)
      throw new IllegalArgumentException ("raster may not be null");
    
    this.raster = raster;
    this.thumbnails = thumbnails;
    this.metadata = metadata;
  }
  
  /**
   * Construct an IIOImage containing rendered image data, thumbnails
   * and metadata.
   *
   * @param image rendered image data
   * @param thumbnails a list of BufferedImage thumbnails or null
   * @param metadata image metadata or null
   *
   * @exception IllegalArgumentException if image is null
   */
  public IIOImage (RenderedImage image, List<? extends BufferedImage> thumbnails,
                   IIOMetadata metadata)
  {
    if (image == null)
      throw new IllegalArgumentException ("image may not be null");
    
    this.image = image;
    this.thumbnails = thumbnails;
    this.metadata = metadata;
  }

  /**
   * Retrieve the image metadata or null if there is no metadata
   * associated with this IIOImage.
   *
   * @return image metadata or null
   */
  public IIOMetadata getMetadata()
  {
    return metadata;
  }

  /**
   * Retrieve the number of thumbnails in this IIOImage.
   *
   * @return the number of thumbnails
   */
  public int getNumThumbnails()
  {
    return thumbnails == null ? 0 : thumbnails.size();
  }

  /**
   * Retrieve the raster image data stored in this IIOImage or null if
   * this image stores data using the RenderedImage representation.
   *
   * @return the raster image data or null
   */
  public Raster getRaster()
  {
    return raster;
  }

  /**
   * Retrieve the rendered image data stored in this IIOImage or null
   * if this image stores data using the Raster representation.
   *
   * @return the rendered image data or null
   */
  public RenderedImage getRenderedImage()
  {
    return image;
  }

  /**
   * Retrieve the thumbnail stored at the specified index in the
   * thumbnails list.
   *
   * @param index the index of the thumbnail to retrieve
   *
   * @return the buffered image thumbnail
   *
   * @exception IndexOutOfBoundsException if index is out-of-bounds
   * @exception ClassCastException if the object returned from the
   * thumbnails list is not a BufferedImage
   */
  public BufferedImage getThumbnail (int index)
  {
    // This throws a ClassCastException if the returned object is not
    // a BufferedImage or an IndexOutOfBoundsException if index is
    // out-of-bounds.
    return (BufferedImage) thumbnails.get (index);
  }

  /**
   * Retrieve the list of thumbnails or null if there are no
   * thumbnails associated with this IIOImage.  The returned reference
   * can be used to update the thumbnails list.
   *
   * @return a list of thumbnails or null
   */
  public List<? extends BufferedImage> getThumbnails()
  {
    return thumbnails;
  }

  /**
   * Check whether this IIOImage stores its image data as a Raster or
   * as a RenderedImage.
   *
   * @return true if this IIOImage uses the Raster representation,
   * false if it uses the RenderedImage representation.
   */
  public boolean hasRaster()
  {
    return raster != null;
  }

  /**
   * Set this IIOImage's metadata.
   *
   * @param metadata the image metadata
   */
  public void setMetadata (IIOMetadata metadata)
  {
    this.metadata = metadata;
  }

  /**
   * Set the raster data for this image.  This disposes of any
   * existing rendered image data stored in this IIOImage.
   *
   * @param raster the image raster data
   *
   * @exception IllegalArgumentException if raster is null
   */
  public void setRaster (Raster raster)
  {
    if (raster == null)
      throw new IllegalArgumentException ("raster may not be null");
    
    this.image = null;
    this.raster = raster;
  }

  /**
   * Set the rendered image data for this image.  This disposes of any
   * existing raster data stored in this IIOImage.
   *
   * @param image the rendered image data
   *
   * @exception IllegalArgumentException if image is null
   */
  public void setRenderedImage (RenderedImage image)
  {
    if (image == null)
      throw new IllegalArgumentException ("image may not be null");

    this.image = image;
    this.raster = null;
  }

  /**
   * Set the list of thumbnails for this IIOImage to a new list of
   * BufferedImages or to null.  Any existing thumbnails list is
   * disposed.
   *
   * @param thumbnails a new list of thumbnails or null
   */
  public void setThumbnails (List<? extends BufferedImage> thumbnails)
  {
    this.thumbnails = thumbnails;
  }
}
