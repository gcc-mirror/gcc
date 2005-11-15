/* IIOParam.java --
   Copyright (C) 2004  Free Software Foundation, Inc.

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

import java.awt.Point;
import java.awt.Rectangle;

/**
 * An IIOParam stores parameters used when encoding or decoding image
 * streams.  ImageReadParam and ImageWriteParam extend this abstract
 * base class.
 *
 * IIOParams allow control over how source pixels converted into
 * destination pixels.  This conversion can take place between a
 * stream and in-memory image data, when an image reader is doing the
 * conversion, or a writer can be doing the conversion from an
 * in-memory source to a stream destination.
 *
 * An image reader can be restricted to only read from a given region;
 * likewise a writer can be restricted to only write output to a given
 * region.
 *
 * For image readers and writers, IIOParam supports image pixelation
 * -- where the input image is approximated by the output image using
 * larger-sized pixel blocks.  For example: FIXME
 *
 * IIOParams can control how pixels are combined into larger blocks
 * using sub-sampling matrices.  For example: FIXME
 *
 * They can also control which source bands are read and written; this
 * example reads the RGBA (red, green, blue, transparency) data from a
 * PNG image and outputs just the red and transparency bands: FIXME
 *
 * @author Thomas Fitzsimmons (fitzsim@redhat.com)
 * @author Michael Koch (konqueror@gmx.de)
 */
public abstract class IIOParam
{
  /**
   * The controller called by this IIOParam to retrieve parameters.
   */
  protected IIOParamController controller = null;

  /**
   * The default controller called by this IIOParam to retrieve
   * parameters.
   */
  protected IIOParamController defaultController = null;

  /**
   * The offset in the destination where the upper-left
   * decoded/encoded pixel should be located.
   */
  protected Point destinationOffset = new Point(0, 0);

  /**
   * Sets the output colour type when writing or the destination image
   * type when reading.
   */
  protected ImageTypeSpecifier destinationType = null;

  /**
   * An array indicating which source bands will be used or null.
   */
  protected int[] sourceBands = null;

  /**
   * The source pixel region or null.
   */
  protected Rectangle sourceRegion = null;

  /**
   * Sample every sourceXSubsampling'th pixel in the source image when
   * pixelating the destination image in the horizontal direction.
   */
  protected int sourceXSubsampling = 1;

  /**
   * Sample every sourceYSubsampling'th pixel in the source image when
   * pixelating the destination image in the vertical direction.
   */
  protected int sourceYSubsampling = 1;

  /**
   * Start sampling at this horizontal offset within the source region
   * when pixelating the destination image in the horizontal
   * direction.
   */
  protected int subsamplingXOffset = 0;

  /**
   * Start sampling at this vertical offset within the source region
   * when pixelating the destination image in the vertical direction.
   */
  protected int subsamplingYOffset = 0;

  /**
   * Indicates whether or not the controller has been explicitly set
   * to null.
   */
  private boolean no_controller = false;

  /**
   * Constructs an IIOParam object.
   */
  protected IIOParam()
  {
  }

  /**
   * Activates the parameter controller by calling its activate method
   * and passing it this IIOParam.  A true return value indicates that
   * this IIOParam's values are ready for the next read or write
   * operation.  A return value of false means that this IIOParam's
   * values have not been affected because the controller operations
   * were cancelled.
   *
   * @return true if parameters were successfully set, false if
   * parameters were not changed
   */
  public boolean activateController()
  {
    if (controller == null)
      {
	if (defaultController == null || no_controller)
	  return false;
	else
	  return defaultController.activate (this);
      }
    else
      return controller.activate(this);
  }

  /**
   * Retrieve the currently set controller if one has been set, or the
   * default controller, or null if the controller has been explicitly
   * set to null.
   *
   * @return the currently used controller or null
   */  
  public IIOParamController getController()
  {
    return controller == null ?
      (no_controller ? null : defaultController) : controller;
  }

  /**
   * Retrieve the default controller regardless of whether or not a
   * non-default controller has been set.  The default controller may
   * be null.
   *
   * @return the default controller or null
   */
  public IIOParamController getDefaultController()
  {
    return defaultController;
  }

  /**
   * Retrieve the offset in the destination where the upper-left
   * decoded/encoded pixel should be located. (0, 0) by default.
   *
   * @return the destination offset
   */
  public Point getDestinationOffset()
  {
    return destinationOffset;
  }

  /**
   * Retrieve the currently set image-type specifier or null if none
   * has been set.
   *
   * @return the current image-type specifier or null
   */
  public ImageTypeSpecifier getDestinationType()
  {
    return destinationType;
  }

  /**
   * Retrieve the current source band values or null if source band
   * values have not been set.
   *
   * The returned array is a copy of this IIOParam's source band
   * array.
   *
   * @return the current set of source band values or null
   */
  public int[] getSourceBands()
  {
    if (sourceBands == null)
      return null;

    int[] sourceBandsCopy = new int[sourceBands.length];
    System.arraycopy (sourceBands, 0, sourceBandsCopy, 0, sourceBands.length);
    return sourceBandsCopy;
  }

  /**
   * Retrieve the source rectangle from which pixels should be read or
   * null if no source region has been set.
   *
   * @return the current source region or null
   */
  public Rectangle getSourceRegion()
  {
    return sourceRegion;
  }

  /**
   * Retrieve the number of pixel columns to advance before taking a
   * pixel sample.
   *
   * @return the horizontal sub-sampling interval
   */
  public int getSourceXSubsampling()
  {
    return sourceXSubsampling;
  }
  
  /**
   * Retrieve the number of pixel rows to advance before taking a
   * pixel sample.
   *
   * @return the vertical sub-sampling interval
   */
  public int getSourceYSubsampling()
  {
    return sourceYSubsampling;
  }

  /**
   * Retrieve the number of pixel columns to advance before taking any
   * pixel samples.
   *
   * @return the horizontal sub-sampling offset
   */
  public int getSubsamplingXOffset()
  {
    return subsamplingXOffset;
  }

  /**
   * Retrieve the number of pixel rows to advance before taking any
   * pixel samples.
   *
   * @return the vertical sub-sampling offset
   */
  public int getSubsamplingYOffset()
  {
    return subsamplingYOffset;
  }

  /**
   * Check if a non-null controller is currently available.
   *
   * @return true if getController returns a non-null value, false if
   * getController returns null
   */
  public boolean hasController()
  {
    return getController() != null;
  }

  /**
   * Sets the controller for this IIOParam.  This is the controller
   * that will be activated when activateController is called.  The
   * argument controller overrides this IIOParam's default controller.
   * If the argument is null then no controller will be set, not even
   * the default one.  To reset the default controller call
   * setController(getDefaultController()).
   *
   * @param controller the controller to set or null
   */
  public void setController(IIOParamController controller)
  {
    if (controller == defaultController)
      {
	this.controller = null;
	no_controller = false;
      }
    else
      {
	no_controller = (controller == null);
	this.controller = controller;
      }
  }

  /**
   * Set the destination image type.
   *
   * If this value is set on an image reader then its read method will
   * return a new BufferedImage of the specified destination type.  In
   * this case any destination image set using setDestination() is
   * ignored.
   *
   * If this is set on an image writer then the destination type
   * affects only the colour model of the destination image.  The
   * destination type's SampleModel is ignored.  The destination
   * type's ColorModel will override the source image's colour model.
   *
   * @param destinationType the sample and colour models of the
   * destination image
   */
  public void setDestinationType (ImageTypeSpecifier destinationType)
  {
    this.destinationType = destinationType;
  }

  /**
   * Specify the destination pixel offset.  Image writers are only
   * affected by this setting when ImageWriter.replacePixels is called
   * in which case the offset is into the region of pixels being
   * changed.
   *
   * @param destinationOffset the offset where pixel writing should
   * begin
   */
  public void setDestinationOffset(Point destinationOffset)
  {
    if (destinationOffset == null)
      throw new IllegalArgumentException("destinationOffset is null");

    this.destinationOffset = destinationOffset;
  }

  /**
   * Set the indices of the source bands to be used.  Duplicate
   * indices are not allowed.  A value of null means use all source
   * bands.  The argument array is copied and stored, so subsequent
   * updates to it will not be reflected in this IIOParam.
   *
   * @param sourceBands the array of source bands to use
   */
  public void setSourceBands(int[] sourceBands)
  {
    int[] sourceBandsCopy = new int[sourceBands.length];
    System.arraycopy (sourceBands, 0, sourceBandsCopy, 0, sourceBands.length);
    this.sourceBands = sourceBandsCopy;
  }

  /**
   * Set the source region from which to read.  The number of pixels
   * sampled from the source region depends on the source sub-sampling
   * settings.  If the combination of this sourceRegion and the
   * current sub-sampling settings would result in no pixels being
   * sampled then an IllegalStateException will be thrown.
   *
   * The source region is specified in the source image coordinate
   * system which has point (0, 0) at the top-left and increases down
   * and to the right.  The argument source region is clipped to the
   * image boundaries at read-time.
   *
   * A null argument sets the source region to null meaning that the
   * whole image should be read.
   *
   * @param sourceRegion the rectangular source region
   *
   * @exception IllegalArgumentException if sourceRegion has width or
   * height <= 0 or x or y < 0
   * @exception IllegalStateException if the given sourceRegion and
   * the current sampling settings would produce zero samples
   */
  public void setSourceRegion(Rectangle sourceRegion)
  {
    if (sourceRegion != null
	&& (sourceRegion.x < 0
	    || sourceRegion.y < 0
	    || sourceRegion.width <= 0
	    || sourceRegion.height <= 0))
      throw new IllegalArgumentException("illegal source region");

    if (sourceRegion != null)
      {
	int num_rows =
	  (sourceRegion.height - subsamplingYOffset + sourceYSubsampling - 1)
	  / sourceYSubsampling;

	int num_columns =
	  (sourceRegion.width - subsamplingXOffset + sourceXSubsampling - 1)
	  / sourceXSubsampling;

	if (num_rows <= 0 || num_columns <= 0)
	  throw new IllegalStateException("zero pixels in source region");
      }

    this.sourceRegion = sourceRegion;
  }

  /**
   * Set the source sampling intervals and offsets.  Every
   * sourceXSubsampling'th pixel horizontally and
   * sourceYSubsampling'th pixel vertically will be sampled.  Sampling
   * will being a the subsamplingXOffset'th column and the
   * subsamplingYOffset'th row.
   *
   * Horizontally, the number of sampled pixels will be:
   *
   * floor((width - subsamplingXOffset + sourceXSubsampling - 1) / sourceXSubsampling)
   *
   * Vertically:
   *
   * floor((height - subsamplingYOffset + sourceYSubsampling - 1) / sourceYSubsampling)
   *
   * If the current source region setting is such that the given
   * sub-sampling arguments would produce zero pixel samples, an
   * IllegalStateException is thrown.
   *
   * The offset parameters can be used to make source regions overlap
   * when tiling across an image.  This can eliminate seams and
   * better-tile images whose width or height is not a multiple of the
   * sampling interval.
   *
   * @param sourceXSubsampling the horizontal sampling interval
   * @param sourceYSubsampling the vertical sampling interval
   * @param subsamplingXOffset the horizontal offset of the initial
   * sample
   * @param subsamplingYOffset the vertical offset of the initial
   * sample
   *
   * @exception IllegalArgumentException if either subsamplingXOffset
   * or subsamplingYOffset is < 0
   * @exception IllegalStateException if the current source region
   * combined with the given sub-sampling parameters would produce
   * zero pixel samples
   */
  public void setSourceSubsampling(int sourceXSubsampling, int sourceYSubsampling,
				   int subsamplingXOffset, int subsamplingYOffset)
  {
    if (subsamplingXOffset < 0 || subsamplingYOffset < 0)
      throw new IllegalArgumentException("subsampling offset < 0");

    if (sourceRegion != null)
      {
	int num_rows =
	  (sourceRegion.height - subsamplingYOffset + sourceYSubsampling - 1)
	  / sourceYSubsampling;

	int num_columns =
	  (sourceRegion.width - subsamplingXOffset + sourceXSubsampling - 1)
	  / sourceXSubsampling;

	if (num_rows <= 0 || num_columns <= 0)
	  throw new IllegalStateException("subsampling parameters would"
					  + " produce zero pixel samples"
					  + " in source region");
      }

    this.sourceXSubsampling = sourceXSubsampling;
    this.sourceYSubsampling = sourceYSubsampling;
    this.subsamplingXOffset = subsamplingXOffset;
    this.subsamplingYOffset = subsamplingYOffset;
  }
}
