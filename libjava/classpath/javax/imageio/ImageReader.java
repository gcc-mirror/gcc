/* ImageReader.java -- Decodes raster images.
   Copyright (C) 2004, 2005  Free Software Foundation, Inc.

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
import java.awt.image.BufferedImage;
import java.awt.image.Raster;
import java.awt.image.RenderedImage;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.ResourceBundle;
import java.util.MissingResourceException;
import java.util.Set;

import javax.imageio.event.IIOReadProgressListener;
import javax.imageio.event.IIOReadUpdateListener;
import javax.imageio.event.IIOReadWarningListener;
import javax.imageio.metadata.IIOMetadata;
import javax.imageio.spi.ImageReaderSpi;
import javax.imageio.stream.ImageInputStream;

/**
 * A class for decoding images within the ImageIO framework.
 *
 * An ImageReader for a given format is instantiated by an
 * ImageReaderSpi for that format.  ImageReaderSpis are registered
 * with the IIORegistry.
 *
 * The ImageReader API supports reading animated images that may have
 * multiple frames; to support such images many methods take an index
 * parameter.
 *
 * Images may also be read in multiple passes, where each successive
 * pass increases the level of detail in the destination image.
 */
public abstract class ImageReader
{
  private boolean aborted;

  /**
   * All locales available for localization of warning messages, or
   * null if localization is not supported.
   */
  protected Locale[] availableLocales = null;

  /**
   * true if the input source does not require metadata to be read,
   * false otherwise.
   */
  protected boolean ignoreMetadata = false;

  /**
   * An ImageInputStream from which image data is read.
   */
  protected Object input = null;

  /**
   * The current locale used to localize warning messages, or null if
   * no locale has been set.
   */
  protected Locale locale = null;

  /**
   * The minimum index at which data can be read.  Constantly 0 if
   * seekForwardOnly is false, always increasing if seekForwardOnly is
   * true.
   */
  protected int minIndex = 0;

  /**
   * The image reader SPI that instantiated this reader.
   */
  protected ImageReaderSpi originatingProvider = null;

  /**
   * A list of installed progress listeners.  Initially null, meaning
   * no installed listeners.
   */
  protected List progressListeners = null;

  /**
   * true if this reader should only read data further ahead in the
   * stream than its current location.  false if it can read backwards
   * in the stream.  If this is true then caching can be avoided.
   */
  protected boolean seekForwardOnly = false;

  /**
   * A list of installed update listeners.  Initially null, meaning no
   * installed listeners.
   */
  protected List updateListeners = null;

  /**
   * A list of installed warning listeners.  Initially null, meaning
   * no installed listeners.
   */
  protected List warningListeners = null;

  /**
   * A list of warning locales corresponding with the list of
   * installed warning listeners.  Initially null, meaning no locales.
   */
  protected List warningLocales = null;

  /**
   * Construct an image reader.
   *
   * @param originatingProvider the provider that is constructing this
   * image reader, or null
   */
  protected ImageReader(ImageReaderSpi originatingProvider)
  {
    this.originatingProvider = originatingProvider;
  }

  /**
   * Request that reading be aborted.  The unread contents of the
   * image will be undefined.
   *
   * Readers should clear the abort flag before starting a read
   * operation, then poll it periodically during the read operation.
   */
  public void abort()
  {
    aborted = true;
  }

  /**
   * Check if the abort flag is set.
   *
   * @return true if the current read operation should be aborted,
   * false otherwise
   */
  protected boolean abortRequested()
  {
    return aborted;
  }

  /**
   * Install a read progress listener.  This method will return
   * immediately if listener is null.
   *
   * @param listener a read progress listener or null
   */
  public void addIIOReadProgressListener(IIOReadProgressListener listener)
  {
    if (listener == null)
      return;
    if (progressListeners == null)
      progressListeners = new ArrayList ();
    progressListeners.add(listener);
  }

  /**
   * Install a read update listener.  This method will return
   * immediately if listener is null.
   *
   * @param listener a read update listener
   */
  public void addIIOReadUpdateListener(IIOReadUpdateListener listener)
  {
    if (listener == null)
      return;
    if (updateListeners == null)
      updateListeners = new ArrayList ();
    updateListeners.add(listener);
  }

  /**
   * Install a read warning listener.  This method will return
   * immediately if listener is null.  Warning messages sent to this
   * listener will be localized using the current locale.  If the
   * current locale is null then this reader will select a sensible
   * default.
   *
   * @param listener a read warning listener
   */
  public void addIIOReadWarningListener(IIOReadWarningListener listener)
  {
    if (listener == null)
      return;
    if (warningListeners == null)
      warningListeners = new ArrayList ();
    warningListeners.add(listener);
  }

  /**
   * Check if this reader can handle raster data.  Determines whether
   * or not readRaster and readTileRaster throw
   * UnsupportedOperationException.
   *
   * @return true if this reader supports raster data, false if not
   */
  public boolean canReadRaster()
  {
    return false;
  }

  /**
   * Clear the abort flag.
   */
  protected void clearAbortRequest()
  {
    aborted = false;
  }

  /**
   * Releases any resources allocated to this object.  Subsequent
   * calls to methods on this object will produce undefined results.
   *
   * The default implementation does nothing; subclasses should use
   * this method ensure that native resources are released.
   */
  public void dispose()
  {
    // The default implementation does nothing.
  }

  /**
   * Returns the aspect ratio of this image, the ration of its width
   * to its height.  The aspect ratio is useful when resizing an image
   * while keeping its proportions constant.
   *
   * @param imageIndex the frame index
   *
   * @return the image's aspect ratio
   *
   * @exception IllegalStateException if input is null
   * @exception IndexOutOfBoundsException if the frame index is
   * out-of-bounds
   * @exception IOException if a read error occurs
   */
  public float getAspectRatio(int imageIndex)
    throws IOException
  {
    if (input == null)
      throw new IllegalStateException("input is null");

    return (float) (getWidth(imageIndex) / getHeight(imageIndex));
  }

  /**
   * Retrieve the available locales.  Return null if no locales are
   * available or a clone of availableLocales.
   *
   * @return an array of locales or null
   */
  public Locale[] getAvailableLocales()
  {
    if (availableLocales == null)
      return null;
    
    return (Locale[]) availableLocales.clone();
  }

  /**
   * Retrieve the default read parameters for this reader's image
   * format.
   *
   * The default implementation returns new ImageReadParam().
   *
   * @return image reading parameters
   */
  public ImageReadParam getDefaultReadParam()
  {
    return new ImageReadParam();
  }

  /**
   * Retrieve the format of the input source.
   *
   * @return the input source format name
   *
   * @exception IOException if a read error occurs
   */
  public String getFormatName()
    throws IOException
  {
    return originatingProvider.getFormatNames()[0];
  }

  /**
   * Get the height of the input image in pixels.  If the input image
   * is resizable then a default height is returned.
   *
   * @param imageIndex the frame index
   *
   * @return the height of the input image
   *
   * @exception IllegalStateException if input has not been set
   * @exception IndexOutOfBoundsException if the frame index is
   * out-of-bounds
   * @exception IOException if a read error occurs
   */
  public abstract int getHeight(int imageIndex)
    throws IOException;

  /**
   * Get the metadata associated with this image.  If the reader is
   * set to ignore metadata or does not support reading metadata, or
   * if no metadata is available then null is returned.
   *
   * @param imageIndex the frame index
   *
   * @return a metadata object, or null
   *
   * @exception IllegalStateException if input has not been set
   * @exception IndexOutOfBoundsException if the frame index is
   * out-of-bounds
   * @exception IOException if a read error occurs
   */
  public abstract IIOMetadata getImageMetadata(int imageIndex)
    throws IOException;

  /**
   * Get an iterator over the collection of image types into which
   * this reader can decode image data.  This method is guaranteed to
   * return at least one valid image type specifier.
   *
   * The elements of the iterator should be ordered; the first element
   * should be the most appropriate image type for this decoder,
   * followed by the second-most appropriate, and so on.
   *
   * @param imageIndex the frame index
   *
   * @return an iterator over a collection of image type specifiers
   *
   * @exception IllegalStateException if input has not been set
   * @exception IndexOutOfBoundsException if the frame index is
   * out-of-bounds
   * @exception IOException if a read error occurs
   */
  public abstract Iterator getImageTypes(int imageIndex)
    throws IOException;

  /**
   * Set the input source to the given object, specify whether this
   * reader should be allowed to read input from the data stream more
   * than once, and specify whether this reader should ignore metadata
   * in the input stream.  The input source must be set before many
   * methods can be called on this reader. (see all ImageReader
   * methods that throw IllegalStateException).  If input is null then
   * the current input source will be removed.
   *
   * Unless this reader has direct access with imaging hardware, input
   * should be an ImageInputStream.
   *
   * @param input the input source object
   * @param seekForwardOnly true if this reader should be allowed to
   * read input from the data stream more than once, false otherwise
   * @param ignoreMetadata true if this reader should ignore metadata
   * associated with the input source, false otherwise
   *
   * @exception IllegalArgumentException if input is not a valid input
   * source for this reader and is not an ImageInputStream
   */
  public void setInput(Object input,
                       boolean seekForwardOnly,
                       boolean ignoreMetadata)
  {
    Class[] okClasses = originatingProvider.getInputTypes();
    if (okClasses == null)
      {
        if (!(input instanceof ImageInputStream))
          throw new IllegalArgumentException();
      }
    else
      {
        boolean classOk = false;
        for (int i = 0; i < okClasses.length; ++i)
          if (okClasses[i].isInstance(input))
            classOk = true;
        if (!classOk)
          throw new IllegalArgumentException();
      }

    this.input = input;
    this.seekForwardOnly = seekForwardOnly;
    this.ignoreMetadata = ignoreMetadata;
    this.minIndex = 0;
  }

  /**
   * Set the input source to the given object and specify whether this
   * reader should be allowed to read input from the data stream more
   * than once.  The input source must be set before many methods can
   * be called on this reader. (see all ImageReader methods that throw
   * IllegalStateException).  If input is null then the current input
   * source will be removed.
   *
   * @param input the input source object
   * @param seekForwardOnly true if this reader should be allowed to
   * read input from the data stream more than once, false otherwise
   *
   * @exception IllegalArgumentException if input is not a valid input
   * source for this reader and is not an ImageInputStream
   */
  public void setInput(Object in, boolean seekForwardOnly)
  {
    setInput(in, seekForwardOnly, false);
  }

  /**
   * Set the input source to the given object.  The input source must
   * be set before many methods can be called on this reader. (see all
   * ImageReader methods that throw IllegalStateException).  If input
   * is null then the current input source will be removed.
   *
   * @param input the input source object
   *
   * @exception IllegalArgumentException if input is not a valid input
   * source for this reader and is not an ImageInputStream
   */
  public void setInput(Object input)
  {
    setInput(input, false, false);
  }

  /**
   * Get this reader's image input source.  null is returned if the
   * image source has not been set.
   *
   * @return an image input source object, or null
   */
  public Object getInput()
  {
    return input;
  }

  /**
   * Get this reader's locale.  null is returned if the locale has not
   * been set.
   *
   * @return this reader's locale, or null
   */
  public Locale getLocale()
  {
    return locale;
  }

  /**
   * Return the number of images available from the image input
   * source, not including thumbnails.  This method will return 1
   * unless this reader is reading an animated image.
   *
   * Certain multi-image formats do not encode the total number of
   * images.  When reading images in those formats it may be necessary
   * to repeatedly call read, incrementing the image index at each
   * call, until an IndexOutOfBoundsException is thrown.
   *
   * The allowSearch parameter determines whether all images must be
   * available at all times.  When allowSearch is false, getNumImages
   * will return -1 if the total number of images is unknown.
   * Otherwise this method returns the number of images.
   *
   * @param allowSearch true if all images should be available at
   * once, false otherwise
   *
   * @return -1 if allowSearch is false and the total number of images
   * is currently unknown, or the number of images
   *
   * @exception IllegalStateException if input has not been set, or if
   * seekForwardOnly is true
   * @exception IOException if a read error occurs
   */
  public abstract int getNumImages(boolean allowSearch)
    throws IOException;

  /**
   * Get the number of thumbnails associated with an image.
   *
   * @param imageIndex the frame index
   *
   * @return the number of thumbnails associated with this image
   */
  public int getNumThumbnails(int imageIndex)
    throws IOException
  {
    return 0;
  }

  /**
   * Get the ImageReaderSpi that created this reader or null.
   *
   * @return an ImageReaderSpi, or null
   */
  public ImageReaderSpi getOriginatingProvider()
  {
    return originatingProvider;
  }

  /**
   * Get the metadata associated with the image being read.  If the
   * reader is set to ignore metadata or does not support reading
   * metadata, or if no metadata is available then null is returned.
   * This method returns metadata associated with the entirety of the
   * image data, whereas getImageMetadata(int) returns metadata
   * associated with a frame within a multi-image data stream.
   *
   * @return metadata associated with the image being read, or null
   *
   * @exception IOException if a read error occurs
   */
  public abstract IIOMetadata getStreamMetadata()
    throws IOException;

  /**
   * Get the height of a thumbnail image.
   *
   * @param imageIndex the frame index
   * @param thumbnailIndex the thumbnail index
   *
   * @return the height of the thumbnail image
   *
   * @exception UnsupportedOperationException if this reader does not
   * support thumbnails
   * @exception IllegalStateException if input is null
   * @exception IndexOutOfBoundsException if either index is
   * out-of-bounds
   * @exception IOException if a read error occurs
   */
  public int getThumbnailHeight(int imageIndex, int thumbnailIndex)
    throws IOException
  {
    return readThumbnail(imageIndex, thumbnailIndex).getHeight();
  }

  /**
   * Get the width of a thumbnail image.
   *
   * @param imageIndex the frame index
   * @param thumbnailIndex the thumbnail index
   *
   * @return the width of the thumbnail image
   *
   * @exception UnsupportedOperationException if this reader does not
   * support thumbnails
   * @exception IllegalStateException if input is null
   * @exception IndexOutOfBoundsException if either index is
   * out-of-bounds
   * @exception IOException if a read error occurs
   */
  public int getThumbnailWidth(int imageIndex, int thumbnailIndex)
    throws IOException
  {
    return readThumbnail(imageIndex, thumbnailIndex).getWidth();
  }

  /**
   * Get the X coordinate in pixels of the top-left corner of the
   * first tile in this image.
   *
   * @param imageIndex the frame index
   *
   * @return the X coordinate of this image's first tile
   *
   * @exception IllegalStateException if input is needed but the input
   * source is not set
   * @exception IndexOutOfBoundsException if the frame index is
   * out-of-bounds
   * @exception IOException if a read error occurs
   */
  public int getTileGridXOffset(int imageIndex)
    throws IOException
  {
    return 0;
  }

  /**
   * Get the Y coordinate in pixels of the top-left corner of the
   * first tile in this image.
   *
   * @param imageIndex the frame index
   *
   * @return the Y coordinate of this image's first tile
   *
   * @exception IllegalStateException if input is needed but the input
   * source is not set
   * @exception IndexOutOfBoundsException if the frame index is
   * out-of-bounds
   * @exception IOException if a read error occurs
   */
  public int getTileGridYOffset(int imageIndex)
    throws IOException
  {
    return 0;
  }

  /**
   * Get the height of an image tile.
   *
   * @param imageIndex the frame index
   *
   * @return the tile height for the given image
   *
   * @exception IllegalStateException if input is null
   * @exception IndexOutOfBoundsException if the frame index is
   * out-of-bounds
   * @exception IOException if a read error occurs
   */
  public int getTileHeight(int imageIndex)
    throws IOException
  {
    return getHeight(imageIndex);
  }

  /**
   * Get the width of an image tile.
   *
   * @param imageIndex the frame index
   *
   * @return the tile width for the given image
   *
   * @exception IllegalStateException if input is null
   * @exception IndexOutOfBoundsException if the frame index is
   * out-of-bounds
   * @exception IOException if a read error occurs
   */
  public int getTileWidth(int imageIndex)
    throws IOException
  {
    return getWidth(imageIndex);
  }

  /**
   * Get the width of the input image in pixels.  If the input image
   * is resizable then a default width is returned.
   *
   * @param imageIndex the image's index
   *
   * @return the width of the input image
   *
   * @exception IllegalStateException if input has not been set
   * @exception IndexOutOfBoundsException if the frame index is
   * out-of-bounds
   * @exception IOException if a read error occurs
   */
  public abstract int getWidth(int imageIndex)
    throws IOException;

  /**
   * Check whether or not the given image has thumbnails associated
   * with it.
   *
   * @return true if the given image has thumbnails, false otherwise
   *
   * @exception IllegalStateException if input is null
   * @exception IndexOutOfBoundsException if the frame index is
   * out-of-bounds
   * @exception IOException if a read error occurs
   */
  public boolean hasThumbnails(int imageIndex)
    throws IOException
  {
    return getNumThumbnails(imageIndex) > 0;
  }

  /**
   * Check if this image reader ignores metadata.  This method simply
   * returns the value of ignoreMetadata.
   *
   * @return true if metadata is being ignored, false otherwise
   */
  public boolean isIgnoringMetadata()
  {
    return ignoreMetadata;
  }

  /**
   * Check if the given image is sub-divided into equal-sized
   * non-overlapping pixel rectangles.
   *
   * A reader may expose tiling in the underlying format, hide it, or
   * simulate tiling even if the underlying format is not tiled.
   *
   * @return true if the given image is tiled, false otherwise
   *
   * @exception IllegalStateException if input is null
   * @exception IndexOutOfBoundsException if the frame index is
   * out-of-bounds
   * @exception IOException if a read error occurs
   */
  public boolean isImageTiled(int imageIndex)
    throws IOException
  {
    return false;
  }

  /**
   * Check if all pixels in this image are readily accessible.  This
   * method should return false for compressed formats.  The return
   * value is a hint as to the efficiency of certain image reader
   * operations.
   *
   * @param imageIndex the frame index
   *
   * @return true if random pixel access is fast, false otherwise
   *
   * @exception IllegalStateException if input is null and it is
   * needed to determine the return value
   * @exception IndexOutOfBoundsException if the frame index is
   * out-of-bounds but the frame data must be accessed to determine
   * the return value
   * @exception IOException if a read error occurs
   */
  public boolean isRandomAccessEasy(int imageIndex)
    throws IOException
  {
    return false;
  }

  /**
   * Check if this image reader may only seek forward within the input
   * stream.
   *
   * @return true if this reader may only seek forward, false
   * otherwise
   */
  public boolean isSeekForwardOnly()
  {
    return seekForwardOnly;
  }

  /**
   * Notifies all installed read progress listeners that image loading
   * has completed by calling their imageComplete methods.
   */
  protected void processImageComplete()
  {
    if (progressListeners != null)
      {
	Iterator it = progressListeners.iterator();

	while (it.hasNext())
	  {
	    IIOReadProgressListener listener =
	      (IIOReadProgressListener) it.next();
	    listener.imageComplete (this);
	  }
      }
  }

  /**
   * Notifies all installed read progress listeners that a certain
   * percentage of the image has been loaded, by calling their
   * imageProgress methods.
   *
   * @param percentageDone the percentage of image data that has been
   * loaded
   */
  protected void processImageProgress(float percentageDone)
  {
     if (progressListeners != null)
      {
	Iterator it = progressListeners.iterator();

	while (it.hasNext())
	  {
	    IIOReadProgressListener listener =
	      (IIOReadProgressListener) it.next();
	    listener.imageProgress(this, percentageDone);
	  }
      }
  }
  /**
   * Notifies all installed read progress listeners, by calling their
   * imageStarted methods, that image loading has started on the given
   * image.
   *
   * @param imageIndex the frame index of the image that has started
   * loading
   */
  protected void processImageStarted(int imageIndex)
  {
     if (progressListeners != null)
      {
	Iterator it = progressListeners.iterator();

	while (it.hasNext())
	  {
	    IIOReadProgressListener listener =
	      (IIOReadProgressListener) it.next();
	    listener.imageStarted(this, imageIndex);
	  }
      }
  }

  /**
   * Notifies all installed read update listeners, by calling their
   * imageUpdate methods, that the set of samples has changed.
   *
   * @param image the buffered image that is being updated
   * @param minX the X coordinate of the top-left pixel in this pass
   * @param minY the Y coordinate of the top-left pixel in this pass
   * @param width the total width of the rectangle covered by this
   * pass, including skipped pixels
   * @param height the total height of the rectangle covered by this
   * pass, including skipped pixels
   * @param periodX the horizontal sample interval
   * @param periodY the vertical sample interval
   * @param bands the affected bands in the destination
   */
  protected void processImageUpdate(BufferedImage image, int minX, int minY,
				    int width, int height, int periodX,
				    int periodY, int[] bands)
  {
    if (updateListeners != null)
      {
	Iterator it = updateListeners.iterator();

	while (it.hasNext())
	  {
	    IIOReadUpdateListener listener = (IIOReadUpdateListener) it.next();
	    listener.imageUpdate(this, image, minX, minY, width, height,
				 periodX, periodY, bands);
	  }
      }
  }

  /**
   * Notifies all installed update progress listeners, by calling
   * their passComplete methods, that a progressive pass has
   * completed.
   *
   * @param image the image that has being updated
   */
  protected void processPassComplete(BufferedImage image)
  {
    if (updateListeners != null)
      {
	Iterator it = updateListeners.iterator();

	while (it.hasNext())
	  {
	    IIOReadUpdateListener listener = (IIOReadUpdateListener) it.next();
	    listener.passComplete(this, image);
	  }
      }
  }

  /**
   * Notifies all installed read update listeners, by calling their
   * passStarted methods, that a new pass has begun.
   *
   * @param image the buffered image that is being updated
   * @param pass the current pass number
   * @param minPass the pass at which decoding will begin
   * @param maxPass the pass at which decoding will end
   * @param minX the X coordinate of the top-left pixel in this pass
   * @param minY the Y coordinate of the top-left pixel in this pass
   * @param width the total width of the rectangle covered by this
   * pass, including skipped pixels
   * @param height the total height of the rectangle covered by this
   * pass, including skipped pixels
   * @param periodX the horizontal sample interval
   * @param periodY the vertical sample interval
   * @param bands the affected bands in the destination
   */
  protected void processPassStarted(BufferedImage image, int pass, int minPass,
				    int maxPass, int minX, int minY,
				    int periodX, int periodY, int[] bands)
  {
    if (updateListeners != null)
      {
	Iterator it = updateListeners.iterator();

	while (it.hasNext())
	  {
	    IIOReadUpdateListener listener = (IIOReadUpdateListener) it.next();
	    listener.passStarted(this, image, pass, minPass, maxPass, minX,
				 minY, periodX, periodY, bands);
	  }
      }
  }

  /**
   * Notifies all installed read progress listeners that image loading
   * has been aborted by calling their readAborted methods.
   */
  protected void processReadAborted()
  {
     if (progressListeners != null)
      {
	Iterator it = progressListeners.iterator();

	while (it.hasNext())
	  {
	    IIOReadProgressListener listener =
	      (IIOReadProgressListener) it.next();
	    listener.readAborted(this);
	  }
      }
  }
  /**
   * Notifies all installed read progress listeners, by calling their
   * sequenceComplete methods, that a sequence of images has completed
   * loading.
   */
  protected void processSequenceComplete()
  {
     if (progressListeners != null)
      {
	Iterator it = progressListeners.iterator();

	while (it.hasNext())
	  {
	    IIOReadProgressListener listener =
	      (IIOReadProgressListener) it.next();
	    listener.sequenceComplete(this);
	  }
      }
  }

  /**
   * Notifies all installed read progress listeners, by calling their
   * sequenceStarted methods, a sequence of images has started
   * loading.
   *
   * @param minIndex the index of the first image in the sequence
   */
  protected void processSequenceStarted(int minIndex)
  {

    if (progressListeners != null)
      {
	Iterator it = progressListeners.iterator();

	while (it.hasNext())
	  {
	    IIOReadProgressListener listener =
	      (IIOReadProgressListener) it.next();
	    listener.sequenceStarted(this, minIndex);
	  }
      }
  }

  /**
   * Notifies all installed read progress listeners, by calling their
   * thumbnailComplete methods, that a thumbnail has completed
   * loading.
   */
  protected void processThumbnailComplete()
  {
    if (progressListeners != null)
      {
	Iterator it = progressListeners.iterator();

	while (it.hasNext())
	  {
	    IIOReadProgressListener listener =
	      (IIOReadProgressListener) it.next();
	    listener.thumbnailComplete(this);
	  }
      }
  }

  /**
   * Notifies all installed update progress listeners, by calling
   * their thumbnailPassComplete methods, that a progressive pass has
   * completed on a thumbnail.
   *
   * @param thumbnail the thumbnail that has being updated
   */
  protected void processThumbnailPassComplete(BufferedImage thumbnail)
  {
    if (updateListeners != null)
      {
	Iterator it = updateListeners.iterator();

	while (it.hasNext())
	  {
	    IIOReadUpdateListener listener = (IIOReadUpdateListener) it.next();
	    listener.thumbnailPassComplete(this, thumbnail);
	  }
      }
  }

  /**
   * Notifies all installed read update listeners, by calling their
   * thumbnailPassStarted methods, that a new pass has begun.
   *
   * @param thumbnail the thumbnail that is being updated
   * @param pass the current pass number
   * @param minPass the pass at which decoding will begin
   * @param maxPass the pass at which decoding will end
   * @param minX the X coordinate of the top-left pixel in this pass
   * @param minY the Y coordinate of the top-left pixel in this pass
   * @param width the total width of the rectangle covered by this
   * pass, including skipped pixels
   * @param height the total height of the rectangle covered by this
   * pass, including skipped pixels
   * @param periodX the horizontal sample interval
   * @param periodY the vertical sample interval
   * @param bands the affected bands in the destination
   */
  protected void processThumbnailPassStarted(BufferedImage thumbnail, int pass,
					     int minPass, int maxPass, int minX,
					     int minY, int periodX, int periodY,
					     int[] bands)
  {
    if (updateListeners != null)
      {
	Iterator it = updateListeners.iterator();

	while (it.hasNext())
	  {
	    IIOReadUpdateListener listener = (IIOReadUpdateListener) it.next();
	    listener.thumbnailPassStarted(this, thumbnail, pass, minPass,
					  maxPass, minX, minY, periodX,
					  periodY, bands);
	  }
      }
  }

  /**
   * Notifies all installed read progress listeners that a certain
   * percentage of a thumbnail has been loaded, by calling their
   * thumbnailProgress methods.
   *
   * @param percentageDone the percentage of thumbnail data that has
   * been loaded
   */
  protected void processThumbnailProgress(float percentageDone)
  {
    if (progressListeners != null)
      {
	Iterator it = progressListeners.iterator();

	while (it.hasNext())
	  {
	    IIOReadProgressListener listener =
	      (IIOReadProgressListener) it.next();
	    listener.thumbnailProgress(this, percentageDone);
	  }
      }
  }

  /**
   * Notifies all installed read progress listeners, by calling their
   * imageStarted methods, that thumbnail loading has started on the
   * given thumbnail of the given image.
   *
   * @param imageIndex the frame index of the image one of who's
   * thumbnails has started loading
   * @param thumbnailIndex the index of the thumbnail that has started
   * loading
   */
  protected void processThumbnailStarted(int imageIndex, int thumbnailIndex)
  {
    if (progressListeners != null)
      {
	Iterator it = progressListeners.iterator();

	while (it.hasNext())
	  {
	    IIOReadProgressListener listener =
	      (IIOReadProgressListener) it.next();
	    listener.thumbnailStarted(this, imageIndex, thumbnailIndex);
	  }
      }
  }

  /**
   * Notifies all installed read update listeners, by calling their
   * thumbnailUpdate methods, that the set of samples has changed.
   *
   * @param image the buffered image that is being updated
   * @param minX the X coordinate of the top-left pixel in this pass
   * @param minY the Y coordinate of the top-left pixel in this pass
   * @param width the total width of the rectangle covered by this
   * pass, including skipped pixels
   * @param height the total height of the rectangle covered by this
   * pass, including skipped pixels
   * @param periodX the horizontal sample interval
   * @param periodY the vertical sample interval
   * @param bands the affected bands in the destination
   */
  protected void processThumbnailUpdate(BufferedImage image, int minX, int minY,
					int width, int height, int periodX,
					int periodY, int[] bands)
  {
    if (updateListeners != null)
      {
	Iterator it = updateListeners.iterator();

	while (it.hasNext())
	  {
	    IIOReadUpdateListener listener = (IIOReadUpdateListener) it.next();
	    listener.thumbnailUpdate(this, image, minX, minY, width, height,
				     periodX, periodY, bands);
	  }
      }
  }

  /**
   * Notifies all installed warning listeners, by calling their
   * warningOccurred methods, that a warning message has been raised.
   *
   * @param warning the warning message
   *
   * @exception IllegalArgumentException if warning is null
   */
  protected void processWarningOccurred(String warning)
  {
    if (warning == null)
      throw new IllegalArgumentException ("null argument");
    if (warningListeners != null)
      {
	Iterator it = warningListeners.iterator();

	while (it.hasNext())
	  {
	    IIOReadWarningListener listener =
	      (IIOReadWarningListener) it.next();
	    listener.warningOccurred(this, warning);
	  }
      }
  }

  /**
   * Notify all installed warning listeners, by calling their
   * warningOccurred methods, that a warning message has been raised.
   * The warning message is retrieved from a resource bundle, using
   * the given basename and keyword.
   *
   * @param baseName the basename of the resource from which to
   * retrieve the warning message
   * @param keyword the keyword used to retrieve the warning from the
   * resource bundle
   *
   * @exception IllegalArgumentException if either baseName or keyword
   * is null
   * @exception IllegalArgumentException if no resource bundle is
   * found using baseName
   * @exception IllegalArgumentException if the given keyword produces
   * no results from the resource bundle
   * @exception IllegalArgumentException if the retrieved object is
   * not a String
   */
  protected void processWarningOccurred(String baseName,
					String keyword)
  {
    if (baseName == null || keyword == null)
      throw new IllegalArgumentException ("null argument");

    ResourceBundle b = null;

    try
      {
	b = ResourceBundle.getBundle(baseName, getLocale());
      }
    catch (MissingResourceException e)
      {
	throw new IllegalArgumentException ("no resource bundle found");
      }

    Object str = null;

    try
      {
	str = b.getObject(keyword);
      }
    catch (MissingResourceException e)
      {
	throw new IllegalArgumentException ("no results found for keyword");
      }

    if (! (str instanceof String))
      throw new IllegalArgumentException ("retrieved object not a String");

    String warning = (String) str;

    if (warningListeners != null)
      {
	Iterator it = warningListeners.iterator();

	while (it.hasNext())
	  {
	    IIOReadWarningListener listener =
	      (IIOReadWarningListener) it.next();
	    listener.warningOccurred(this, warning);
	  }
      }
  }

  /**
   * Read the given frame into a buffered image using the given read
   * parameters.  Listeners will be notified of image loading progress
   * and warnings.
   *
   * @param imageIndex the index of the frame to read
   * @param param the image read parameters to use when reading
   *
   * @return a buffered image
   *
   * @exception IllegalStateException if input is null
   * @exception IndexOutOfBoundsException if the frame index is
   * out-of-bounds
   * @exception IOException if a read error occurs
   */
  public abstract BufferedImage read(int imageIndex, ImageReadParam param)
    throws IOException;

  /**
   * Check if this reader supports reading thumbnails.
   *
   * @return true if this reader supports reading thumbnails, false
   * otherwise
   */
  public boolean readerSupportsThumbnails()
  {
    return false;
  }

  /**
   * Read raw raster data.  The image type specifier in param is
   * ignored but all other parameters are used.  Offset parameters are
   * translated into the raster's coordinate space.  This method may
   * be implemented by image readers that want to provide direct
   * access to raw image data.
   *
   * @param imageIndex the frame index
   * @param param the image read parameters
   *
   * @return a raster containing the read image data
   *
   * @exception UnsupportedOperationException if this reader doesn't
   * support rasters
   * @exception IllegalStateException if input is null
   * @exception IndexOutOfBoundsException if the frame index is
   * out-of-bounds
   * @exception IOException if a read error occurs
   */
  public Raster readRaster(int imageIndex, ImageReadParam param)
    throws IOException
  {
    throw new UnsupportedOperationException();
  }

  /**
   * Read a thumbnail.
   *
   * @param imageIndex the frame index
   * @param thumbnailIndex the thumbnail index
   *
   * @return a buffered image of the thumbnail
   *
   * @exception UnsupportedOperationException if this reader doesn't
   * support thumbnails
   * @exception IllegalStateException if input is null
   * @exception IndexOutOfBoundsException if either the frame index or
   * the thumbnail index is out-of-bounds
   * @exception IOException if a read error occurs
   * 
   */
  public BufferedImage readThumbnail(int imageIndex, int thumbnailIndex)
    throws IOException
  {
    throw new UnsupportedOperationException();
  }

  /**
   * Uninstall all read progress listeners.
   */
  public void removeAllIIOReadProgressListeners()
  {
    progressListeners = null;
  }

  /**
   * Uninstall all read update listeners.
   */
  public void removeAllIIOReadUpdateListeners()
  {
    updateListeners = null;
  }

  /**
   * Uninstall all read warning listeners.
   */
  public void removeAllIIOReadWarningListeners()
  {
    warningListeners = null;
  }

  /**
   * Uninstall the given read progress listener.
   *
   * @param listener the listener to remove
   */
  public void removeIIOReadProgressListener(IIOReadProgressListener listener) 
  {
    if (listener == null)
      return;
    if (progressListeners != null)
      {
	progressListeners.remove(listener);
      }
  }

  /**
   * Uninstall the given read update listener.
   *
   * @param listener the listener to remove
   */
  public void removeIIOReadUpdateListener(IIOReadUpdateListener listener) 
  {
    if (listener == null)
      return;

    if (updateListeners != null)
      {
	updateListeners.remove(listener);
      }
  }

  /**
   * Uninstall the given read warning listener.
   *
   * @param listener the listener to remove
   */
  public void removeIIOReadWarningListener(IIOReadWarningListener listener)
  {
    if (listener == null)
      return;
    if (warningListeners != null)
      {
	warningListeners.remove(listener);
      }
  }

  /**
   * Set the current locale or use the default locale.
   *
   * @param locale the locale to set, or null
   */
  public void setLocale(Locale locale)
  {
    if (locale != null)
      {
	// Check if its a valid locale.
	boolean found = false;

	if (availableLocales != null)
	  for (int i = availableLocales.length - 1; i >= 0; --i)
	    if (availableLocales[i].equals(locale))
	      found = true;

	if (! found)
	  throw new IllegalArgumentException("looale not available");
      }

    this.locale = locale;
  }

  /**
   * Check that the given read parameters have valid source and
   * destination band settings.  If the param.getSourceBands() returns
   * null, the array is assumed to include all band indices, 0 to
   * numSrcBands - 1; likewise if param.getDestinationBands() returns
   * null, it is assumed to be an array containing indices 0 to
   * numDstBands - 1.  A failure will cause this method to throw
   * IllegalArgumentException.
   *
   * @param param the image parameters to check
   * @param numSrcBands the number of input source bands
   * @param numDstBands the number of ouput destination bands
   *
   * @exception IllegalArgumentException if either the given source or
   * destination band indices are invalid
   */
  protected static void checkReadParamBandSettings(ImageReadParam param,
						   int numSrcBands,
						   int numDstBands)
  {
    int[] srcBands = param.getSourceBands();
    int[] dstBands = param.getDestinationBands();
    boolean lengthsDiffer = false;
    boolean srcOOB = false;
    boolean dstOOB = false;

    if (srcBands == null)
      {
        if (dstBands == null)
          {
            if (numSrcBands != numDstBands)
              lengthsDiffer = true;
          }
        else
          {
            if (numSrcBands != dstBands.length)
              lengthsDiffer = true;

            for (int i = 0; i < dstBands.length; i++)
              if (dstBands[i] > numSrcBands - 1)
                {
                  dstOOB = true;
                  break;
                }
          }
      }
    else
      {
        if (dstBands == null)
          {
            if (srcBands.length != numDstBands)
              lengthsDiffer = true;

            for (int i = 0; i < srcBands.length; i++)
              if (srcBands[i] > numDstBands - 1)
                {
                  srcOOB = true;
                  break;
                }
          }
        else
          {
            if (srcBands.length != dstBands.length)
              lengthsDiffer = true;

            for (int i = 0; i < srcBands.length; i++)
              if (srcBands[i] > numDstBands - 1)
                {
                  srcOOB = true;
                  break;
                }

            for (int i = 0; i < dstBands.length; i++)
              if (dstBands[i] > numSrcBands - 1)
                {
                  dstOOB = true;
                  break;
                }
          }
      }

    if (lengthsDiffer)
      throw new IllegalArgumentException ("array lengths differ");

    if (srcOOB)
      throw new IllegalArgumentException ("source band index"
                                          + " out-of-bounds");

    if (dstOOB)
      throw new IllegalArgumentException ("destination band index"
                                          + " out-of-bounds");
  }

  /**
   * Calcluate the source and destination regions that will be read
   * from and written to, given image parameters and/or a destination
   * buffered image.  The source region will be clipped if any of its
   * bounds are outside the destination region.  Clipping will account
   * for subsampling and destination offsets.  Likewise, the
   * destination region is clipped to the given destination image, if
   * it is not null, using the given image parameters, if they are not
   * null.  IllegalArgumentException is thrown if either region will
   * contain 0 pixels after clipping.
   *
   * @param image read parameters, or null
   * @param srcWidth the width of the source image
   * @param srcHeight the height of the source image
   * @param image the destination image, or null
   * @param srcRegion a rectangle whose values will be set to the
   * clipped source region
   * @param destRegion a rectangle whose values will be set to the
   * clipped destination region
   *
   * @exception IllegalArgumentException if either srcRegion or
   * destRegion is null
   * @exception IllegalArgumentException if either of the calculated
   * regions is empty
   */
  protected static void computeRegions (ImageReadParam param,
					int srcWidth,
					int srcHeight,
					BufferedImage image,
					Rectangle srcRegion,
					Rectangle destRegion)
  {
    if (srcRegion == null || destRegion == null)
      throw new IllegalArgumentException ("null region");

    if (srcWidth == 0 || srcHeight == 0)
      throw new IllegalArgumentException ("zero-sized region");

    srcRegion = getSourceRegion(param, srcWidth, srcHeight);
    if (image != null)
      destRegion = new Rectangle (0, 0, image.getWidth(), image.getHeight());
    else
      destRegion = new Rectangle (0, 0, srcWidth, srcHeight);

    if (param != null)
      {
        Point offset = param.getDestinationOffset();

        if (offset.x < 0)
          {
            srcRegion.x -= offset.x;
            srcRegion.width += offset.x;
          }
        if (offset.y < 0)
          {
            srcRegion.y -= offset.y;
            srcRegion.height += offset.y;
          }

        srcRegion.width = srcRegion.width > destRegion.width
          ? destRegion.width : srcRegion.width;
        srcRegion.height = srcRegion.height > destRegion.height
          ? destRegion.height : srcRegion.height;

        if (offset.x >= 0)
          {
            destRegion.x += offset.x;
            destRegion.width -= offset.x;
          }
        if (offset.y >= 0)
          {
            destRegion.y += offset.y;
            destRegion.height -= offset.y;
          }
      }

    if (srcRegion.isEmpty() || destRegion.isEmpty())
      throw new IllegalArgumentException ("zero-sized region");
  }

  /**
   * Return a suitable destination buffered image.  If
   * param.getDestination() is non-null, then it is returned,
   * otherwise a buffered image is created using
   * param.getDestinationType() if it is non-null and also in the
   * given imageTypes collection, or the first element of imageTypes
   * otherwise.
   *
   * @param param image read parameters from which a destination image
   * or image type is retrieved, or null
   * @param imageTypes a collection of legal image types
   * @param width the width of the source image
   * @param height the height of the source image
   *
   * @return a suitable destination buffered image
   *
   * @exception IIOException if param.getDestinationType() does not
   * return an image type in imageTypes
   * @exception IllegalArgumentException if imageTypes is null or
   * empty, or if a non-ImageTypeSpecifier object is retrieved from
   * imageTypes
   * @exception IllegalArgumentException if the resulting destination
   * region is empty
   * @exception IllegalArgumentException if the product of width and
   * height is greater than Integer.MAX_VALUE
   */
  protected static BufferedImage getDestination (ImageReadParam param,
						 Iterator imageTypes,
						 int width,
						 int height)
    throws IIOException
  {
    if (imageTypes == null || !imageTypes.hasNext())
      throw new IllegalArgumentException ("imageTypes null or empty");

    if (width < 0 || height < 0)
      throw new IllegalArgumentException ("negative dimension");

    // test for overflow
    if (width * height < Math.min (width, height))
      throw new IllegalArgumentException ("width * height > Integer.MAX_VALUE");

    BufferedImage dest = null;
    ImageTypeSpecifier destType = null;

    if (param != null)
      {
        dest = param.getDestination ();
        if (dest == null)
          {
            ImageTypeSpecifier type = param.getDestinationType();
            if (type != null)
              {
                Iterator it = imageTypes;

                while (it.hasNext())
                  {
                    Object o = it.next ();
                    if (! (o instanceof ImageTypeSpecifier))
                      throw new IllegalArgumentException ("non-ImageTypeSpecifier object");

                    ImageTypeSpecifier t = (ImageTypeSpecifier) o;
                    if (t.equals (type))
                      {
                        dest = t.createBufferedImage (width, height);
                        break;
                      }
                    if (destType == null)
                      throw new IIOException ("invalid destination type");

                  }
              }
          }
      }
    if (dest == null)
      {
        Rectangle srcRegion = new Rectangle ();
        Rectangle destRegion = new Rectangle ();

        computeRegions (param, width, height, null, srcRegion, destRegion);

        if (destRegion.isEmpty())
          throw new IllegalArgumentException ("destination region empty");

        if (destType == null)
          {
            Object o = imageTypes.next();
            if (! (o instanceof ImageTypeSpecifier))
              throw new IllegalArgumentException ("non-ImageTypeSpecifier"
                                                  + " object");

            dest = ((ImageTypeSpecifier) o).createBufferedImage
              (destRegion.width, destRegion.height);
          }
        else
          dest = destType.createBufferedImage
            (destRegion.width, destRegion.height);
      }
    return dest;
  }

  /**
   * Get the metadata associated with this image.  If the reader is
   * set to ignore metadata or does not support reading metadata, or
   * if no metadata is available then null is returned.
   *
   * This more specific version of getImageMetadata(int) can be used
   * to restrict metadata retrieval to specific formats and node
   * names, which can limit the amount of data that needs to be
   * processed.
   *
   * @param imageIndex the frame index
   * @param formatName the format of metadata requested
   * @param nodeNames a set of Strings specifiying node names to be
   * retrieved
   *
   * @return a metadata object, or null
   *
   * @exception IllegalStateException if input has not been set
   * @exception IndexOutOfBoundsException if the frame index is
   * out-of-bounds
   * @exception IllegalArgumentException if formatName is null
   * @exception IllegalArgumentException if nodeNames is null
   * @exception IOException if a read error occurs
   */
  public IIOMetadata getImageMetadata (int imageIndex,
                                       String formatName,
                                       Set nodeNames)
    throws IOException
  {
    if (formatName == null || nodeNames == null)
      throw new IllegalArgumentException ("null argument");

    return getImageMetadata (imageIndex);
  }

  /**
   * Get the index at which the next image will be read.  If
   * seekForwardOnly is true then the returned value will increase
   * monotonically each time an image frame is read.  If
   * seekForwardOnly is false then the returned value will always be
   * 0.
   *
   * @return the current frame index
   */
  public int getMinIndex()
  {
    return minIndex;
  }

  /**
   * Get the image type specifier that most closely represents the
   * internal data representation used by this reader.  This value
   * should be included in the return value of getImageTypes.
   *
   * @param imageIndex the frame index
   *
   * @return an image type specifier
   *
   * @exception IllegalStateException if input has not been set
   * @exception IndexOutOfBoundsException if the frame index is
   * out-of-bounds
   * @exception IOException if a read error occurs
   */
  public ImageTypeSpecifier getRawImageType (int imageIndex)
    throws IOException
  {
    return (ImageTypeSpecifier) getImageTypes(imageIndex).next();
  }

  /**
   * Calculate a source region based on the given source image
   * dimensions and parameters.  Subsampling offsets and a source
   * region are taken from the given image read parameters and used to
   * clip the given image dimensions, returning a new rectangular
   * region as a result.
   *
   * @param param image parameters, or null
   * @param srcWidth the width of the source image
   * @param srcHeight the height of the source image
   *
   * @return a clipped rectangle
   */
  protected static Rectangle getSourceRegion (ImageReadParam param,
					      int srcWidth,
					      int srcHeight)
  {
    Rectangle clippedRegion = new Rectangle (0, 0, srcWidth, srcHeight);

    if (param != null)
      {
        Rectangle srcRegion = param.getSourceRegion();

        if (srcRegion != null)
          {
            clippedRegion.x = srcRegion.x > clippedRegion.x
              ? srcRegion.x : clippedRegion.x;
            clippedRegion.y = srcRegion.y > clippedRegion.y
              ? srcRegion.y : clippedRegion.y;
            clippedRegion.width = srcRegion.width > clippedRegion.width
              ? srcRegion.width : clippedRegion.width;
            clippedRegion.height = srcRegion.height > clippedRegion.height
              ? srcRegion.height : clippedRegion.height;
          }

        int xOffset = param.getSubsamplingXOffset();

        clippedRegion.x += xOffset;
        clippedRegion.width -= xOffset;

        int yOffset = param.getSubsamplingYOffset();

        clippedRegion.y += yOffset;
        clippedRegion.height -= yOffset;
      }
    return clippedRegion;
  }

  /**
   * Get the metadata associated with the image being read.  If the
   * reader is set to ignore metadata or does not support reading
   * metadata, or if no metadata is available then null is returned.
   * This method returns metadata associated with the entirety of the
   * image data, whereas getStreamMetadata() returns metadata
   * associated with a frame within a multi-image data stream.
   *
   * This more specific version of getStreamMetadata() can be used to
   * restrict metadata retrieval to specific formats and node names,
   * which can limit the amount of data that needs to be processed.
   *
   * @param formatName the format of metadata requested
   * @param nodeNames a set of Strings specifiying node names to be
   * retrieved
   *
   * @return metadata associated with the image being read, or null
   *
   * @exception IllegalArgumentException if formatName is null
   * @exception IllegalArgumentException if nodeNames is null
   * @exception IOException if a read error occurs
   */
  public IIOMetadata getStreamMetadata (String formatName,
                                        Set nodeNames)
    throws IOException
  {
    if (formatName == null || nodeNames == null)
      throw new IllegalArgumentException ("null argument");

    return getStreamMetadata();
  }

  /**
   * Read the given frame all at once, using default image read
   * parameters, and return a buffered image.
   *
   * The returned image will be formatted according to the
   * currently-preferred image type specifier.
   *
   * Installed read progress listeners, update progress listeners and
   * warning listeners will be notified of read progress, changes in
   * sample sets and warnings respectively.
   *
   * @param the index of the image frame to read
   *
   * @return a buffered image
   *
   * @exception IllegalStateException if input has not been set
   * @exception IndexOutOfBoundsException if the frame index is
   * out-of-bounds
   * @exception IOException if a read error occurs
   */
  public BufferedImage read (int imageIndex)
    throws IOException
  {
    return read (imageIndex, null);
  }

  /**
   * Read the given frame all at once, using the given image read
   * parameters, and return an IIOImage.  The IIOImage will contain a
   * buffered image as returned by getDestination.
   *
   * Installed read progress listeners, update progress listeners and
   * warning listeners will be notified of read progress, changes in
   * sample sets and warnings respectively.
   *
   * The source and destination band settings are checked with a call
   * to checkReadParamBandSettings.
   *
   * @param the index of the image frame to read
   * @param the image read parameters
   *
   * @return an IIOImage
   *
   * @exception IllegalStateException if input has not been set
   * @exception IndexOutOfBoundsException if the frame index is
   * out-of-bounds
   * @exception IllegalArgumentException if param.getSourceBands() and
   * param.getDestinationBands() are incompatible
   * @exception IllegalArgumentException if either the source or
   * destination image regions are empty
   * @exception IOException if a read error occurs
   */
  public IIOImage readAll (int imageIndex,
			   ImageReadParam param)
    throws IOException
  {
    checkReadParamBandSettings (param,
                                param.getSourceBands().length,
                                param.getDestinationBands().length);

    List l = new ArrayList ();

    for (int i = 0; i < getNumThumbnails (imageIndex); i++)
      l.add (readThumbnail(imageIndex, i));

    return new IIOImage (getDestination(param, getImageTypes(imageIndex),
                                        getWidth(imageIndex),
                                        getHeight(imageIndex)),
                         l,
                         getImageMetadata (imageIndex));
  }

  /**
   * Read all image frames all at once, using the given image read
   * parameters iterator, and return an iterator over a collection of
   * IIOImages.  Each IIOImage in the collection will contain a
   * buffered image as returned by getDestination.
   *
   * Installed read progress listeners, update progress listeners and
   * warning listeners will be notified of read progress, changes in
   * sample sets and warnings respectively.
   *
   * Each set of source and destination band settings are checked with
   * a call to checkReadParamBandSettings.
   *
   * @param an iterator over the image read parameters
   *
   * @return an IIOImage
   *
   * @exception IllegalStateException if input has not been set
   * @exception IllegalArgumentException if a non-ImageReadParam is
   * found in params
   * @exception IllegalArgumentException if param.getSourceBands() and
   * param.getDestinationBands() are incompatible
   * @exception IllegalArgumentException if either the source or
   * destination image regions are empty
   * @exception IOException if a read error occurs
   */
  public Iterator readAll (Iterator params)
    throws IOException
  {
    List l = new ArrayList ();
    int index = 0;

    while (params.hasNext())
      {
        if (params != null && ! (params instanceof ImageReadParam))
          throw new IllegalArgumentException ("non-ImageReadParam found");

        l.add (readAll(index++, (ImageReadParam) params.next ()));
      }

    return l.iterator();
  }

  /**
   * Read a rendered image.  This is a more general counterpart to
   * read (int, ImageReadParam).  All image data may not be read
   * before this method returns and so listeners will not necessarily
   * be notified.
   *
   * @param the index of the image frame to read
   * @param the image read parameters
   *
   * @return a rendered image
   *
   * @exception IllegalStateException if input is null
   * @exception IndexOutOfBoundsException if the frame index is
   * out-of-bounds
   * @exception IllegalArgumentException if param.getSourceBands() and
   * param.getDestinationBands() are incompatible
   * @exception IllegalArgumentException if either the source or
   * destination image regions are empty
   * @exception IOException if a read error occurs
   */
  public RenderedImage readAsRenderedImage (int imageIndex,
					    ImageReadParam param)
    throws IOException
  {
    return read (imageIndex, param);
  }

  /**
   * Read the given tile into a buffered image.  If the tile
   * coordinates are out-of-bounds an exception is thrown.  If the
   * image is not tiled then the coordinates 0, 0 are expected and the
   * entire image will be read.
   *
   * @param imageIndex the frame index
   * @param tileX the horizontal tile coordinate
   * @param tileY the vertical tile coordinate
   *
   * @return the contents of the tile as a buffered image
   *
   * @exception IllegalStateException if input is null
   * @exception IndexOutOfBoundsException if the frame index is
   * out-of-bounds
   * @exception IllegalArgumentException if the tile coordinates are
   * out-of-bounds
   * @exception IOException if a read error occurs
   */
  public BufferedImage readTile (int imageIndex, int tileX, int tileY)
    throws IOException
  {
    if (tileX != 0 || tileY != 0)
      throw new IllegalArgumentException ("tileX not 0 or tileY not 0");

    return read (imageIndex);
  }

  /**
   * Read the given tile into a raster containing the raw image data.
   * If the tile coordinates are out-of-bounds an exception is thrown.
   * If the image is not tiled then the coordinates 0, 0 are expected
   * and the entire image will be read.
   *
   * @param imageIndex the frame index
   * @param tileX the horizontal tile coordinate
   * @param tileY the vertical tile coordinate
   *
   * @return the contents of the tile as a raster
   *
   * @exception UnsupportedOperationException if rasters are not
   * supported
   * @exception IllegalStateException if input is null
   * @exception IndexOutOfBoundsException if the frame index is
   * out-of-bounds
   * @exception IllegalArgumentException if the tile coordinates are
   * out-of-bounds
   * @exception IOException if a read error occurs
   */
  public Raster readTileRaster (int imageIndex, int tileX, int tileY)
    throws IOException
  {
    if (!canReadRaster())
      throw new UnsupportedOperationException ("cannot read rasters");

    if (tileX != 0 || tileY != 0)
      throw new IllegalArgumentException ("tileX not 0 or tileY not 0");

    return readRaster (imageIndex, null);
  }

  /**
   * Reset this reader's internal state.
   */
  public void reset ()
  {
    setInput (null, false);
    setLocale (null);
    removeAllIIOReadUpdateListeners ();
    removeAllIIOReadWarningListeners ();
    removeAllIIOReadProgressListeners ();
    clearAbortRequest ();
  }
}

