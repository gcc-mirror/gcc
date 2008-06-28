/* ImageWriter.java -- Encodes raster images.
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

import java.awt.Dimension;
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

import javax.imageio.event.IIOWriteProgressListener;
import javax.imageio.event.IIOWriteWarningListener;
import javax.imageio.metadata.IIOMetadata;

import javax.imageio.spi.ImageWriterSpi;

/**
 * A class for encoding images within the ImageIO framework.
 *
 * An ImageWriter for a given format is instantiated by an
 * ImageWriterSpi for that format.  ImageWriterSpis are registered
 * with the IIORegistry.
 *
 * The ImageWriter API supports writing animated images that may have
 * multiple frames; to support such images many methods take an index
 * parameter.
 *
 * Images may also be written in multiple passes, where each
 * successive pass increases the level of detail in the destination
 * image.
 */
public abstract class ImageWriter
  implements ImageTranscoder
{
  private boolean aborted;
  
  /**
   * All locales available for localization of warning messages, or
   * null if localization is not supported.
   */
  protected Locale[] availableLocales = null;

  /**
   * The current locale used to localize warning messages, or null if
   * no locale has been set.
   */
  protected Locale locale = null;

  /**
   * The image writer SPI that instantiated this writer.
   */
  protected ImageWriterSpi originatingProvider = null;

  /**
   * An ImageInputStream to which image data is written.
   */
  protected Object output = null;

  /**
   * A list of installed progress listeners.  Initially null, meaning
   * no installed listeners.
   */
  protected List<IIOWriteProgressListener> progressListeners = null;

  /**
   * A list of installed warning listeners.  Initially null, meaning
   * no installed listeners.
   */
  protected List<IIOWriteWarningListener> warningListeners = null;

  /**
   * A list of warning locales corresponding with the list of
   * installed warning listeners.  Initially null, meaning no locales.
   */
  protected List<Locale> warningLocales = null;

  /**
   * Construct an image writer.
   *
   * @param originatingProvider the provider that is constructing this
   * image writer, or null
   */
  protected ImageWriter(ImageWriterSpi originatingProvider)
  {
    this.originatingProvider = originatingProvider;
  }

  /**
   * Throw an IllegalStateException if output is null.
   *
   * @exception IllegalStateException if output is null
   */
  private void checkOutputSet()
  {
    if (output == null)
      throw new IllegalStateException("no output set");
  }
  
  /**
   * Request that writing be aborted.  The unwritten portions of the
   * destination image will be undefined.
   *
   * Writers should clear the abort flag before starting a write
   * operation, then poll it periodically during the write operation.
   */
  public void abort()
  {
    aborted = true;
  }

  /**
   * Check if the abort flag is set.
   *
   * @return true if the current write operation should be aborted,
   * false otherwise
   */
  protected boolean abortRequested()
  {
    return aborted;
  }

  /**
   * Install a write progress listener.  This method will return
   * immediately if listener is null.
   *
   * @param listener a write progress listener or null
   */
  public void addIIOWriteProgressListener(IIOWriteProgressListener listener)
  {
    if (listener == null)
      return;
    if (progressListeners == null)
      progressListeners = new ArrayList ();
    progressListeners.add(listener);
  }

  /**
   * Install a write warning listener.  This method will return
   * immediately if listener is null.  Warning messages sent to this
   * listener will be localized using the current locale.  If the
   * current locale is null then this writer will select a sensible
   * default.
   *
   * @param listener a write warning listener
   */
  public void addIIOWriteWarningListener (IIOWriteWarningListener listener)
  {
    if (listener == null)
      return;
    if (warningListeners == null)
      warningListeners = new ArrayList ();
    warningListeners.add(listener);
  }

  /**
   * Check whether a new empty image can be inserted at the given
   * frame index.  Pixel values may be filled in later using the
   * replacePixels methods.  Indices greater than the insertion index
   * will be incremented.  If imageIndex is -1, the image will be
   * appended at the end of the current image list.
   *
   * @param imageIndex the frame index
   *
   * @return true if an empty image can be inserted at imageIndex,
   * false otherwise
   *
   * @exception IllegalStateException if output is null
   * @exception IndexOutOfBoundsException if imageIndex is less than
   * -1 or greater than the last index in the current image list
   * @exception IOException if a write error occurs
   */
  public boolean canInsertEmpty(int imageIndex)
    throws IOException
  {
    checkOutputSet();
    return false;
  }

  /**
   * Check whether an image can be inserted at the given frame index.
   * Indices greater than the insertion index will be incremented.  If
   * imageIndex is -1, the image will be appended at the end of the
   * current image list.
   *
   * @param imageIndex the frame index
   *
   * @return true if an image can be inserted at imageIndex, false
   * otherwise
   *
   * @exception IllegalStateException if output is null
   * @exception IndexOutOfBoundsException if imageIndex is less than
   * -1 or greater than the last index in the current image list
   * @exception IOException if a write error occurs
   */
  public boolean canInsertImage(int imageIndex)
    throws IOException
  {
    checkOutputSet();
    return false;
  }

  /**
   * Check whether an image can be removed from the given frame index.
   * Indices greater than the removal index will be decremented.
   *
   * @param imageIndex the frame index
   *
   * @return true if an image can be removed from imageIndex, false
   * otherwise
   *
   * @exception IllegalStateException if output is null
   * @exception IndexOutOfBoundsException if imageIndex is less than 0
   * or greater than the last index in the current image list
   * @exception IOException if a write error occurs
   */
  public boolean canRemoveImage(int imageIndex)
    throws IOException
  {
    checkOutputSet();
    return false;
  }

  /**
   * Check whether the metadata associated the image at the given
   * frame index can be replaced.
   *
   * @param imageIndex the frame index
   *
   * @return true if the metadata associated with the image at
   * imageIndex can be replaced, false otherwise
   *
   * @exception IllegalStateException if output is null
   * @exception IndexOutOfBoundsException if imageIndex is less than 0
   * or greater than the last index in the current image list
   * @exception IOException if a write error occurs
   */
  public boolean canReplaceImageMetadata(int imageIndex)
    throws IOException
  {
    checkOutputSet();
    return false;
  }

  /**
   * Check whether the pixels within the image at the given index can
   * be replaced.
   *
   * @param imageIndex the frame index
   *
   * @return true if the pixels in the image at imageIndex can be
   * replaced, false otherwise
   *
   * @exception IllegalStateException if output is null
   * @exception IndexOutOfBoundsException if imageIndex is less than 0
   * or greater than the last index in the current image list
   * @exception IOException if a write error occurs
   */
  public boolean canReplacePixels(int imageIndex)
    throws IOException
  {
    checkOutputSet();
    return false;
  }

  /**
   * Check whether the metadata associated the entire image stream can
   * be replaced.
   *
   * @return true if the stream metadata can be replaced, false
   * otherwise
   *
   * @exception IllegalStateException if output is null
   * @exception IOException if a write error occurs
   */
  public boolean canReplaceStreamMetadata()
    throws IOException
  {
    checkOutputSet();
    return false;
  }

  /**
   * Check whether an entire empty image, including empty metadata and
   * empty thumbnails, can be written to the output stream, leaving
   * pixel values to be filled in later using the replacePixels
   * methods.
   *
   * @return true if an entire empty image can be written before its
   * contents are filled in, false otherwise
   *
   * @exception IllegalStateException if output is null
   * @exception IOException if a write error occurs
   */
  public boolean canWriteEmpty()
    throws IOException
  {
    checkOutputSet();
    return false;
  }

  /**
   * Check if IIOImages containing raster data are supported.
   *
   * @return true if raster IIOImages are supported, false otherwise
   */
  public boolean canWriteRasters()
  {
    return false;
  }

  /**
   * Check if an image can be appended at the end of the current list
   * of images even if prior images have already been written.
   *
   * @return true if sequences of images can be written, false
   * otherwise
   */
  public boolean canWriteSequence()
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
   * Convert IIOMetadata from an input reader format, returning an
   * IIOMetadata suitable for use by an image writer.
   *
   * The ImageTypeSpecifier specifies the destination image type.
   *
   * An optional ImageWriteParam argument is available in case the
   * image writing parameters affect the metadata conversion.
   *
   * @param inData the metadata coming from an image reader
   * @param imageType the output image type of the writer
   * @param param the image writing parameters or null
   *
   * @return the converted metadata that should be used by the image
   * writer, or null if this ImageTranscoder has no knowledge of the
   * input metadata
   *
   * @exception IllegalArgumentException if either inData or imageType
   * is null
   */
  public abstract IIOMetadata convertImageMetadata (IIOMetadata inData,
		                                    ImageTypeSpecifier imageType,
				                    ImageWriteParam param);

  /**
   * Convert IIOMetadata from an input stream format, returning an
   * IIOMetadata suitable for use by an image writer.
   *
   * An optional ImageWriteParam argument is available in case the
   * image writing parameters affect the metadata conversion.
   *
   * @param inData the metadata coming from an input image stream
   * @param param the image writing parameters or null
   *
   * @return the converted metadata that should be used by the image
   * writer, or null if this ImageTranscoder has no knowledge of the
   * input metadata
   *
   * @exception IllegalArgumentException if inData is null
   */
  public abstract IIOMetadata convertStreamMetadata (IIOMetadata inData,
					             ImageWriteParam param);

  /**
   * Releases any resources allocated to this object.  Subsequent
   * calls to methods on this object will produce undefined results.
   *
   * The default implementation does nothing; subclasses should use
   * this method ensure that native resources are released.
   */
  public void dispose()
  {
    // The default implementation is empty. Subclasses have to overwrite it.
  }
  
  /**
   * Retrieve the available locales.  Return null if no locales are
   * available or a clone of availableLocales.
   *
   * @return an array of locales or null
   */
  public Locale[] getAvailableLocales()
  {
    return availableLocales;
  }

  /**
   * Get a metadata object appropriate for encoding an image specified
   * by the given image type specifier and optional image write
   * parameters.
   *
   * @param imageType an image type specifier
   * @param param image writing parameters, or null
   *
   * @return a metadata object appropriate for encoding an image of
   * the given type with the given parameters
   */
  public abstract IIOMetadata getDefaultImageMetadata (ImageTypeSpecifier imageType, ImageWriteParam param);

  /**
   * Get a metadata object appropriate for encoding the default image
   * type handled by this writer, optionally considering image write
   * parameters.
   *
   * @param param image writing parameters, or null
   *
   * @return a metadata object appropriate for encoding an image of
   * the default type with the given parameters
   */
  public abstract IIOMetadata getDefaultStreamMetadata (ImageWriteParam param);

  /**
   * Retrieve the default write parameters for this writer's image
   * format.
   *
   * The default implementation returns new ImageWriteParam().
   *
   * @return image writing parameters
   */
  public ImageWriteParam getDefaultWriteParam()
  {
    return new ImageWriteParam(getLocale());
  }

  /**
   * Get this writer's locale.  null is returned if the locale has not
   * been set.
   *
   * @return this writer's locale, or null
   */
  public Locale getLocale()
  {
    return locale;
  }

  /**
   * Get the number of thumbnails supported by this image writer,
   * based on the given image type, image writing parameters, and
   * stream and image metadata.  The image writing parameters are
   * optional, in case they affect the number of thumbnails supported.
   *
   * @param imageType an image type specifier, or null
   * @param param image writing parameters, or null
   * @param streamMetadata the metadata associated with this stream,
   * or null
   * @param imageMetadata the metadata associated with this image, or
   * null
   *
   * @return the number of thumbnails that this writer supports
   * writing or -1 if the given information is insufficient
   */
  public int getNumThumbnailsSupported (ImageTypeSpecifier imageType,
                                        ImageWriteParam param,
		                        IIOMetadata streamMetadata,
                                        IIOMetadata imageMetadata)
  {
    return 0;
  }

  /**
   * Get the ImageWriterSpi that created this writer or null.
   *
   * @return an ImageWriterSpi, or null
   */
  public ImageWriterSpi getOriginatingProvider()
  {
    return originatingProvider;
  }

  /**
   * Get this reader's image output destination.  null is returned if
   * the image destination has not been set.
   *
   * @return an image output destination object, or null
   */
  public Object getOutput()
  {
    return output;
  }

  /**
   * Get the preferred sizes for thumbnails based on the given image
   * type, image writing parameters, and stream and image metadata.
   * The preferred sizes are returned in pairs of dimension values;
   * the first value in the array is a dimension object representing
   * the minimum thumbnail size, the second value is a dimension
   * object representing a maximum thumbnail size.  The writer can
   * select a size within the range given by each pair, or it can
   * ignore these size hints.
   *
   * @param imageType an image type specifier, or null
   * @param param image writing parameters, or null
   * @param streamMetadata the metadata associated with this stream,
   * or null
   * @param imageMetadata the metadata associated with this image, or
   * null
   *
   * @return an array of dimension pairs whose length is a multiple of
   * 2, or null if there is no preferred size (any size is allowed) or
   * if the size is unknown (insufficient information was provided)
   */
  public Dimension[] getPreferredThumbnailSizes (ImageTypeSpecifier imageType,
		                                 ImageWriteParam param,
						 IIOMetadata streamMetadata,
						 IIOMetadata imageMetadata)
  {
    return null;
  }

  /**
   * Notifies all installed write progress listeners that image
   * loading has completed by calling their imageComplete methods.
   */
  protected void processImageComplete()
  {
    if (progressListeners != null)
      {
	Iterator it = progressListeners.iterator();

	while (it.hasNext())
	  {
	    IIOWriteProgressListener listener =
	      (IIOWriteProgressListener) it.next();
	    listener.imageComplete(this);
	  }
      }
  }

  /**
   * Notifies all installed write progress listeners that a certain
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
	    IIOWriteProgressListener listener =
	      (IIOWriteProgressListener) it.next();
	    listener.imageProgress(this, percentageDone);
	  }
      }
  }

  /**
   * Notifies all installed write progress listeners, by calling their
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
	    IIOWriteProgressListener listener =
	      (IIOWriteProgressListener) it.next();
	    listener.imageStarted(this, imageIndex);
	  }
      }
  }

  /**
   * Notifies all installed write progress listeners, by calling their
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
	    IIOWriteProgressListener listener =
	      (IIOWriteProgressListener) it.next();
	    listener.thumbnailComplete(this);
	  }
      }
  }

  /**
   * Notifies all installed write progress listeners that a certain
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
	    IIOWriteProgressListener listener =
	      (IIOWriteProgressListener) it.next();
	    listener.thumbnailProgress(this, percentageDone);
	  }
      }
  }

  /**
   * Notifies all installed write progress listeners, by calling their
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
	    IIOWriteProgressListener listener =
	      (IIOWriteProgressListener) it.next();
	    listener.thumbnailStarted(this, imageIndex, thumbnailIndex);
	  }
      }
  }

  /**
   * Notifies all installed warning listeners, by calling their
   * warningOccurred methods, that a warning message has been raised.
   *
   * @param imageIndex the index of the image that was being written
   * when the warning was raised
   * @param warning the warning message
   *
   * @exception IllegalArgumentException if warning is null
   */
  protected void processWarningOccurred(int imageIndex, String warning)
  {
     if (warningListeners != null)
      {
	Iterator it = warningListeners.iterator();

	while (it.hasNext())
	  {
	    IIOWriteWarningListener listener =
	      (IIOWriteWarningListener) it.next();
	    listener.warningOccurred(this, imageIndex, warning);
	  }
      }
  }

  /**
   * Notify all installed warning listeners, by calling their
   * warningOccurred methods, that a warning message has been raised.
   * The warning message is retrieved from a resource bundle, using
   * the given basename and keyword.
   *
   * @param imageIndex the index of the image that was being written
   * when the warning was raised
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
  protected void processWarningOccurred(int imageIndex,
					String baseName,
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
	    IIOWriteWarningListener listener =
	      (IIOWriteWarningListener) it.next();
	    listener.warningOccurred(this, imageIndex, warning);
	  }
      }
  }

  /**
   * Notifies all installed write progress listeners that image
   * loading has been aborted by calling their writeAborted methods.
   */
  protected void processWriteAborted() 
  {
    if (progressListeners != null)
      {
	Iterator it = progressListeners.iterator();

	while (it.hasNext())
	  {
	    IIOWriteProgressListener listener =
	      (IIOWriteProgressListener) it.next();
	    listener.writeAborted(this);
	  }
      }
  }

  /**
   * Uninstall all write progress listeners.
   */
  public void removeAllIIOWriteProgressListeners()
  {
    if (progressListeners != null)
      {
	progressListeners.clear();
      }
  }

  /**
   * Uninstall all write warning listeners.
   */
  public void removeAllIIOWriteWarningListeners()
  {
    if (progressListeners != null)
      {
	progressListeners.clear();
      }
  }

  /**
   * Uninstall the given write progress listener.
   *
   * @param listener the listener to remove
   */
  public void removeIIOWriteProgressListener (IIOWriteProgressListener listener)
  {
    if (listener == null)
      return;
    if (progressListeners != null)
      {
	progressListeners.remove(listener);
      }
  }
  /**
   * Uninstall the given write warning listener.
   *
   * @param listener the listener to remove
   */
  public void removeIIOWriteWarningListener (IIOWriteWarningListener listener)
  {
    if (listener == null)
      return;
    if (warningListeners != null)
      {
	warningListeners.remove(listener);
      }
  }
  /**
   * Reset this writer's internal state.
   */
  public void reset()
  {
    setOutput(null);
    setLocale(null);
    removeAllIIOWriteWarningListeners();
    removeAllIIOWriteProgressListeners();
    clearAbortRequest();
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
   * Set the output destination of the given object.  The output
   * destination must be set before many methods can be called on this
   * writer. (see all ImageWriter methods that throw
   * IllegalStateException).  If input is null then the current input
   * source will be removed.
   *
   * @param output the output destination object
   *
   * @exception IllegalArgumentException if input is not a valid input
   * source for this writer and is not an ImageInputStream
   */
  public void setOutput(Object output)
  {
    if (output != null)
      {
	// Check if its a valid output object.
	boolean found = false;
	Class[] types = null;

	if (originatingProvider != null)
	  types = originatingProvider.getOutputTypes();
        
	if (types != null)
	  for (int i = types.length - 1; i >= 0; --i)
            if (types[i].isInstance(output))
              found = true;

	if (! found)
	  throw new IllegalArgumentException("output type not available");
      }

    this.output = output;
  }

  /**
   * Write an image stream, including thumbnails and metadata to the
   * output stream.  The output must have been set prior to this
   * method being called.  Metadata associated with the stream may be
   * supplied, or it can be left null.  IIOImage may contain raster
   * data if this writer supports rasters, or it will contain a
   * rendered image.  Thumbnails are resized if need be.  Image
   * writing parameters may be specified to affect writing, or may be
   * left null.
   *
   * @param streamMetadata metadata associated with this stream, or
   * null
   * @param image an IIOImage containing image data, metadata and
   * thumbnails to be written
   * @param param image writing parameters, or null
   *
   * @exception IllegalStateException if output is null
   * @exception UnsupportedOperationException if image contains raster
   * data but this writer does not support rasters
   * @exception IllegalArgumentException if image is null
   * @exception IOException if a write error occurs
   */
  public abstract void write (IIOMetadata streamMetadata, IIOImage image, ImageWriteParam param)
    throws IOException;

  /**
   * Complete inserting an empty image in the output stream.
   *
   * @exception IllegalStateException if output is null
   * @exception UnsupportedOperationException if inserting empty
   * images is not supported
   * @exception IllegalArgumentException if a call to
   * prepareInsertEmpty was not called previous to this method being
   * called (a sequence of prepareInsertEmpty calls must be terminated
   * by a call to endInsertEmpty)
   * @exception IllegalArgumentException if prepareWriteEmpty was
   * called before this method being called (without a terminating
   * call to endWriteEmpty)
   * @exception IllegalArgumentException if prepareReplacePixels was
   * called before this method being called (without a terminating
   * call to endReplacePixels)
   * @exception IOException if a write error occurs
   */
  public void endInsertEmpty ()
    throws IOException
  {
    if (!canInsertEmpty(0))
      throw new UnsupportedOperationException();
  }

  /**
   * Complete replacing pixels in an image in the output stream.
   *
   * @exception IllegalStateException if output is null
   * @exception UnsupportedOperationException if replacing pixels is
   * not supported by this writer
   * @exception IllegalArgumentException if prepareReplacePixels was
   * not called before this method being called
   * @exception IOException if a write error occurs
   */
  public void endReplacePixels ()
    throws IOException
  {
    if (!canReplacePixels(0))
      throw new UnsupportedOperationException();
  }

  /**
   * Complete writing an empty image to the image output stream.
   *
   * @exception IllegalStateException if output is null
   * @exception UnsupportedOperationException if writing empty images
   * is not supported
   * @exception IllegalArgumentException if a call to
   * prepareWriteEmpty was not called previous to this method being
   * called (a sequence of prepareWriteEmpty calls must be terminated
   * by a call to endWriteEmpty)
   * @exception IllegalArgumentException if prepareInsertEmpty was
   * called before this method being called (without a terminating
   * call to endInsertEmpty)
   * @exception IllegalArgumentException if prepareReplacePixels was
   * called before this method being called (without a terminating
   * call to endReplacePixels)
   * @exception IOException if a write error occurs
   */
  public void endWriteEmpty ()
    throws IOException
  {
    if (!canWriteEmpty())
      throw new UnsupportedOperationException();
  }

  /**
   * Complete writing a sequence of images to the output stream.  This
   * method may patch header data and write out footer data.
   *
   * @exception IllegalStateException if output is null
   * @exception IllegalStateException if prepareWriteSequence has not
   * been called
   * @exception UnsupportedOperationException if writing a sequence of
   * images is not supported
   * @exception IOException if a write error occurs
   */
  public void endWriteSequence ()
    throws IOException
  {
    checkOutputSet();
    if (!canWriteSequence())
      throw new UnsupportedOperationException();
  }

  /**
   * Start inserting an empty image in the image output stream.  All
   * indices after the specified index are incremented.  An index of
   * -1 implies that the empty image should be appended to the end of
   * the current image list.
   *
   * The insertion that this method call starts is not complete until
   * endInsertEmpty is called.  prepareInsertEmpty cannot be called
   * again until endInsertEmpty is called and calls to
   * prepareWriteEmpty and prepareInsertEmpty may not be intersperced.
   *
   * @param imageIndex the image index
   * @param imageType the image type specifier
   * @param width the image width
   * @param height the image height
   * @param imageMetadata the image metadata, or null
   * @param thumbnails a list of thumbnails, or null
   * @param param image write parameters, or null
   *
   * @exception IllegalStateException if output is null
   * @exception UnsupportedOperationException if inserting empty
   * images is not supported
   * @exception IndexOutOfBoundsException if imageIndex is less than
   * -1 or greater than the last index in the current image list
   * @exception IllegalStateException if a previous call to
   * prepareInsertEmpty was made (without a terminating call to
   * endInsertEmpty)
   * @exception IllegalStateException if a previous call to
   * prepareWriteEmpty was made (without a terminating call to
   * endWriteEmpty)
   * @exception IllegalArgumentException if imageType is null or
   * thumbnails contain non-BufferedImage objects
   * @exception IllegalArgumentException if either width or height is
   * less than 1
   * @exception IOException if a write error occurs
   */
  public void prepareInsertEmpty (int imageIndex, ImageTypeSpecifier imageType,
                                  int width, int height,
                                  IIOMetadata imageMetadata,
                                  List<? extends BufferedImage> thumbnails,
                                  ImageWriteParam param)
    throws IOException
  {
    if (!canInsertEmpty(imageIndex))
      throw new UnsupportedOperationException();
  }

  /**
   * Start the replacement of pixels within an image in the output
   * stream.  Output pixels will be clipped to lie within region.
   *
   * @param imageIndex the index of the image in which pixels are
   * being replaced
   * @param region the rectangle to which to limit pixel replacement
   *
   * @exception IllegalStateException if output is null
   * @exception UnsupportedOperationException if replacing pixels is
   * not supported
   * @exception IndexOutOfBoundsException if imageIndex is less than 0
   * or greater than the last index in the current image list
   * @exception IllegalStateException if a previous call to
   * prepareReplacePixels was made (without a terminating call to
   * endReplacePixels)
   * @exception IllegalArgumentException if either region.width or
   * region.height is less than 1, or if region is null
   * @exception IOException if a write error occurs
   */
  public void prepareReplacePixels (int imageIndex, Rectangle region)
    throws IOException
  {
    if (canReplacePixels(imageIndex))
      throw new UnsupportedOperationException();
  }

  /**
   * Start writing an empty image to the end of the image output
   * stream.
   *
   * The writing that this method call starts is not complete until
   * endWriteEmpty is called.  prepareWritetEmpty cannot be called
   * again until endWriteEmpty is called and calls to
   * prepareWriteEmpty and prepareInsertEmpty may not be intersperced.
   *
   * @param streamMetadata metadata associated with the stream, or null
   * @param imageType the image type specifier
   * @param width the image width
   * @param height the image height
   * @param imageMetadata the image metadata, or null
   * @param thumbnails a list of thumbnails, or null
   * @param param image write parameters, or null
   *
   * @exception IllegalStateException if output is null
   * @exception UnsupportedOperationException if writing empty images
   * is not supported
   * @exception IndexOutOfBoundsException if imageIndex is less than
   * -1 or greater than the last index in the current image list
   * @exception IllegalStateException if a previous call to
   * prepareInsertEmpty was made (without a terminating call to
   * endInsertEmpty)
   * @exception IllegalStateException if a previous call to
   * prepareWriteEmpty was made (without a terminating call to
   * endWriteEmpty)
   * @exception IllegalArgumentException if imageType is null or
   * thumbnails contain non-BufferedImage objects
   * @exception IllegalArgumentException if either width or height is
   * less than 1
   * @exception IOException if a write error occurs
   */
  public void prepareWriteEmpty (IIOMetadata streamMetadata,
                                 ImageTypeSpecifier imageType,
                                 int width, int height,
                                 IIOMetadata imageMetadata,
                                 List<? extends BufferedImage> thumbnails,
                                 ImageWriteParam param)
    throws IOException
  {
    if (!canWriteEmpty())
      throw new UnsupportedOperationException();
  }

  /**
   * Start the writing of a sequence of images.
   *
   * @param streamMetadata the stream metadata, or null
   *
   * @exception IllegalStateException if output is null
   * @exception UnsupportedOperationException if writing sequences of
   * images is not supported
   * @exception IOException if a write error occurs
   */
  public void prepareWriteSequence (IIOMetadata streamMetadata)
    throws IOException
  {
    checkOutputSet();
    if (!canWriteSequence())
      throw new UnsupportedOperationException();
  }

  /**
   * Remove the image at the specified index from the output stream.
   *
   * @param imageIndex the frame index from which to remove the image
   *
   * @exception IllegalStateException if output is null
   * @exception UnsupportedOperationException if removing this image
   * is not supported
   * @exception IndexOutOfBoundsException if imageIndex is less than 0
   * or greater than the last index in the current image list
   * @exception IOException if a write error occurs
   */
  public void removeImage (int imageIndex)
    throws IOException
  {
    if (!canRemoveImage(imageIndex))
      throw new UnsupportedOperationException();
  }

  /**
   * Replace the metadata associated with the image at the given
   * index.
   *
   * @param imageIndex the index of the image whose metadata should be
   * replaced
   * @param imageMetadata the metadata, or null
   *
   * @exception IllegalStateException if output is null
   * @exception UnsupportedOperationException if replacing this
   * image's metadata is not supported
   * @exception IndexOutOfBoundsException if imageIndex is less than 0
   * or greater than the last index in the current image list
   * @exception IOException if a write error occurs
   */
  public void replaceImageMetadata (int imageIndex, IIOMetadata imageMetadata)
    throws IOException
  {
    if (!canReplaceImageMetadata(imageIndex))
      throw new UnsupportedOperationException();
  }

  /**
   * Replace a region of an image in the output stream with a portion
   * of the given rendered image.  The image data must be of the same
   * type as that in the output stream.  The destination region is
   * given by the image writing parameters and the source region is
   * the one given to prepareReplacePixels.
   *
   * @param image the rendered image with which to overwrite the image
   * region in the stream
   * @param param the image writing parameters
   *
   * @exception IllegalStateException if output is null
   * @exception UnsupportedOperationException if replacing pixels is
   * not supported
   * @exception IllegalStateException if prepareReplacePixels was not
   * called before this method was called
   * @exception IllegalArgumentException if image is null or if param
   * is null or if the overlap of the source and destination regions
   * contains no pixels or if the image types differ and no conversion
   * is possible
   * @exception IOException if a write error occurs
   */
  public void replacePixels (RenderedImage image,
                             ImageWriteParam param)
    throws IOException
  {
    if (!canReplacePixels(0))
      throw new UnsupportedOperationException();
  }

  /**
   * Replace a region of an image in the output stream with a portion
   * of the given raster data.  The image data must be of the same
   * type as that in the output stream.  The destination region is
   * given by the image writing parameters and the source region is
   * the one given to prepareReplacePixels.
   *
   * @param raster the raster data with which to overwrite the image
   * region in the stream
   * @param param the image writing parameters
   *
   * @exception IllegalStateException if output is null
   * @exception UnsupportedOperationException if replacing pixels is
   * not supported
   * @exception IllegalStateException if prepareReplacePixels was not
   * called before this method was called
   * @exception UnsupportedOperationException if raster data is not
   * supported
   * @exception IllegalArgumentException if raster is null or if param
   * is null or if the overlap of the source and destination regions
   * contains no pixels or if the image types differ and no conversion
   * is possible
   * @exception IOException if a write error occurs
   */
  public void replacePixels (Raster raster, ImageWriteParam param)
    throws IOException
  {
    if (!canReplacePixels(0))
    throw new UnsupportedOperationException();
  }

  /**
   * Replace the metadata associated with this image stream.
   *
   * @param streamMetadata the stream metadata, or null
   *
   * @exception IllegalStateException if output is null
   * @exception UnsupportedOperationException if replacing the stream
   * metadata is not supported
   * @exception IOException if a write error occurs
   */
  public void replaceStreamMetadata (IIOMetadata streamMetadata)
    throws IOException
  {
    if (!canReplaceStreamMetadata())
      throw new UnsupportedOperationException();
  }

  /**
   * Write a rendered image to the output stream.
   *
   * @param image a rendered image containing image data to be written
   *
   * @exception IllegalStateException if output is null
   * @exception IllegalArgumentException if image is null
   * @exception IOException if a write error occurs
   */
  public void write (RenderedImage image)
    throws IOException
  {
    checkOutputSet();
    write (null, new IIOImage(image, null, null), null);
  }

  /**
   * Write a image data, metadata and thumbnails to the output stream.
   *
   * @param image image data, metadata and thumbnails to be written
   *
   * @exception IllegalStateException if output is null
   * @exception UnsupportedOperationException if image contains raster
   * data but this writer does not support rasters
   * @exception IllegalArgumentException if image is null
   * @exception IOException if a write error occurs
   */
  public void write (IIOImage image)
    throws IOException
  {
    checkOutputSet();
    write (null, image, null);
  }

  /**
   * Insert an image into the output stream.  Indices greater than the
   * specified index are incremented accordingly.  Specifying an index
   * of -1 causes the image to be appended at the end of the current
   * image list.
   *
   * @param imageIndex the frame index at which to insert the image
   * @param image the image data, metadata and thumbnails to be
   * inserted
   * @param param image write parameters, or null
   *
   * @exception IllegalStateException if output is null
   * @exception UnsupportedOperationException if image insertion is
   * not supported
   * @exception IllegalArgumentException if image is null
   * @exception IndexOutOfBoundsException if imageIndex is less than
   * -1 or greater than the last index in the current image list
   * @exception UnsupportedOperationException if image contains raster
   * data but this writer does not support rasters
   * @exception IOException if a write error occurs
   */
  public void writeInsert (int imageIndex, IIOImage image, ImageWriteParam param)
    throws IOException
  {
    if (!canInsertImage(imageIndex))
      throw new UnsupportedOperationException();
  }

  /**
   * Write a sequence of images, including thumbnails and metadata, to
   * the output stream.  The output must have been set prior to this
   * method being called.  Metadata associated with the stream may be
   * supplied, or it can be left null.  IIOImage may contain raster
   * data if this writer supports rasters, or it will contain a
   * rendered image.  Thumbnails are resized if need be.  Image
   * writing parameters may be specified to affect writing, or may be
   * left null.
   *
   * @param streamMetadata metadata associated with this stream, or
   * null
   * @param image an IIOImage containing image data, metadata and
   * thumbnails to be written
   * @param param image writing parameters, or null
   *
   * @exception IllegalStateException if output is null
   * @exception UnsupportedOperationException if writing sequences of
   * images is not supported
   * @exception IllegalArgumentException if image is null
   * @exception UnsupportedOperationException if image contains raster
   * data but this writer does not support rasters
   * @exception IOException if a write error occurs
   */
  public void writeToSequence (IIOImage image, ImageWriteParam param)
    throws IOException
  {
    if (!canWriteSequence())
      throw new UnsupportedOperationException();
  }
}
