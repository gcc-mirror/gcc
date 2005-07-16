/* ImageReader.java -- Decodes raster images.
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

import java.awt.image.BufferedImage;
import java.awt.image.Raster;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;

import javax.imageio.event.IIOReadProgressListener;
import javax.imageio.event.IIOReadUpdateListener;
import javax.imageio.event.IIOReadWarningListener;
import javax.imageio.metadata.IIOMetadata;
import javax.imageio.spi.ImageReaderSpi;
import javax.imageio.stream.ImageInputStream;

public abstract class ImageReader
{
  private boolean aborted;
  
  protected Locale[] availableLocales;
  protected boolean ignoreMetadata;
  protected Object input;
  protected Locale locale;
  protected int minIndex;
  protected ImageReaderSpi originatingProvider;
  protected List progressListeners = new ArrayList();
  protected boolean seekForwardOnly;
  protected List updateListeners = new ArrayList();
  protected List warningListeners = new ArrayList();
  protected List warningLocales = new ArrayList();

  protected ImageReader(ImageReaderSpi originatingProvider)
  {
    this.originatingProvider = originatingProvider;
  }

  public void abort()
  {
    aborted = true;
  }

  protected boolean abortRequested()
  {
    return aborted;
  }

  public void addIIOReadProgressListener(IIOReadProgressListener listener)
  {
    if (listener == null)
      return;
    
    progressListeners.add(listener);    
  }

  public void addIIOReadUpdateListener(IIOReadUpdateListener listener)
  {
    if (listener == null)
      return;
    
    updateListeners.add(listener);    
  }
  
  public void addIIOReadWarningListener(IIOReadWarningListener listener)
  {
    if (listener == null)
      return;
    
    warningListeners.add(listener);    
  }

  public boolean canReadRaster()
  {
    return false;
  }

  protected void clearAbortRequest()
  {
    aborted = false;
  }
  
  public void dispose()
  {
    // The default implementation does nothing.
  }
  
  public float getAspectRatio(int imageIndex)
    throws IOException
  {
    return (float) (getWidth(imageIndex) / getHeight(imageIndex));
  }

  public Locale[] getAvailableLocales()
  {
    if (availableLocales == null)
      return null;
    
    return (Locale[]) availableLocales.clone();
  }

  public ImageReadParam getDefaultReadParam()
  {
    return new ImageReadParam();
  }

  public String getFormatName()
    throws IOException
  {
    return originatingProvider.getFormatNames()[0];
  }

  public abstract int getHeight(int imageIndex)
    throws IOException;

  public abstract IIOMetadata getImageMetadata(int imageIndex)
    throws IOException;

  public abstract Iterator getImageTypes(int imageIndex)
    throws IOException;

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

  public void setInput(Object in, boolean seekForwardOnly)
  {
    setInput(in, seekForwardOnly, false);
  }

  public void setInput(Object in)
  {
    setInput(in, false, false);
  }

  public Object getInput()
  {
    return input;
  }

  public Locale getLocale()
  {
    return locale;
  }

  public abstract int getNumImages(boolean allowSearch)
    throws IOException;

  public int getNumThumbnails(int imageIndex)
    throws IOException
  {
    return 0;
  }

  public ImageReaderSpi getOriginatingProvider()
  {
    return originatingProvider;
  }

  public abstract IIOMetadata getStreamMetadata()
    throws IOException;

  public int getThumbnailHeight(int imageIndex, int thumbnailIndex)
    throws IOException
  {
    return readThumbnail(imageIndex, thumbnailIndex).getHeight();
  }

  public int getThumbnailWidth(int imageIndex, int thumbnailIndex)
    throws IOException
  {
    return readThumbnail(imageIndex, thumbnailIndex).getWidth();
  }

  public int getTileGridXOffset(int imageIndex)
    throws IOException
  {
    return 0;
  }

  public int getTileGridYOffset(int imageIndex)
    throws IOException
  {
    return 0;
  }

  public int getTileHeight(int imageIndex)
    throws IOException
  {
    return getHeight(imageIndex);
  }

  public int getTileWidth(int imageIndex)
    throws IOException
  {
    return getWidth(imageIndex);
  }

  public abstract int getWidth(int imageIndex)
    throws IOException;

  public boolean hasThumbnails(int imageIndex)
    throws IOException
  {
    return getNumThumbnails(imageIndex) > 0;
  }

  public boolean isIgnoringMetadata()
  {
    return ignoreMetadata;
  }

  public boolean isImageTiled(int imageIndex)
    throws IOException
  {
    return false;
  }

  public boolean isRandomAccessEasy(int imageIndex)
    throws IOException
  {
    return false;
  }

  public boolean isSeekForwardOnly()
  {
    return seekForwardOnly;
  }

  protected void processImageComplete()
  {
    Iterator it = progressListeners.iterator();

    while (it.hasNext())
      {
	IIOReadProgressListener listener = (IIOReadProgressListener) it.next();
	listener.imageComplete (this);
      }
  }

  protected void processImageProgress(float percentageDone)
  {
    Iterator it = progressListeners.iterator();

    while (it.hasNext())
      {
	IIOReadProgressListener listener = (IIOReadProgressListener) it.next();
	listener.imageProgress(this, percentageDone);
      }
  }

  protected void processImageStarted(int imageIndex)
  {
    Iterator it = progressListeners.iterator();

    while (it.hasNext())
      {
	IIOReadProgressListener listener = (IIOReadProgressListener) it.next();
	listener.imageStarted(this, imageIndex);
      }
  }

  protected void processImageUpdate(BufferedImage image, int minX, int minY,
				    int width, int height, int periodX,
				    int periodY, int[] bands)
  {
    Iterator it = updateListeners.iterator();

    while (it.hasNext())
      {
	IIOReadUpdateListener listener = (IIOReadUpdateListener) it.next();
	listener.imageUpdate(this, image, minX, minY, width, height, periodX,
			     periodY, bands);
      }
  }

  protected void processPassComplete(BufferedImage image)
  {
    Iterator it = updateListeners.iterator();

    while (it.hasNext())
      {
	IIOReadUpdateListener listener = (IIOReadUpdateListener) it.next();
	listener.passComplete(this, image);
      }
  }

  protected void processPassStarted(BufferedImage image, int pass, int minPass,
				    int maxPass, int minX, int minY,
				    int periodX, int periodY, int[] bands)
  {
    Iterator it = updateListeners.iterator();

    while (it.hasNext())
      {
	IIOReadUpdateListener listener = (IIOReadUpdateListener) it.next();
	listener.passStarted(this, image, pass, minPass, maxPass, minX, minY,
			     periodX, periodY, bands);
      }
  }

  protected void processReadAborted()
  {
    Iterator it = progressListeners.iterator();

    while (it.hasNext())
      {
	IIOReadProgressListener listener = (IIOReadProgressListener) it.next();
	listener.readAborted(this);
      }
  }

  protected void processSequenceComplete()
  {
    Iterator it = progressListeners.iterator();

    while (it.hasNext())
      {
	IIOReadProgressListener listener = (IIOReadProgressListener) it.next();
	listener.sequenceComplete(this);
      }
  }

  protected void processSequenceStarted(int minIndex)
  {
    Iterator it = progressListeners.iterator();

    while (it.hasNext())
      {
	IIOReadProgressListener listener = (IIOReadProgressListener) it.next();
	listener.sequenceStarted(this, minIndex);
      }
  }

  protected void processThumbnailComplete()
  {
    Iterator it = progressListeners.iterator();

    while (it.hasNext())
      {
	IIOReadProgressListener listener = (IIOReadProgressListener) it.next();
	listener.thumbnailComplete(this);
      }
  }

  protected void processThumbnailPassComplete(BufferedImage thumbnail)
  {
    Iterator it = updateListeners.iterator();

    while (it.hasNext())
      {
	IIOReadUpdateListener listener = (IIOReadUpdateListener) it.next();
	listener.thumbnailPassComplete(this, thumbnail);
      }
  }

  protected void processThumbnailPassStarted(BufferedImage thumbnail, int pass,
					     int minPass, int maxPass, int minX,
					     int minY, int periodX, int periodY,
					     int[] bands)
  {
    Iterator it = updateListeners.iterator();

    while (it.hasNext())
      {
	IIOReadUpdateListener listener = (IIOReadUpdateListener) it.next();
	listener.thumbnailPassStarted(this, thumbnail, pass, minPass, maxPass,
				      minX, minY, periodX, periodY, bands);
      }
  }
  
  protected void processThumbnailProgress(float percentageDone)
  {
    Iterator it = progressListeners.iterator();

    while (it.hasNext())
      {
	IIOReadProgressListener listener = (IIOReadProgressListener) it.next();
	listener.thumbnailProgress(this, percentageDone);
      }
  }

  protected void processThumbnailStarted(int imageIndex, int thumbnailIndex)
  {
    Iterator it = progressListeners.iterator();

    while (it.hasNext())
      {
	IIOReadProgressListener listener = (IIOReadProgressListener) it.next();
	listener.thumbnailStarted(this, imageIndex, thumbnailIndex);
      }
  }

  protected void processThumbnailUpdate(BufferedImage image, int minX, int minY,
					int width, int height, int periodX,
					int periodY, int[] bands)
  {
    Iterator it = updateListeners.iterator();

    while (it.hasNext())
      {
	IIOReadUpdateListener listener = (IIOReadUpdateListener) it.next();
	listener.thumbnailUpdate(this, image, minX, minY, width, height,
				 periodX, periodY, bands);
      }
  }

  protected void processWarningOccurred(String warning)
  {
    Iterator it = warningListeners.iterator();

    while (it.hasNext())
      {
	IIOReadWarningListener listener = (IIOReadWarningListener) it.next();
	listener.warningOccurred(this, warning);
      }
  }

  public abstract BufferedImage read(int imageIndex, ImageReadParam param)
    throws IOException;

  public boolean readerSupportsThumbnails()
  {
    return false;
  }

  public Raster readRaster(int imageIndex, ImageReadParam param)
    throws IOException
  {
    throw new UnsupportedOperationException();
  }

  public BufferedImage readThumbnail(int imageIndex, int thumbnailIndex)
    throws IOException
  {
    throw new UnsupportedOperationException();
  }

  public void removeAllIIOReadProgressListeners()
  {
    progressListeners.clear();
  }

  public void removeAllIIOReadUpdateListeners()
  {
    updateListeners.clear();
  }

  public void removeAllIIOReadWarningListeners()
  {
    warningListeners.clear();
  }
  
  public void removeIIOReadProgressListener(IIOReadProgressListener listener) 
  {
    if (listener == null)
      return;
 
    progressListeners.remove(listener);
  }
  
  public void removeIIOReadUpdateListener(IIOReadUpdateListener listener) 
  {
    if (listener == null)
      return;
    
    updateListeners.remove(listener);
  }
  
  public void removeIIOReadWarningListener(IIOReadWarningListener listener)
  {
    if (listener == null)
      return;
    
    warningListeners.remove(listener);
  }
  
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
}
