/* ImageWriter.java -- Encodes raster images.
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

import java.awt.Dimension;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;

import javax.imageio.event.IIOWriteProgressListener;
import javax.imageio.event.IIOWriteWarningListener;
import javax.imageio.metadata.IIOMetadata;

import javax.imageio.spi.ImageWriterSpi;

public abstract class ImageWriter
  implements ImageTranscoder
{
  private boolean aborted;
  
  protected Locale[] availableLocales;
  protected Locale locale;
  protected ImageWriterSpi originatingProvider;
  protected Object output;
  protected List progressListeners = new ArrayList();
  protected List warningListeners = new ArrayList();
  protected List warningLocales = new ArrayList();

  protected ImageWriter(ImageWriterSpi originatingProvider)
  {
    this.originatingProvider = originatingProvider;
  }

  private void checkOutputSet()
  {
    if (output == null)
      throw new IllegalStateException("no output set");
  }
  
  public void abort()
  {
    aborted = true;
  }

  protected boolean abortRequested()
  {
    return aborted;
  }

  public void addIIOWriteProgressListener(IIOWriteProgressListener listener)
  {
    if (listener == null)
      return;
    
    progressListeners.add(listener);
  }
  
  public void addIIOWriteWarningListener (IIOWriteWarningListener listener)
  {
    if (listener == null)
      return;
    
    warningListeners.add(listener);
  }

  public boolean canInsertEmpty(int imageIndex)
    throws IOException
  {
    checkOutputSet();
    return false;
  }

  public boolean canInsertImage(int imageIndex)
    throws IOException
  {
    checkOutputSet();
    return false;
  }

  public boolean canRemoveImage(int imageIndex)
    throws IOException
  {
    checkOutputSet();
    return false;
  }

  public boolean canReplaceImageMetadata(int imageIndex)
    throws IOException
  {
    checkOutputSet();
    return false;
  }

  public boolean canReplacePixels(int imageIndex)
    throws IOException
  {
    checkOutputSet();
    return false;
  }

  public boolean canReplaceStreamMetadata()
    throws IOException
  {
    checkOutputSet();
    return false;
  }

  public boolean canWriteEmpty()
    throws IOException
  {
    checkOutputSet();
    return false;
  }

  public boolean canWriteRasters()
  {
    return false;
  }

  public boolean canWriteSequence()
  {
    return false;
  }

  protected void clearAbortRequest()
  {
    aborted = false;
  }
  
  public abstract IIOMetadata convertImageMetadata (IIOMetadata inData,
		                                    ImageTypeSpecifier imageType,
				                    ImageWriteParam param);

  public abstract IIOMetadata convertStreamMetadata (IIOMetadata inData,
					             ImageWriteParam param);

  public void dispose()
  {
    // The default implementation is empty. Subclasses have to overwrite it.
  }
  
  public Locale[] getAvailableLocales()
  {
    return availableLocales;
  }

  public abstract IIOMetadata getDefaultImageMetadata (ImageTypeSpecifier imageType, ImageWriteParam param);

  public abstract IIOMetadata getDefaultStreamMetadata (ImageWriteParam param);

  public ImageWriteParam getDefaultWriteParam()
  {
    return new ImageWriteParam(getLocale());
  }

  public Locale getLocale()
  {
    return locale;
  }

  public int getNumThumbnailsSupported (ImageTypeSpecifier imageType, ImageWriteParam param,
		                        IIOMetadata streamMetadata, IIOMetadata imageMetadata)
  {
    return 0;
  }

  public ImageWriterSpi getOriginatingProvider()
  {
    return originatingProvider;
  }

  public Object getOutput()
  {
    return output;
  }

  public Dimension[] getPreferredThumbnailSizes (ImageTypeSpecifier imageType,
		                                 ImageWriteParam param,
						 IIOMetadata streamMetadata,
						 IIOMetadata imageMetadata)
  {
    return null;
  }

  protected void processImageComplete()
  {
    Iterator it = progressListeners.iterator();

    while (it.hasNext())
      {
	IIOWriteProgressListener listener = (IIOWriteProgressListener) it.next();
	listener.imageComplete(this);
      }
  }

  protected void processImageProgress(float percentageDone)
  {
    Iterator it = progressListeners.iterator();

    while (it.hasNext())
      {
	IIOWriteProgressListener listener = (IIOWriteProgressListener) it.next();
	listener.imageProgress(this, percentageDone);
      }
  }

  protected void processImageStarted(int imageIndex)
  {
    Iterator it = progressListeners.iterator();

    while (it.hasNext())
      {
	IIOWriteProgressListener listener = (IIOWriteProgressListener) it.next();
	listener.imageStarted(this, imageIndex);
      }
  }

  protected void processThumbnailComplete()
  {
    Iterator it = progressListeners.iterator();

    while (it.hasNext())
      {
	IIOWriteProgressListener listener = (IIOWriteProgressListener) it.next();
	listener.thumbnailComplete(this);
      }
  }

  protected void processThumbnailProgress(float percentageDone)
  {
    Iterator it = progressListeners.iterator();

    while (it.hasNext())
      {
	IIOWriteProgressListener listener = (IIOWriteProgressListener) it.next();
	listener.thumbnailProgress(this, percentageDone);
      }
  }

  protected void processThumbnailStarted(int imageIndex, int thumbnailIndex)
  {
    Iterator it = progressListeners.iterator();

    while (it.hasNext())
      {
	IIOWriteProgressListener listener = (IIOWriteProgressListener) it.next();
	listener.thumbnailStarted(this, imageIndex, thumbnailIndex);
      }
  }

  protected void processWarningOccurred(int imageIndex, String warning)
  {
    Iterator it = warningListeners.iterator();

    while (it.hasNext())
      {
	IIOWriteWarningListener listener = (IIOWriteWarningListener) it.next();
	listener.warningOccurred(this, imageIndex, warning);
      }
  }

  protected void processWriteAborted() 
  {
    Iterator it = progressListeners.iterator();

    while (it.hasNext())
      {
	IIOWriteProgressListener listener = (IIOWriteProgressListener) it.next();
	listener.writeAborted(this);
      }
  }

  public void removeAllIIOWriteProgressListeners()
  {
    progressListeners.clear();
  }

  public void removeAllIIOWriteWarningListeners()
  {
    progressListeners.clear();
  }
  
  public void removeIIOWriteProgressListener (IIOWriteProgressListener listener) 
  {
    if (listener == null)
      return;
    
    progressListeners.remove(listener);
  }
  
  public void removeIIOWriteWarningListener (IIOWriteWarningListener listener)
  {
    if (listener == null)
      return;
    
    warningListeners.remove(listener);
  }
  
  public void reset()
  {
    setOutput(null);
    setLocale(null);
    removeAllIIOWriteWarningListeners();
    removeAllIIOWriteProgressListeners();
    clearAbortRequest();
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

  public abstract void write (IIOMetadata streamMetadata, IIOImage image, ImageWriteParam param)
    throws IOException;
}
