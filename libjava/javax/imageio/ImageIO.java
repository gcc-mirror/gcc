/* ImageIO.java --
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
import java.awt.image.RenderedImage;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;

import javax.imageio.spi.IIORegistry;
import javax.imageio.spi.ImageReaderSpi;
import javax.imageio.spi.ImageWriterSpi;
import javax.imageio.spi.ServiceRegistry;
import javax.imageio.stream.ImageInputStream;
import javax.imageio.stream.ImageOutputStream;
import javax.imageio.stream.MemoryCacheImageInputStream;
import javax.imageio.stream.MemoryCacheImageOutputStream;

public final class ImageIO
{
  /**
   * This class isn't intended to be instantiated.
   */
  private ImageIO() {}

  private static final class ReaderFormatFilter implements ServiceRegistry.Filter
  {
    private String formatName;

    public ReaderFormatFilter(String formatName)
    {
      this.formatName = formatName;
    }

    public boolean filter (Object provider)
    {
      if (provider instanceof ImageReaderSpi)
        {
          ImageReaderSpi spi = (ImageReaderSpi) provider;
          String[] formatNames = spi.getFormatNames();

          for (int i = formatNames.length - 1; i >= 0; --i)
            if (formatName.equals(formatNames[i]))
              return true;
        }

      return false;
    }
  }

  private static final class ReaderMIMETypeFilter implements ServiceRegistry.Filter
  {
    private String MIMEType;

    public ReaderMIMETypeFilter(String MIMEType)
    {
      this.MIMEType = MIMEType;
    }

    public boolean filter(Object provider)
    {
      if (provider instanceof ImageReaderSpi)
        {
          ImageReaderSpi spi = (ImageReaderSpi) provider;
          String[] mimetypes = spi.getMIMETypes();

          for (int i = mimetypes.length - 1; i >= 0; --i)
            if (MIMEType.equals(mimetypes[i]))
              return true;
        }

      return false;
    }
  }
  
  private static final class ReaderSuffixFilter implements ServiceRegistry.Filter
  {
    private String fileSuffix;

    public ReaderSuffixFilter(String fileSuffix)
    {
      this.fileSuffix = fileSuffix;
    }

    public boolean filter(Object provider)
    {
      if (provider instanceof ImageReaderSpi)
        {
          ImageReaderSpi spi = (ImageReaderSpi) provider;
          String[] suffixes = spi.getFileSuffixes();

          for (int i = suffixes.length - 1; i >= 0; --i)
            if (fileSuffix.equals(suffixes[i]))
              return true;
        }

      return false;
    }
  }
  
  private static final class WriterFormatFilter implements ServiceRegistry.Filter
  {
    private String formatName;

    public WriterFormatFilter(String formatName)
    {
      this.formatName = formatName;
    }

    public boolean filter(Object provider)
    {
      if (provider instanceof ImageWriterSpi)
	{
	  ImageWriterSpi spi = (ImageWriterSpi) provider;
	  String[] formatNames = spi.getFormatNames();
	  
	  for (int i = formatNames.length - 1; i >= 0; --i)
            if (formatName.equals(formatNames[i]))
              return true;
	}

      return false;
    }
  }

  private static final class WriterMIMETypeFilter implements ServiceRegistry.Filter
  {
    private String MIMEType;

    public WriterMIMETypeFilter(String MIMEType)
    {
      this.MIMEType = MIMEType;
    }

    public boolean filter(Object provider)
    {
      if (provider instanceof ImageWriterSpi)
        {
          ImageWriterSpi spi = (ImageWriterSpi) provider;
          String[] mimetypes = spi.getMIMETypes();

          for (int i = mimetypes.length - 1; i >= 0; --i)
            if (MIMEType.equals(mimetypes[i]))
              return true;
        }

      return false;
    }
  }
  
  private static final class WriterSuffixFilter implements ServiceRegistry.Filter
  {
    private String fileSuffix;

    public WriterSuffixFilter(String fileSuffix)
    {
      this.fileSuffix = fileSuffix;
    }

    public boolean filter(Object provider)
    {
      if (provider instanceof ImageWriterSpi)
        {
          ImageWriterSpi spi = (ImageWriterSpi) provider;
          String[] suffixes = spi.getFileSuffixes();

          for (int i = suffixes.length - 1; i >= 0; --i)
            if (fileSuffix.equals(suffixes[i]))
              return true;
        }

      return false;
    }
  }

  private static final class ImageReaderIterator implements Iterator
  {
    Iterator it;
    Object readerExtension;
    
    public ImageReaderIterator(Iterator it, Object readerExtension)
    {
      this.it = it;
      this.readerExtension = readerExtension;
    }

    public boolean hasNext()
    {
      return it.hasNext();
    }

    public Object next()
    {
      try
        {
          return ((ImageReaderSpi) it.next()).createReaderInstance(readerExtension);
        }
      catch (IOException e)
        {
          return null;
        }
    }

    public void remove()
    {
      throw new UnsupportedOperationException();
    }
  }

  private static final class ImageWriterIterator implements Iterator
  {
    Iterator it;
    Object writerExtension;
    
    public ImageWriterIterator(Iterator it, Object writerExtension)
    {
      this.it = it;
      this.writerExtension = writerExtension;
    }

    public boolean hasNext()
    {
      return it.hasNext();
    }

    public Object next()
    {
      try
        {
          return ((ImageWriterSpi) it.next()).createWriterInstance(writerExtension);
        }
      catch (IOException e)
        {
          return null;
        }
    }

    public void remove()
    {
      throw new UnsupportedOperationException();
    }
  }
  
  private static File cacheDirectory;
  private static boolean useCache = true;

  private static Iterator getReadersByFilter(Class type,
                                             ServiceRegistry.Filter filter,
                                             Object readerExtension)
  {
    try
      {
        Iterator it = getRegistry().getServiceProviders(type, filter, true);
        return new ImageReaderIterator(it, readerExtension);
      }
    catch (IllegalArgumentException e)
      {
        return Collections.EMPTY_SET.iterator();
      }
  }
  
  private static Iterator getWritersByFilter(Class type,
					     ServiceRegistry.Filter filter,
                                             Object writerExtension)
  {
    try
      {
        Iterator it = getRegistry().getServiceProviders(type, filter, true);
        return new ImageWriterIterator(it, writerExtension);
      }
    catch (IllegalArgumentException e)
      {
        return Collections.EMPTY_SET.iterator();
      }
  }

  public static File getCacheDirectory()
  {
    return cacheDirectory;
  }

  public static Iterator getImageReadersByFormatName(String formatName)
  {
    if (formatName == null)
      throw new IllegalArgumentException("formatName may not be null");

    return getReadersByFilter(ImageReaderSpi.class,
                              new ReaderFormatFilter(formatName),
                              formatName);
  }

  public static Iterator getImageReadersByMIMEType(String MIMEType)
  {
    if (MIMEType == null)
      throw new IllegalArgumentException("MIMEType may not be null");

    return getReadersByFilter(ImageReaderSpi.class,
                              new ReaderMIMETypeFilter(MIMEType),
                              MIMEType);
  }

  public static Iterator getImageReadersBySuffix(String fileSuffix)
  {
    if (fileSuffix == null)
      throw new IllegalArgumentException("formatName may not be null");
    
    return getReadersByFilter(ImageReaderSpi.class,
                              new ReaderSuffixFilter(fileSuffix),
                              fileSuffix);
  }

  public static Iterator getImageWritersByFormatName(String formatName)
  {
    if (formatName == null)
      throw new IllegalArgumentException("formatName may not be null");
    
    return getWritersByFilter(ImageWriterSpi.class,
                              new WriterFormatFilter(formatName),
                              formatName);
  }

  public static Iterator getImageWritersByMIMEType(String MIMEType)
  {
    if (MIMEType == null)
      throw new IllegalArgumentException("MIMEType may not be null");
    
    return getWritersByFilter(ImageWriterSpi.class,
                              new WriterMIMETypeFilter(MIMEType),
                              MIMEType);
  }

  public static Iterator getImageWritersBySuffix(String fileSuffix)
  {
    if (fileSuffix == null)
      throw new IllegalArgumentException("fileSuffix may not be null");
    
    return getWritersByFilter(ImageWriterSpi.class,
                              new WriterSuffixFilter(fileSuffix),
                              fileSuffix);
  }

  public static String[] getReaderFormatNames()
  {
    try
      {
        Iterator it =
	  getRegistry().getServiceProviders(ImageReaderSpi.class, true);
	ArrayList result = new ArrayList();

	while (it.hasNext())
	  {
	    ImageReaderSpi spi = (ImageReaderSpi) it.next();
	    String[] names = spi.getFormatNames();

	    for (int i = names.length - 1; i >= 0; --i)
	      result.add(names[i]);
	  }

	return (String[]) result.toArray(new String[result.size()]);
      }
    catch (IllegalArgumentException e)
      {
        return new String[0];
      }
  }

  public static String[] getReaderMIMETypes()
  {
    try
      {
        Iterator it =
	  getRegistry().getServiceProviders(ImageReaderSpi.class, true);
	ArrayList result = new ArrayList();

	while (it.hasNext())
	  {
	    ImageReaderSpi spi = (ImageReaderSpi) it.next();
	    String[] names = spi.getMIMETypes();

	    for (int i = names.length - 1; i >= 0; --i)
	      result.add(names[i]);
	  }

	return (String[]) result.toArray(new String[result.size()]);
      }
    catch (IllegalArgumentException e)
      {
        return new String[0];
      }
  }

  private static IIORegistry getRegistry()
  {
    return IIORegistry.getDefaultInstance();
  }

  public static boolean getUseCache()
  {
    return useCache;
  }

  public static String[] getWriterFormatNames()
  {
    try
      {
        Iterator it =
	  getRegistry().getServiceProviders(ImageWriterSpi.class, true);
	ArrayList result = new ArrayList();

	while (it.hasNext())
	  {
	    ImageWriterSpi spi = (ImageWriterSpi) it.next();
	    String[] names = spi.getFormatNames();

	    for (int i = names.length - 1; i >= 0; --i)
	      result.add(names[i]);
	  }

	return (String[]) result.toArray(new String[result.size()]);
      }
    catch (IllegalArgumentException e)
      {
        return new String[0];
      }
  }

  public static String[] getWriterMIMETypes()
  {
    try
      {
        Iterator it =
	  getRegistry().getServiceProviders(ImageWriterSpi.class, true);
	ArrayList result = new ArrayList();

	while (it.hasNext())
	  {
	    ImageWriterSpi spi = (ImageWriterSpi) it.next();
	    String[] names = spi.getMIMETypes();

	    for (int i = names.length - 1; i >= 0; --i)
	      result.add(names[i]);
	  }

	return (String[]) result.toArray(new String[result.size()]);
      }
    catch (IllegalArgumentException e)
      {
        return new String[0];
      }
  }
  
  /**
   * Rescans the application classpath for ImageIO service providers
   * and registers them.
   */
  public static void scanForPlugins()
  {
    IIORegistry.getDefaultInstance().registerApplicationClasspathSpis();
  }

  public static void setCacheDirectory(File cacheDirectory)
  {
    if (cacheDirectory != null)
      {
        if (!cacheDirectory.isDirectory())
          throw new IllegalArgumentException("cacheDirectory must be a directory");

        cacheDirectory.canWrite();
      }
    
    ImageIO.cacheDirectory = cacheDirectory;
  }

  public static void setUseCache(boolean useCache)
  {
    ImageIO.useCache = useCache;
  }

  /* 
   * "Standard" simplified entry points.
   */

  public static boolean write(RenderedImage im,
                              String formatName,
                              File output)
    throws IOException
  {
    return write(im, formatName, new FileOutputStream(output));
  }

  public static boolean write(RenderedImage im,
                              String formatName,
                              OutputStream output)
    throws IOException
  {
    return write(im, formatName, new MemoryCacheImageOutputStream(output));
  }
  
  
  public static boolean write(RenderedImage im,
                              String formatName,
                              ImageOutputStream output)
    throws IOException
  {
    Iterator writers = getImageWritersByFormatName(formatName);
    IIOImage img = new IIOImage(im, null, null);
    while (writers.hasNext())
      {
        ImageWriter w = (ImageWriter) writers.next();
        try 
          {
            w.setOutput(output);
          }
        catch (IllegalArgumentException e)
          {
            continue;
          }
        
        w.write(null, img, null);
        output.close();
        return true;
      }
    return false;
  }

  public static BufferedImage read(ImageInputStream stream)
    throws IOException
  {
    Iterator providers = getRegistry().getServiceProviders(ImageReaderSpi.class, true);
    while (providers.hasNext())
      {
        ImageReaderSpi spi = (ImageReaderSpi) providers.next();
        if (spi.canDecodeInput(stream))
          {
            ImageReader reader = spi.createReaderInstance();
            reader.setInput(stream);
            return reader.read(0, null);
          }
      }
    return null;
  }
        
  public static BufferedImage read(URL input)
    throws IOException
  {
    return read(input.openStream());
  }

  public static BufferedImage read(InputStream input)
    throws IOException
  {
    return read(new MemoryCacheImageInputStream(input));
  }

  public static BufferedImage read(File input)
    throws IOException
  {
    return read(new FileInputStream(input));
  }

}
