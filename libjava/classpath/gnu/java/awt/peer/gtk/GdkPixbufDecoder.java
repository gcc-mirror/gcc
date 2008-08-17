/* GdkPixbufDecoder.java -- Image data decoding object
   Copyright (C) 2003, 2004, 2005, 2006  Free Software Foundation, Inc.

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


package gnu.java.awt.peer.gtk;

import java.awt.image.BufferedImage;
import java.awt.image.ColorModel;
import java.awt.image.DirectColorModel;
import java.awt.image.ImageConsumer;
import java.awt.image.Raster;
import java.awt.image.RenderedImage;
import java.io.DataInput;
import java.io.DataOutput;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Locale;
import java.util.Vector;

import javax.imageio.IIOImage;
import javax.imageio.ImageReadParam;
import javax.imageio.ImageReader;
import javax.imageio.ImageTypeSpecifier;
import javax.imageio.ImageWriteParam;
import javax.imageio.ImageWriter;
import javax.imageio.metadata.IIOMetadata;
import javax.imageio.spi.IIORegistry;
import javax.imageio.spi.ImageReaderSpi;
import javax.imageio.spi.ImageWriterSpi;
import javax.imageio.stream.ImageInputStream;
import javax.imageio.stream.ImageOutputStream;

import gnu.classpath.Configuration;
import gnu.classpath.Pointer;

public class GdkPixbufDecoder extends gnu.java.awt.image.ImageDecoder
{
  static 
  {
    if (true) // GCJ LOCAL
      {
        System.loadLibrary("gtkpeer");
      }

    initStaticState ();
  }
  
  /**
   * Lock that should be held for all gdkpixbuf operations. We don't use
   * the global gdk_threads_enter/leave functions since gdkpixbuf
   * operations can be done in parallel to drawing and manipulating gtk
   * widgets.
   */
  static Object pixbufLock = new Object();

  static native void initStaticState();
  private final int native_state = GtkGenericPeer.getUniqueInteger ();

  // initState() has been called, but pumpDone() has not yet been called.
  private boolean needsClose = false;

  // the current set of ImageConsumers for this decoder
  Vector curr;

  /**
   * The pointer to the native pixbuf loader.
   *
   * This field is manipulated by native code. Don't change or remove
   * without adjusting the native code.
   */
  private Pointer nativeDecoder;

  // interface to GdkPixbuf
  // These native functions should be called with the pixbufLock held.
  native void initState ();
  native void pumpBytes (byte[] bytes, int len) throws IOException;
  native void pumpDone () throws IOException;
  native void finish (boolean needsClose);

  /**
   * Converts given image to bytes.
   * Will call the GdkPixbufWriter for each chunk.
   */
  static native void streamImage(int[] bytes, String format,
                                 int width, int height,
                                 boolean hasAlpha, GdkPixbufWriter writer);

  // gdk-pixbuf provids data in RGBA format
  static final ColorModel cm = new DirectColorModel (32, 0xff000000, 
                                                     0x00ff0000, 
                                                     0x0000ff00, 
                                                     0x000000ff);
  public GdkPixbufDecoder (DataInput datainput)
  {
    super (datainput);
  }

  public GdkPixbufDecoder (InputStream in)
  {
    super (in);
  }

  public GdkPixbufDecoder (String filename)
  {
    super (filename);
  }
  
  public GdkPixbufDecoder (URL url)
  {
    super (url);
  }

  public GdkPixbufDecoder (byte[] imagedata, int imageoffset, int imagelength)
  {
    super (imagedata, imageoffset, imagelength);
  }

  // called back by native side: area_prepared_cb
  void areaPrepared (int width, int height)
  {

    if (curr == null)
      return;

    for (int i = 0; i < curr.size (); i++)
      {
        ImageConsumer ic = (ImageConsumer) curr.elementAt (i);
        ic.setDimensions (width, height);
        ic.setColorModel (cm);
        ic.setHints (ImageConsumer.RANDOMPIXELORDER);
      }
  }
  
  // called back by native side: area_updated_cb
  void areaUpdated (int x, int y, int width, int height, 
                    int pixels[], int scansize)
  {
    if (curr == null)
      return;
    
    for (int i = 0; i < curr.size (); i++)
      {
        ImageConsumer ic = (ImageConsumer) curr.elementAt (i);
        ic.setPixels (x, y, width, height, cm, pixels, 0, scansize);
      }
  }
  
  // called from an async image loader of one sort or another, this method
  // repeatedly reads bytes from the input stream and passes them through a
  // GdkPixbufLoader using the native method pumpBytes. pumpBytes in turn
  // decodes the image data and calls back areaPrepared and areaUpdated on
  // this object, feeding back decoded pixel blocks, which we pass to each
  // of the ImageConsumers in the provided Vector.

  public void produce (Vector v, InputStream is) throws IOException
  {
    curr = v;

    byte bytes[] = new byte[4096];
    int len = 0;
    synchronized(pixbufLock)
      {
        initState();
      }
    needsClose = true;

    // Note: We don't want the pixbufLock while reading from the InputStream.
    while ((len = is.read (bytes)) != -1)
      {
        synchronized(pixbufLock)
          {
            pumpBytes (bytes, len);
          }
      }

    synchronized(pixbufLock)
      {
        pumpDone();
      }

    needsClose = false;
    
    for (int i = 0; i < curr.size (); i++)
      {
        ImageConsumer ic = (ImageConsumer) curr.elementAt (i);
        ic.imageComplete (ImageConsumer.STATICIMAGEDONE);
      }

    curr = null;
  }

  public void finalize()
  {
    synchronized(pixbufLock)
      {
        finish(needsClose);
      }
  }


  public static class ImageFormatSpec
  {
    public String name;
    public boolean writable = false;    
    public ArrayList<String> mimeTypes = new ArrayList<String>();
    public ArrayList<String> extensions = new ArrayList<String>();

    public ImageFormatSpec(String name, boolean writable)
    {
      this.name = name;
      this.writable = writable;
    }

    public synchronized void addMimeType(String m)
    {
      mimeTypes.add(m);
    }

    public synchronized void addExtension(String e)
    {
      extensions.add(e);
    }    
  }

  static ArrayList<ImageFormatSpec> imageFormatSpecs;

  public static ImageFormatSpec registerFormat(String name, boolean writable) 
  {
    ImageFormatSpec ifs = new ImageFormatSpec(name, writable);
    synchronized(GdkPixbufDecoder.class)
      {
        if (imageFormatSpecs == null)
          imageFormatSpecs = new ArrayList<ImageFormatSpec>();
        imageFormatSpecs.add(ifs);
      }
    return ifs;
  }

  static String[] getFormatNames(boolean writable)
  {
    ArrayList<String> names = new ArrayList<String>();
    synchronized (imageFormatSpecs) 
      {
        Iterator<ImageFormatSpec> i = imageFormatSpecs.iterator();
        while (i.hasNext())
          {
            ImageFormatSpec ifs = i.next();
            if (writable && !ifs.writable)
              continue;
            names.add(ifs.name);

            /* 
             * In order to make the filtering code work, we need to register
             * this type under every "format name" likely to be used as a synonym.
             * This generally means "all the extensions people might use". 
             */

            Iterator<String> j = ifs.extensions.iterator();
            while (j.hasNext())
              names.add(j.next());
          }
      }
    return names.toArray(new String[names.size()]);
  }

  static String[] getFormatExtensions(boolean writable)
  {
    ArrayList<String> extensions = new ArrayList<String>();
    synchronized (imageFormatSpecs) 
      {
        Iterator<ImageFormatSpec> i = imageFormatSpecs.iterator();
        while (i.hasNext())
          {
            ImageFormatSpec ifs = i.next();
            if (writable && !ifs.writable)
              continue;
            Iterator<String> j = ifs.extensions.iterator();
            while (j.hasNext())
              extensions.add(j.next());
          }
      }
    return extensions.toArray(new String[extensions.size()]);
  }

  static String[] getFormatMimeTypes(boolean writable)
  {
    ArrayList<String> mimeTypes = new ArrayList<String>();
    synchronized (imageFormatSpecs) 
      {
        Iterator<ImageFormatSpec> i = imageFormatSpecs.iterator();
        while (i.hasNext())
          {
            ImageFormatSpec ifs = i.next();
            if (writable && !ifs.writable)
              continue;
            Iterator<String> j = ifs.mimeTypes.iterator();
            while (j.hasNext())
              mimeTypes.add(j.next());
          }
      }
    return mimeTypes.toArray(new String[mimeTypes.size()]);
  }

  
  static String findFormatName(Object ext, boolean needWritable)
  {
    if (ext == null)
      return null;

    if (!(ext instanceof String))
      throw new IllegalArgumentException("extension is not a string");

    String str = (String) ext;

    Iterator<ImageFormatSpec> i = imageFormatSpecs.iterator();
    while (i.hasNext())
      {
        ImageFormatSpec ifs = i.next();

        if (needWritable && !ifs.writable)
          continue;

        if (ifs.name.equals(str))
          return str;

        Iterator<String> j = ifs.extensions.iterator(); 
        while (j.hasNext())
          {
            String extension = j.next();
            if (extension.equals(str))
              return ifs.name;
          }

        j = ifs.mimeTypes.iterator(); 
        while (j.hasNext())
          {
            String mimeType = j.next();
            if (mimeType.equals(str))
              return ifs.name;
          }
      }      
    throw new IllegalArgumentException("unknown extension '" + str + "'");
  }

  private static GdkPixbufReaderSpi readerSpi;
  private static GdkPixbufWriterSpi writerSpi;

  public static synchronized GdkPixbufReaderSpi getReaderSpi()
  {
    if (readerSpi == null)
      readerSpi = new GdkPixbufReaderSpi();
    return readerSpi;
  }

  public static synchronized GdkPixbufWriterSpi getWriterSpi()
  {
    if (writerSpi == null)
      writerSpi = new GdkPixbufWriterSpi();
    return writerSpi;
  }

  public static void registerSpis(IIORegistry reg) 
  {
    reg.registerServiceProvider(getReaderSpi(), ImageReaderSpi.class);
    reg.registerServiceProvider(getWriterSpi(), ImageWriterSpi.class);
  }

  public static class GdkPixbufWriterSpi extends ImageWriterSpi
  {
    public GdkPixbufWriterSpi() 
    {      
      super("GdkPixbuf", "2.x",
            GdkPixbufDecoder.getFormatNames(true), 
            GdkPixbufDecoder.getFormatExtensions(true), 
            GdkPixbufDecoder.getFormatMimeTypes(true),
            "gnu.java.awt.peer.gtk.GdkPixbufDecoder$GdkPixbufWriter",
            new Class[] { ImageOutputStream.class },
            new String[] { "gnu.java.awt.peer.gtk.GdkPixbufDecoder$GdkPixbufReaderSpi" },
            false, null, null, null, null,
            false, null, null, null, null);
    }

    public boolean canEncodeImage(ImageTypeSpecifier ts)
    {
      return true;
    }

    public ImageWriter createWriterInstance(Object ext)
    {
      return new GdkPixbufWriter(this, ext);
    }

    public String getDescription(java.util.Locale loc)
    {
      return "GdkPixbuf Writer SPI";
    }

  }

  public static class GdkPixbufReaderSpi extends ImageReaderSpi
  {
    public GdkPixbufReaderSpi() 
    { 
      super("GdkPixbuf", "2.x",
            GdkPixbufDecoder.getFormatNames(false), 
            GdkPixbufDecoder.getFormatExtensions(false), 
            GdkPixbufDecoder.getFormatMimeTypes(false),
            "gnu.java.awt.peer.gtk.GdkPixbufDecoder$GdkPixbufReader",
            new Class[] { ImageInputStream.class },
            new String[] { "gnu.java.awt.peer.gtk.GdkPixbufDecoder$GdkPixbufWriterSpi" },
            false, null, null, null, null,
            false, null, null, null, null);
    }

    public boolean canDecodeInput(Object obj) 
    { 
      return true; 
    }

    public ImageReader createReaderInstance(Object ext)
    {
      return new GdkPixbufReader(this, ext);
    }

    public String getDescription(Locale loc)
    {
      return "GdkPixbuf Reader SPI";
    }
  }

  private static class GdkPixbufWriter
    extends ImageWriter implements Runnable
  {
    String ext;
    public GdkPixbufWriter(GdkPixbufWriterSpi ownerSpi, Object ext)
    {
      super(ownerSpi);
      this.ext = findFormatName(ext, true);
    }

    public IIOMetadata convertImageMetadata (IIOMetadata inData,
                                             ImageTypeSpecifier imageType,
                                             ImageWriteParam param)
    {
      return null;
    }

    public IIOMetadata convertStreamMetadata (IIOMetadata inData,
                                              ImageWriteParam param)
    {
      return null;
    }

    public IIOMetadata getDefaultImageMetadata (ImageTypeSpecifier imageType, 
                                                ImageWriteParam param)
    {
      return null;
    }

    public IIOMetadata getDefaultStreamMetadata (ImageWriteParam param)
    {
      return null;
    }

  public void write (IIOMetadata streamMetadata, IIOImage i, ImageWriteParam param)
    throws IOException
    {
      RenderedImage image = i.getRenderedImage();
      Raster ras = image.getData();
      int width = ras.getWidth();
      int height = ras.getHeight();
      ColorModel model = image.getColorModel();
      int[] pixels = CairoGraphics2D.findSimpleIntegerArray (image.getColorModel(), ras);
      
      if (pixels == null)
        {
          BufferedImage img;
          if(model != null && model.hasAlpha())
            img = CairoSurface.getBufferedImage(width, height);
          img = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
          int[] pix = new int[4];
          for (int y = 0; y < height; ++y)
            for (int x = 0; x < width; ++x)
              img.setRGB(x, y, model.getRGB(ras.getPixel(x, y, pix)));
          pixels = CairoGraphics2D.findSimpleIntegerArray (img.getColorModel(), 
                                                         img.getRaster());
          model = img.getColorModel();
        }

      Thread workerThread = new Thread(this, "GdkPixbufWriter");
      workerThread.start();
      processImageStarted(1);
      synchronized(pixbufLock)
        {
          streamImage(pixels, this.ext, width, height, model.hasAlpha(), 
                      this);
        }
      synchronized(data)
        {
          data.add(DATADONE);
          data.notifyAll();
        }

      while (workerThread.isAlive())
        {
          try
            {
              workerThread.join();
            }
          catch (InterruptedException ioe)
            {
              // Ignored.
            }
        }

      if (exception != null)
        throw exception;

      processImageComplete();
    }    

    /**
     * Object marking end of data from native streamImage code.
     */
    private static final Object DATADONE = new Object();

    /**
     * Holds the data gotten from the native streamImage code.
     * A worker thread will pull data out.
     * Needs to be synchronized for access.
     * The special object DATADONE is added when all data has been delivered.
     */
    private ArrayList<Object> data = new ArrayList<Object>();

    /**
     * Holds any IOException thrown by the run method that needs
     * to be rethrown by the write method.
     */
    private IOException exception;

    /** Callback for streamImage native code. **/
    private void write(byte[] bs)
    {
      synchronized(data)
        {
          data.add(bs);
          data.notifyAll();
        }
    }

    public void run()
    {
      boolean done = false;
      while (!done)
        {
          synchronized(data)
            {
              while (data.isEmpty())
                {
                  try
                    {
                      data.wait();
                    }
                  catch (InterruptedException ie)
                    {
                      /* ignore */
                    }
                }

              Object o = data.remove(0);
              if (o == DATADONE)
                done = true;
              else
                {
                  DataOutput out = (DataOutput) getOutput();
                  try
                    {
                      out.write((byte[]) o);
                    }
                  catch (IOException ioe)
                    {
                      // We are only interested in the first exception.
                      if (exception == null)
                        exception = ioe;
                    }
                }
            }
        }
    }
  }

  private static class GdkPixbufReader 
    extends ImageReader
    implements ImageConsumer
  {
    // ImageConsumer parts
    GdkPixbufDecoder dec;
    BufferedImage bufferedImage;
    ColorModel defaultModel;
    int width;
    int height;
    String ext;
    
    public GdkPixbufReader(GdkPixbufReaderSpi ownerSpi, Object ext)
    {
      super(ownerSpi);
      this.ext = findFormatName(ext, false);
    }

    public GdkPixbufReader(GdkPixbufReaderSpi ownerSpi, Object ext,
                           GdkPixbufDecoder d)
    {
      this(ownerSpi, ext);
      dec = d;
    }

    public void setDimensions(int w, int h)
    {
      processImageStarted(1);
      width = w;
      height = h;
    }
    
    public void setProperties(Hashtable props) {}

    public void setColorModel(ColorModel model) 
    {
      defaultModel = model;
    }

    public void setHints(int flags) {}

    public void setPixels(int x, int y, int w, int h, 
                          ColorModel model, byte[] pixels, 
                          int offset, int scansize)
    {
    }      

    public void setPixels(int x, int y, int w, int h, 
                          ColorModel model, int[] pixels, 
                          int offset, int scansize)
    {
      if (model == null)
        model = defaultModel;
      
      if (bufferedImage == null)
        {
          if(model != null && model.hasAlpha())
            bufferedImage = new BufferedImage (width, height,
                                               BufferedImage.TYPE_INT_ARGB);
          else
            bufferedImage = new BufferedImage (width, height,
                                               BufferedImage.TYPE_INT_RGB);
        }

      int pixels2[];
      if (model != null)
        {
          pixels2 = new int[pixels.length];
          for (int yy = 0; yy < h; yy++)
            for (int xx = 0; xx < w; xx++)
              {
                int i = yy * scansize + xx;
                pixels2[i] = model.getRGB (pixels[i]);
              }
        }
      else
        pixels2 = pixels;

      bufferedImage.setRGB (x, y, w, h, pixels2, offset, scansize);
      processImageProgress(y / (height == 0 ? 1 : height));
    }

    public void imageComplete(int status) 
    {
      processImageComplete();
    }

    public BufferedImage getBufferedImage()
    {
      if (bufferedImage == null && dec != null)
        dec.startProduction (this);
      return bufferedImage;
    }

    // ImageReader parts

    public int getNumImages(boolean allowSearch)
      throws IOException
    {
      return 1;
    }

    public IIOMetadata getImageMetadata(int i) 
    {
      return null;
    }

    public IIOMetadata getStreamMetadata()
      throws IOException
    {
      return null;
    }

    public Iterator<ImageTypeSpecifier> getImageTypes(int imageIndex)
      throws IOException
    {
      BufferedImage img = getBufferedImage();
      Vector<ImageTypeSpecifier> vec = new Vector<ImageTypeSpecifier>();
      vec.add(new ImageTypeSpecifier(img));
      return vec.iterator();
    }
    
    public int getHeight(int imageIndex)
      throws IOException
    {
      return getBufferedImage().getHeight();
    }

    public int getWidth(int imageIndex)
      throws IOException
    {
      return getBufferedImage().getWidth();
    }

    public void setInput(Object input,
                         boolean seekForwardOnly,
                         boolean ignoreMetadata)
    {
      super.setInput(input, seekForwardOnly, ignoreMetadata);
      Object get = getInput();
      if (get instanceof InputStream)
        dec = new GdkPixbufDecoder((InputStream) get);
      else if (get instanceof DataInput)
        dec = new GdkPixbufDecoder((DataInput) get);
      else
        throw new IllegalArgumentException("input object not supported: "
                                           + get);
    }

    public BufferedImage read(int imageIndex, ImageReadParam param)
      throws IOException
    {
      return getBufferedImage ();
    }
  }
}
