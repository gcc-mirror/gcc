/* GdkPixbufDecoder.java -- Image data decoding object
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


package gnu.java.awt.peer.gtk;

import java.awt.image.*;
import java.io.FileDescriptor;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.IOException;
import java.net.URL;
import java.util.Vector;
import java.util.Hashtable;
import gnu.classpath.Configuration;

public class GdkPixbufDecoder extends gnu.java.awt.image.ImageDecoder
{
  static 
  {
    if (Configuration.INIT_LOAD_LIBRARY)
      {
        System.loadLibrary("gtkpeer");
      }
    initStaticState ();
  }
  native static void initStaticState ();
  private final int native_state = GtkGenericPeer.getUniqueInteger ();

  // the current set of ImageConsumers for this decoder
  Vector curr;

  // interface to GdkPixbuf
  native void initState ();
  native void pumpBytes (byte bytes[], int len);
  native void finish ();
  
  // gdk-pixbuf provids data in RGBA format
  static final ColorModel cm = new DirectColorModel (32, 0xff000000, 
                                                     0x00ff0000, 
                                                     0x0000ff00, 
                                                     0x000000ff);
  public GdkPixbufDecoder (String filename)
  {
    super (filename);
    initState ();
  }
  
  public GdkPixbufDecoder (URL url)
  {
    super (url);
    initState ();
  }

  public GdkPixbufDecoder (byte[] imagedata, int imageoffset, int imagelength)
  {
    super (imagedata, imageoffset, imagelength);
    initState ();
  }

  // called back by native side
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
  
  // called back by native side
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
    while ((len = is.read (bytes)) != -1)
      pumpBytes (bytes, len);
    
    for (int i = 0; i < curr.size (); i++)
      {
        ImageConsumer ic = (ImageConsumer) curr.elementAt (i);
        ic.imageComplete (ImageConsumer.STATICIMAGEDONE);
      }

    curr = null;
  }

  // remaining helper class and static method is a convenience for the Gtk
  // peers, for loading a BufferedImage in off a disk file. one would think
  // this ought to be fairly straightforward, but it does not appear
  // anywhere else I can find.

  private static class BufferedImageBuilder implements ImageConsumer
  {
    BufferedImage bufferedImage;
    ColorModel defaultModel;

    public BufferedImage getBufferedImage()
    {
      return bufferedImage;
    }

    public void setDimensions(int width, int height)
    {
      bufferedImage = new BufferedImage (width, height, BufferedImage.TYPE_INT_ARGB);
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
      if (bufferedImage != null)
        {

          if (model == null)
            model = defaultModel;

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
        }
    }

    public void imageComplete(int status) {}
  }

  public static BufferedImage createBufferedImage (String filename)
  {
    BufferedImageBuilder bb = new BufferedImageBuilder ();
    GdkPixbufDecoder dec = new GdkPixbufDecoder (filename);
    dec.startProduction (bb);
    return bb.getBufferedImage ();
  }
}
