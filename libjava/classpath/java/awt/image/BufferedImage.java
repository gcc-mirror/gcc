/* BufferedImage.java --
   Copyright (C) 2000, 2002, 2003, 2004, 2005, 2006,  Free Software Foundation

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


package java.awt.image;

import gnu.java.awt.Buffers;
import gnu.java.awt.ClasspathGraphicsEnvironment;
import gnu.java.awt.ComponentDataBlitOp;
import gnu.java.awt.peer.gtk.CairoSurface;

import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GraphicsEnvironment;
import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Transparency;
import java.awt.color.ColorSpace;
import java.util.Hashtable;
import java.util.Vector;

/**
 * A buffered image always starts at coordinates (0, 0).
 *
 * The buffered image is not subdivided into multiple tiles. Instead,
 * the image consists of one large tile (0,0) with the width and
 * height of the image. This tile is always considered to be checked
 * out.
 * 
 * @author Rolf W. Rasmussen (rolfwr@ii.uib.no)
 */
public class BufferedImage extends Image
  implements WritableRenderedImage, Transparency
{
  public static final int TYPE_CUSTOM         =  0,
                          TYPE_INT_RGB        =  1,
                          TYPE_INT_ARGB       =  2,
                          TYPE_INT_ARGB_PRE   =  3,
                          TYPE_INT_BGR        =  4,
                          TYPE_3BYTE_BGR      =  5,
                          TYPE_4BYTE_ABGR     =  6,
                          TYPE_4BYTE_ABGR_PRE =  7,
                          TYPE_USHORT_565_RGB =  8,
                          TYPE_USHORT_555_RGB =  9,
                          TYPE_BYTE_GRAY      = 10,
                          TYPE_USHORT_GRAY    = 11,
                          TYPE_BYTE_BINARY    = 12,
                          TYPE_BYTE_INDEXED   = 13;
  
  /**
   * Vector of TileObservers (or null)
   */
  Vector<TileObserver> tileObservers;
  
  /**
   * The image's WritableRaster
   */
  WritableRaster raster;

  /**
   * The associated ColorModel
   */
  ColorModel colorModel;

  /**
   * The image's properties (or null)
   */
  Hashtable properties;

  /**
   * Whether alpha is premultiplied
   */
  boolean isPremultiplied;

  /**
   * The predefined type, if any.
   */
  int type;

  /**
   * Creates a new <code>BufferedImage</code> with the specified width, height
   * and type.  Valid <code>type</code> values are:
   * 
   * <ul>
   *   <li>{@link #TYPE_INT_RGB}</li>
   *   <li>{@link #TYPE_INT_ARGB}</li>
   *   <li>{@link #TYPE_INT_ARGB_PRE}</li>
   *   <li>{@link #TYPE_INT_BGR}</li>
   *   <li>{@link #TYPE_3BYTE_BGR}</li>
   *   <li>{@link #TYPE_4BYTE_ABGR}</li>
   *   <li>{@link #TYPE_4BYTE_ABGR_PRE}</li>
   *   <li>{@link #TYPE_USHORT_565_RGB}</li>
   *   <li>{@link #TYPE_USHORT_555_RGB}</li>
   *   <li>{@link #TYPE_BYTE_GRAY}</li>
   *   <li>{@link #TYPE_USHORT_GRAY}</li>
   *   <li>{@link #TYPE_BYTE_BINARY}</li>
   *   <li>{@link #TYPE_BYTE_INDEXED}</li>
   * </ul>
   * 
   * @param width the width (must be > 0).
   * @param height the height (must be > 0).
   * @param type  the image type (see the list of valid types above).
   * 
   * @throws IllegalArgumentException if <code>width</code> or
   *     <code>height</code> is less than or equal to zero.
   * @throws IllegalArgumentException if <code>type</code> is not one of the
   *     specified values.
   */
  public BufferedImage(int width, int height, int type)
  {
    SampleModel sm = null;
    ColorModel cm = null;
    boolean premultiplied = (type == BufferedImage.TYPE_INT_ARGB_PRE
                            || type == BufferedImage.TYPE_4BYTE_ABGR_PRE);

    switch( type )
      {
      case BufferedImage.TYPE_INT_RGB:
        sm = new SinglePixelPackedSampleModel( DataBuffer.TYPE_INT, 
                                               width, height,
                                               new int[]{ 0x00FF0000, 
                                                          0x0000FF00, 
                                                          0x000000FF } ) ;
        cm = new DirectColorModel( 24, 0xff0000, 0xff00, 0xff );
        break;
	
      case BufferedImage.TYPE_3BYTE_BGR:
        sm = new PixelInterleavedSampleModel( DataBuffer.TYPE_BYTE,
                                              width, height,
                                              3, width * 3, 
                                              new int[]{ 2, 1, 0 } );
        cm = new ComponentColorModel(ColorSpace.getInstance(ColorSpace.CS_sRGB),
                                     false, false,
                                     BufferedImage.OPAQUE,
                                     DataBuffer.TYPE_BYTE);
        break;

      case BufferedImage.TYPE_INT_ARGB:
      case BufferedImage.TYPE_INT_ARGB_PRE:
        sm = new SinglePixelPackedSampleModel( DataBuffer.TYPE_INT, 
                                               width, height,
                                               new int[]{ 0x00FF0000, 
                                                          0x0000FF00, 
                                                          0x000000FF, 
                                                          0xFF000000 } );
        if (premultiplied)
          cm = new DirectColorModel( ColorSpace.getInstance(ColorSpace.CS_sRGB),
                                     32, 0xff0000, 0xff00, 0xff, 0xff000000,
                                     true,
                                     Buffers.smallestAppropriateTransferType(32));
        else
          cm = new DirectColorModel( 32, 0xff0000, 0xff00, 0xff, 0xff000000 );
        
        break;

      case BufferedImage.TYPE_4BYTE_ABGR:
      case BufferedImage.TYPE_4BYTE_ABGR_PRE:
        sm = new PixelInterleavedSampleModel(DataBuffer.TYPE_BYTE, 
                                             width, height,
                                             4, 4*width,
                                             new int[]{3, 2, 1, 0});
        cm = new ComponentColorModel(ColorSpace.getInstance(ColorSpace.CS_sRGB),
                                     true, premultiplied,
                                     BufferedImage.TRANSLUCENT,
                                     DataBuffer.TYPE_BYTE);
        break;

      case BufferedImage.TYPE_INT_BGR:
        sm = new SinglePixelPackedSampleModel( DataBuffer.TYPE_INT, 
                                               width, height,
                                               new int[]{ 0x000000FF, 
                                                          0x0000FF00, 
                                                          0x00FF0000 } ) ;
        cm = new DirectColorModel( 24, 0xff, 0xff00, 0xff0000 );
        break;

      case BufferedImage.TYPE_USHORT_565_RGB:
        sm = new SinglePixelPackedSampleModel( DataBuffer.TYPE_USHORT,
                                               width, height,
                                               new int[]{ 0xF800, 
                                                          0x7E0, 
                                                          0x1F } ) ;
        cm = new DirectColorModel( 16, 0xF800, 0x7E0, 0x1F );
        break;
        
      case BufferedImage.TYPE_USHORT_555_RGB:
        sm = new SinglePixelPackedSampleModel( DataBuffer.TYPE_USHORT,
                                               width, height,
                                               new int[]{ 0x7C00, 
                                                          0x3E0, 
                                                          0x1F } ) ;
        cm = new DirectColorModel( 15, 0x7C00, 0x3E0, 0x1F );
        break;

      case BufferedImage.TYPE_BYTE_INDEXED:
        cm = createDefaultIndexedColorModel( false );

      case BufferedImage.TYPE_BYTE_GRAY:
        sm = new PixelInterleavedSampleModel( DataBuffer.TYPE_BYTE,
                                              width, height,
                                              1, width, new int[]{ 0 } );
        break;

      case BufferedImage.TYPE_USHORT_GRAY:
        sm = new PixelInterleavedSampleModel( DataBuffer.TYPE_USHORT,
                                              width, height,
                                              1, width, new int[]{ 0 } );
        break;

      case BufferedImage.TYPE_BYTE_BINARY:
        cm = createDefaultIndexedColorModel( true );
        sm = new MultiPixelPackedSampleModel(DataBuffer.TYPE_BYTE, 
                                             width, height, 1);
        break;

      default:
        sm = null;
      }

    if( sm == null )
      throw new IllegalArgumentException("Unknown predefined image type.");
    
    if( cm == null ) // only for the grayscale types 
      {
        int buftype;
        int[] bits = new int[1];
        if( type == BufferedImage.TYPE_BYTE_GRAY )
          {
            buftype = DataBuffer.TYPE_BYTE;
            bits[0] = 8;
          }
        else
          {
            buftype = DataBuffer.TYPE_USHORT;
            bits[0] = 16;
          }
        ColorSpace graySpace = ColorSpace.getInstance( ColorSpace.CS_GRAY );
        
        cm = new ComponentColorModel( graySpace, bits, false, false, 
                                      Transparency.OPAQUE, buftype );
      }

    WritableRaster rst = null;
    
    // Attempt to create an accelerated backend for this image
    GraphicsEnvironment env = GraphicsEnvironment.getLocalGraphicsEnvironment();
    if (env instanceof ClasspathGraphicsEnvironment)
      rst = ((ClasspathGraphicsEnvironment)env).createRaster(cm, sm);
    
    // Default to a standard Java raster & databuffer if needed
    if (rst == null)
      rst = Raster.createWritableRaster(sm, new Point( 0, 0 ) );
    
    init(cm, rst, premultiplied,
         null, // no properties
         type );
  }

  public BufferedImage(int w, int h, int type, IndexColorModel indexcolormodel)
  {
    if ((type != TYPE_BYTE_BINARY) && (type != TYPE_BYTE_INDEXED))
      throw new IllegalArgumentException("Type must be TYPE_BYTE_BINARY or TYPE_BYTE_INDEXED");
    if( indexcolormodel.getMapSize() > 16 && type == TYPE_BYTE_BINARY )
      throw new IllegalArgumentException("Type TYPE_BYTE_BINARY cannot have a larger than 16-color palette.");
    if( indexcolormodel.getMapSize() > 256 )
      throw new IllegalArgumentException("Byte type cannot have a larger than 256-color palette.");

    init( indexcolormodel,
          indexcolormodel.createCompatibleWritableRaster(w, h),
          indexcolormodel.isAlphaPremultiplied(),
          null, // no properties
          type );
  }

  public BufferedImage(ColorModel colormodel, WritableRaster writableraster,
		       boolean premultiplied, Hashtable<?,?> properties)
  {
    init(colormodel, writableraster, premultiplied, properties, TYPE_CUSTOM);
  }
 

  private void init(ColorModel cm, WritableRaster writableraster, 
                    boolean premultiplied, Hashtable properties, int type)
  {
    raster = writableraster;
    colorModel = cm;
    this.properties = properties;
    isPremultiplied = premultiplied;
    this.type = type;
  }

  /**
   * Creates the default palettes for the predefined indexed color types
   * (256-color or black-and-white)
   *
   * @param binary - If <code>true</code>, a black and white palette,
   * otherwise a default 256-color palette is returned.
   */    
  private IndexColorModel createDefaultIndexedColorModel( boolean binary )
  {
    if( binary )
      {
        byte[] t = new byte[]{ 0, (byte)255 };
        return new IndexColorModel( 1, 2, t, t, t );
      }

    byte[] r = new byte[256];
    byte[] g = new byte[256];
    byte[] b = new byte[256];
    
    int index = 0;
    for( int i = 0; i < 6; i++ )
      for( int j = 0; j < 6; j++ )
        for( int k = 0; k < 6; k++ )
          {
            r[ index ] = (byte)(i * 51);
            g[ index ] = (byte)(j * 51);
            b[ index ] = (byte)(k * 51);
            index++;
          }
    
    while( index < 256 )
      {
        r[ index ] = g[ index ] = b[ index ] = 
          (byte)(18 + (index - 216) * 6);
        index++;
      }
    
    return new IndexColorModel( 8, 256, r, g, b );
  }
  
  public void coerceData(boolean premultiplied)
  {
    colorModel = colorModel.coerceData(raster, premultiplied);
    isPremultiplied = premultiplied;
  }

  public WritableRaster copyData(WritableRaster dest)
  {
    if (dest == null)
      dest = raster.createCompatibleWritableRaster(getMinX(), getMinY(),
                                                   getWidth(),getHeight());

    int x = dest.getMinX();
    int y = dest.getMinY();
    int w = dest.getWidth();
    int h = dest.getHeight();
    
    // create a src child that has the right bounds...
    WritableRaster src =
      raster.createWritableChild(x, y, w, h, x, y,
                                 null);  // same bands
    
    if (src.getSampleModel () instanceof ComponentSampleModel
        && dest.getSampleModel () instanceof ComponentSampleModel)
      // Refer to ComponentDataBlitOp for optimized data blitting:
      ComponentDataBlitOp.INSTANCE.filter(src, dest);
    
    else
      {
        // slower path
        int samples[] = src.getPixels (x, y, w, h, (int [])null);
        dest.setPixels (x, y, w, h, samples);
      }
    return dest;
  }

  public Graphics2D createGraphics()
  {
    GraphicsEnvironment env;
    env = GraphicsEnvironment.getLocalGraphicsEnvironment ();
    return env.createGraphics (this);
  }

  public void flush()
  {
  }
  
  public WritableRaster getAlphaRaster()
  {
    return colorModel.getAlphaRaster(raster);
  }
  
  public ColorModel getColorModel()
  {
    return colorModel;
  }
  
  public Raster getData()
  {
    return copyData(null);
    /* TODO: this might be optimized by returning the same
       raster (not writable) as long as image data doesn't change. */
  }

  public Raster getData(Rectangle rectangle)
  {
    WritableRaster dest =
      raster.createCompatibleWritableRaster(rectangle);
    return copyData(dest);
  }
  
  public Graphics getGraphics()
  {
    return createGraphics();
  }

  public int getHeight()
  {
    return raster.getHeight();
  }
  
  public int getHeight(ImageObserver imageobserver)
  {
    return getHeight();
  }
    
  public int getMinTileX()
  {
    return 0;
  }
  
  public int getMinTileY()
  {
    return 0;
  }

  public int getMinX()
  {
    return 0; 
  }

  public int getMinY() 
  {
    return 0;
  }
  
  public int getNumXTiles()
  {
    return 1;
  }

  public int getNumYTiles()
  {
	return 1;
  }

  /**
   * Returns the value of the specified property, or 
   * {@link Image#UndefinedProperty} if the property is not defined.
   * 
   * @param string  the property key (<code>null</code> not permitted).
   * 
   * @return The property value.
   * 
   * @throws NullPointerException if <code>string</code> is <code>null</code>.
   */
  public Object getProperty(String string)
  {
    if (string == null)
      throw new NullPointerException("The property name cannot be null.");
    Object result = Image.UndefinedProperty;
    if (properties != null)
      {
        Object v = properties.get(string);
        if (v != null)
          result = v;
      }
    return result;
  }

  public Object getProperty(String string, ImageObserver imageobserver)
  {
    return getProperty(string);
  }

  /**
   * Returns <code>null</code> always.
   * 
   * @return <code>null</code> always.
   */
  public String[] getPropertyNames()
  {
    // This method should always return null, see:
    // http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=4640609
    return null;
  }

  public int getRGB(int x, int y)
  {
    Object rgbElem = raster.getDataElements(x, y, null);
    return colorModel.getRGB(rgbElem);
  }
    
  public int[] getRGB(int startX, int startY, int w, int h, int[] rgbArray,
                      int offset, int scanlineStride)
  {
    if (rgbArray == null)
      {
        /*
	      000000000000000000
	      00000[#######-----   [ = start
	      -----########-----   ] = end
	      -----#######]00000
	      000000000000000000 
        */
        int size = (h-1)*scanlineStride + w;
        rgbArray = new int[size];
    }
	
    int endX = startX + w;
    int endY = startY + h;
    
    /* *TODO*:
       Opportunity for optimization by examining color models...
       
       Perhaps wrap the rgbArray up in a WritableRaster with packed
       sRGB color model and perform optimized rendering into the
       array. */

    Object rgbElem = null;
    for (int y=startY; y<endY; y++)
      {
        int xoffset = offset;
        for (int x=startX; x<endX; x++)
          {
            int rgb;
            rgbElem = raster.getDataElements(x, y, rgbElem);
            rgb = colorModel.getRGB(rgbElem);
            rgbArray[xoffset++] = rgb;
          }
        offset += scanlineStride;
      }
    return rgbArray;
  }

  public WritableRaster getRaster()
  {
    return raster;
  }
  
  public SampleModel getSampleModel()
  {
    return raster.getSampleModel();
  }
    
  public ImageProducer getSource()
  {
    return new ImageProducer()
      {
        Vector<ImageConsumer> consumers = new Vector<ImageConsumer>();

        public void addConsumer(ImageConsumer ic)
        {
          if(!consumers.contains(ic))
            consumers.add(ic);
        }

        public boolean isConsumer(ImageConsumer ic)
        {
          return consumers.contains(ic);
        }

        public void removeConsumer(ImageConsumer ic)
        {
          consumers.remove(ic);
        }

        public void startProduction(ImageConsumer ic)
        {
          int x = 0;
          int y = 0;
          int width = getWidth();
          int height = getHeight();
          int stride = width;
          int offset = 0;
          int[] pixels = getRGB(x, y, 
                                width, height, 
                                (int[])null, offset, stride);
          // We already convert the color to RGB in the getRGB call, so
          // we pass a simple RGB color model to the consumers.
          ColorModel model = new DirectColorModel(32, 0xff0000, 0xff00, 0xff,
                                                  0xff000000);

          consumers.add(ic);

          for(int i = 0; i < consumers.size(); i++)
            {
              ImageConsumer c = consumers.elementAt(i);
              c.setHints(ImageConsumer.SINGLEPASS);
              c.setDimensions(getWidth(), getHeight());
              c.setPixels(x, y, width, height, model, pixels, offset, stride);
              c.imageComplete(ImageConsumer.STATICIMAGEDONE);
            }
        }

        public void requestTopDownLeftRightResend(ImageConsumer ic)
        {
          startProduction(ic);
        }

      };
  }
  
  public Vector<RenderedImage> getSources()
  {
    return null;
  }
  
  public BufferedImage getSubimage(int x, int y, int w, int h)
  {
    WritableRaster subRaster = 
      getRaster().createWritableChild(x, y, w, h, 0, 0, null);
    
    return new BufferedImage(getColorModel(), subRaster, isPremultiplied,
                             properties);
  }

  public Raster getTile(int tileX, int tileY)
  {
    return getWritableTile(tileX, tileY);
  }
    
  public int getTileGridXOffset()
  {
    return 0; // according to javadocs
  }

  public int getTileGridYOffset()
  {
    return 0; // according to javadocs
  }

  public int getTileHeight()
  {
    return getHeight(); // image is one big tile
  }

  public int getTileWidth()
  {
    return getWidth(); // image is one big tile
  }

  public int getType()
  {
    return type;
  }

  public int getWidth()
  {
    return raster.getWidth();
  }

  public int getWidth(ImageObserver imageobserver)
  {
    return getWidth();
  }

  public WritableRaster getWritableTile(int tileX, int tileY)
  {
    isTileWritable(tileX, tileY);  // for exception
    return raster;
  }

  private static final Point[] tileIndices = { new Point() };
    
  public Point[] getWritableTileIndices()
  {
    return tileIndices;
  }

  public boolean hasTileWriters()
  {
    return true;
  }
  
  public boolean isAlphaPremultiplied()
  {
    return isPremultiplied;
  }

  public boolean isTileWritable(int tileX, int tileY)
  {
    if ((tileX != 0) || (tileY != 0))
      throw new ArrayIndexOutOfBoundsException("only tile is (0,0)");
    return true;
  }

  public void releaseWritableTile(int tileX, int tileY)
  {
    isTileWritable(tileX, tileY);  // for exception
  }

  //public void removeTileObserver(TileObserver tileobserver) {}

  public void setData(Raster src)
  {
    int x = src.getMinX();
    int y = src.getMinY();
    int w = src.getWidth();
    int h = src.getHeight();
    
    // create a dest child that has the right bounds...
    WritableRaster dest =
      raster.createWritableChild(x, y, w, h, x, y, null);

    if (src.getSampleModel () instanceof ComponentSampleModel
        && dest.getSampleModel () instanceof ComponentSampleModel)

      // Refer to ComponentDataBlitOp for optimized data blitting:
      ComponentDataBlitOp.INSTANCE.filter(src, dest);
    else
      {
        // slower path
        int samples[] = src.getPixels (x, y, w, h, (int [])null);
        dest.setPixels (x, y, w, h, samples);
      }
  }

  public void setRGB(int x, int y, int argb)
  {
    Object rgbElem = colorModel.getDataElements(argb, null);
    raster.setDataElements(x, y, rgbElem);
  }
  
  public void setRGB(int startX, int startY, int w, int h,
		     int[] argbArray, int offset, int scanlineStride)
  {
    int endX = startX + w;
    int endY = startY + h;
    
    Object rgbElem = null;
    for (int y=startY; y<endY; y++)
      {
        int xoffset = offset;
        for (int x=startX; x<endX; x++)
          {
            int argb = argbArray[xoffset++];
            rgbElem = colorModel.getDataElements(argb, rgbElem);
            raster.setDataElements(x, y, rgbElem);
          }
        offset += scanlineStride;    
      }
  }
    
  public String toString()
  {
    StringBuffer buf;

    buf = new StringBuffer(/* estimated length */ 120);
    buf.append("BufferedImage@");
    buf.append(Integer.toHexString(hashCode()));
    buf.append(": type=");
    buf.append(type);
    buf.append(' ');
    buf.append(colorModel);
    buf.append(' ');
    buf.append(raster);

    return buf.toString();
  }


  /**
   * Adds a tile observer. If the observer is already present, it receives
   * multiple notifications.
   *
   * @param to The TileObserver to add.
   */
  public void addTileObserver (TileObserver to)
  {
    if (tileObservers == null)
      tileObservers = new Vector<TileObserver>();
	
    tileObservers.add (to);
  }
	
  /**
   * Removes a tile observer. If the observer was not registered,
   * nothing happens. If the observer was registered for multiple
   * notifications, it is now registered for one fewer notification.
   *
   * @param to The TileObserver to remove.
   */
  public void removeTileObserver (TileObserver to)
  {
    if (tileObservers == null)
      return;
	
    tileObservers.remove (to);
  }

  /**
   * Return the transparency type.
   *
   * @return One of {@link #OPAQUE}, {@link #BITMASK}, or {@link #TRANSLUCENT}.
   * @see Transparency#getTransparency()
   * @since 1.5
   */
  public int getTransparency()
  {
    return colorModel.getTransparency();
  }
}
