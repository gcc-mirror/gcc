/* MediaSize.java -- 
   Copyright (C) 2005, 2006  Free Software Foundation, Inc.

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


package javax.print.attribute.standard;

import java.util.ArrayList;

import javax.print.attribute.Attribute;
import javax.print.attribute.Size2DSyntax;

/**
 * The <code>MediaSize</code> printing attribute class specifies the size
 * of a printing media. The size is defined in portrait orientation with 
 * x at the bottom edge and y at the left edge.
 * <p>
 * There are several media sizes predefined through the nested classes. Further
 * sizes may be provided by the application. <code>MediaSize</code> is not used
 * as a printing attribute currently. It may be used to get the actual sizes 
 * for a named media or to find a suitable <code>MediaSizeName</code> instance
 * by querying with the needed sizes.
 * </p> 
 * <p>
 * <b>IPP Compatibility:</b> MediaSize is not an IPP 1.1 attribute.
 * </p>
 * @see javax.print.attribute.standard.MediaSizeName
 * 
 * @author Michael Koch (konqueror@gmx.de)
 * @author Wolfgang Baer (WBaer@gmx.de)
 */
public class MediaSize extends Size2DSyntax
  implements Attribute
{
  private static final long serialVersionUID = -1967958664615414771L;
  
  private static ArrayList<MediaSize> mediaCache;
  
  static
    {
      mediaCache = new ArrayList<MediaSize>();

      // We call one instance of every container class to make sure it gets
      // loaded during class initialization and therefore all other static
      // fields of this container class also.
      
      // This is needed to put all MediaSize instance into the mediaCache
      // for use by the static methods in this class.
      
      MediaSize tmp = MediaSize.ISO.A0;
      tmp = MediaSize.JIS.B0;
      tmp = MediaSize.Engineering.A;
      tmp = MediaSize.NA.LEGAL;
      tmp = MediaSize.Other.EXECUTIVE;
    }

  private MediaSizeName mediaName;
  
  /**
   * Creates a <code>MediaSize</code> object. The created object will be added 
   * to an internal cache used in the static methods of this class for lookup 
   * of available <code>MediaSize</code> instances.
   *
   * @param x the size in x direction
   * @param y the size in y direction
   * @param units the units to use for the sizes
   *
   * @exception IllegalArgumentException if x or y &lt; 0 or units &lt; 1
   * 
   * @see #findMedia(float, float, int)
   * @see #getMediaSizeForName(MediaSizeName)
   */
  public MediaSize(float x, float y, int units)
  {
    super(x, y, units);
    mediaCache.add(this);
  }
  
  /**
   * Creates a <code>MediaSize</code> object associated with the given
   * media name. The created object will be added to an internal cache used 
   * in the static methods of this class for lookup of available 
   * <code>MediaSize</code> instances.
   *
   * @param x the size in x direction
   * @param y the size in y direction
   * @param units the units to use for the sizes
   * @param media the media name to associate
   *
   * @exception IllegalArgumentException if x or y &lt; 0 or units &lt; 1
   * 
   * @see #findMedia(float, float, int)
   * @see #getMediaSizeForName(MediaSizeName)
   */
  public MediaSize(float x, float y, int units, MediaSizeName media)
  {
    super(x, y, units);
    mediaName = media;
    mediaCache.add(this);
  }
  
  /**
   * Creates a <code>MediaSize</code> object. The created object will be added 
   * to an internal cache used in the static methods of this class for lookup 
   * of available <code>MediaSize</code> instances.
   *
   * @param x the size in x direction
   * @param y the size in y direction
   * @param units the units to use for the sizes
   *
   * @exception IllegalArgumentException if x or y &lt; 0 or units &lt; 1
   * 
   * @see #findMedia(float, float, int)
   * @see #getMediaSizeForName(MediaSizeName)
   */
  public MediaSize(int x, int y, int units)
  {
    super(x, y, units);
    mediaCache.add(this);
  }
  
  /**
   * Creates a <code>MediaSize</code> object associated with the given
   * media name. The created object will be added to an internal cache used 
   * in the static methods of this class for lookup of available 
   * <code>MediaSize</code> instances.
   *
   * @param x the size in x direction
   * @param y the size in y direction
   * @param units the units to use for the sizes
   * @param media the media name to associate
   *
   * @exception IllegalArgumentException if x or y &lt; 0 or units &lt; 1
   * 
   * @see #findMedia(float, float, int)
   * @see #getMediaSizeForName(MediaSizeName)
   */
  public MediaSize(int x, int y, int units, MediaSizeName media)
  {
    super(x, y, units);
    mediaName = media;
    mediaCache.add(this);
  }
  
  /**
   * Returns category of this class.
   *
   * @return The class <code>MediaSize</code> itself.
   */
  public Class< ? extends Attribute> getCategory()
  {
    return MediaSize.class;
  }

    
  /**
   * Searches for a MediaSize object with the given dimensions.
   * If none is found with exact dimensions, the closest match is used.
   * Afterwards the MediaSizeName of the found MediaSize object is 
   * returned - which might be null if none is specified.
   * 
   * @param x the dimension for x
   * @param y the dimension for y
   * @param units the units to be used for comparison
   * @return the corresponding MediaSizeName object, or null
   */
  public static MediaSizeName findMedia(float x, float y, int units)
  {
    if (x <= 0.0f || y <= 0.0f)
      throw new IllegalArgumentException(
        "x and/or y may not be less or equal 0");
  
    if (units < 1)
      throw new IllegalArgumentException("units may not be less then 1");

    MediaSize bestMatch = null;
    int bestDistance = Integer.MAX_VALUE;

    int xMicro = (int) x * units;
    int yMicro = (int) y * units;

    for (int i = 0; i < mediaCache.size(); i++)
      {
        MediaSize size = mediaCache.get(i);
        int dist = (Math.abs(size.getXMicrometers() - xMicro) 
                    + Math.abs(size.getYMicrometers() - yMicro));

        if (dist < bestDistance)
          {
            bestMatch = size;
            bestDistance = dist;
          }
      }

    return bestMatch.getMediaSizeName();
  }
  
  /**
   * Returns the associated <code>MediaSize</code> instance for the 
   * given named media <code>MediaSizeName</code> instance.
   * 
   * @param media the named media to search for.
   * @return The corresponding <code>MediaSize</code> instance or 
   * <code>null</code> if none found.
   */
  public static MediaSize getMediaSizeForName(MediaSizeName media)
  {
    for (int i = 0; i < mediaCache.size(); i++)
      {
	MediaSize size = mediaCache.get(i);
	
	if (size.getMediaSizeName().equals(media))
	  return size;
      }

    return null;
  }
  
  /**
   * Tests if the given object is equal to this object.
   *
   * @param obj the object to test
   *
   * @return <code>true</code> if both objects are equal, 
   * <code>false</code> otherwise.
   */
  public boolean equals(Object obj)
  {
    if (!(obj instanceof MediaSize))
      return false;

    MediaSize tmp = (MediaSize) obj;
    return (tmp.getXMicrometers() == this.getXMicrometers()
            && tmp.getYMicrometers() == this.getYMicrometers());
  }
  
  /**
   * Returns the media name of this size.
   * 
   * @return The media name.
   */
  public MediaSizeName getMediaSizeName()
  {
    return mediaName;
  }

  /**
   * Returns the name of this attribute.
   *
   * @return The name "media-size".
   */
  public final String getName()
  {
    return "media-size";
  }

  /**
   * Container class for predefined ISO media sizes.
   * 
   * @author Sven de Marothy (sven@physto.se)
   */
  public static final class ISO 
  {
    private ISO()
    {
      // prevent instantiation
    }
    
    /**
     * ISO A0 paper, 841 mm x 1189 mm.
     */
    public static final MediaSize A0 = new MediaSize(841, 1189, 
					       MediaSize.MM, 
					       MediaSizeName.ISO_A0);

    /**
     * ISO A1 paper, 594 mm x 841 mm
     */
    public static final MediaSize A1 = new MediaSize(594, 841, MediaSize.MM, 
					       MediaSizeName.ISO_A1);

    /**
     * ISO A2 paper, 420 mm x 594 mm
     */
    public static final MediaSize A2 = new MediaSize(420, 594, MediaSize.MM, MediaSizeName.ISO_A2);

    /**
     * ISO A3 paper, 297 mm x 420 mm
     */
    public static final MediaSize A3 = new MediaSize(297, 420, MediaSize.MM, MediaSizeName.ISO_A3);

    /**
     * ISO A4 paper, 210 mm x 297 mm
     */
    public static final MediaSize A4 = new MediaSize(210, 297, MediaSize.MM, MediaSizeName.ISO_A4);

    /**
     * ISO A5 paper, 148 mm x 210 mm
     */
    public static final MediaSize A5 = new MediaSize(148, 210, MediaSize.MM, MediaSizeName.ISO_A5);

    /**
     * ISO A6 paper, 105 mm x 148 mm
     */
    public static final MediaSize A6 = new MediaSize(105, 148, MediaSize.MM, MediaSizeName.ISO_A6);

    /**
     * ISO A7 paper, 74 mm x 105 mm
     */
    public static final MediaSize A7 = new MediaSize(74, 105, MediaSize.MM, MediaSizeName.ISO_A7);

    /**
     * ISO A8 paper, 52 mm x 74 mm
     */
    public static final MediaSize A8 = new MediaSize(52, 74, MediaSize.MM, MediaSizeName.ISO_A8);

    /**
     * ISO A9 paper, 37 mm x 52 mm
     */
    public static final MediaSize A9 = new MediaSize(37, 52, MediaSize.MM, MediaSizeName.ISO_A9);

    /**
     * ISO A10 paper, 26 mm x 37 mm
     */
    public static final MediaSize A10 = new MediaSize(26, 37, MediaSize.MM, MediaSizeName.ISO_A10);


    /**
     * ISO B0 paper, 1000 mm x 1414 mm
     */
    public static final MediaSize B0 = new MediaSize(1000, 1414, MediaSize.MM, MediaSizeName.ISO_B0);

    /**
     * ISO B1 paper, 707 mm x 1000 mm
     */
    public static final MediaSize B1 = new MediaSize(707, 1000, MediaSize.MM, MediaSizeName.ISO_B1);

    /**
     * ISO B2 paper, 500 mm x 707 mm
     */
    public static final MediaSize B2 = new MediaSize(500, 707, MediaSize.MM, MediaSizeName.ISO_B2);

    /**
     * ISO B3 paper, 353 mm x 500 mm
     */
    public static final MediaSize B3 = new MediaSize(353, 500, MediaSize.MM, MediaSizeName.ISO_B3);

    /**
     * ISO B4 paper, 250 mm x 353 mm
     */
    public static final MediaSize B4 = new MediaSize(250, 353, MediaSize.MM, MediaSizeName.ISO_B4);

    /**
     * ISO B5 paper, 176 mm x 250 mm
     */
    public static final MediaSize B5 = new MediaSize(176, 250, MediaSize.MM, MediaSizeName.ISO_B5);

    /**
     * ISO B6 paper, 125 mm x 176 mm
     */
    public static final MediaSize B6 = new MediaSize(125, 176, MediaSize.MM, MediaSizeName.ISO_B6);

    /**
     * ISO B7 paper, 88 mm x 125 mm
     */
    public static final MediaSize B7 = new MediaSize(88, 125, MediaSize.MM, MediaSizeName.ISO_B7);

    /**
     * ISO B8 paper, 62 mm x 88 mm
     */
    public static final MediaSize B8 = new MediaSize(62, 88, MediaSize.MM, MediaSizeName.ISO_B8);

    /**
     * ISO B9 paper, 44 mm x 62 mm
     */
    public static final MediaSize B9 = new MediaSize(44, 62, MediaSize.MM, MediaSizeName.ISO_B9);

    /**
     * ISO B10 paper, 31 mm x 44 mm
     */
    public static final MediaSize B10 = new MediaSize(31, 44, MediaSize.MM, MediaSizeName.ISO_B10);
    
    /**
     * ISO C3 envelope, 324 mm x 458 mm
     */
    public static final MediaSize C3 = new MediaSize(324, 458, MediaSize.MM, MediaSizeName.ISO_C3);

    /**
     * ISO C4 envelope, 229 mm x 324 mm
     */
    public static final MediaSize C4 = new MediaSize(229, 324, MediaSize.MM, MediaSizeName.ISO_C4);

    /**
     * ISO C5 envelope, 162 mm x 229 mm
     */
    public static final MediaSize C5 = new MediaSize(162, 229, MediaSize.MM, MediaSizeName.ISO_C5);

    /**
     * ISO C6 envelope, 114 mm x 162 mm
     */
    public static final MediaSize C6 = new MediaSize(114, 162, MediaSize.MM, MediaSizeName.ISO_C6);

    /**
     * ISO ISO Designated Long paper, 324 mm x 458 mm
     */
    public static final MediaSize DESIGNATED_LONG = 
      new MediaSize(324, 458, MediaSize.MM, MediaSizeName.ISO_DESIGNATED_LONG);
  } 

  /**
   * Container class for predefined North American media sizes.
   * 
   * @author Sven de Marothy (sven@physto.se)
   */
  public static final class NA
  {
    private NA()
    {
      // prevent instantiation
    }
    
    /**
     * US Legal paper size, 8.5 inch x 14 inch
     */
    public static final MediaSize LEGAL = new MediaSize(8.5f, 14f, MediaSize.INCH, 
						  MediaSizeName.NA_LEGAL);

    /**
     * US Letter paper size, 8.5 inch x 11 inch
     */
    public static final MediaSize LETTER = new MediaSize(8.5f, 11f, MediaSize.INCH,
						   MediaSizeName.NA_LETTER);

    /**
     * 5 inch x 7 inch paper size.
     */
    public static final MediaSize NA_5X7 = new MediaSize(5, 7, MediaSize.INCH,
							 MediaSizeName.NA_5X7);

    /**
     * 8 inch x 10 inch paper size.
     */
    public static final MediaSize NA_8X10 = new MediaSize(8, 10, MediaSize.INCH,
							  MediaSizeName.NA_8X10);

    /**
     * 6 inch x 9 inch envelope size.
     */
    public static final MediaSize NA_6X9_ENVELOPE = new MediaSize(6f, 9f, 
								  MediaSize.INCH,
								  MediaSizeName.NA_6X9_ENVELOPE);

    /**
     * 7 inch x 9 inch envelope size.
     */
    public static final MediaSize NA_7X9_ENVELOPE = new MediaSize(7f, 9f, 
								  MediaSize.INCH,
								  MediaSizeName.NA_7X9_ENVELOPE);

    /**
     * 9 inch x 11 inch envelope size.
     */
    public static final MediaSize NA_9x11_ENVELOPE = new MediaSize(9f, 11f, 
							     MediaSize.INCH,
							     MediaSizeName.NA_9X11_ENVELOPE);

    /**
     * 9 inch x 12 inch envelope size.
     */
    public static final MediaSize NA_9x12_ENVELOPE = new MediaSize(9f, 12f, 
							     MediaSize.INCH,
							     MediaSizeName.NA_9X12_ENVELOPE);


    /**
     * 10 inch x 13 inch envelope size.
     */
    public static final MediaSize NA_10x13_ENVELOPE = new MediaSize(10f, 13f, 
							      MediaSize.INCH,
							      MediaSizeName.NA_10X13_ENVELOPE);

    /**
     * 10 inch x 14 inch envelope size.
     */
    public static final MediaSize NA_10x14_ENVELOPE = new MediaSize(10f, 14f, 
							      MediaSize.INCH,
							      MediaSizeName.NA_10X14_ENVELOPE);

    /**
     * 10 inch x 15 inch envelope size.
     */
    public static final MediaSize NA_10X15_ENVELOPE = new MediaSize(10f, 15f, 
							      MediaSize.INCH,
							      MediaSizeName.NA_10X15_ENVELOPE);

    /**
     * Number 9 envelope size. 4.5 inch x 10.375 inch
     */
    public static final MediaSize NA_NUMBER_9_ENVELOPE = new MediaSize(3.875f, 8.875f,
								 MediaSize.INCH,
								 MediaSizeName.NA_NUMBER_9_ENVELOPE);

    /**
     * Number 10 envelope size. 4.125 inch x 9.5 inch
     */
    public static final MediaSize NA_NUMBER_10_ENVELOPE = 
      new MediaSize(4.125f, 9.5f, MediaSize.INCH, MediaSizeName.NA_NUMBER_10_ENVELOPE);

    /**
     * Number 11 envelope size. 4.5 inch x 10.375 inch
     */
    public static final MediaSize NA_NUMBER_11_ENVELOPE = new MediaSize(4.5f, 10.375f, MediaSize.INCH,
								  MediaSizeName.NA_NUMBER_11_ENVELOPE);
    
    /**
     * Number 12 envelope size. 4.75 inch x 11 inch
     */
    public static final MediaSize NA_NUMBER_12_ENVELOPE = new MediaSize(4.75f, 11f, 
								  MediaSize.INCH,
								  MediaSizeName.NA_NUMBER_12_ENVELOPE);

  /**
   * Number 14 envelope size. 5 inch x 11.5 inch
   */
  public static final MediaSize NA_NUMBER_14_ENVELOPE = new MediaSize(5f, 11.5f, 
								MediaSize.INCH,
								MediaSizeName.NA_NUMBER_14_ENVELOPE);
  }

  /**
   * Container class for predefined US Engineering media sizes.
   * 
   * @author Sven de Marothy (sven@physto.se)
   */
  public static final class Engineering 
  {
    private Engineering()
    {
      // prevent instantiation
    }
    
    /**
     * ANSI A paper size. 8.5 inch x 11 inch
     */
    public static final MediaSize A = new MediaSize(8.5f, 11f, 
					      MediaSize.INCH, MediaSizeName.A);

    /**
     * ANSI B paper size. 11 inch x 17 inch
     */
    public static final MediaSize B = new MediaSize(11f, 17f, 
					      MediaSize.INCH, MediaSizeName.B);

    /**
     * ANSI C paper size. 17 inch x 22 inch
     */
    public static final MediaSize C = new MediaSize(17f, 22f, 
					      MediaSize.INCH, MediaSizeName.C);

    /**
     * ANSI D paper size. 22 inch x 34 inch
     */
    public static final MediaSize D = new MediaSize(22f, 34f, 
					      MediaSize.INCH, MediaSizeName.D);

    /**
     * ANSI E paper size. 33 inch x 44 inch
     */
    public static final MediaSize E = new MediaSize(34f, 44f, 
					      MediaSize.INCH, MediaSizeName.E);
  }

  /**
   * Container class for predefined Japanese JIS media sizes.
   * 
   * @author Sven de Marothy (sven@physto.se)
   */
  public static final class JIS 
  {
    private JIS()
    {
      // prevent instantiation
    }
    
    /**
     * JIS B0 paper. 1030 mm x 1456 mm
     * Note: The JIS B-series is not identical to the ISO B-series.
     */
    public static final MediaSize B0 = new MediaSize(1030, 1456, MediaSize.MM, MediaSizeName.JIS_B0);

    /**
     * JIS B1 paper. 1030 mm x 1456 mm
     * Note: The JIS B-series is not identical to the ISO B-series.
     */
    public static final MediaSize B1 = new MediaSize(728, 1030, MediaSize.MM, MediaSizeName.JIS_B1);

    /**
     * JIS B2 paper. 515 mm x 728 mm
     * Note: The JIS B-series is not identical to the ISO B-series.
     */
    public static final MediaSize B2 = new MediaSize(515, 728, MediaSize.MM, MediaSizeName.JIS_B2);

    /**
     * JIS B3 paper. 364 mm x 515 mm
     * Note: The JIS B-series is not identical to the ISO B-series.
     */
    public static final MediaSize B3 = new MediaSize(364, 515, MediaSize.MM, MediaSizeName.JIS_B3);

    /**
     * JIS B4 paper. 257 mm x 364 mm
     * Note: The JIS B-series is not identical to the ISO B-series.
     */
    public static final MediaSize B4 = new MediaSize(257, 364, MediaSize.MM, MediaSizeName.JIS_B4);

    /**
     * JIS B5 paper. 1030 mm x 1456 mm
     * Note: The JIS B-series is not identical to the ISO B-series.
     */
    public static final MediaSize B5 = new MediaSize(182, 257, MediaSize.MM, MediaSizeName.JIS_B5);

    /**
     * JIS B6 paper. 128 mm x 182 mm
     * Note: The JIS B-series is not identical to the ISO B-series.
     */
    public static final MediaSize B6 = new MediaSize(128, 182, MediaSize.MM, MediaSizeName.JIS_B6);

    /**
     * JIS B7 paper. 91 mm x 128 mm
     * Note: The JIS B-series is not identical to the ISO B-series.
     */
    public static final MediaSize B7 = new MediaSize(91, 128, MediaSize.MM, MediaSizeName.JIS_B7);

    /**
     * JIS B8 paper. 64 mm x 91 mm
     * Note: The JIS B-series is not identical to the ISO B-series.
     */
    public static final MediaSize B8 = new MediaSize(64, 91, MediaSize.MM, MediaSizeName.JIS_B8);

    /**
     * JIS B9 paper. 45 mm x 64 mm
     * Note: The JIS B-series is not identical to the ISO B-series.
     */
    public static final MediaSize B9 = new MediaSize(45, 64, MediaSize.MM, MediaSizeName.JIS_B9);

    /**
     * JIS B10 paper. 32 mm x 45 mm
     * Note: The JIS B-series is not identical to the ISO B-series.
     */
    public static final MediaSize B10 = new MediaSize(32, 45, MediaSize.MM, MediaSizeName.JIS_B10);

    /**
     * JIS chou #1 envelope size, 142 mm x 332 mm
     */
    public static final MediaSize CHOU_1 = new MediaSize(142, 332, MediaSize.MM);

    /**
     * JIS chou #2 envelope size, 119 mm x 227 mm
     */
    public static final MediaSize CHOU_2 = new MediaSize(119, 227, MediaSize.MM);

    /**
     * JIS chou #3 envelope size, 120 mm x 235 mm
     */
    public static final MediaSize CHOU_3 = new MediaSize(120, 235, MediaSize.MM);

    /**
     * JIS chou #4 envelope size, 90 mm x 205 mm
     */
    public static final MediaSize CHOU_4 = new MediaSize(90, 205, MediaSize.MM);

    /**
     * JIS chou #30 envelope size, 92 mm x 235 mm
     */
    public static final MediaSize CHOU_30 = new MediaSize(92, 235, MediaSize.MM);

    /**
     * JIS chou #40 envelope size, 90 mm x 225 mm
     */
    public static final MediaSize CHOU_40 = new MediaSize(90, 225, MediaSize.MM);

    /**
     * JIS kaku #0 envelope size, 287 mm x 382 mm
     */
    public static final MediaSize KAKU_0 = new MediaSize(287, 382, MediaSize.MM);

    /**
     * JIS kaku #1 envelope size, 270 mm x 382 mm
     */
    public static final MediaSize KAKU_1 = new MediaSize(270, 382, MediaSize.MM);

    /**
     * JIS kaku #2 envelope size, 240 mm x 332 mm
     */
    public static final MediaSize KAKU_2 = new MediaSize(240, 332, MediaSize.MM);

    /**
     * JIS kaku #20 envelope size, 229 mm x 324 mm
     */
    public static final MediaSize KAKU_20 = new MediaSize(229, 324, MediaSize.MM);

    /**
     * JIS kaku #3 envelope size, 216 mm x 227 mm
     */
    public static final MediaSize KAKU_3 = new MediaSize(216, 227, MediaSize.MM);

    /**
     * JIS kaku #4 envelope size, 197 mm x 267 mm
     */
    public static final MediaSize KAKU_4 = new MediaSize(197, 267, MediaSize.MM);

    /**
     * JIS kaku #5 envelope size, 190 mm x 240 mm
     */
    public static final MediaSize KAKU_5 = new MediaSize(190, 240, MediaSize.MM);

    /**
     * JIS kaku #6 envelope size, 162 mm x 229 mm
     */
    public static final MediaSize KAKU_6 = new MediaSize(162, 229, MediaSize.MM);

    /**
     * JIS kaku #7 envelope size, 142 mm x 205 mm
     */
    public static final MediaSize KAKU_7 = new MediaSize(142, 205, MediaSize.MM);

    /**
     * JIS kaku #8 envelope size, 119 mm x 197 mm
     */
    public static final MediaSize KAKU_8 = new MediaSize(119, 197, MediaSize.MM);

    /**
     * JIS kaku A4 envelope size, 228 mm x 312 mm
     */
    public static final MediaSize KAKU_A4 = new MediaSize(228, 312, MediaSize.MM);

    /**
     * JIS you #1 envelope size, 120 mm x 176 mm
     */
    public static final MediaSize YOU_1 = new MediaSize(120, 176, MediaSize.MM);

    /**
     * JIS you #2 envelope size, 114 mm x 162 mm
     */
    public static final MediaSize YOU_2 = new MediaSize(114, 162, MediaSize.MM);

    /**
     * JIS you #3 envelope size, 98 mm x 148 mm
     */
    public static final MediaSize YOU_3 = new MediaSize(98, 148, MediaSize.MM);

    /**
     * JIS you #4 envelope size, 105 mm x 235 mm
     */
    public static final MediaSize YOU_4 = new MediaSize(105, 235, MediaSize.MM);

    /**
     * JIS you #5 envelope size, 95 mm x 217 mm
     */
    public static final MediaSize YOU_5 = new MediaSize(95, 217, MediaSize.MM);

    /**
     * JIS you #6 envelope size, 98 mm x 190 mm
     */
    public static final MediaSize YOU_6 = new MediaSize(98, 190, MediaSize.MM);

    /**
     * JIS you #7 envelope size, 92 mm x 165 mm
     */
    public static final MediaSize YOU_7 = new MediaSize(92, 165, MediaSize.MM);
  }

  /**
   * Container class for miscellaneous media sizes.
   * 
   * @author Sven de Marothy (sven@physto.se)
   */
  public static final class Other
  {
    private Other()
    {
      // prevent instantiation
    }
    
    /**
     * US Executive paper size, 7.25 inch x 10.5 inch
     */
    public static final MediaSize EXECUTIVE = new MediaSize(7.25f, 10.5f, 
						      MediaSize.INCH, MediaSizeName.EXECUTIVE);

    /**
     * US Folio paper size, 8.5 inch x 13 inch
     */
    public static final MediaSize FOLIO = new MediaSize(8.5f, 13f, MediaSize.INCH, MediaSizeName.FOLIO);

    /**
     * US Quarto paper size, 8.5 inches by 10.83 inches.
     */
    public static final MediaSize QUARTO = new MediaSize(8.5f, 10.83f, MediaSize.INCH,
						   MediaSizeName.QUARTO);

    /**
     * US Invoice size, 5.5 inch x 8.5 inch
     */
    public static final MediaSize INVOICE = new MediaSize(5.5f, 8.5f, 
						    MediaSize.INCH, MediaSizeName.INVOICE);

    /**
     * US Ledger size, 11 inch x 17 inch
     */
    public static final MediaSize LEDGER = new MediaSize(11, 17, MediaSize.INCH, 
						   MediaSizeName.LEDGER);

    /**
     * Monarch (7 3/4) envelope size, 3.87 inch x 7.5 inch
     */
    public static final MediaSize MONARCH_ENVELOPE = new MediaSize(3.87f, 7.5f, 
							     MediaSize.INCH,
							     MediaSizeName.MONARCH_ENVELOPE);

    /**
     * Personal envelope size, 3.625 inch x 6.5 inch.
     */
    public static final MediaSize PERSONAL_ENVELOPE = new MediaSize(3.625f, 6.5f, MediaSize.INCH,
							      MediaSizeName.PERSONAL_ENVELOPE);

    /**
     * Italian envelope size, 110 mm x 230 mm
     */
    public static final MediaSize ITALY_ENVELOPE = new MediaSize(110, 230, 
							   MediaSize.MM,
							   MediaSizeName.ITALY_ENVELOPE);

    /**
     * Japanese postcard, 100 mm x 148 mm
     */
    public static final MediaSize JAPANESE_POSTCARD = new MediaSize(100, 148, MediaSize.MM, MediaSizeName.JAPANESE_POSTCARD);

    /**
     * Japanese double postcard, 148 mm x 200 mm
     */
    public static final MediaSize JAPANESE_DOUBLE_POSTCARD = new MediaSize(148, 200, MediaSize.MM, MediaSizeName.JAPANESE_DOUBLE_POSTCARD);
    
    /**
     * Tabloid size, 11 inch x 17 inch.
     * @since 1.5
     */
    public static final MediaSize TABLOID = 
      new MediaSize(11, 17, Size2DSyntax.INCH, MediaSizeName.TABLOID);
  }
}

