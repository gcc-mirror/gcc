/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.awt.color;

/**
 * @author Rolf W. Rasmussen <rolfwr@ii.uib.no>
 */
public abstract class ColorSpace
{
  public static final int TYPE_XYZ   = 0;
  public static final int TYPE_Lab   = 1;
  public static final int TYPE_Luv   = 2;
  public static final int TYPE_YCbCr = 3;
  public static final int TYPE_Yxy   = 4;
  public static final int TYPE_RGB   = 5;
  public static final int TYPE_GRAY  = 6;
  public static final int TYPE_HSV   = 7;
  public static final int TYPE_HLS   = 8;
  public static final int TYPE_CMYK  = 9;
  // mysterious gap in the enumeration sequenece
  public static final int TYPE_CMY  = 11;
  public static final int TYPE_2CLR = 12;
  public static final int TYPE_3CLR = 13;
  public static final int TYPE_4CLR = 14;
  public static final int TYPE_5CLR = 15;
  public static final int TYPE_6CLR = 16;
  public static final int TYPE_7CLR = 17;
  public static final int TYPE_8CLR = 18;
  public static final int TYPE_9CLR = 19;
  public static final int TYPE_ACLR = 20;
  public static final int TYPE_BCLR = 21;
  public static final int TYPE_CCLR = 22;
  public static final int TYPE_DCLR = 23;
  public static final int TYPE_ECLR = 24;
  public static final int TYPE_FCLR = 25;
  
  public static final int CS_sRGB       = 1000;
  public static final int CS_CIEXYZ     = 1001;
  public static final int CS_PYCC       = 1002;
  public static final int CS_GRAY       = 1003;
  public static final int CS_LINEAR_RGB = 1004;
  
  private static final int CS_BASE  = CS_sRGB;
  private static final int CS_END   = CS_LINEAR_RGB+1;
  private static final int CS_COUNT = CS_END - CS_BASE;
  
  // Instances are lazily instantiated
  private static final ColorSpace[] INSTANCES = new ColorSpace[CS_COUNT];

  private int type;
  private int numcomponents;
  protected ColorSpace(int type, int numcomponents)
  {
    this.type = type;
    this.numcomponents = numcomponents;
  }
	
  public static ColorSpace getInstance(int colorspace)
  {
    if ((colorspace >= CS_BASE) && (colorspace < CS_END))
      {
	int instanceIndex = colorspace - CS_BASE;
	if (INSTANCES[instanceIndex] == null)
	  {
	    ICC_Profile profile = new ICC_Profile(colorspace);
	    INSTANCES[instanceIndex] = new ICC_ColorSpace(profile);
	  }
	return INSTANCES[instanceIndex];
      }
    throw new IllegalArgumentException("unknown/unsupported colorspace");
  }
  
  public boolean isCS_sRGB()
  {
    return false;
  }

  public abstract float[] toRGB(float[] colorvalue);
  
  public abstract float[] fromRGB(float[] rgbvalue);
  
  public abstract float[] toCIEXYZ(float[] colorvalue);
  
  public abstract float[] fromCIEXYZ(float[] colorvalue);

  public int getType()
  {
    return type;
  }

  public int getNumComponents()
  {
    return numcomponents;
  }
  
  public String getName(int idx)
  {
    return "type " + type;
  }
  
  public String toString()
  {
    return getClass().getName() + "[type=" + type + "]";
  }
}
