/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.awt.image;
import java.util.Hashtable;

public interface ImageConsumer
{

  public static final int RANDOMPIXELORDER  = 1 << 0,
			  TOPDOWNLEFTRIGHT  = 1 << 1,
			  COMPLETESCANLINES = 1 << 2,
			  SINGLEPASS        = 1 << 3,
			  SINGLEFRAME       = 1 << 4;
			  
  public static final int IMAGEERROR        = 1,
			  SINGLEFRAMEDONE   = 2,
			  STATICIMAGEDONE   = 3,
			  IMAGEABORTED      = 4;

  public void setDimensions(int width, int height);
  public void setProperties(Hashtable props);
  public void setColorModel(ColorModel model);
  public void setHints(int hintflags);
  public void setPixels(int x, int y, int w, int h, ColorModel model,
                	byte[] pixels, int off, int scansize);
  public void setPixels(int x, int y, int w, int h, ColorModel model,
                	int[] pixels, int off, int scansize);
  public void imageComplete(int status);
}
