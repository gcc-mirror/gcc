/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.awt.image;

import java.awt.Image;

public interface ImageObserver 
{
  int WIDTH      = 1 << 0;
  int HEIGHT     = 1 << 1;
  int PROPERTIES = 1 << 2;
  int SOMEBITS   = 1 << 3;
  int FRAMEBITS  = 1 << 4;
  int ALLBITS    = 1 << 5;
  int ERROR      = 1 << 6;
  int ABORT      = 1 << 7;

  boolean imageUpdate(Image image, int infoFlags, int x, int y, int width, 
                      int height);
}


