/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.awt;

import java.awt.image.ImageObserver;
import java.awt.image.ImageProducer;

/**
 * @author Warren Levy <warrenl@cygnus.com>
 * @date March 15, 2000.
 */

/**
 * Written using on-line Java Platform 1.2 API Specification, as well
 * as "The Java Class Libraries", 2nd edition (Addison-Wesley, 1998).
 * Status:  Mostly complete, but look for FIXMEs.
 */

public abstract class Image extends Object
{
  public static final Object UndefinedProperty = new Object();

  public static final int SCALE_DEFAULT        = 1<<0,
                          SCALE_FAST           = 1<<1,
                          SCALE_SMOOTH         = 1<<2,
                          SCALE_REPLICATE      = 1<<3,
                          SCALE_AREA_AVERAGING = 1<<4;

  public abstract int getWidth(ImageObserver observer);

  public abstract int getHeight(ImageObserver observer);

  public abstract ImageProducer getSource();
  
  public abstract Graphics getGraphics();

  public abstract Object getProperty(String name, ImageObserver observer);

  public Image getScaledInstance(int width, int height, int hints)
  {
    throw new UnsupportedOperationException("FIXME: not implemented yet");
  }

  public abstract void flush();
}
