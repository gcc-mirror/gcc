/* Copyright (C) 2000  Free Software Foundation

   This file is part of libjava.

This software is copyrighted work licensed under the terms of the
Libjava License.  Please consult the file "LIBJAVA_LICENSE" for
details.  */

package java.awt.geom;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date April 16, 2000
 */

public interface PathIterator
{
  public static final int SEG_CLOSE = 4;
  public static final int SEG_CUBICTO = 3;
  public static final int SEG_LINETO = 1;
  public static final int SEG_MOVETO = 0;
  public static final int SEG_QUADTO = 2;
  public static final int WIND_EVEN_ODD = 0;
  public static final int WIND_NON_ZERO = 1;

  public int currentSegment (double[] coords);
  public int currentSegment (float[] coords);
  public int getWindingRule ();
  public boolean isDone ();
  public void next ();
}
