/* Copyright (C) 1999, 2000  Free Software Foundation

   This file is part of libjava.

This software is copyrighted work licensed under the terms of the
Libjava License.  Please consult the file "LIBJAVA_LICENSE" for
details.  */

package java.awt;
import java.awt.geom.*;

/**
 * @author Per Bothner <bothner@cygnus.com>
 * @date February 8, 1999.
 */

/* Written using "Java Class Libraries", 2nd edition.
 * Status:  Believed complete and correct to JDK 1.2.
 */

public interface Shape
{
  public boolean contains (double x, double y);
  public boolean contains (double x, double y, double w, double h);
  public boolean contains (Point2D p);
  public boolean contains (Rectangle2D r);
  public Rectangle getBounds ();
  public Rectangle2D getBounds2D ();
  public PathIterator getPathIterator (AffineTransform at);
  public PathIterator getPathIterator (AffineTransform at, double flatness);
  public boolean intersects (double x, double y, double w, double h);
  public boolean intersects (Rectangle2D r);
}
