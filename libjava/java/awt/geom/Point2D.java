/* Copyright (C) 1999  Red Hat, Inc.

   This file is part of libjava.

This software is copyrighted work licensed under the terms of the
Libjava License.  Please consult the file "LIBJAVA_LICENSE" for
details.  */

package java.awt.geom;

/**
 * @author Per Bothner <bothner@cygnus.com>
 * @date Fenruary 8, 1999.
 */

/* Written using "Java Class Libraries", 2nd edition, plus online
 * API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete and correct, except that neither toString
 * nor hashCode have been compared with JDK output.
 */

public abstract class Point2D implements Cloneable
{
  public abstract double getX();
  public abstract double getY();

  public abstract void setLocation (double x, double y);

  public void setLocation (Point2D pt)  { setLocation(pt.getX(), pt.getY()); }

  static public double distanceSq (double X1, double Y1, double X2, double Y2)
  {
    X2 -= X1;
    Y2 -= Y1;
    return X2*X2 + Y2*Y2;
  }

  static public double distance (double X1, double Y1, double X2, double Y2)
  {
    return Math.sqrt(distance(X1, Y1, X2, Y2));
  }

  public double distanceSq (double PX, double PY)
  {
    return distanceSq (getX(), PX, getY(), PY);
  }

  public double distance (double PX, double PY)
  {
    return distance (getX(), PX, getY(), PY);
  }

  public double distanceSq (Point2D pt)
  {
    return distanceSq (getX(), pt.getX(), getY(), pt.getY());
  }

  public double distance (Point2D pt)
  {
    return distance (getX(), pt.getX(), getY(), pt.getY());
  }

  public int hashCode() { return (int) getX() ^ (int) getY(); }

  public Object clone()
  {
    return super.clone();
  }
}
