/* Copyright (C) 1999, 2000, 2002  Free Software Foundation

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

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */

package java.awt.geom;

/**
 * @author Per Bothner <bothner@cygnus.com>
 * @date February 8, 1999.
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

  protected Point2D ()
  {
  }

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
    try
    {
      return super.clone ();
    } 
    catch (CloneNotSupportedException _) {return null;}
  }

  public boolean equals (Object o)
  {
    if (! (o instanceof Point2D))
      return false;
    Point2D p = (Point2D) o;
    return getX () == p.getX () && getY () == p.getY ();
  }

  public static class Double extends Point2D
  {
    public double x;
    public double y;

    public Double ()
    {
      x = 0;
      y = 0;
    }

    public Double (double x, double y)
    {
      this.x = x;
      this.y = y;
    }

    public double getX ()
    {
      return x;
    }

    public double getY ()
    {
      return y;
    }

    public void setLocation (double x, double y)
    {
      this.x = x;
      this.y = y;
    }

    public String toString ()
    {
      return "(" + x + ", " + y + ")";
    }
  }

  public static class Float extends Point2D
  {
    public float x;
    public float y;

    public Float ()
    {
      x = 0;
      y = 0;
    }

    public Float (float x, float y)
    {
      this.x = x;
      this.y = y;
    }

    public double getX ()
    {
      return x;
    }

    public double getY ()
    {
      return y;
    }

    public void setLocation (double x, double y)
    {
      this.x = (float) x;
      this.y = (float) y;
    }

    public void setLocation (float x, float y)
    {
      this.x = x;
      this.y = y;
    }

    public String toString ()
    {
      return "(" + x + ", " + y + ")";
    }
  }
}
