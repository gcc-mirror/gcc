/* Copyright (C) 1999  Red Hat, Inc.

   This file is part of libjava.

This software is copyrighted work licensed under the terms of the
Libjava License.  Please consult the file "LIBJAVA_LICENSE" for
details.  */

package java.awt;
import java.awt.geom.Point2D;

/**
 * @author Per Bothner <bothner@cygnus.com>
 * @date February 8, 1999.
 */

/* Written using "Java Class Libraries", 2nd edition, plus online
 * API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete and correct, except that neither toString
 * nor hashCode have been compared with JDK output.
 */

public class Point extends Point2D implements java.io.Serializable
{
  public int x;
  public int y;

  public Point () { }

  public Point (Point p) { this.x = p.x;  this.y = p.y; }

  public Point (int x, int y) { this.x = x;  this.y = y; }

  public boolean equals (Object obj)
  {
    if (! (obj instanceof Point))
      return false;
    Point p = (Point) obj;
    return this.x == p.x && this.y == p.y;
  }

  public int hashCode () { return x ^ y; }

  public Point getLocation () { return new Point(this); }

  public void move (int x, int y) { this.x = x;  this.y = y; }

  public void setLocation (int x, int y) { this.x = x;  this.y = y; }

  public void setLocation (Point pt) { this.x = pt.x;  this.y = pt.y; }

  public void translate (int x, int y) { this.x += x;  this.y += y; }

  public String toString ()
  {
    return "Point[x:"+x+",y:"+y+']';
  }

  public double getX() { return x; }
  public double getY() { return y; }

  public void setLocation (double x, double y)
  { this.x = (int) x;  this.y = (int) y; }

}
