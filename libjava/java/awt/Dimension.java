/* Copyright (C) 1999  Red Hat, Inc.

   This file is part of libjava.

This software is copyrighted work licensed under the terms of the
Libjava License.  Please consult the file "LIBJAVA_LICENSE" for
details.  */

package java.awt;

/**
 * @author Per Bothner <bothner@cygnus.com>
 * @date Fenruary 8, 1999.
 */

/* Written using "Java Class Libraries", 2nd edition, plus online
 * API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete and correct, except that neither toString
 * has not been compared with JDK output.
 */

public class Dimension extends java.awt.geom.Dimension2D
{
  public int height;
  public int width;

  public Dimension () { }

  public Dimension (Dimension dim)
  {
    this.width = dim.width;
    this.height = dim.height;
  }

  public Dimension (int width, int height)
  {
    this.width = width;
    this.height = height;
  }

  public boolean equals (Object obj)
  {
    if (! (obj instanceof Dimension))
      return false;
    Dimension dim = (Dimension) obj;
    return height == dim.height && width == dim.width;
  }

  public Dimension getSize () { return new Dimension(this); }

  public void setSize (Dimension dim)
  {
    this.width = dim.width;
    this.height = dim.height;
  }

  public void setSize (int width, int height)
  {
    this.width = width;
    this.height = height;
  }

  public String toString ()
  {
    return "Dimension[w:"+width+",h:"+height+']';
  }

  /* Note:  There is no Dimension.hashCode. */

  public double getWidth() { return width; }
  public double getHeight() { return height; }

  public void setSize (double width, double height)
  {
    this.width = (int) width;
    this.height = (int) height;
  }
}
