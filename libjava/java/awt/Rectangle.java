/* Copyright (C) 1999  Red Hat, Inc.

   This file is part of libjava.

This software is copyrighted work licensed under the terms of the
Libjava License.  Please consult the file "LIBJAVA_LICENSE" for
details.  */

package java.awt;

/* Status:  Quite imcomplete. */

public class Rectangle implements Shape
{
  public int x;
  public int y;
  public int width;
  public int height;

  public Rectangle () { }

  public Rectangle (int width, int height)
  { this.width = width;  this.height = height; }

  public Rectangle (int x, int y, int width, int height) 
  {
    this.x = x;  this.y = y;
    this.width = width;  this.height = height;
  }

  public Rectangle getBounds ()
  {
    return new Rectangle (x, y, width, height);
  }
}
