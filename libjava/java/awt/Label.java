/* Copyright (C) 2000  Free Software Foundation

   This file is part of libjava.

This software is copyrighted work licensed under the terms of the
Libjava License.  Please consult the file "LIBJAVA_LICENSE" for
details.  */

package java.awt;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date April 12, 2000
 */

/* Status: addNotify() not written.  */

public class Label extends Component
{
  public static final int CENTER = 1;
  public static final int LEFT = 0;
  public static final int RIGHT = 2;

  public Label ()
  {
    this ("", LEFT);
  }

  public Label (String text)
  {
    this (text, LEFT);
  }

  public Label (String text, int alignment)
  {
    if (alignment != CENTER && alignment != LEFT && alignment != RIGHT)
      throw new IllegalArgumentException ();
    this.text = text;
    this.alignment = alignment;
  }

  public void addNotify ()
  {
    // FIXME
  }

  public int getAlignment ()
  {
    return alignment;
  }

  public String getText ()
  {
    return text;
  }

  protected String paramString ()
  {
    return "Label[" + alignment + "," + text + "]";
  }

  public void setAlignment (int alignment)
  {
    if (alignment != CENTER && alignment != LEFT && alignment != RIGHT)
      throw new IllegalArgumentException ();
    this.alignment = alignment;
  }

  public void setText (String text)
  {
    this.text = text;
  }

  private String text;
  private int alignment;
}
