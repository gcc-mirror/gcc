/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.awt;

public class Insets implements Cloneable, java.io.Serializable
{
  public int top;
  public int left;
  public int bottom;
  public int right;
  
  public Insets(int top, int left, int bottom, int right)
  {
    this.top = top;
    this.left = left;
    this.bottom = bottom;
    this.right = right;  
  }

  public boolean equals(Object obj)
  {
    if (obj instanceof Insets)
      {
        Insets i = (Insets) obj;
	return (i.top == top 
        	&& i.left == left 
		&& i.bottom == bottom 
		&& i.right == right);    
      }
    return false;
  }

  public int hashCode()
  {
    // FIXME: what is the correct algorithm for this?
    return (top * (2 * left) * (3 * right) * (4 * bottom));
  }

  public String toString()
  {
    return (getClass() + "[top=" + top + ",left=" + left + ",bottom=" 
            + bottom + ",right=" + right + "]");
  }
  
  public Object clone()
  {
    Insets r = new Insets(top, left, bottom, right);
    return r;
  }
}
