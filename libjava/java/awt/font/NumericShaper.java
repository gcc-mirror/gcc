/* NumericShaper.java
   Copyright (C) 2003 Free Software Foundation, Inc.

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

Linking this library statically or dynamically with other modules is
making a combined work based on this library.  Thus, the terms and
conditions of the GNU General Public License cover the whole
combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent
modules, and to copy and distribute the resulting executable under
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */


package java.awt.font;

import java.io.Serializable;

/**
 * @author Michael Koch
 * @since 1.4
 */
public final class NumericShaper implements Serializable
{
  private static final long serialVersionUID = -8022764705923730308L;
  
  public static final int ALL_RANGES  = 524287;
  public static final int ARABIC  = 2;
  public static final int BENGALI  = 16;
  public static final int DEVANAGARI  = 8;
  public static final int EASTERN_ARABIC  = 4;
  public static final int ETHIOPIC  = 65536;
  public static final int EUROPEAN  = 1;
  public static final int GUJARATI  = 64;
  public static final int GURMUKHI  = 32;
  public static final int KANNADA  = 1024;
  public static final int KHMER  = 131072;
  public static final int LAO  = 8192;
  public static final int MALAYALAM  = 2048;
  public static final int MONGOLIAN  = 262144;
  public static final int MYANMAR  = 32768;
  public static final int ORIYA  = 128;
  public static final int TAMIL  = 256;
  public static final int TELUGU  = 512;
  public static final int THAI  = 4096;
  public static final int TIBETAN  = 16384;

  private int ranges;
  private int context;
  
  private NumericShaper (int ranges, int context)
  {
    this.ranges = ranges;
    this.context = context;
  }

  public boolean equals (Object obj)
  {
    if (! (obj instanceof NumericShaper))
      return false;

    NumericShaper tmp = (NumericShaper) obj;
    
    return (ranges == tmp.ranges
            && context == tmp.context);
  }

  public static NumericShaper getContextualShaper (int ranges)
  {
    throw new Error ("not implemented");
  }

  public static NumericShaper getContextualShaper (int ranges,
                                                   int defaultContext)
  {
    throw new Error ("not implemented");
  }

  public int getRanges ()
  {
    return ranges;
  }

  public static NumericShaper getShaper (int singleRange)
  {
    throw new Error ("not implemented");
  }

  public int hashCode ()
  {
    throw new Error ("not implemented");
  }

  public boolean isContextual ()
  {
    throw new Error ("not implemented");
  }

  public void shape (char[] text, int start, int count)
  {
    shape (text, start, count, context);
  }

  public void shape (char[] text, int start, int count, int context)
  {
    throw new Error ("not implemented");
  }

  public String toString ()
  {
    throw new Error ("not implemented");
  }
}
