/* GdkFontMetrics.java
   Copyright (C) 1999, 2002, 2004  Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

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


package gnu.java.awt.peer.gtk;

import gnu.java.awt.ClasspathToolkit;

import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Toolkit;

public class GdkFontMetrics extends FontMetrics
{
  
  private int[] font_metrics;
  GdkFontPeer peer;

  static final int FONT_METRICS_ASCENT = 0;
  static final int FONT_METRICS_MAX_ASCENT = 1;
  static final int FONT_METRICS_DESCENT = 2;
  static final int FONT_METRICS_MAX_DESCENT = 3;
  static final int FONT_METRICS_MAX_ADVANCE = 4;

  static final int TEXT_METRICS_X_BEARING = 0;
  static final int TEXT_METRICS_Y_BEARING = 1;
  static final int TEXT_METRICS_WIDTH = 2;
  static final int TEXT_METRICS_HEIGHT = 3;
  static final int TEXT_METRICS_X_ADVANCE = 4;
  static final int TEXT_METRICS_Y_ADVANCE = 5;


  public GdkFontMetrics (Font font)
  {    
    super (font.getPeer() instanceof GdkFontPeer 
           ? font 
           : ((ClasspathToolkit)(Toolkit.getDefaultToolkit ()))
           .getFont (font.getName(), font.getAttributes ()));
    
    peer = (GdkFontPeer) this.font.getPeer();

    font_metrics = new int[5];
    double [] hires = new double[5];
    peer.getFontMetrics (hires);
    for (int i = 0; i < 5; ++i)
      font_metrics[i] = (int) hires[i];
  }
  
  public int stringWidth (String str)
  {
    double [] hires = new double[6];
    peer.getTextMetrics(str, hires);
    return (int) hires [TEXT_METRICS_WIDTH];
  }

  public int charWidth (char ch)
  {
    return stringWidth (new String (new char[] { ch }));
  }

  public int charsWidth (char data[], int off, int len)
  {
    return stringWidth (new String (data, off, len));
  }

  /* 
     Sun's Motif implementation always returns 0 or 1 here (???), but
     going by the X11 man pages, it seems as though we should return
     font.ascent + font.descent.
  */
  public int getLeading ()
  {
    return 1;
  }

  public int getAscent ()
  {
    return font_metrics[FONT_METRICS_ASCENT];
  }

  public int getMaxAscent ()
  {
    return font_metrics[FONT_METRICS_MAX_ASCENT];
  }

  public int getDescent ()
  {
    return font_metrics[FONT_METRICS_DESCENT];
  }

  public int getMaxDescent ()
  {
    return font_metrics[FONT_METRICS_MAX_DESCENT];
  }

  public int getMaxAdvance ()
  {
    return font_metrics[FONT_METRICS_MAX_ADVANCE];
  }
}
