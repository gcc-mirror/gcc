/* GdkClasspathFontPeerMetrics.java
   Copyright (C) 1999, 2002 Free Software Foundation, Inc.

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


package gnu.java.awt.peer.gtk;

import java.awt.*;
import java.awt.font.*;
import java.awt.geom.*;

public class GdkClasspathFontPeerMetrics extends FontMetrics
{
  private final int native_state = GtkGenericPeer.getUniqueInteger();

  private static final int ASCENT = 0, MAX_ASCENT = 1, 
                       DESCENT = 2, MAX_DESCENT = 3, 
                       MAX_ADVANCE = 4;

  private int[] metrics;
  private native int[] initState (Object font);

  public GdkClasspathFontPeerMetrics (Font font)
  {
    super (font);
    metrics = initState (font.getPeer());
  }

  public int stringWidth (String str)
  {
    GlyphVector gv = font.createGlyphVector 
      (new FontRenderContext 
       (new AffineTransform (), false, false), str);
    Rectangle2D r = gv.getVisualBounds ();
    return (int) r.getWidth ();
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
//      return metrics[ASCENT] + metrics[DESCENT];
  }

  public int getAscent ()
  {
    return metrics[ASCENT];
  }

  public int getMaxAscent ()
  {
    return metrics[MAX_ASCENT];
  }

  public int getDescent ()
  {
    return metrics[DESCENT];
  }

  public int getMaxDescent ()
  {
    return metrics[MAX_DESCENT];
  }

  public int getMaxAdvance ()
  {
    return metrics[MAX_ADVANCE];
  }
}
