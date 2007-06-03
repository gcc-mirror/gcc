/* AutoHinter.java -- The entry point into the hinter implementation.
   Copyright (C) 2006 Free Software Foundation, Inc.

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


package gnu.java.awt.font.autofit;

import gnu.java.awt.font.opentype.Hinter;
import gnu.java.awt.font.opentype.OpenTypeFont;
import gnu.java.awt.font.opentype.truetype.Fixed;
import gnu.java.awt.font.opentype.truetype.Zone;

/**
 * The public interface to the automatic gridfitter.
 */
public class AutoHinter
  implements Hinter
{
  Latin latinScript;
  LatinMetrics metrics;
  GlyphHints hints;

  HintScaler scaler = new HintScaler();
  public void init(OpenTypeFont font)
  {
    // TODO: Should support other scripts too.
    latinScript = new Latin();
    metrics = new LatinMetrics(font);
    latinScript.initMetrics(metrics, font);
    scaler.face = font;
  }

  public void applyHints(Zone outline)
  {
    if (hints == null)
      hints = new GlyphHints();
    scaler.xScale = Fixed.valueOf16(outline.scaleX * 64);
    scaler.yScale = Fixed.valueOf16(outline.scaleY * 64);
    latinScript.scaleMetrics(metrics, scaler);
    latinScript.applyHints(hints, outline, metrics);
  }

  public void setFlags(int flags)
  {
    if (hints == null)
      hints = new GlyphHints();
    hints.flags = flags;
  }

}
