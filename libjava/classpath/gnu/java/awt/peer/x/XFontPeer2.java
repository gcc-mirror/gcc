/* XFontPeer2.java -- A Java based TTF font peer for X
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

package gnu.java.awt.peer.x;

import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.font.FontRenderContext;
import java.awt.font.GlyphVector;
import java.awt.font.LineMetrics;
import java.awt.geom.AffineTransform;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.io.File;
import java.io.FileInputStream;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.text.CharacterIterator;
import java.text.StringCharacterIterator;
import java.util.Locale;
import java.util.Map;

import gnu.java.awt.font.FontDelegate;
import gnu.java.awt.font.FontFactory;
import gnu.java.awt.peer.ClasspathFontPeer;

public class XFontPeer2
  extends ClasspathFontPeer
{

  private class XLineMetrics
    extends LineMetrics
  {

    private Font font;
//    private CharacterIterator characterIterator;
//    private int begin;
//    private int limit;
    private FontRenderContext fontRenderContext;
    XLineMetrics(Font f, CharacterIterator ci, int b, int l,
                 FontRenderContext rc)
    {
      font = f;
//      characterIterator = ci;
//      begin = b;
//      limit = l;
      fontRenderContext = rc;
    }

    public float getAscent()
    {
      return fontDelegate.getAscent(font.getSize(), fontRenderContext.getTransform(),
                             fontRenderContext.isAntiAliased(),
                             fontRenderContext.usesFractionalMetrics(), true);
      }

    public int getBaselineIndex()
    {
      // FIXME: Implement this.
      throw new UnsupportedOperationException("Not yet implemented");
    }

    public float[] getBaselineOffsets()
    {
      // FIXME: Implement this.
      throw new UnsupportedOperationException("Not yet implemented");
    }

    public float getDescent()
    {
      return (int) fontDelegate.getDescent(font.getSize(),
                                           new AffineTransform(), false, false,
                                           false);
    }

    public float getHeight()
    {
      // FIXME: Implement this.
      throw new UnsupportedOperationException("Not yet implemented");
    }

    public float getLeading()
    {
      // FIXME: Implement this.
      throw new UnsupportedOperationException("Not yet implemented");
    }

    public int getNumChars()
    {
      // FIXME: Implement this.
      throw new UnsupportedOperationException("Not yet implemented");
    }

    public float getStrikethroughOffset()
    {
      return 0.F;
    }

    public float getStrikethroughThickness()
    {
      return 0.F;
    }

    public float getUnderlineOffset()
    {
      return 0.F;
    }

    public float getUnderlineThickness()
    {
      return 0.F;
    }
    
  }

  private class XFontMetrics
    extends FontMetrics
  {
    XFontMetrics(Font f)
    {
      super(f);
    }

    public int getAscent()
    {
      return (int) fontDelegate.getAscent(getFont().getSize(),
                                          new AffineTransform(), false, false,
                                          false);
    }

    public int getDescent()
    {
      return (int) fontDelegate.getDescent(getFont().getSize(),
                                           new AffineTransform(), false, false,
                                           false);
    }
    
    public int getHeight()
    {
      GlyphVector gv = fontDelegate.createGlyphVector(getFont(),
                    new FontRenderContext(new AffineTransform(), false, false),
                    new StringCharacterIterator("m"));
      Rectangle2D b = gv.getVisualBounds();
      return (int) b.getHeight();
    }

    public int charWidth(char c)
    {
      Point2D advance = new Point2D.Double();
      fontDelegate.getAdvance(c, getFont().getSize(), new AffineTransform(),
                              false, false, true, advance);
      return (int) advance.getX();
    }

    public int charsWidth(char[] chars, int offs, int len)
    {
      return stringWidth(new String(chars, offs, len));
    }

    public int stringWidth(String s)
    {
      GlyphVector gv = fontDelegate.createGlyphVector(getFont(),
                    new FontRenderContext(new AffineTransform(), false, false),
                    new StringCharacterIterator(s));
      Rectangle2D b = gv.getVisualBounds();
      return (int) b.getWidth();
    }
  }

  private FontDelegate fontDelegate;

  XFontPeer2(String name, int style, int size)
  {
    super(name, style, size);
    try
      {
        File fontfile = new File("/usr/share/fonts/truetype/ttf-bitstream-vera/Vera.ttf");
        FileInputStream in = new FileInputStream(fontfile);
        FileChannel ch = in.getChannel();
        ByteBuffer buffer = ch.map(FileChannel.MapMode.READ_ONLY, 0,
                                   fontfile.length());
        fontDelegate = FontFactory.createFonts(buffer)[0];
      }
    catch (Exception ex)
      {
        ex.printStackTrace();
      }
  }

  XFontPeer2(String name, Map atts)
  {
    super(name, atts);
    try
      {
        File fontfile = new File("/usr/share/fonts/truetype/freefont/FreeSans.ttf");
        FileInputStream in = new FileInputStream(fontfile);
        FileChannel ch = in.getChannel();
        ByteBuffer buffer = ch.map(FileChannel.MapMode.READ_ONLY, 0,
                                   fontfile.length());
        fontDelegate = FontFactory.createFonts(buffer)[0];
      }
    catch (Exception ex)
      {
        ex.printStackTrace();
      }
  }

  public boolean canDisplay(Font font, char c)
  {
    // FIXME: Implement this.
    throw new UnsupportedOperationException("Not yet implemented");
  }

  public int canDisplayUpTo(Font font, CharacterIterator i, int start, int limit)
  {
    // FIXME: Implement this.
    throw new UnsupportedOperationException("Not yet implemented");
  }

  public String getSubFamilyName(Font font, Locale locale)
  {
    // FIXME: Implement this.
    throw new UnsupportedOperationException("Not yet implemented");
  }

  public String getPostScriptName(Font font)
  {
    // FIXME: Implement this.
    throw new UnsupportedOperationException("Not yet implemented");
  }

  public int getNumGlyphs(Font font)
  {
    // FIXME: Implement this.
    throw new UnsupportedOperationException("Not yet implemented");
  }

  public int getMissingGlyphCode(Font font)
  {
    // FIXME: Implement this.
    throw new UnsupportedOperationException("Not yet implemented");
  }

  public byte getBaselineFor(Font font, char c)
  {
    // FIXME: Implement this.
    throw new UnsupportedOperationException("Not yet implemented");
  }

  public String getGlyphName(Font font, int glyphIndex)
  {
    // FIXME: Implement this.
    throw new UnsupportedOperationException("Not yet implemented");
  }

  public GlyphVector createGlyphVector(Font font, FontRenderContext frc, CharacterIterator ci)
  {
    return fontDelegate.createGlyphVector(font, frc, ci);
  }

  public GlyphVector createGlyphVector(Font font, FontRenderContext ctx, int[] glyphCodes)
  {
    // FIXME: Implement this.
    throw new UnsupportedOperationException("Not yet implemented");
  }

  public GlyphVector layoutGlyphVector(Font font, FontRenderContext frc, char[] chars, int start, int limit, int flags)
  {
    StringCharacterIterator i = new StringCharacterIterator(new String(chars), start, limit, 0);
    return fontDelegate.createGlyphVector(font, frc, i);
  }

  public FontMetrics getFontMetrics(Font font)
  {
    return new XFontMetrics(font);
  }

  public boolean hasUniformLineMetrics(Font font)
  {
    // FIXME: Implement this.
    throw new UnsupportedOperationException("Not yet implemented");
  }

  public LineMetrics getLineMetrics(Font font, CharacterIterator ci, int begin, int limit, FontRenderContext rc)
  {
    return new XLineMetrics(font, ci, begin, limit, rc);
  }

  public Rectangle2D getMaxCharBounds(Font font, FontRenderContext rc)
  {
    // FIXME: Implement this.
    throw new UnsupportedOperationException("Not yet implemented");
  }

}
