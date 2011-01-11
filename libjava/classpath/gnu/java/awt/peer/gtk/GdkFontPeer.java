/* GdkFontPeer.java -- Implements FontPeer with GTK+
   Copyright (C) 1999, 2004, 2005, 2006  Free Software Foundation, Inc.

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

import gnu.classpath.Configuration;
import gnu.classpath.Pointer;

import gnu.java.awt.ClasspathToolkit;
import gnu.java.awt.peer.ClasspathFontPeer;
import gnu.java.awt.font.opentype.NameDecoder;

import gnu.java.lang.CPStringBuilder;

import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Toolkit;
import java.awt.font.FontRenderContext;
import java.awt.font.GlyphVector;
import java.awt.font.GlyphMetrics;
import java.awt.font.LineMetrics;
import java.awt.font.TextLayout;
import java.awt.geom.Rectangle2D;
import java.text.CharacterIterator;
import java.util.Locale;
import java.util.Map;
import java.nio.ByteBuffer;
import java.util.HashMap;

public class GdkFontPeer extends ClasspathFontPeer
{
  static final FontRenderContext DEFAULT_CTX =
    new FontRenderContext(null, false, false);

  /**
   * Caches TextLayout instances for use in charsWidth() and drawString().
   * The size of the cache has been chosen so that relativly large GUIs with
   * text documents are still efficient.
   */
  HashMap<String,TextLayout> textLayoutCache = new GtkToolkit.LRUCache<String,TextLayout>(500);

  private class GdkFontMetrics extends FontMetrics
  {

    public GdkFontMetrics (Font font)
    {
      super(initFont(font));
    }

    public int stringWidth (String str)
    {
      TextLayout tl = textLayoutCache.get(str);
      if (tl == null)
        {
          tl = new TextLayout(str, font, DEFAULT_CTX);
          textLayoutCache.put(str, tl);
        }
      return (int) tl.getAdvance();
    }

    public int charWidth (char ch)
    {
      return stringWidth (new String (new char[] { ch }));
    }

    public int charsWidth (char data[], int off, int len)
    {
      return stringWidth (new String (data, off, len));
    }

    public int getHeight()
    {
      return (int) height;
    }

    public int getLeading ()
    {
      return (int) (height - (ascent + descent));
    }

    public int getAscent ()
    {
      return (int) ascent;
    }

    public int getMaxAscent ()
    {
      return (int) ascent;
    }

    public int getDescent ()
    {
      return (int) descent;
    }

    public int getMaxDescent ()
    {
      return (int) maxDescent;
    }

    public int getMaxAdvance ()
    {
      return (int) maxAdvance;
    }
  }

  static native void initStaticState();
  private final int native_state = GtkGenericPeer.getUniqueInteger ();

  /**
   * Cache GlyphMetrics objects.
   */
  private HashMap<Integer,GlyphMetrics> metricsCache;

  private static final int FONT_METRICS_ASCENT = 0;
  private static final int FONT_METRICS_MAX_ASCENT = 1;
  private static final int FONT_METRICS_DESCENT = 2;
  private static final int FONT_METRICS_MAX_DESCENT = 3;
  private static final int FONT_METRICS_MAX_ADVANCE = 4;
  private static final int FONT_METRICS_HEIGHT = 5;
  private static final int FONT_METRICS_UNDERLINE_OFFSET = 6;
  private static final int FONT_METRICS_UNDERLINE_THICKNESS = 7;

  float ascent;
  float descent;
  float maxAscent;
  float maxDescent;
  float maxAdvance;
  float height;
  float underlineOffset;
  float underlineThickness;

  GdkFontMetrics metrics;

  static
  {
    if (true) // GCJ LOCAL
      {
        System.loadLibrary("gtkpeer");
      }

    initStaticState ();

  }

  private ByteBuffer nameTable = null;

  /**
   * The pointer to the native font data.
   *
   * This field is manipulated by native code. Don't change or remove
   * without adjusting the native code.
   */
  private Pointer nativeFont;

  private native void initState ();
  private native void dispose ();
  private native void setFont (String family, int style, int size);

  native synchronized void getFontMetrics(double [] metrics);
  native synchronized void getTextMetrics(String str, double [] metrics);

  native void releasePeerGraphicsResource();


  protected void finalize ()
  {
    releasePeerGraphicsResource();
    dispose ();
  }

  /*
   * Helpers for the 3-way overloading that this class seems to suffer
   * from. Remove them if you feel like they're a performance bottleneck,
   * for the time being I prefer my code not be written and debugged in
   * triplicate.
   */

  private String buildString(CharacterIterator iter)
  {
    CPStringBuilder sb = new CPStringBuilder();
    for(char c = iter.first(); c != CharacterIterator.DONE; c = iter.next())
      sb.append(c);
    return sb.toString();
  }

  private String buildString(CharacterIterator iter, int begin, int limit)
  {
    CPStringBuilder sb = new CPStringBuilder();
    int i = 0;
    for(char c = iter.first(); c != CharacterIterator.DONE; c = iter.next(), i++)
      {
        if (begin <= i)
          sb.append(c);
        if (limit <= i)
          break;
      }
    return sb.toString();
  }

  private String buildString(char[] chars, int begin, int limit)
  {
    return new String(chars, begin, limit - begin);
  }

  /* Public API */

  public GdkFontPeer (String name, int style)
  {
    // All fonts get a default size of 12 if size is not specified.
    this(name, style, 12);
  }

  public GdkFontPeer (String name, int style, int size)
  {
    super(name, style, size);
    initState ();
    setFont (this.familyName, this.style, (int)this.size);
    metricsCache = new HashMap<Integer,GlyphMetrics>();
    setupMetrics();
  }

  public GdkFontPeer (String name, Map attributes)
  {
    super(name, attributes);
    initState ();
    setFont (this.familyName, this.style, (int)this.size);
    metricsCache = new HashMap<Integer,GlyphMetrics>();
    setupMetrics();
  }


  /**
   * Makes sure to return a Font based on the given Font that has as
   * peer a GdkFontPeer. Used in the initializer.
   */
  static Font initFont(Font font)
  {
    if (font == null)
      return new Font("Dialog", Font.PLAIN, 12);
    else if (font.getPeer() instanceof GdkFontPeer)
      return font;
    else
      {
        ClasspathToolkit toolkit;
        toolkit = (ClasspathToolkit) Toolkit.getDefaultToolkit();
        return toolkit.getFont(font.getName(), font.getAttributes());
      }
  }

  private void setupMetrics()
  {
    double [] hires = new double[8];
    getFontMetrics(hires);
    ascent = (float) hires[FONT_METRICS_ASCENT];
    maxAscent = (float) hires[FONT_METRICS_MAX_ASCENT];
    descent = (float) hires[FONT_METRICS_DESCENT];
    maxDescent = (float) hires[FONT_METRICS_MAX_DESCENT];
    maxAdvance = (float) hires[FONT_METRICS_MAX_ADVANCE];
    height = (float) hires[FONT_METRICS_HEIGHT];
    underlineOffset = (float) hires[FONT_METRICS_UNDERLINE_OFFSET];
    underlineThickness = (float) hires[FONT_METRICS_UNDERLINE_THICKNESS];
  }

  /**
   * Unneeded, but implemented anyway.
   */
  public String getSubFamilyName(Font font, Locale locale)
  {
    String name;

    if (locale == null)
      locale = Locale.getDefault();

    name = getName(NameDecoder.NAME_SUBFAMILY, locale);
    if (name == null)
      {
        name = getName(NameDecoder.NAME_SUBFAMILY, Locale.ENGLISH);
        if ("Regular".equals(name))
          name = null;
      }

    return name;
  }

  /**
   * Returns the bytes belonging to a TrueType/OpenType table,
   * Parameters n,a,m,e identify the 4-byte ASCII tag of the table.
   *
   * Returns null if the font is not TT, the table is nonexistant,
   * or if some other unexpected error occured.
   *
   */
  private native byte[] getTrueTypeTable(byte n, byte a, byte m, byte e);

  /**
   * Returns the PostScript name of the font, defaults to the familyName if
   * a PS name could not be retrieved.
   */
  public String getPostScriptName(Font font)
  {
    String name = getName(NameDecoder.NAME_POSTSCRIPT,
                          /* any language */ null);
    if( name == null )
      return this.familyName;

    return name;
  }

  /**
   * Extracts a String from the font&#x2019;s name table.
   *
   * @param name the numeric TrueType or OpenType name ID.
   *
   * @param locale the locale for which names shall be localized, or
   * <code>null</code> if the locale does mot matter because the name
   * is known to be language-independent (for example, because it is
   * the PostScript name).
   */
  private String getName(int name, Locale locale)
  {
    if (nameTable == null)
      {
        byte[] data = getTrueTypeTable((byte)'n', (byte) 'a',
                                       (byte) 'm', (byte) 'e');
        if( data == null )
          return null;

        nameTable = ByteBuffer.wrap( data );
      }

    return NameDecoder.getName(nameTable, name, locale);
  }

  public boolean canDisplay (Font font, int c)
  {
    // FIXME: inquire with pango
    return true;
  }

  public int canDisplayUpTo (Font font, CharacterIterator i, int start, int limit)
  {
    // FIXME: inquire with pango
    return -1;
  }

  public GlyphVector createGlyphVector (Font font,
                                        FontRenderContext ctx,
                                        CharacterIterator i)
  {
    return new FreetypeGlyphVector(font, buildString (i), ctx);
  }

  public GlyphVector createGlyphVector (Font font,
                                        FontRenderContext ctx,
                                        int[] glyphCodes)
  {
    return new FreetypeGlyphVector(font, glyphCodes, ctx);
  }

  public byte getBaselineFor (Font font, char c)
  {
    // FIXME: Actually check.
    return Font.ROMAN_BASELINE;
  }

  private class GdkFontLineMetrics extends LineMetrics
  {
    private int nchars;
    public GdkFontLineMetrics (GdkFontPeer fp, int n)
    {
      nchars = n;
    }

    public float getAscent()
    {
      return ascent;
    }

    public int getBaselineIndex()
    {
      // FIXME
      return Font.ROMAN_BASELINE;
    }

    public float[] getBaselineOffsets()
    {
      return new float[3];
    }

    public float getDescent()
    {
      return descent;
    }

    public float getHeight()
    {
      return height;
    }

    public float getLeading()
    {
      return height - (ascent + descent);
    }

    public int getNumChars()
    {
      return nchars;
    }

    public float getStrikethroughOffset()
    {
      // FreeType doesn't seem to provide a value here.
      return ascent / 2;
    }

    public float getStrikethroughThickness()
    {
      // FreeType doesn't seem to provide a value here.
      return 1.f;
    }

    public float getUnderlineOffset()
    {
      return underlineOffset;
    }

    public float getUnderlineThickness()
    {
      return underlineThickness;
    }

  }

  public LineMetrics getLineMetrics (Font font, CharacterIterator ci,
                                     int begin, int limit, FontRenderContext rc)
  {
    return new GdkFontLineMetrics (this, limit - begin);
  }

  public Rectangle2D getMaxCharBounds (Font font, FontRenderContext rc)
  {
    throw new UnsupportedOperationException ();
  }

  public int getMissingGlyphCode (Font font)
  {
    throw new UnsupportedOperationException ();
  }

  public String getGlyphName (Font font, int glyphIndex)
  {
    throw new UnsupportedOperationException ();
  }

  public int getNumGlyphs (Font font)
  {
    byte[] data = getTrueTypeTable((byte)'m', (byte) 'a',
                                   (byte)'x', (byte) 'p');
    if( data == null )
      return -1;

    ByteBuffer buf = ByteBuffer.wrap( data );
    return buf.getShort(4);
  }

  public boolean hasUniformLineMetrics (Font font)
  {
    return true;
  }

  public GlyphVector layoutGlyphVector (Font font, FontRenderContext frc,
                                        char[] chars, int start, int limit,
                                        int flags)
  {
    return new FreetypeGlyphVector(font, chars, start, limit - start,
                                   frc, flags);
  }

  public LineMetrics getLineMetrics (Font font, String str,
                                     FontRenderContext frc)
  {
    return new GdkFontLineMetrics (this, str.length ());
  }

  public FontMetrics getFontMetrics (Font font)
  {
    if (metrics == null)
      metrics = new GdkFontMetrics(font);
    return metrics;
  }

  /**
   * Returns a cached GlyphMetrics object for a given glyphcode,
   * or null if it doesn't exist in the cache.
   */
  GlyphMetrics getGlyphMetrics( int glyphCode )
  {
    return metricsCache.get(new Integer(glyphCode));
  }

  /**
   * Put a GlyphMetrics object in the cache.
   */
  void putGlyphMetrics( int glyphCode, GlyphMetrics metrics )
  {
    metricsCache.put( new Integer( glyphCode ), metrics );
  }

}
