/* Copyright (C) 2000, 2002, 2003  Free Software Foundation
 
   This file is part of libgcj.
 
This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.awt.xlib;

import java.awt.*;
import gnu.java.awt.ClasspathToolkit;
import gnu.java.awt.peer.ClasspathFontPeer;
import java.util.Locale;
import java.awt.font.*;
import java.awt.geom.*;
import java.text.CharacterIterator;

/**
 * Classpath-compatible peer for a font
 */
public class XFontPeer extends ClasspathFontPeer
{
  public XFontPeer (String name, int style)
  {
    this (name, style, 12 /* kludge */);
  }
  
  public XFontPeer (String name, int style, float size)
  {
    super (name, style, (int)size);
  }
  
  /**
   * Implementation of {@link Font#canDisplay(char)}
   *
   * @param font the font this peer is being called from. This may be
   * useful if you are sharing peers between Font objects. Otherwise it may
   * be ignored.
   */
  public boolean canDisplay (Font font, char c)
  {
    throw new UnsupportedOperationException ();
  }
  
  /**
   * Implementation of {@link Font#canDisplay(String)},
   * {@link Font#canDisplay(char [], int, int)}, and
   * {@link Font#canDisplay(CharacterIterator, int, int)}.
   *
   * @param font the font this peer is being called from. This may be
   * useful if you are sharing peers between Font objects. Otherwise it may
   * be ignored.
   */
  public int canDisplayUpTo (Font font, CharacterIterator i, int start, int limit)
  {
    throw new UnsupportedOperationException ();
  }
  
  /**
   * Implementation of {@link
   * Font#createGlyphVector(FontRenderContext, String)}, {@link
   * Font#createGlyphVector(FontRenderContext, char[])}, and {@link
   * Font#createGlyphVector(FontRenderContext, CharacterIterator)}.
   *
   * @param font the font object that the created GlyphVector will return
   * when it gets asked for its font. This argument is needed because the
   * public API of {@link GlyphVector} works with {@link java.awt.Font},
   * not with font peers.
   */
  public GlyphVector createGlyphVector (Font font, FontRenderContext frc, CharacterIterator ci)
  {
    throw new UnsupportedOperationException ();
  }
  
  /**
   * Implementation of {@link Font#createGlyphVector(FontRenderContext,
   * int[])}.
   *
   * @param font the font object that the created GlyphVector will return
   * when it gets asked for its font. This argument is needed because the
   * public API of {@link GlyphVector} works with {@link java.awt.Font},
   * not with font peers.
   */
  public GlyphVector createGlyphVector (Font font, FontRenderContext ctx, int[] glyphCodes)
  {
    throw new UnsupportedOperationException ();
  }
  
  /**
   * Implementation of {@link Font#getBaselineFor(char)}
   *
   * @param font the font this peer is being called from. This may be
   * useful if you are sharing peers between Font objects. Otherwise it may
   * be ignored.
   */
  public byte getBaselineFor (Font font, char c)
  {
    throw new UnsupportedOperationException ();
  }
  
  /**
   * Implementation of {@link Font#getFontMetrics()}
   *
   * @param font the font this peer is being called from. This may be
   * useful if you are sharing peers between Font objects. Otherwise it may
   * be ignored.
   */
  public FontMetrics getFontMetrics (Font font)
  {
    throw new UnsupportedOperationException ();
  }
  
  /** Returns a name for the specified glyph. This is useful for
   * generating PostScript or PDF files that embed some glyphs of a
   * font. If the implementation follows glyph naming conventions
   * specified by Adobe, search engines can extract the original text
   * from the generated PostScript and PDF files.
   *
   * <p>This method is currently not used by GNU Classpath. However,
   * it would be very useful for someone wishing to write a good
   * PostScript or PDF stream provider for the
   * <code>javax.print</code> package.
   *
   * <p><b>Names are not unique:</b> Under some rare circumstances,
   * the same name can be returned for different glyphs. It is
   * therefore recommended that printer drivers check whether the same
   * name has already been returned for antoher glyph, and make the
   * name unique by adding the string ".alt" followed by the glyph
   * index.</p>
   *
   * <p>This situation would occur for an OpenType or TrueType font
   * that has a <code>post</code> table of format 3 and provides a
   * mapping from glyph IDs to Unicode sequences through a
   * <code>Zapf</code> table. If the same sequence of Unicode
   * codepoints leads to different glyphs (depending on contextual
   * position, for example, or on typographic sophistication level),
   * the same name would get synthesized for those glyphs. To avoid
   * this, the font peer would have to go through the names of all
   * glyphs, which would make this operation very inefficient with
   * large fonts.
   *
   * @param font the font containing the glyph whose name is
   * requested.
   *
   * @param glyphIndex the glyph whose name the caller wants to
   * retrieve.
   *
   * @return the glyph name, or <code>null</code> if a font does not
   * provide glyph names.
   */
  public String getGlyphName (Font font, int glyphIndex)
  {
    throw new UnsupportedOperationException ();
  }
  
  /**
   * Implementation of {@link Font#getLineMetrics(CharacterIterator, int,
   * int, FontRenderContext)}
   *
   * @param font the font this peer is being called from. This may be
   * useful if you are sharing peers between Font objects. Otherwise it may
   * be ignored.
   */
  public LineMetrics getLineMetrics (Font font, CharacterIterator ci, int begin, int limit, FontRenderContext rc)
  {
    throw new UnsupportedOperationException ();
  }
  
  /**
   * Implementation of {@link Font#getMaxCharBounds(FontRenderContext)}
   *
   * @param font the font this peer is being called from. This may be
   * useful if you are sharing peers between Font objects. Otherwise it may
   * be ignored.
   */
  public Rectangle2D getMaxCharBounds (Font font, FontRenderContext rc)
  {
    throw new UnsupportedOperationException ();
  }
  
  /**
   * Implementation of {@link Font#getMissingGlyphCode()}
   *
   * @param font the font this peer is being called from. This may be
   * useful if you are sharing peers between Font objects. Otherwise it may
   * be ignored.
   */
  public int getMissingGlyphCode (Font font)
  {
    throw new UnsupportedOperationException ();
  }
  
  /**
   * Implementation of {@link Font#getNumGlyphs()}
   *
   * @param font the font this peer is being called from. This may be
   * useful if you are sharing peers between Font objects. Otherwise it may
   * be ignored.
   */
  public int getNumGlyphs (Font font)
  {
    throw new UnsupportedOperationException ();
  }
  
  /**
   * Implementation of {@link Font#getPSName()}
   *
   * @param font the font this peer is being called from. This may be
   * useful if you are sharing peers between Font objects. Otherwise it may
   * be ignored.
   */
  public String getPostScriptName (Font font)
  {
    throw new UnsupportedOperationException ();
  }
  
  /**
   * Implementation of {@link Font#getStringBounds(CharacterIterator, int,
   * int, FontRenderContext)}
   *
   * @param font the font this peer is being called from. This may be
   * useful if you are sharing peers between Font objects. Otherwise it may
   * be ignored.
   */
  public Rectangle2D getStringBounds (Font font, CharacterIterator ci, int begin, int limit, FontRenderContext frc)
  {
    throw new UnsupportedOperationException ();
  }
  
  /** Returns the name of this font face inside the family, for example
   * <i>&#x201c;Light&#x201d;</i>.
   *
   * <p>This method is currently not used by {@link Font}. However,
   * this name would be needed by any serious desktop publishing
   * application.
   *
   * @param font the font whose sub-family name is requested.
   *
   * @param locale the locale for which to localize the name.  If
   * <code>locale</code> is <code>null</code>, the returned name is
   * localized to the user&#x2019;s default locale.
   *
   * @return the name of the face inside its family, or
   * <code>null</code> if the font does not provide a sub-family name.
   */
  public String getSubFamilyName (Font font, Locale locale)
  {
    throw new UnsupportedOperationException ();
  }
  
  /**
   * Implementation of {@link Font#hasUniformLineMetrics()}
   *
   * @param font the font this peer is being called from. This may be
   * useful if you are sharing peers between Font objects. Otherwise it may
   * be ignored.
   */
  public boolean hasUniformLineMetrics (Font font)
  {
    throw new UnsupportedOperationException ();
  }
  
  /**
   * Implementation of {@link Font#layoutGlyphVector(FontRenderContext,
   * char[], int, int, int)}.
   *
   * @param font the font object that the created GlyphVector will return
   * when it gets asked for its font. This argument is needed because the
   * public API of {@link GlyphVector} works with {@link java.awt.Font},
   * not with font peers.
   */
  public GlyphVector layoutGlyphVector (Font font, FontRenderContext frc, char[] chars, int start, int limit, int flags)
  {
    throw new UnsupportedOperationException ();
  }
}
