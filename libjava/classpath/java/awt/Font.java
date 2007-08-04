/* Font.java -- Font object
   Copyright (C) 1999, 2002, 2004, 2005  Free Software Foundation, Inc.

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


package java.awt;

import gnu.java.awt.ClasspathToolkit;
import gnu.java.awt.peer.ClasspathFontPeer;

import java.awt.font.FontRenderContext;
import java.awt.font.GlyphVector;
import java.awt.font.LineMetrics;
import java.awt.font.TextAttribute;
import java.awt.font.TextLayout;
import java.awt.geom.AffineTransform;
import java.awt.geom.Rectangle2D;
import java.awt.peer.FontPeer;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.io.Serializable;
import java.text.AttributedCharacterIterator;
import java.text.CharacterIterator;
import java.text.StringCharacterIterator;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.StringTokenizer;

/**
 * This class represents a windowing system font.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @author Warren Levy (warrenl@cygnus.com)
 * @author Graydon Hoare (graydon@redhat.com)
 */
public class Font implements Serializable
{

  /**
   * Constant indicating a "plain" font.
   */
  public static final int PLAIN = 0;

  /**
   * Constant indicating a "bold" font.
   */
  public static final int BOLD = 1;

  /**
   * Constant indicating an "italic" font.
   */
  public static final int ITALIC = 2;

  /**
   * Constant indicating the baseline mode characteristic of Roman.
   */
  public static final int ROMAN_BASELINE = 0;

  /**
   * Constant indicating the baseline mode characteristic of Chinese.
   */
  public static final int CENTER_BASELINE = 1;

  /**
   * Constant indicating the baseline mode characteristic of Devanigri.
   */
  public static final int HANGING_BASELINE = 2;  


  /**
   * Indicates to <code>createFont</code> that the supplied font data
   * is in TrueType format.
   *
   * <p><em>Specification Note:</em> The Sun JavaDoc for J2SE 1.4 does
   * not indicate whether this value also subsumes OpenType. OpenType
   * is essentially the same format as TrueType, but allows to define
   * glyph shapes in the same way as PostScript, using cubic bezier
   * curves.
   *
   * @since 1.3
   */
  public static final int TRUETYPE_FONT = 0;
  
  /**
   * Indicates to <code>createFont</code> that the supplied font data
   * is in Type1 format.
   * 
   * @since 1.5
   */
  public static final int TYPE1_FONT = 1;

  /**
   * A flag for <code>layoutGlyphVector</code>, indicating that the
   * orientation of a text run is from left to right.
   *
   * @since 1.4
   */
  public static final int LAYOUT_LEFT_TO_RIGHT = 0;


  /**
   * A flag for <code>layoutGlyphVector</code>, indicating that the
   * orientation of a text run is from right to left.
   *
   * @since 1.4
   */
  public static final int LAYOUT_RIGHT_TO_LEFT = 1;


  /**
   * A flag for <code>layoutGlyphVector</code>, indicating that the
   * text does not contain valid characters before the
   * <code>start</code> position.  If this flag is set,
   * <code>layoutGlyphVector</code> does not examine the text before
   * <code>start</code>, even if this would be necessary to select the
   * correct glyphs (e.g., for Arabic text).
   *
   * @since 1.4
   */
  public static final int LAYOUT_NO_START_CONTEXT = 2;


  /**
   * A flag for <code>layoutGlyphVector</code>, indicating that the
   * text does not contain valid characters after the
   * <code>limit</code> position.  If this flag is set,
   * <code>layoutGlyphVector</code> does not examine the text after
   * <code>limit</code>, even if this would be necessary to select the
   * correct glyphs (e.g., for Arabic text).
   *
   * @since 1.4
   */
  public static final int LAYOUT_NO_LIMIT_CONTEXT = 4;

  /**
   * @since 1.6
   */
  public static final String DIALOG = "Dialog";

  /**
   * @since 1.6
   */
  public static final String DIALOG_INPUT = "DialogInput";

  /**
   * @since 1.6
   */
  public static final String MONOSPACED = "Monospaced";

  /**
   * @since 1.6
   */
  public static final String SANS_SERIF = "SansSerif";

  /**
   * @since 1.6
   */
  public static final String SERIF = "Serif";

  /**
   * The logical name of this font.
   *
   * @since 1.0
   */
  protected String name;

  /**
   * The size of this font in points, rounded.
   *
   * @since 1.0
   */
  protected int size;

  /**
   * The size of this font in points.
   *
   * @since 1.0
   */
  protected float pointSize;

  /**
   * The style of this font -- PLAIN, BOLD, ITALIC or BOLD+ITALIC.
   *
   * @since 1.0
   */
  protected int style;

//Serialization constant
  private static final long serialVersionUID = -4206021311591459213L;


  // The ClasspathToolkit-provided peer which implements this font
  private transient ClasspathFontPeer peer;


  /**
   * Creates a <code>Font</code> object from the specified string, which
   * is in one of the following formats:
   * <p>
   * <ul>
   * <li>fontname-style-pointsize
   * <li>fontname-style
   * <li>fontname-pointsize
   * <li>fontname
   * </ul>
   * <p>
   * The style should be one of BOLD, ITALIC, or BOLDITALIC.  The default
   * style if none is specified is PLAIN.  The default size if none
   * is specified is 12.
   * 
   * @param fontspec  a string specifying the required font (<code>null</code> 
   *                  permitted, interpreted as 'Dialog-PLAIN-12').
   * 
   * @return A font.
   */
  public static Font decode(String fontspec)
  {
    if (fontspec == null) 
      fontspec = "Dialog-PLAIN-12";
    String name = null;
    int style = PLAIN;
    int size = 12;

    StringTokenizer st = new StringTokenizer(fontspec, "- ");
    while (st.hasMoreTokens())
      {
        String token = st.nextToken();
        if (name == null)
          {
            name = token;
            continue;
          }

        if (token.toUpperCase().equals("BOLD"))
          {
            style = BOLD;
            continue;
          }
        if (token.toUpperCase().equals("ITALIC"))
          {
            style = ITALIC;
            continue;
          }
        if (token.toUpperCase().equals("BOLDITALIC"))
          {
            style = BOLD | ITALIC;
            continue;
          }

        int tokenval = 0;
        try
          {
            tokenval = Integer.parseInt(token);
          }
        catch (NumberFormatException e)
          {
            // Ignored.
          }

      if (tokenval != 0)
        size = tokenval;
    }

    HashMap attrs = new HashMap();
    ClasspathFontPeer.copyStyleToAttrs(style, attrs);
    ClasspathFontPeer.copySizeToAttrs(size, attrs);

    return getFontFromToolkit(name, attrs);
  }

  /* These methods delegate to the toolkit. */

  static ClasspathToolkit tk()
  {
    return (ClasspathToolkit) Toolkit.getDefaultToolkit();
  }

  /* Every factory method in Font should eventually call this. */
  static Font getFontFromToolkit(String name, Map attribs)
  {
    return tk().getFont(name, attribs);
  }

  /* Every Font constructor should eventually call this. */
  static ClasspathFontPeer getPeerFromToolkit(String name, Map attrs)
  {
    return tk().getClasspathFontPeer(name, attrs);
  }


  /**
   * Returns a <code>Font</code> object from the passed property name.
   *
   * @param propname The name of the system property.
   * @param defval Value to use if the property is not found.
   *
   * @return The requested font, or <code>default</code> if the property 
   * not exist or is malformed.
   */
  public static Font getFont(String propname, Font defval)
  {
    String propval = System.getProperty(propname);
    if (propval != null)
      return decode(propval);
    return defval;
  }

  /**
   * Returns a <code>Font</code> object from the passed property name.
   *
   * @param propname The name of the system property.
   *
   * @return The requested font, or <code>null</code> if the property 
   * not exist or is malformed.
   */
  public static Font getFont(String propname)
  {
    return getFont(propname, (Font) null);
  }

  protected Font(Font font)
  {
    this(font.getName(), font.getAttributes());
  }

  /**
   * Initializes a new instance of <code>Font</code> with the specified
   * attributes.
   *
   * @param name The name of the font.
   * @param style The font style.
   * @param size The font point size.
   */
  public Font(String name, int style, int size)
  {
    HashMap attrs = new HashMap();
    ClasspathFontPeer.copyStyleToAttrs(style, attrs);
    ClasspathFontPeer.copySizeToAttrs(size, attrs);
    this.peer = getPeerFromToolkit(name, attrs);
    this.size = size;
    this.pointSize = (float) size;
    if (name != null)
      this.name = name;
    else
      this.name = peer.getName(this);
  }

  public Font(Map<? extends AttributedCharacterIterator.Attribute, ?> attrs)
  {
    this(null, attrs);
  }

  /* This extra constructor is here to permit ClasspathToolkit and to
   build a font with a "logical name" as well as attrs.
   ClasspathToolkit.getFont(String,Map) uses reflection to call this
   package-private constructor. */
  Font(String name, Map attrs)
  {
    // If attrs is null, setting it to an empty HashMap will give this
    // Font default attributes.
    if (attrs == null)
      attrs = new HashMap();
    peer = getPeerFromToolkit(name, attrs);
    size = (int) peer.getSize(this);
    pointSize = peer.getSize(this);
    if (name != null)
      this.name = name;
    else
      this.name = peer.getName(this);
  }

  /**
   * Returns the logical name of the font.  A logical name is the name the
   * font was constructed with. It may be the name of a logical font (one
   * of 6 required names in all java environments) or it may be a face
   * name.
   *
   * @return The logical name of the font.
   *
   * @see #getFamily()
   * @see #getFontName()
   */
  public String getName ()
  {
    return peer.getName(this);
  }

  /**
   * Returns the size of the font, in typographics points (1/72 of an inch),
   * rounded to an integer.
   * 
   * @return The font size
   */
  public int getSize()
  {
    return size;
  }

  /**
   * Returns the size of the font, in typographics points (1/72 of an inch).
   * 
   * @return The font size
   */
  public float getSize2D()
  {
    return pointSize;
  }

  /**
   * Tests whether or not this is a plain font.  This will be true if
   * and only if neither the bold nor the italics style is set.
   *
   * @return <code>true</code> if this is a plain font, <code>false</code>
   * otherwise.
   */
  public boolean isPlain()
  {
    return peer.isPlain(this); 
  }

  /**
   * Tests whether or not this font is bold.
   *
   * @return <code>true</code> if this font is bold, <code>false</code>
   * otherwise.
   */
  public boolean isBold()
  {
    return peer.isBold(this);
  }

  /**
   * Tests whether or not this font is italic.
   *
   * @return <code>true</code> if this font is italic, <code>false</code>
   * otherwise.
   */
  public boolean isItalic()
  {
    return peer.isItalic(this);
  }

  /**
   * Returns the family name of this font. A family name describes a design
   * or "brand name" (such as Helvetica or Palatino). It is less specific
   * than a font face name (such as Helvetica Bold).
   *
   * @return A string containing the font family name.
   *
   * @since 1.2
   *
   * @see #getName()
   * @see #getFontName()
   * @see GraphicsEnvironment#getAvailableFontFamilyNames()
   */
  public String getFamily()
  {
    return peer.getFamily(this);
  }

  /**
   * Returns integer code representing the sum of style flags of this font, a
   * combination of either {@link #PLAIN}, {@link #BOLD}, or {@link #ITALIC}.
   *
   * @return code representing the style of this font.
   *
   * @see #isPlain()
   * @see #isBold()
   * @see #isItalic()
   */
  public int getStyle()
  {
    return peer.getStyle(this);
  }

  /**
   * Checks if specified character maps to a glyph in this font.
   *
   * @param c The character to check.
   *
   * @return Whether the character has a corresponding glyph in this font.
   *
   * @since 1.2
   */
  public boolean canDisplay(char c)
  {
    return canDisplay((int) c);
  }

  public boolean canDisplay(int codePoint)
  {
    return peer.canDisplay(this, codePoint);
  }

  /**
   * Checks how much of a given string can be mapped to glyphs in 
   * this font.
   *
   * @param s The string to check.
   *
   * @return The index of the first character in <code>s</code> which cannot
   * be converted to a glyph by this font, or <code>-1</code> if all
   * characters can be mapped to glyphs.
   *
   * @since 1.2
   */
  public int canDisplayUpTo(String s)
  {
    return peer.canDisplayUpTo(this, new StringCharacterIterator(s), 
                               0, s.length() - 1);
  }

  /**
   * Checks how much of a given sequence of text can be mapped to glyphs in
   * this font.
   *
   * @param text Array containing the text to check.
   * @param start Position of first character to check in <code>text</code>.
   * @param limit Position of last character to check in <code>text</code>.
   *
   * @return The index of the first character in the indicated range which
   * cannot be converted to a glyph by this font, or <code>-1</code> if all
   * characters can be mapped to glyphs.
   *
   * @since 1.2
   *
   * @throws IndexOutOfBoundsException if the range [start, limit] is
   * invalid in <code>text</code>.
   */
  public int canDisplayUpTo (char[] text, int start, int limit)
  {
    return peer.canDisplayUpTo(this,
                               new StringCharacterIterator(new String (text)),
                               start, limit);
  }

  /**
   * Checks how much of a given sequence of text can be mapped to glyphs in
   * this font.
   *
   * @param i Iterator over the text to check.
   * @param start Position of first character to check in <code>i</code>.
   * @param limit Position of last character to check in <code>i</code>.
   *
   * @return The index of the first character in the indicated range which
   * cannot be converted to a glyph by this font, or <code>-1</code> if all
   * characters can be mapped to glyphs.
   *
   * @since 1.2
   *
   * @throws IndexOutOfBoundsException if the range [start, limit] is
   * invalid in <code>i</code>.
   */
  public int canDisplayUpTo(CharacterIterator i, int start, int limit)
  {
    return peer.canDisplayUpTo(this, i, start, limit);    
  }

  /**
   * Creates a new font with point size 1 and {@link #PLAIN} style,
   * reading font data from the provided input stream. The resulting font
   * can have further fonts derived from it using its
   * <code>deriveFont</code> method.
   *
   * @param fontFormat Integer code indicating the format the font data is
   * in.Currently this can only be {@link #TRUETYPE_FONT}.
   * @param is {@link InputStream} from which font data will be read. This
   * stream is not closed after font data is extracted.
   *
   * @return A new {@link Font} of the format indicated.
   *
   * @throws IllegalArgumentException if <code>fontType</code> is not
   * recognized.
   * @throws FontFormatException if data in InputStream is not of format
   * indicated.
   * @throws IOException if insufficient data is present on InputStream.
   *
   * @since 1.3
   */
  public static Font createFont (int fontFormat, InputStream is)
    throws FontFormatException, IOException
  {
    return tk().createFont(fontFormat, is);
  }

  /**
   * Creates a new font from a File object.
   *
   * @see #layoutGlyphVector(FontRenderContext, char[], int, int, int)
   *
   * @param fontFormat - Integer code indicating the format the font data is
   * in.Currently this can only be {@link #TRUETYPE_FONT}.
   * @param file - a {@link File} from which font data will be read.
   *
   * @return A new {@link Font} of the format indicated.
   *
   * @throws IllegalArgumentException if <code>fontType</code> is not
   * recognized.
   * @throws NullPointerException if <code>file</code> is <code>null</code>.
   * @throws FontFormatException if data in the file is invalid or cannot be read..
   * @throws SecurityException if the caller has no read permission for the file.
   * @throws IOException if the file cannot be read
   *
   * @since 1.5
   */
  public static Font createFont (int fontFormat, File file)
    throws FontFormatException, IOException
  {
    if( file == null )
      throw new NullPointerException("Null file argument");
    return tk().createFont(fontFormat, new FileInputStream( file ));
  }

  /**
   * Maps characters to glyphs in a one-to-one relationship, returning a new
   * {@link GlyphVector} with a mapped glyph for each input character. This
   * sort of mapping is often sufficient for some scripts such as Roman, but
   * is inappropriate for scripts with special shaping or contextual layout
   * requirements such as Arabic, Indic, Hebrew or Thai.
   *
   * @param ctx The rendering context used for precise glyph placement.
   * @param str The string to convert to Glyphs.
   *
   * @return A new {@link GlyphVector} containing glyphs mapped from str,
   * through the font's cmap table.
   *
   * @see #layoutGlyphVector(FontRenderContext, char[], int, int, int)
   */
  public GlyphVector createGlyphVector(FontRenderContext ctx, String str)
  {
    return peer.createGlyphVector(this, ctx, new StringCharacterIterator(str));
  }

  /**
   * Maps characters to glyphs in a one-to-one relationship, returning a new
   * {@link GlyphVector} with a mapped glyph for each input character. This
   * sort of mapping is often sufficient for some scripts such as Roman, but
   * is inappropriate for scripts with special shaping or contextual layout
   * requirements such as Arabic, Indic, Hebrew or Thai.
   *
   * @param ctx The rendering context used for precise glyph placement.
   * @param i Iterator over the text to convert to glyphs.
   *
   * @return A new {@link GlyphVector} containing glyphs mapped from str,
   * through the font's cmap table.
   *
   * @see #layoutGlyphVector(FontRenderContext, char[], int, int, int)
   */
  public GlyphVector createGlyphVector(FontRenderContext ctx,
                                       CharacterIterator i)
  {
    return peer.createGlyphVector(this, ctx, i);
  }

  /**
   * Maps characters to glyphs in a one-to-one relationship, returning a new
   * {@link GlyphVector} with a mapped glyph for each input character. This
   * sort of mapping is often sufficient for some scripts such as Roman, but
   * is inappropriate for scripts with special shaping or contextual layout
   * requirements such as Arabic, Indic, Hebrew or Thai.
   *
   * @param ctx The rendering context used for precise glyph placement.
   * @param chars Array of characters to convert to glyphs.
   *
   * @return A new {@link GlyphVector} containing glyphs mapped from str,
   * through the font's cmap table.
   *
   * @see #layoutGlyphVector(FontRenderContext, char[], int, int, int)
   */
  public GlyphVector createGlyphVector(FontRenderContext ctx, char[] chars)
  {
    return peer.createGlyphVector(this, ctx,
                               new StringCharacterIterator(new String(chars)));
  }

  /**
   * Extracts a sequence of glyphs from a font, returning a new {@link
   * GlyphVector} with a mapped glyph for each input glyph code. 
   *
   * @param ctx The rendering context used for precise glyph placement.
   * @param glyphCodes Array of characters to convert to glyphs.
   *
   * @return A new {@link GlyphVector} containing glyphs mapped from str,
   * through the font's cmap table.
   *
   * @see #layoutGlyphVector(FontRenderContext, char[], int, int, int)
   *
   * @specnote This method is documented to perform character-to-glyph
   * conversions, in the Sun documentation, but its second parameter name is
   * "glyphCodes" and it is not clear to me why it would exist if its
   * purpose was to transport character codes inside integers. I assume it
   * is mis-documented in the Sun documentation.
   */
  public GlyphVector createGlyphVector(FontRenderContext ctx, int[] glyphCodes)
  {
    return peer.createGlyphVector(this, ctx, glyphCodes);
  }

  /**
   * Produces a new {@link Font} based on the current font, adjusted to a
   * new size and style.
   *
   * @param style The style of the newly created font.
   * @param size The size of the newly created font.
   *
   * @return A clone of the current font, with the specified size and style.
   *
   * @since 1.2
   */
  public Font deriveFont(int style, float size)
  {
    return peer.deriveFont(this, style, size);
  }

  /**
   * Produces a new {@link Font} based on the current font, adjusted to a
   * new size.
   *
   * @param size The size of the newly created font.
   *
   * @return A clone of the current font, with the specified size.
   *
   * @since 1.2
   */
  public Font deriveFont(float size)
  {
    return peer.deriveFont(this, size);
  }

  /**
   * Produces a new {@link Font} based on the current font, adjusted to a
   * new style.
   *
   * @param style The style of the newly created font.
   *
   * @return A clone of the current font, with the specified style.
   *
   * @since 1.2
   */
  public Font deriveFont(int style)
  {
    return peer.deriveFont(this, style);
  }

  /**
   * Produces a new {@link Font} based on the current font, adjusted to a
   * new style and subjected to a new affine transformation.
   *
   * @param style The style of the newly created font.
   * @param a The transformation to apply.
   *
   * @return A clone of the current font, with the specified style and
   * transform.
   *
   * @throws IllegalArgumentException If transformation is
   * <code>null</code>.
   *
   * @since 1.2
   */
  public Font deriveFont(int style, AffineTransform a)
  {
    if (a == null)
      throw new IllegalArgumentException("Affine transformation is null");

    return peer.deriveFont(this, style, a);
  }

  /**
   * Produces a new {@link Font} based on the current font, subjected
   * to a new affine transformation.
   *
   * @param a The transformation to apply.
   *
   * @return A clone of the current font, with the specified transform.
   *
   * @throws IllegalArgumentException If transformation is
   * <code>null</code>.
   *
   * @since 1.2
   */
  public Font deriveFont(AffineTransform a)
  {
    if (a == null)
      throw new IllegalArgumentException("Affine transformation is null");

    return peer.deriveFont(this, a);
  }

  /**
   * Produces a new {@link Font} based on the current font, adjusted to a
   * new set of attributes.
   *
   * @param attributes Attributes of the newly created font.
   *
   * @return A clone of the current font, with the specified attributes.
   *
   * @since 1.2
   */
  public Font deriveFont(Map<? extends AttributedCharacterIterator.Attribute, ?> attributes)
  {
    return peer.deriveFont(this, attributes);
  }

  /**
   * Returns a map of chracter attributes which this font currently has set.
   *
   * @return A map of chracter attributes which this font currently has set.
   *
   * @see #getAvailableAttributes()
   * @see java.text.AttributedCharacterIterator.Attribute
   * @see java.awt.font.TextAttribute
   */
  public Map<TextAttribute, ?> getAttributes()
  {
    return peer.getAttributes(this);
  }

  /**
   * Returns an array of chracter attribute keys which this font understands. 
   *
   * @return An array of chracter attribute keys which this font understands.
   *
   * @see #getAttributes()
   * @see java.text.AttributedCharacterIterator.Attribute
   * @see java.awt.font.TextAttribute
   */
  public AttributedCharacterIterator.Attribute[] getAvailableAttributes()
  {
    return peer.getAvailableAttributes(this);
  }

  /**
   * Returns a baseline code (one of {@link #ROMAN_BASELINE}, {@link
   * #CENTER_BASELINE} or {@link #HANGING_BASELINE}) indicating which baseline
   * this font will measure baseline offsets for, when presenting glyph
   * metrics for a given character.
   *
   * Baseline offsets describe the position of a glyph relative to an
   * invisible line drawn under, through the center of, or over a line of
   * rendered text, respectively. Different scripts use different baseline
   * modes, so clients should not assume all baseline offsets in a glyph
   * vector are from a common baseline.
   *
   * @param c The character code to select a baseline mode for.
   *
   * @return The baseline mode which would be used in a glyph associated
   * with the provided character.
   *
   * @since 1.2
   *
   * @see LineMetrics#getBaselineOffsets()
   */
  public byte getBaselineFor(char c)
  {
    return peer.getBaselineFor(this, c);
  }

  /**
   * Returns the family name of this font. A family name describes a
   * typographic style (such as Helvetica or Palatino). It is more specific
   * than a logical font name (such as Sans Serif) but less specific than a
   * font face name (such as Helvetica Bold).
   *
   * @param lc The locale in which to describe the name of the font family.
   *
   * @return A string containing the font family name, localized for the
   * provided locale.
   *
   * @since 1.2
   *
   * @see #getName()
   * @see #getFontName()
   * @see GraphicsEnvironment#getAvailableFontFamilyNames()
   * @see Locale
   */
  public String getFamily(Locale lc)
  {
    return peer.getFamily(this, lc); 
  }

  /**
   * Returns a font appropriate for the given attribute set.
   *
   * @param attributes The attributes required for the new font.
   *
   * @return A new Font with the given attributes.
   *
   * @since 1.2
   *
   * @see java.awt.font.TextAttribute  
   */
  public static Font getFont(Map<? extends AttributedCharacterIterator.Attribute, ?> attributes)
  {
    return getFontFromToolkit(null, attributes);
  }

  /**
   * Returns the font face name of the font.  A font face name describes a
   * specific variant of a font family (such as Helvetica Bold). It is more
   * specific than both a font family name (such as Helvetica) and a logical
   * font name (such as Sans Serif).
   *
   * @return The font face name of the font.
   *
   * @since 1.2
   *
   * @see #getName()
   * @see #getFamily()
   */
  public String getFontName()
  {
    return peer.getFontName(this);
  }

  /**
   * Returns the font face name of the font.  A font face name describes a
   * specific variant of a font family (such as Helvetica Bold). It is more
   * specific than both a font family name (such as Helvetica).
   *
   * @param lc The locale in which to describe the name of the font face.
   *
   * @return A string containing the font face name, localized for the
   * provided locale.
   *
   * @since 1.2
   *
   * @see #getName()
   * @see #getFamily()
   */
  public String getFontName(Locale lc)
  {
    return peer.getFontName(this, lc);
  }

  /**
   * Returns the italic angle of this font, a measurement of its slant when
   * style is {@link #ITALIC}. The precise meaning is the inverse slope of a
   * caret line which "best measures" the font's italic posture.
   *
   * @return The italic angle.
   *
   * @see java.awt.font.TextAttribute#POSTURE
   */
  public float getItalicAngle()
  {
    return peer.getItalicAngle(this);
  }

  /**
   * Returns a {@link LineMetrics} object constructed with the specified
   * text and {@link FontRenderContext}. 
   *
   * @param text The string to calculate metrics from.
   * @param begin Index of first character in <code>text</code> to measure.
   * @param limit Index of last character in <code>text</code> to measure.
   * @param rc Context for calculating precise glyph placement and hints.
   *
   * @return A new {@link LineMetrics} object.
   *
   * @throws IndexOutOfBoundsException if the range [begin, limit] is
   * invalid in <code>text</code>.
   */
  public LineMetrics getLineMetrics(String text, int begin, 
                                    int limit, FontRenderContext rc)
  {
    return peer.getLineMetrics(this, new StringCharacterIterator(text), 
                               begin, limit, rc);
  }

  /**
   * Returns a {@link LineMetrics} object constructed with the specified
   * text and {@link FontRenderContext}. 
   *
   * @param chars The string to calculate metrics from.
   * @param begin Index of first character in <code>text</code> to measure.
   * @param limit Index of last character in <code>text</code> to measure.
   * @param rc Context for calculating precise glyph placement and hints.
   *
   * @return A new {@link LineMetrics} object.
   *
   * @throws IndexOutOfBoundsException if the range [begin, limit] is
   * invalid in <code>chars</code>.
   */
  public LineMetrics getLineMetrics(char[] chars, int begin, 
                                    int limit, FontRenderContext rc)
  {
    return peer.getLineMetrics(this,
                               new StringCharacterIterator(new String(chars)), 
                               begin, limit, rc);
  }

  /**
   * Returns a {@link LineMetrics} object constructed with the specified
   * text and {@link FontRenderContext}. 
   *
   * @param ci The string to calculate metrics from.
   * @param begin Index of first character in <code>text</code> to measure.
   * @param limit Index of last character in <code>text</code> to measure.
   * @param rc Context for calculating precise glyph placement and hints.
   *
   * @return A new {@link LineMetrics} object.
   *
   * @throws IndexOutOfBoundsException if the range [begin, limit] is
   * invalid in <code>ci</code>.
   */
  public LineMetrics getLineMetrics(CharacterIterator ci, int begin, 
                                    int limit, FontRenderContext rc)
  {
    return peer.getLineMetrics(this, ci, begin, limit, rc);
  }

  /**
   * Returns the maximal bounding box of all the bounding boxes in this
   * font, when the font's bounding boxes are evaluated in a given {@link
   * FontRenderContext}
   *
   * @param rc Context in which to evaluate bounding boxes.
   *
   * @return The maximal bounding box.
   */
  public Rectangle2D getMaxCharBounds(FontRenderContext rc)
  {
    return peer.getMaxCharBounds(this, rc);
  }

  /**
   * Returns the glyph code this font uses to represent missing glyphs. This
   * code will be present in glyph vectors when the font was unable to
   * locate a glyph to represent a particular character code.
   *
   * @return The missing glyph code.
   *
   * @since 1.2
   */
  public int getMissingGlyphCode()
  {
    return peer.getMissingGlyphCode(this);
  }

  /**
   * Returns the overall number of glyphs in this font. This number is one
   * more than the greatest glyph code used in any glyph vectors this font
   * produces. In other words, glyph codes are taken from the range
   * <code>[ 0, getNumGlyphs() - 1 ]</code>.
   *
   * @return The number of glyphs in this font.
   * 
   * @since 1.2
   */
  public int getNumGlyphs()
  {
    return peer.getNumGlyphs(this);
  }

  /**
   * Returns the PostScript Name of this font.   
   *
   * @return The PostScript Name of this font.
   *
   * @since 1.2
   *
   * @see #getName()
   * @see #getFamily()
   * @see #getFontName()
   */
  public String getPSName()
  {
    return peer.getPostScriptName(this);
  }

  /**
   * Returns the logical bounds of the specified string when rendered with this
   * font in the specified {@link FontRenderContext}. This box will include the
   * glyph origin, ascent, advance, height, and leading, but may not include all
   * diacritics or accents. To get the complete visual bounding box of all the
   * glyphs in a run of text, use the {@link TextLayout#getBounds} method of 
   * {@link TextLayout}.
   *
   * @param str The string to measure.
   * @param frc The context in which to make the precise glyph measurements.
   * 
   * @return A bounding box covering the logical bounds of the specified text.
   *
   * @see #createGlyphVector(FontRenderContext, String)
   */
  public Rectangle2D getStringBounds(String str, FontRenderContext frc)
  {
    char[] chars = str.toCharArray();
    return getStringBounds(chars, 0, chars.length, frc);
  }

  /**
   * Returns the logical bounds of the specified string when rendered with this
   * font in the specified {@link FontRenderContext}. This box will include the
   * glyph origin, ascent, advance, height, and leading, but may not include all
   * diacritics or accents. To get the complete visual bounding box of all the
   * glyphs in a run of text, use the {@link TextLayout#getBounds} method of
   * {@link TextLayout}.
   *
   * @param str The string to measure.
   * @param begin Index of the first character in <code>str</code> to measure.
   * @param limit Index of the last character in <code>str</code> to measure.
   * @param frc The context in which to make the precise glyph measurements.
   * 
   * @return A bounding box covering the logical bounds of the specified text.
   *
   * @throws IndexOutOfBoundsException if the range [begin, limit] is
   * invalid in <code>str</code>.
   *
   * @since 1.2
   *
   * @see #createGlyphVector(FontRenderContext, String)
   */
  public Rectangle2D getStringBounds(String str, int begin, 
                                     int limit, FontRenderContext frc)
  {
    String sub = str.substring(begin, limit);
    return getStringBounds(sub, frc);
  }

  /**
   * Returns the logical bounds of the specified string when rendered with this
   * font in the specified {@link FontRenderContext}. This box will include the
   * glyph origin, ascent, advance, height, and leading, but may not include all
   * diacritics or accents. To get the complete visual bounding box of all the
   * glyphs in a run of text, use the {@link TextLayout#getBounds} method of
   * {@link TextLayout}.
   *
   * @param ci The text to measure.
   * @param begin Index of the first character in <code>ci</code> to measure.
   * @param limit Index of the last character in <code>ci</code> to measure.
   * @param frc The context in which to make the precise glyph measurements.
   * 
   * @return A bounding box covering the logical bounds of the specified text.
   *
   * @throws IndexOutOfBoundsException if the range [begin, limit] is
   * invalid in <code>ci</code>.
   *
   * @since 1.2
   *
   * @see #createGlyphVector(FontRenderContext, CharacterIterator)
   */
  public Rectangle2D getStringBounds(CharacterIterator ci, int begin, 
                                     int limit, FontRenderContext frc)
  {
    int start = ci.getBeginIndex();
    int end = ci.getEndIndex();
    char[] chars = new char[limit - start];
    ci.setIndex(start);
    for (int index = 0; index < chars.length; index++)
      {
        chars[index] = ci.current();
        ci.next();
      }
    return getStringBounds(chars, 0, chars.length, frc);
  }

  /**
   * Returns the logical bounds of the specified string when rendered with this
   * font in the specified {@link FontRenderContext}. This box will include the
   * glyph origin, ascent, advance, height, and leading, but may not include all
   * diacritics or accents. To get the complete visual bounding box of all the
   * glyphs in a run of text, use the {@link TextLayout#getBounds} method of
   * {@link TextLayout}.
   *
   * @param chars The text to measure.
   * @param begin Index of the first character in <code>ci</code> to measure.
   * @param limit Index of the last character in <code>ci</code> to measure.
   * @param frc The context in which to make the precise glyph measurements.
   * 
   * @return A bounding box covering the logical bounds of the specified text.
   *
   * @throws IndexOutOfBoundsException if the range [begin, limit] is
   * invalid in <code>chars</code>.
   *
   * @since 1.2
   *
   * @see #createGlyphVector(FontRenderContext, char[])
   */
  public Rectangle2D getStringBounds(char[] chars, int begin, 
                                     int limit, FontRenderContext frc)
  {
    String str = new String(chars, begin, limit - begin);
    TextLayout layout = new TextLayout(str, this, frc);
    return new Rectangle2D.Float(0, -layout.getAscent(), layout.getAdvance(),
                                layout.getDescent() + layout.getLeading());
  }

  /**
   * Returns a copy of the affine transformation this font is currently
   * subject to, if any.
   *
   * @return The current transformation.
   */
  public AffineTransform getTransform()
  {
    return peer.getTransform(this);
  }

  /**
   * Indicates whether this font's line metrics are uniform. A font may be
   * composed of several "subfonts", each covering a different code range,
   * and each with their own line metrics. A font with no subfonts, or
   * subfonts with identical line metrics, is said to have "uniform" line
   * metrics.
   *
   * @return Whether this font has uniform line metrics.
   *
   * @see LineMetrics
   * @see #getLineMetrics(String, FontRenderContext)
   */
  public boolean hasUniformLineMetrics()
  {
    return peer.hasUniformLineMetrics(this);
  }

  /**
   * Indicates whether this font is subject to a non-identity affine
   * transformation.
   *
   * @return <code>true</code> iff the font has a non-identity affine
   * transformation applied to it.
   */
  public boolean isTransformed()
  {
    return peer.isTransformed(this);
  }

  /**
   * Produces a glyph vector representing a full layout fo the specified
   * text in this font. Full layouts may include complex shaping and
   * reordering operations, for scripts such as Arabic or Hindi.
   *
   * Bidirectional (bidi) layout is not performed in this method; text
   * should have its bidi direction specified with one of the flags {@link
   * #LAYOUT_LEFT_TO_RIGHT} or {@link #LAYOUT_RIGHT_TO_LEFT}.
   *
   * Some types of layout (notably Arabic glyph shaping) may examine context
   * characters beyond the bounds of the indicated range, in order to select
   * an appropriate shape. The flags {@link #LAYOUT_NO_START_CONTEXT} and
   * {@link #LAYOUT_NO_LIMIT_CONTEXT} can be provided to prevent these extra
   * context areas from being examined, for instance if they contain invalid
   * characters.
   *
   * @param frc Context in which to perform the layout.
   * @param chars Text to perform layout on.
   * @param start Index of first character to perform layout on.
   * @param limit Index of last character to perform layout on.
   * @param flags Combination of flags controlling layout.
   *
   * @return A new {@link GlyphVector} representing the specified text.
   *
   * @throws IndexOutOfBoundsException if the range [begin, limit] is
   * invalid in <code>chars</code>. 
   */
  public GlyphVector layoutGlyphVector(FontRenderContext frc, 
                                       char[] chars, int start, 
                                       int limit, int flags)
  {
    return peer.layoutGlyphVector(this, frc, chars, start, limit, flags);
  }


  /**
   * Returns a native peer object for this font.
   *
   * @return A native peer object for this font.
   *
   * @deprecated
   */
  public FontPeer getPeer()
  {
    return peer;
  }


  /**
   * Returns a hash value for this font.
   * 
   * @return A hash for this font.
   */
  public int hashCode()
  {
    return this.toString().hashCode();
  }


  /**
   * Tests whether or not the specified object is equal to this font.  This
   * will be true if and only if:
   * <P>
   * <ul>
   * <li>The object is not <code>null</code>.
   * <li>The object is an instance of <code>Font</code>.
   * <li>The object has the same names, style, size, and transform as this object.
   * </ul>
   *
   * @return <code>true</code> if the specified object is equal to this
   * object, <code>false</code> otherwise.
   */
  public boolean equals(Object obj)
  {
    if (obj == null)
      return false;

    if (! (obj instanceof Font))
      return false;

    Font f = (Font) obj;

    return (f.getName().equals(this.getName())
            && f.getFamily().equals(this.getFamily())
            && f.getFontName().equals(this.getFontName())
            && f.getTransform().equals(this.getTransform ())
            && f.getSize() == this.getSize()
            && f.getStyle() == this.getStyle());
  }

  /**
   * Returns a string representation of this font.
   *
   * @return A string representation of this font.
   */
  public String toString()
  {
    String styleString = "";

    switch (getStyle())
      {
      case 0:
        styleString = "plain";
        break;
      case 1:
        styleString = "bold";
        break;
      case 2:
        styleString = "italic";
        break;
      default:
        styleString = "unknown";
     }
    
    return getClass().getName() 
             + "[family=" + getFamily ()
             + ",name=" + getFontName ()
             + ",style=" + styleString
             + ",size=" + getSize () + "]";
  }


  /**
   * Determines the line metrics for a run of text.
   *
   * @param str the text run to be measured.
   *
   * @param frc the font rendering parameters that are used for the
   *        measurement. The exact placement and size of text slightly
   *        depends on device-specific characteristics, for instance
   *        the device resolution or anti-aliasing.  For this reason,
   *        the returned measurement will only be accurate if the
   *        passed <code>FontRenderContext</code> correctly reflects
   *        the relevant parameters. Hence, <code>frc</code> should be
   *        obtained from the same <code>Graphics2D</code> that will
   *        be used for drawing, and any rendering hints should be set
   *        to the desired values before obtaining <code>frc</code>.
   *
   * @see java.awt.Graphics2D#getFontRenderContext()
   */
  public LineMetrics getLineMetrics(String str, FontRenderContext frc)
  {
    return getLineMetrics(str, 0, str.length() - 1, frc);
  }

  public boolean hasLayoutAttributes()
  {
    // TODO: Implement properly.
    return false;
  }

  /**
   * Reads the normal fields from the stream and then constructs the
   * peer from the style and size through getPeerFromToolkit().
   */
  private void readObject(ObjectInputStream ois)
    throws IOException, ClassNotFoundException
  {
    ois.defaultReadObject();

    HashMap attrs = new HashMap();
    ClasspathFontPeer.copyStyleToAttrs(style, attrs);
    ClasspathFontPeer.copySizeToAttrs(size, attrs);
    peer = getPeerFromToolkit(name, attrs);

  }
}
