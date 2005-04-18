/* ClasspathFontPeer.java -- Font peer used by GNU Classpath.
   Copyright (C) 2003, 2004 Free Software Foundation, Inc.

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


package gnu.java.awt.peer;

import gnu.java.awt.ClasspathToolkit;

import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Toolkit;
import java.awt.font.FontRenderContext;
import java.awt.font.GlyphVector;
import java.awt.font.LineMetrics;
import java.awt.font.TextAttribute;
import java.awt.font.TransformAttribute;
import java.awt.geom.AffineTransform;
import java.awt.geom.Rectangle2D;
import java.awt.peer.FontPeer;
import java.text.AttributedCharacterIterator;
import java.text.CharacterIterator;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

/**
 * A peer for fonts that are used inside Classpath. The purpose of
 * this interface is to abstract from platform-specific font handling
 * in the Classpath implementation of java.awt.Font and related
 * classes.
 *
 * <p><b>State kept by the peer:</b> a peer is generated for each Font
 * object in the default implementation. If you wish to share peers between
 * fonts, you will need to subclass both ClasspathFontPeer and
 * {@link ClasspathToolKit}.</p>
 * 
 * <p><b>Thread Safety:</b> Methods of this interface may be called
 * from arbitrary threads at any time. Implementations of the
 * <code>ClasspathFontPeer</code> interface are required to perform
 * the necessary synchronization.</p>
 *
 * @see java.awt.Font#getPeer
 * @see java.awt.Toolkit#getFontPeer
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 * @author Graydon Hoare (graydon@redhat.com)
 */
public abstract class ClasspathFontPeer
  implements FontPeer
{

  /*************************************************************************/
  
  /*
   * Instance Variables
   */
  
  /**
   * The 3 names of this font. all fonts have 3 names, some of which
   * may be equal:
   *
   * logical -- name the font was constructed from
   * family  -- a designer or brand name (Helvetica)
   * face -- specific instance of a design (Helvetica Regular)
   *
   * @see isLogicalFontName 
   */
  
  protected String logicalName;
  protected String familyName;
  protected String faceName;
  
  /**
   * The font style, which is a combination (by OR-ing) of the font style
   * constants PLAIN, BOLD and ITALIC, in this class.
   */
  protected int style;
  
  /**
   * The font point size. A point is 1/72 of an inch.
   */
  protected float size;

  /**
   * The affine transformation the font is currently subject to.
   */
  protected AffineTransform transform;

  protected static ClasspathToolkit tk()
  {
    return (ClasspathToolkit)(Toolkit.getDefaultToolkit ());
  }

  /* 
   * Confusingly, a Logical Font is a concept unrelated to
   * a Font's Logical Name. 
   *
   * A Logical Font is one of 6 built-in, abstract font types
   * which must be supported by any java environment: SansSerif,
   * Serif, Monospaced, Dialog, and DialogInput. 
   *
   * A Font's Logical Name is the name the font was constructed
   * from. This might be the name of a Logical Font, or it might
   * be the name of a Font Face.
   */

  protected static boolean isLogicalFontName(String name)
  {
    String uname = name.toUpperCase ();
    return (uname.equals ("SANSSERIF") ||
            uname.equals ("SERIF") ||
            uname.equals ("MONOSPACED") ||
            uname.equals ("DIALOG") ||
            uname.equals ("DIALOGINPUT"));
  }

  protected static String logicalFontNameToFaceName (String name)
  {
    String uname = name.toUpperCase ();
    if (uname.equals("SANSSERIF"))
      return "Helvetica";
    else if (uname.equals ("SERIF"))
      return "Times";
    else if (uname.equals ("MONOSPACED"))
      return "Courier";
    else if (uname.equals ("DIALOG"))
      return "Helvetica";
    else if (uname.equals ("DIALOGINPUT"))
      return "Helvetica";
    else
      return "Helvetica";
  }

  protected static String faceNameToFamilyName (String name)
  {
    return name;
  }

  public static void copyStyleToAttrs (int style, Map attrs)
  {
    if ((style & Font.BOLD) == Font.BOLD)
      attrs.put (TextAttribute.WEIGHT, TextAttribute.WEIGHT_BOLD);
    else
      attrs.put (TextAttribute.WEIGHT, TextAttribute.WEIGHT_REGULAR);

    if ((style & Font.ITALIC) == Font.ITALIC)
      attrs.put (TextAttribute.POSTURE, TextAttribute.POSTURE_OBLIQUE);
    else
      attrs.put (TextAttribute.POSTURE, TextAttribute.POSTURE_REGULAR);
  }

  protected static void copyFamilyToAttrs (String fam, Map attrs)
  {
    if (fam != null)
      attrs.put (TextAttribute.FAMILY, fam);
  }
  
  public static void copySizeToAttrs (float size, Map attrs)
  {
    attrs.put (TextAttribute.SIZE, new Float (size));
  }
  
  protected static void copyTransformToAttrs (AffineTransform trans, Map attrs)
  {
    if (trans != null)
      attrs.put(TextAttribute.TRANSFORM, new TransformAttribute (trans));
  }


  protected void setStandardAttributes (String name, String family, int style, 
                                        float size, AffineTransform trans)
  {
    this.logicalName = name;

    if (isLogicalFontName (name))
      this.faceName = logicalFontNameToFaceName (name);
    else
      this.faceName = name;

    if (family != null)
      this.familyName = family;
    else
      this.familyName = faceNameToFamilyName (faceName);
    
    this.style = style;
    this.size = size;
    this.transform = trans;
  }


  protected void setStandardAttributes (String name, Map attribs)
  {
    String family = this.familyName;
    AffineTransform trans = this.transform;
    float size = this.size;
    int style = this.style;

    if (attribs.containsKey (TextAttribute.FAMILY))
      family = (String) attribs.get (TextAttribute.FAMILY);

    if (name == null)
      name = "SansSerif";

    if (attribs.containsKey (TextAttribute.WEIGHT))
      {
        Float weight = (Float) attribs.get (TextAttribute.WEIGHT);
        if (weight.floatValue () >= TextAttribute.WEIGHT_BOLD.floatValue ())
          style += Font.BOLD;
      }

    if (attribs.containsKey (TextAttribute.POSTURE))
      {
        Float posture = (Float) attribs.get (TextAttribute.POSTURE);
        if (posture.floatValue () >= TextAttribute.POSTURE_OBLIQUE.floatValue ())
          style += Font.ITALIC;
      }

    if (attribs.containsKey (TextAttribute.SIZE))
      {
        Float sz = (Float) attribs.get (TextAttribute.SIZE);
        size = sz.floatValue ();

        // Pango doesn't accept 0 as a font size.
        if (size < 1)
          size = 1;
      }
    else
      size = 12;

    if (attribs.containsKey (TextAttribute.TRANSFORM))
      {
        TransformAttribute ta = (TransformAttribute) 
          attribs.get(TextAttribute.TRANSFORM);
        trans = ta.getTransform ();        
      }

    setStandardAttributes (name, family, style, size, trans);
  }

  protected void getStandardAttributes (Map attrs)
  {    
    copyFamilyToAttrs (this.familyName, attrs);
    copySizeToAttrs (this.size, attrs);
    copyStyleToAttrs (this.style, attrs);
    copyTransformToAttrs (this.transform, attrs);
  }


  /* Begin public API */

  public ClasspathFontPeer (String name, Map attrs)
  {
    setStandardAttributes (name, attrs);
  }

  public ClasspathFontPeer (String name, int style, int size)
  {
    setStandardAttributes (name, (String)null, style, 
                           (float)size, (AffineTransform)null);
  }

  /** 
   * Implementation of {@link Font#getName}
   *
   * @param font the font this peer is being called from. This may be
   * useful if you are sharing peers between Font objects. Otherwise it may
   * be ignored.
   */

  public String getName (Font font) 
  { 
    return logicalName; 
  }

  /** 
   * Implementation of {@link Font#getFamily()}
   *
   * @param font the font this peer is being called from. This may be
   * useful if you are sharing peers between Font objects. Otherwise it may
   * be ignored.
   */

  public String getFamily (Font font) 
  { 
    return familyName; 
  }

  /** 
   * Implementation of {@link Font#getFamily(Locale)}
   *
   * @param font the font this peer is being called from. This may be
   * useful if you are sharing peers between Font objects. Otherwise it may
   * be ignored.
   */

  public String getFamily (Font font, Locale lc) 
  { 
    return familyName; 
  }

  /** 
   * Implementation of {@link Font#getFontName()}
   *
   * @param font the font this peer is being called from. This may be
   * useful if you are sharing peers between Font objects. Otherwise it may
   * be ignored.
   */

  public String getFontName (Font font) 
  { 
    return faceName; 
  }

  /** 
   * Implementation of {@link Font#getFontName(Locale)}
   *
   * @param font the font this peer is being called from. This may be
   * useful if you are sharing peers between Font objects. Otherwise it may
   * be ignored.
   */

  public String getFontName (Font font, Locale lc) 
  { 
    return faceName; 
  }

  /** 
   * Implementation of {@link Font#getSize}
   *
   * @param font the font this peer is being called from. This may be
   * useful if you are sharing peers between Font objects. Otherwise it may
   * be ignored.
   */

  public float getSize (Font font) 
  { 
    return size; 
  }

  /** 
   * Implementation of {@link Font#isPlain}
   *
   * @param font the font this peer is being called from. This may be
   * useful if you are sharing peers between Font objects. Otherwise it may
   * be ignored.
   */
  
  public boolean isPlain (Font font) 
  { 
    return style == Font.PLAIN; 
  }

  /** 
   * Implementation of {@link Font#isBold}
   *
   * @param font the font this peer is being called from. This may be
   * useful if you are sharing peers between Font objects. Otherwise it may
   * be ignored.
   */
  
  public boolean isBold (Font font) 
  { 
    return ((style & Font.BOLD) == Font.BOLD); 
  }

  /** 
   * Implementation of {@link Font#isItalic}
   *
   * @param font the font this peer is being called from. This may be
   * useful if you are sharing peers between Font objects. Otherwise it may
   * be ignored.
   */

  public boolean isItalic (Font font) 
  { 
    return ((style & Font.ITALIC) == Font.ITALIC); 
  }

  /** 
   * Implementation of {@link Font#deriveFont(int, float)}
   *
   * @param font the font this peer is being called from. This may be
   * useful if you are sharing peers between Font objects. Otherwise it may
   * be ignored.
   */

  public Font deriveFont (Font font, int style, float size)
  {
    Map attrs = new HashMap ();
    getStandardAttributes (attrs);
    copyStyleToAttrs (style, attrs);
    copySizeToAttrs (size, attrs);
    return tk().getFont (logicalName, attrs);
  }

  /** 
   * Implementation of {@link Font#deriveFont(float)}
   *
   * @param font the font this peer is being called from. This may be
   * useful if you are sharing peers between Font objects. Otherwise it may
   * be ignored.
   */

  public Font deriveFont (Font font, float size)
  {
    Map attrs = new HashMap ();
    getStandardAttributes (attrs);
    copySizeToAttrs (size, attrs);
    return tk().getFont (logicalName, attrs);
  }

  /** 
   * Implementation of {@link Font#deriveFont(int)}
   *
   * @param font the font this peer is being called from. This may be
   * useful if you are sharing peers between Font objects. Otherwise it may
   * be ignored.
   */

  public Font deriveFont (Font font, int style)
  {
    Map attrs = new HashMap ();
    getStandardAttributes (attrs);
    copyStyleToAttrs (style, attrs);
    return tk().getFont (logicalName, attrs);
  }

  /** 
   * Implementation of {@link Font#deriveFont(int, AffineTransform)}
   *
   * @param font the font this peer is being called from. This may be
   * useful if you are sharing peers between Font objects. Otherwise it may
   * be ignored.
   */

  public Font deriveFont (Font font, int style, AffineTransform t)
  {
    Map attrs = new HashMap ();
    getStandardAttributes (attrs);
    copyStyleToAttrs (style, attrs);
    copyTransformToAttrs (t, attrs);
    return tk().getFont (logicalName, attrs);
  }

  /** 
   * Implementation of {@link Font#deriveFont(AffineTransform)}
   *
   * @param font the font this peer is being called from. This may be
   * useful if you are sharing peers between Font objects. Otherwise it may
   * be ignored.
   */

  public Font deriveFont (Font font, AffineTransform t)
  {
    Map attrs = new HashMap ();
    getStandardAttributes (attrs);
    copyTransformToAttrs (t, attrs);
    return tk().getFont (logicalName, attrs);
  }

  /** 
   * Implementation of {@link Font#deriveFont(Map)}
   *
   * @param font the font this peer is being called from. This may be
   * useful if you are sharing peers between Font objects. Otherwise it may
   * be ignored.
   */

  public Font deriveFont (Font font, Map attrs)
  {
    return tk().getFont (logicalName, attrs);
  }

  /** 
   * Implementation of {@link Font#getAttributes()}
   *
   * @param font the font this peer is being called from. This may be
   * useful if you are sharing peers between Font objects. Otherwise it may
   * be ignored.
   */

  public Map getAttributes (Font font)
  {
    HashMap h = new HashMap ();
    getStandardAttributes (h);
    return h;
  }

  /** 
   * Implementation of {@link Font#getAvailableAttributes()}
   *
   * @param font the font this peer is being called from. This may be
   * useful if you are sharing peers between Font objects. Otherwise it may
   * be ignored.
   */

  public AttributedCharacterIterator.Attribute[] getAvailableAttributes(Font font)
  {
    AttributedCharacterIterator.Attribute a[] = 
      new AttributedCharacterIterator.Attribute[5];
    a[0] = TextAttribute.FAMILY;
    a[1] = TextAttribute.SIZE;
    a[2] = TextAttribute.POSTURE;
    a[3] = TextAttribute.WEIGHT;
    a[4] = TextAttribute.TRANSFORM;
    return a;
  }

  /** 
   * Implementation of {@link Font#getTransform()}
   *
   * @param font the font this peer is being called from. This may be
   * useful if you are sharing peers between Font objects. Otherwise it may
   * be ignored.
   */

  public AffineTransform getTransform (Font font)
  {
    if (transform == null)
      transform = new AffineTransform ();
    return transform;
  }

  /** 
   * Implementation of {@link Font#isTransformed()}
   *
   * @param font the font this peer is being called from. This may be
   * useful if you are sharing peers between Font objects. Otherwise it may
   * be ignored.
   */

  public boolean isTransformed (Font font)
  {
    return ! transform.isIdentity ();
  }

  /** 
   * Implementation of {@link Font#getItalicAngle()}
   *
   * @param font the font this peer is being called from. This may be
   * useful if you are sharing peers between Font objects. Otherwise it may
   * be ignored.
   */

  public float getItalicAngle (Font font)
  {
    if ((style & Font.ITALIC) == Font.ITALIC)
      return TextAttribute.POSTURE_OBLIQUE.floatValue ();
    else
      return TextAttribute.POSTURE_REGULAR.floatValue ();
  }


  /** 
   * Implementation of {@link Font#getStyle()}
   *
   * @param font the font this peer is being called from. This may be
   * useful if you are sharing peers between Font objects. Otherwise it may
   * be ignored.
   */

  public int getStyle (Font font) 
  { 
    return style; 
  }




  /* Remaining methods are abstract */

  /** 
   * Implementation of {@link Font#canDisplay(char)}
   *
   * @param font the font this peer is being called from. This may be
   * useful if you are sharing peers between Font objects. Otherwise it may
   * be ignored.
   */

  public abstract boolean canDisplay (Font font, char c);

  /** 
   * Implementation of {@link Font#canDisplay(String)},
   * {@link Font#canDisplay(char [], int, int)}, and
   * {@link Font#canDisplay(CharacterIterator, int, int)}.
   *
   * @param font the font this peer is being called from. This may be
   * useful if you are sharing peers between Font objects. Otherwise it may
   * be ignored.
   */

  public abstract int canDisplayUpTo (Font font, CharacterIterator i, int start, int limit);


  /**
   * Returns the name of this font face inside the family, for example
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

  public abstract String getSubFamilyName (Font font, Locale locale);
  

  /** 
   * Implementation of {@link Font#getPSName()}
   *
   * @param font the font this peer is being called from. This may be
   * useful if you are sharing peers between Font objects. Otherwise it may
   * be ignored.
   */

  public abstract String getPostScriptName (Font font);


  /** 
   * Implementation of {@link Font#getNumGlyphs()}
   *
   * @param font the font this peer is being called from. This may be
   * useful if you are sharing peers between Font objects. Otherwise it may
   * be ignored.
   */

  public abstract int getNumGlyphs (Font font);


  /** 
   * Implementation of {@link Font#getMissingGlyphCode()}
   *
   * @param font the font this peer is being called from. This may be
   * useful if you are sharing peers between Font objects. Otherwise it may
   * be ignored.
   */

  public abstract int getMissingGlyphCode (Font font);


  /** 
   * Implementation of {@link Font#getBaselineFor(char)}
   *
   * @param font the font this peer is being called from. This may be
   * useful if you are sharing peers between Font objects. Otherwise it may
   * be ignored.
   */

  public abstract byte getBaselineFor (Font font, char c);


  /**
   * Returns a name for the specified glyph. This is useful for
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

  public abstract String getGlyphName (Font font, int glyphIndex);


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

  public abstract GlyphVector createGlyphVector (Font font,
                                                 FontRenderContext frc,
                                                 CharacterIterator ci);
  

  /** 
   * Implementation of {@link Font#createGlyphVector(FontRenderContext,
   * int[])}.
   *
   * @param font the font object that the created GlyphVector will return
   * when it gets asked for its font. This argument is needed because the
   * public API of {@link GlyphVector} works with {@link java.awt.Font},
   * not with font peers.
   */

  public abstract GlyphVector createGlyphVector (Font font, 
                                                 FontRenderContext ctx, 
                                                 int[] glyphCodes);


  /** 
   * Implementation of {@link Font#layoutGlyphVector(FontRenderContext,
   * char[], int, int, int)}.
   *
   * @param font the font object that the created GlyphVector will return
   * when it gets asked for its font. This argument is needed because the
   * public API of {@link GlyphVector} works with {@link java.awt.Font},
   * not with font peers.
   */

  public abstract GlyphVector layoutGlyphVector (Font font, 
                                                 FontRenderContext frc, 
                                                 char[] chars, int start, 
                                                 int limit, int flags);


  /** 
   * Implementation of {@link Font#getFontMetrics()}
   *
   * @param font the font this peer is being called from. This may be
   * useful if you are sharing peers between Font objects. Otherwise it may
   * be ignored.
   */

  public abstract FontMetrics getFontMetrics (Font font);


  /** 
   * Implementation of {@link Font#hasUniformLineMetrics()}
   *
   * @param font the font this peer is being called from. This may be
   * useful if you are sharing peers between Font objects. Otherwise it may
   * be ignored.
   */

  public abstract boolean hasUniformLineMetrics (Font font);


  /** 
   * Implementation of {@link Font#getLineMetrics(CharacterIterator, int,
   * int, FontRenderContext)}
   *
   * @param font the font this peer is being called from. This may be
   * useful if you are sharing peers between Font objects. Otherwise it may
   * be ignored.
   */

  public abstract LineMetrics getLineMetrics (Font font, 
                                              CharacterIterator ci, 
                                              int begin, int limit, 
                                              FontRenderContext rc);

  /** 
   * Implementation of {@link Font#getMaxCharBounds(FontRenderContext)}
   *
   * @param font the font this peer is being called from. This may be
   * useful if you are sharing peers between Font objects. Otherwise it may
   * be ignored.
   */

  public abstract Rectangle2D getMaxCharBounds (Font font, 
                                                FontRenderContext rc);

  /** 
   * Implementation of {@link Font#getStringBounds(CharacterIterator, int,
   * int, FontRenderContext)}
   *
   * @param font the font this peer is being called from. This may be
   * useful if you are sharing peers between Font objects. Otherwise it may
   * be ignored.
   */

  public abstract Rectangle2D getStringBounds (Font font, 
                                               CharacterIterator ci, 
                                               int begin, int limit, 
                                               FontRenderContext frc);

}
