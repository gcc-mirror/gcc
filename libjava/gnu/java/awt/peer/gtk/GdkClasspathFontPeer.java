/* GdkClasspathFontPeer.java -- backend implementation for Font object
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


package gnu.java.awt.peer.gtk;

import java.awt.*;
import java.awt.font.*;
import java.awt.geom.*;
import java.io.InputStream;
import java.io.IOException;
import java.io.Serializable;
import java.util.Locale;
import java.util.Map;
import java.util.StringTokenizer;
import java.text.CharacterIterator;
import java.text.AttributedCharacterIterator;
import java.text.StringCharacterIterator;
import java.awt.font.TextAttribute;
import gnu.classpath.Configuration;
import gnu.java.awt.peer.ClasspathFontPeer;

/**
 * This class represents a windowing system font using the Pango
 * unicode/glyph/font library and the Cairo rendering library.
 *
 * @author Graydon Hoare (graydon@redhat.com)
 */

public class GdkClasspathFontPeer extends ClasspathFontPeer
{
  
  static 
  {
    if (Configuration.INIT_LOAD_LIBRARY)
      {
        System.loadLibrary("gtkpeer");
      }

    if (GtkToolkit.useGraphics2D ())
      initStaticState ();
  }
  native static void initStaticState ();
  private final int native_state = GtkGenericPeer.getUniqueInteger ();


  /* Instance Variables */

  private native void initState ();
  private native void dispose ();
  private native void setFont (String family, int style, int size);

  protected void sync ()
  {
    this.setFont (this.familyName, this.style, (int)this.size);
  }

  protected void finalize ()
  {
    dispose ();
  }

  /* 
   * Helpers for the 3-way overloading that this class seems to suffer
   * from. Remove them if you feel like they're a performance bottleneck,
   * for the time being I prefer my code not be written and debugged in
   * triplicate.
   */

  private String buildString(CharacterIterator i) {
    String s = new String ();
    for(char c = i.first(); c != CharacterIterator.DONE; c = i.next()) 
      s += c;
    return s;
  }

  private String buildString(CharacterIterator iter, int begin, int limit) {
    String s = new String ();
    int i = 0;
    for(char c = iter.first(); c != CharacterIterator.DONE; c = iter.next(), i++) 
      {
        if (begin <= i)
          s += c;
        if (limit <= i)
          break;
      }
    return s;
  }
  
  private String buildString(char[] chars, int begin, int limit) {
    String s = new String ();
    for(int i = begin; i <= limit; i++)
      s += chars[i];
    return s;
  }

  /* Public API */

  public GdkClasspathFontPeer (String name, int style, int size)
  {  
    super(name, style, size);    
    initState ();
    setFont (this.familyName, this.style, (int)this.size);
  }

  public GdkClasspathFontPeer (String name, Map attributes)
  {
    super(name, attributes);
    initState ();
    setFont (this.familyName, this.style, (int)this.size);
  }

  public String getSubFamilyName(Font font, Locale locale)
  {
    return null;
  }

  public String getPostScriptName(Font font)
  {
    return null;
  }

  public boolean canDisplay (Font font, char c)
  {
    throw new UnsupportedOperationException ();
  }

  public int canDisplayUpTo (Font font, CharacterIterator i, int start, int limit)
  {
    throw new UnsupportedOperationException ();
  }

  public GlyphVector createGlyphVector (Font font, 
                                        FontRenderContext ctx, 
                                        CharacterIterator i)
  {
    return new GdkGlyphVector(font, this, ctx, buildString (i));
  }

  public GlyphVector createGlyphVector (Font font, 
                                        FontRenderContext ctx, 
                                        int[] glyphCodes)
  {
    return new GdkGlyphVector (font, this, ctx, glyphCodes);
  }

  public byte getBaselineFor (Font font, char c)
  {
    throw new UnsupportedOperationException ();
  }

  protected class GdkFontLineMetrics extends LineMetrics
  {
    FontMetrics fm;
    int nchars; 

    public GdkFontLineMetrics (FontMetrics m, int n)
    {
      fm = m;
      nchars = n;
    }

    public float getAscent()
    {
      return (float) fm.getAscent ();
    }
  
    public int getBaselineIndex()
    {
      return Font.ROMAN_BASELINE;
    }
    
    public float[] getBaselineOffsets()
    {
      return new float[3];
    }
    
    public float getDescent()
    {
      return (float) fm.getDescent ();
    }
    
    public float getHeight()
    {
      return (float) fm.getHeight ();
    }
    
    public float getLeading() { return 0.f; }    
    public int getNumChars() { return nchars; }
    public float getStrikethroughOffset() { return 0.f; }    
    public float getStrikethroughThickness() { return 0.f; }  
    public float getUnderlineOffset() { return 0.f; }
    public float getUnderlineThickness() { return 0.f; }

  }


  public LineMetrics getLineMetrics (Font font, CharacterIterator ci, 
                                     int begin, int limit, FontRenderContext rc)
  {
    return new GdkFontLineMetrics (getFontMetrics (font), limit - begin);
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
    throw new UnsupportedOperationException ();
  }

  public Rectangle2D getStringBounds (Font font, CharacterIterator ci, 
                                      int begin, int limit, FontRenderContext frc)
  {
    throw new UnsupportedOperationException ();
  }

  public boolean hasUniformLineMetrics (Font font)
  {
    return true;
  }

  public GlyphVector layoutGlyphVector (Font font, FontRenderContext frc, 
                                        char[] chars, int start, int limit, 
                                        int flags)
  {
    int nchars = (limit - start) + 1;
    char[] nc = new char[nchars];

    for (int i = 0; i < nchars; ++i)
      nc[i] = chars[start + i];

    return createGlyphVector (font, frc, 
                              new StringCharacterIterator (new String (nc)));
  }

  public LineMetrics getLineMetrics (Font font, String str, 
                                     FontRenderContext frc)
  {
    return new GdkFontLineMetrics (getFontMetrics (font), str.length ());
  }

  public FontMetrics getFontMetrics (Font font)
  {
    return new GdkClasspathFontPeerMetrics (font);
  }

}

