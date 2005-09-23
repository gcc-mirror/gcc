/* QtFontPeer.java --
   Copyright (C)  2005  Free Software Foundation, Inc.

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

package gnu.java.awt.peer.qt;

import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.geom.Rectangle2D;
import java.awt.font.FontRenderContext;
import java.awt.font.GlyphVector;
import java.awt.font.LineMetrics;
import java.text.CharacterIterator;
import java.util.Locale;
import java.util.Map;
import java.awt.peer.FontPeer;

import gnu.java.awt.peer.ClasspathFontPeer;

public class QtFontPeer extends ClasspathFontPeer
{
  // Pointer to native QFont structure.
  private long nativeObject;
  private QtFontMetrics metrics;


  public QtFontPeer (String name, int style)
  {
    this(name, style, 12);
  }

  public QtFontPeer (String name, int style, int size)
  {
    super(name, style, size);
    init();
  }

  public QtFontPeer (String name, Map attributes)
  {
    super(name, attributes);
    init();
  }

  public void init()
  {
    if(this.familyName == null)
      throw new IllegalArgumentException("null family name");
    if(this.familyName.equals("Helvetica"))
      this.familyName = "sans serif";
    if(this.familyName.equals("Dialog"))
      this.familyName = "sans serif";
    create(this.familyName, this.style, (int)this.size);
    metrics = new QtFontMetrics(this);
  }

  /**
   * Creates the QFont object.
   */
  private native void create(String name, int style, int size);

  /**
   * Destroys the QFont.
   */
  public native void dispose();


  // ****************** ClasspathFontPeer Methods.

  public boolean canDisplay (Font font, char c)
  {
    return metrics.canDisplay( c );
  }

  public int canDisplayUpTo (Font font, CharacterIterator i, 
			     int start, int limit)
  {
    int index = start;
    char c = i.setIndex( index );
    while( index <= limit )
      {
	if(!canDisplay(font, c))
	  return index;
	index++;
	c = i.next();
      }
    return -1;
  }

  public String getSubFamilyName (Font font, Locale locale)
  {
    throw new UnsupportedOperationException();
  }

  public String getPostScriptName (Font font)
  {
    throw new UnsupportedOperationException();
  }

  public int getNumGlyphs (Font font)
  {
    throw new UnsupportedOperationException();
  }

  public int getMissingGlyphCode (Font font)
  {
    throw new UnsupportedOperationException();
  }

  public byte getBaselineFor (Font font, char c)
  {
    throw new UnsupportedOperationException();
  }

  public String getGlyphName (Font font, int glyphIndex)
  {
    throw new UnsupportedOperationException();
  }

  public GlyphVector createGlyphVector (Font font,
					FontRenderContext frc,
					CharacterIterator ci)
  {
    throw new UnsupportedOperationException();
  }

  public GlyphVector createGlyphVector (Font font, 
					FontRenderContext ctx, 
					int[] glyphCodes)
  {
    throw new UnsupportedOperationException();
  }

  public GlyphVector layoutGlyphVector (Font font, 
					FontRenderContext frc, 
					char[] chars, int start, 
					int limit, int flags)
  {
    throw new UnsupportedOperationException();
  }

  public FontMetrics getFontMetrics (Font font)
  {
    return new QtFontMetrics( this );
  }

  public boolean hasUniformLineMetrics (Font font)
  {
    throw new UnsupportedOperationException();
  }

  public LineMetrics getLineMetrics (Font font, 
				     CharacterIterator ci, 
				     int begin, int limit, 
				     FontRenderContext rc)
  {
    throw new UnsupportedOperationException();
  }

  public Rectangle2D getMaxCharBounds (Font font, 
				       FontRenderContext rc)
  {
    throw new UnsupportedOperationException();
  }

  public Rectangle2D getStringBounds (Font font, 
				      CharacterIterator ci, 
				      int begin, int limit, 
				      FontRenderContext frc)
  {
    int index = begin;
    String s = "" + ci.setIndex( index );
    while( index++ <= limit )
      s = s + ci.next();
    return metrics.getStringBounds(s);
  }
}
