/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.awt;

/**
 * Status:  Stubbed; A very incomplete implementation.
 */

public class FontMetrics implements java.io.Serializable
{
  protected Font font;
  
  protected FontMetrics(Font font)
  {
    this.font = font;
  }

  public Font getFont()
  {
    return font;
  }

  public int getLeading()
  {
    // FIXME??
    return getHeight() - (getDescent() + getAscent());
  }

  public int getAscent()
  {
    // FIXME??
    return getHeight() - (getDescent() + getLeading());
  }

  public int getDescent()
  {
    // FIXME??
    return getHeight() - getDescent();
  }

  public int getHeight()
  {
    // FIXME??
    return getLeading() + getAscent() + getDescent();
  }

  public int getMaxAscent()
  {
    // FIXME
    return 0;
  }

  public int getMaxDescent()
  {
    // FIXME
    return 0;
  }

  /* @deprecated Use getMaxDescent() instead. */
  public int getMaxDecent()
  {
    return getMaxDescent();
  }

  public int getMaxAdvance()
  {
    // FIXME
    return 0;
  }

  public int charWidth(int ch)
  {
    // FIXME
    return 0;
  }

  public int charWidth(char ch)
  {
    // FIXME
    return 0;
  }

  public int stringWidth(String str)
  {
    return charsWidth(str.toCharArray(), 0, str.length());
  }

  public int charsWidth(char[] data, int off, int len)
  {
    // FIXME
    return -1;
  }

  public int bytesWidth(byte[] data, int off, int len)
  {
    // FIXME?
    return -1;
  }

  public int[] getWidths()
  {
    // FIXME
    return new int[0];
  }

  public boolean hasUniformLineMetrics()
  {
    // FIXME
    return false;
  }

  // Don't have LineMetrics yet...
  /*
  public LineMetrics getLineMetrics(String str, Graphics context)

  public LineMetrics getLineMetrics(String str, int beginIndex, int limit,
                                    Graphics context)

  public LineMetrics getLineMetrics(char[] chars, int beginIndex, int limit,
                                    Graphics context)

  public LineMetrics getLineMetrics(CharacterIterator ci, int beginIndex,
				    int limit, Graphics context)
  */

  // Don't have Java2D yet.
  /*
  public Rectangle2D getStringBounds(String str, Graphics context)

  public Rectangle2D getStringBounds(String str, int beginIndex, int limit,
                                     Graphics context)

  public Rectangle2D getStringBounds(char[] chars, int beginIndex, int limit,
                                     Graphics context)

  public Rectangle2D getStringBounds(CharacterIterator ci, int beginIndex,
                                     int limit, Graphics context)

  public Rectangle2D getMaxCharBounds(Graphics context)
  */

  public String toString()
  {
    return this.getClass() + "[font=" + font + ",ascent=" + getAscent() 
	   + ",descent=" + getDescent() + ",height=" + getHeight() + "]";
  }
}
