/* TextLayout.java --
   Copyright (C) 2006  Free Software Foundation, Inc.

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


package java.awt.font;

import gnu.java.lang.CPStringBuilder;

import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.Shape;
import java.awt.geom.AffineTransform;
import java.awt.geom.Line2D;
import java.awt.geom.Rectangle2D;
import java.awt.geom.GeneralPath;
import java.awt.geom.Point2D;
import java.text.CharacterIterator;
import java.text.AttributedCharacterIterator;
import java.text.Bidi;
import java.util.ArrayList;
import java.util.Map;

/**
 * @author Sven de Marothy
 */
public final class TextLayout implements Cloneable
{
  /**
   * Holds the layout data that belongs to one run of characters.
   */
  private class Run
  {
    /**
     * The actual glyph vector.
     */
    GlyphVector glyphVector;

    /**
     * The font for this text run.
     */
    Font font;

    /**
     * The start of the run.
     */
    int runStart;

    /**
     * The end of the run.
     */
    int runEnd;

    /**
     * The layout location of the beginning of the run.
     */
    float location;

    /**
     * Initializes the Run instance.
     *
     * @param gv the glyph vector
     * @param start the start index of the run
     * @param end the end index of the run
     */
    Run(GlyphVector gv, Font f, int start, int end)
    {
      glyphVector = gv;
      font = f;
      runStart = start;
      runEnd = end;
    }

    /**
     * Returns <code>true</code> when this run is left to right,
     * <code>false</code> otherwise.
     *
     * @return <code>true</code> when this run is left to right,
     *         <code>false</code> otherwise
     */
    boolean isLeftToRight()
    {
      return (glyphVector.getLayoutFlags() & GlyphVector.FLAG_RUN_RTL) == 0;
    }
  }

  /**
   * The laid out character runs.
   */
  private Run[] runs;

  private FontRenderContext frc;
  private char[] string;
  private int offset;
  private int length;
  private Rectangle2D boundsCache;
  private LineMetrics lm;

  /**
   * The total advance of this text layout. This is cache for maximum
   * performance.
   */
  private float totalAdvance = -1F;

  /**
   * The cached natural bounds.
   */
  private Rectangle2D naturalBounds;

  /**
   * Character indices.
   * Fixt index is the glyphvector, second index is the (first) glyph.
   */
  private int[][] charIndices;

  /**
   * Base directionality, determined from the first char.
   */
  private boolean leftToRight;

  /**
   * Whether this layout contains whitespace or not.
   */
  private boolean hasWhitespace = false;

  /**
   * The {@link Bidi} object that is used for reordering and by
   * {@link #getCharacterLevel(int)}.
   */
  private Bidi bidi;

  /**
   * Mpas the logical position of each individual character in the original
   * string to its visual position.
   */
  private int[] logicalToVisual;

  /**
   * Maps visual positions of a character to its logical position
   * in the original string.
   */
  private int[] visualToLogical;

  /**
   * The cached hashCode.
   */
  private int hash;

  /**
   * The default caret policy.
   */
  public static final TextLayout.CaretPolicy DEFAULT_CARET_POLICY =
    new CaretPolicy();

  /**
   * Constructs a TextLayout.
   */
  public TextLayout (String str, Font font, FontRenderContext frc)
  {
    this.frc = frc;
    string = str.toCharArray();
    offset = 0;
    length = this.string.length;
    lm = font.getLineMetrics(this.string, offset, length, frc);

    // Get base direction and whitespace info
    getStringProperties();

    if (Bidi.requiresBidi(string, offset, offset + length))
      {
        bidi = new Bidi(str, leftToRight ? Bidi.DIRECTION_LEFT_TO_RIGHT
                                         : Bidi.DIRECTION_RIGHT_TO_LEFT );
        int rc = bidi.getRunCount();
        byte[] table = new byte[ rc ];
        for(int i = 0; i < table.length; i++)
          table[i] = (byte)bidi.getRunLevel(i);

        runs = new Run[rc];
        for(int i = 0; i < rc; i++)
          {
            int start = bidi.getRunStart(i);
            int end = bidi.getRunLimit(i);
            if(start != end) // no empty runs.
              {
                GlyphVector gv = font.layoutGlyphVector(frc,
                                                        string, start, end,
                           ((table[i] & 1) == 0) ? Font.LAYOUT_LEFT_TO_RIGHT
                                                 : Font.LAYOUT_RIGHT_TO_LEFT );
                runs[i] = new Run(gv, font, start, end);
              }
          }
        Bidi.reorderVisually( table, 0, runs, 0, runs.length );
        // Clean up null runs.
        ArrayList cleaned = new ArrayList(rc);
        for (int i = 0; i < rc; i++)
          {
            if (runs[i] != null)
              cleaned.add(runs[i]);
          }
        runs = new Run[cleaned.size()];
        runs = (Run[]) cleaned.toArray(runs);
      }
    else
      {
        GlyphVector gv = font.layoutGlyphVector( frc, string, offset, length,
                                     leftToRight ? Font.LAYOUT_LEFT_TO_RIGHT
                                                 : Font.LAYOUT_RIGHT_TO_LEFT );
        Run run = new Run(gv, font, 0, length);
        runs = new Run[]{ run };
      }
    setCharIndices();
    setupMappings();
    layoutRuns();
  }

  public TextLayout (String string,
                     Map<? extends AttributedCharacterIterator.Attribute, ?> attributes,
                     FontRenderContext frc)
  {
    this( string, new Font( attributes ), frc );
  }

  public TextLayout (AttributedCharacterIterator text, FontRenderContext frc)
  {
    // FIXME: Very rudimentary.
    this(getText(text), getFont(text), frc);
  }

  /**
   * Package-private constructor to make a textlayout from an existing one.
   * This is used by TextMeasurer for returning sub-layouts, and it
   * saves a lot of time in not having to relayout the text.
   */
  TextLayout(TextLayout t, int startIndex, int endIndex)
  {
    frc = t.frc;
    boundsCache = null;
    lm = t.lm;
    leftToRight = t.leftToRight;

    if( endIndex > t.getCharacterCount() )
      endIndex = t.getCharacterCount();
    string = t.string;
    offset = startIndex + offset;
    length = endIndex - startIndex;

    int startingRun = t.charIndices[startIndex][0];
    int nRuns = 1 + t.charIndices[endIndex - 1][0] - startingRun;

    runs = new Run[nRuns];
    for( int i = 0; i < nRuns; i++ )
      {
        Run run = t.runs[i + startingRun];
        GlyphVector gv = run.glyphVector;
        Font font = run.font;
        // Copy only the relevant parts of the first and last runs.
        int beginGlyphIndex = (i > 0) ? 0 : t.charIndices[startIndex][1];
        int numEntries = ( i < nRuns - 1) ? gv.getNumGlyphs() :
          1 + t.charIndices[endIndex - 1][1] - beginGlyphIndex;

        int[] codes = gv.getGlyphCodes(beginGlyphIndex, numEntries, null);
        gv = font.createGlyphVector(frc, codes);
        runs[i] = new Run(gv, font, run.runStart - startIndex,
                          run.runEnd - startIndex);
      }
    runs[nRuns - 1].runEnd = endIndex - 1;

    setCharIndices();
    setupMappings();
    determineWhiteSpace();
    layoutRuns();
  }

  private void setCharIndices()
  {
    charIndices = new int[ getCharacterCount() ][2];
    int i = 0;
    int currentChar = 0;
    for(int run = 0; run < runs.length; run++)
      {
        currentChar = -1;
        Run current = runs[run];
        GlyphVector gv = current.glyphVector;
        for( int gi = 0; gi < gv.getNumGlyphs(); gi++)
          {
            if( gv.getGlyphCharIndex( gi ) != currentChar )
              {
                charIndices[ i ][0] = run;
                charIndices[ i ][1] = gi;
                currentChar = gv.getGlyphCharIndex( gi );
                i++;
              }
          }
      }
  }

  /**
   * Initializes the logicalToVisual and visualToLogial maps.
   */
  private void setupMappings()
  {
    int numChars = getCharacterCount();
    logicalToVisual = new int[numChars];
    visualToLogical = new int[numChars];
    int lIndex = 0;
    int vIndex = 0;
    // We scan the runs in visual order and set the mappings accordingly.
    for (int i = 0; i < runs.length; i++)
      {
        Run run = runs[i];
        if (run.isLeftToRight())
          {
            for (lIndex = run.runStart; lIndex < run.runEnd; lIndex++)
              {
                logicalToVisual[lIndex] = vIndex;
                visualToLogical[vIndex] = lIndex;
                vIndex++;
              }
          }
        else
          {
            for (lIndex = run.runEnd - 1; lIndex >= run.runStart; lIndex--)
              {
                logicalToVisual[lIndex] = vIndex;
                visualToLogical[vIndex] = lIndex;
                vIndex++;
              }
          }
      }
  }

  private static String getText(AttributedCharacterIterator iter)
  {
    CPStringBuilder sb = new CPStringBuilder();
    int idx = iter.getIndex();
    for(char c = iter.first(); c != CharacterIterator.DONE; c = iter.next())
      sb.append(c);
    iter.setIndex( idx );
    return sb.toString();
  }

  private static Font getFont(AttributedCharacterIterator iter)
  {
    Font f = (Font)iter.getAttribute(TextAttribute.FONT);
    if( f == null )
      {
        int size;
        Float i = (Float)iter.getAttribute(TextAttribute.SIZE);
        if( i != null )
          size = (int)i.floatValue();
        else
          size = 14;
        f = new Font("Dialog", Font.PLAIN, size );
      }
    return f;
  }

  /**
   * Scan the character run for the first strongly directional character,
   * which in turn defines the base directionality of the whole layout.
   */
  private void getStringProperties()
  {
    boolean gotDirection = false;
    int i = offset;
    int endOffs = offset + length;
    leftToRight = true;
    while( i < endOffs && !gotDirection )
      switch( Character.getDirectionality(string[i++]) )
        {
        case Character.DIRECTIONALITY_LEFT_TO_RIGHT:
        case Character.DIRECTIONALITY_LEFT_TO_RIGHT_EMBEDDING:
        case Character.DIRECTIONALITY_LEFT_TO_RIGHT_OVERRIDE:
          gotDirection = true;
          break;

        case Character.DIRECTIONALITY_RIGHT_TO_LEFT:
        case Character.DIRECTIONALITY_RIGHT_TO_LEFT_ARABIC:
        case Character.DIRECTIONALITY_RIGHT_TO_LEFT_EMBEDDING:
        case Character.DIRECTIONALITY_RIGHT_TO_LEFT_OVERRIDE:
          leftToRight = false;
          gotDirection = true;
          break;
        }
    determineWhiteSpace();
  }

  private void determineWhiteSpace()
  {
    // Determine if there's whitespace in the thing.
    // Ignore trailing chars.
    int i = offset + length - 1;
    hasWhitespace = false;
    while( i >= offset && Character.isWhitespace( string[i] ) )
      i--;
    // Check the remaining chars
    while( i >= offset )
      if( Character.isWhitespace( string[i--] ) )
        hasWhitespace = true;
  }

  protected Object clone ()
  {
    return new TextLayout( this, 0, length);
  }

  public void draw (Graphics2D g2, float x, float y)
  {
    for(int i = 0; i < runs.length; i++)
      {
        Run run = runs[i];
        GlyphVector gv = run.glyphVector;
        g2.drawGlyphVector(gv, x, y);
        Rectangle2D r = gv.getLogicalBounds();
        x += r.getWidth();
      }
  }

  public boolean equals (Object obj)
  {
    if( !( obj instanceof TextLayout) )
      return false;

    return equals( (TextLayout) obj );
  }

  public boolean equals (TextLayout tl)
  {
    if( runs.length != tl.runs.length )
      return false;
    // Compare all glyph vectors.
    for( int i = 0; i < runs.length; i++ )
      if( !runs[i].equals( tl.runs[i] ) )
        return false;
    return true;
  }

  public float getAdvance ()
  {
    if (totalAdvance == -1F)
      {
        totalAdvance = 0f;
        for(int i = 0; i < runs.length; i++)
          {
            Run run = runs[i];
            GlyphVector gv = run.glyphVector;
            totalAdvance += gv.getLogicalBounds().getWidth();
          }
      }
    return totalAdvance;
  }

  public float getAscent ()
  {
    return lm.getAscent();
  }

  public byte getBaseline ()
  {
    return (byte)lm.getBaselineIndex();
  }

  public float[] getBaselineOffsets ()
  {
    return lm.getBaselineOffsets();
  }

  public Shape getBlackBoxBounds (int firstEndpoint, int secondEndpoint)
  {
    if( secondEndpoint - firstEndpoint <= 0 )
      return new Rectangle2D.Float(); // Hmm?

    if( firstEndpoint < 0 || secondEndpoint > getCharacterCount())
      return new Rectangle2D.Float();

    GeneralPath gp = new GeneralPath();

    int ri = charIndices[ firstEndpoint ][0];
    int gi = charIndices[ firstEndpoint ][1];

    double advance = 0;

    for( int i = 0; i < ri; i++ )
      {
        Run run = runs[i];
        GlyphVector gv = run.glyphVector;
        advance += gv.getLogicalBounds().getWidth();
      }

    for( int i = ri; i <= charIndices[ secondEndpoint - 1 ][0]; i++ )
      {
        Run run = runs[i];
        GlyphVector gv = run.glyphVector;
        int dg;
        if( i == charIndices[ secondEndpoint - 1 ][0] )
          dg = charIndices[ secondEndpoint - 1][1];
        else
          dg = gv.getNumGlyphs() - 1;

        for( int j = 0; j <= dg; j++ )
          {
            Rectangle2D r2 = (gv.getGlyphVisualBounds( j )).
              getBounds2D();
            Point2D p = gv.getGlyphPosition( j );
            r2.setRect( advance + r2.getX(), r2.getY(),
                        r2.getWidth(), r2.getHeight() );
            gp.append(r2, false);
          }

        advance += gv.getLogicalBounds().getWidth();
      }
    return gp;
  }

  public Rectangle2D getBounds()
  {
    if( boundsCache == null )
      boundsCache = getOutline(new AffineTransform()).getBounds();
    return boundsCache;
  }

  public float[] getCaretInfo (TextHitInfo hit)
  {
    return getCaretInfo(hit, getNaturalBounds());
  }

  public float[] getCaretInfo (TextHitInfo hit, Rectangle2D bounds)
  {
    float[] info = new float[2];
    int index = hit.getCharIndex();
    boolean leading = hit.isLeadingEdge();
    // For the boundary cases we return the boundary runs.
    Run run;

    if (index >= length)
      {
        info[0] = getAdvance();
        info[1] = 0;
      }
    else
      {
        if (index < 0)
          {
            run = runs[0];
            index = 0;
            leading = true;
          }
        else
          run = findRunAtIndex(index);

        int glyphIndex = index - run.runStart;
        Shape glyphBounds = run.glyphVector.getGlyphLogicalBounds(glyphIndex);
        Rectangle2D glyphRect = glyphBounds.getBounds2D();
        if (isVertical())
          {
            if (leading)
              info[0] = (float) glyphRect.getMinY();
            else
              info[0] = (float) glyphRect.getMaxY();
          }
        else
          {
            if (leading)
              info[0] = (float) glyphRect.getMinX();
            else
              info[0] = (float) glyphRect.getMaxX();
          }
        info[0] += run.location;
        info[1] = run.font.getItalicAngle();
      }
    return info;
  }

  public Shape getCaretShape(TextHitInfo hit)
  {
    return getCaretShape(hit, getBounds());
  }

  public Shape getCaretShape(TextHitInfo hit, Rectangle2D bounds)
  {
    // TODO: Handle vertical shapes somehow.
    float[] info = getCaretInfo(hit);
    float x1 = info[0];
    float y1 = (float) bounds.getMinY();
    float x2 = info[0];
    float y2 = (float) bounds.getMaxY();
    if (info[1] != 0)
      {
        // Shift x1 and x2 according to the slope.
        x1 -= y1 * info[1];
        x2 -= y2 * info[1];
      }
    GeneralPath path = new GeneralPath(GeneralPath.WIND_EVEN_ODD, 2);
    path.moveTo(x1, y1);
    path.lineTo(x2, y2);
    return path;
  }

  public Shape[] getCaretShapes(int offset)
  {
    return getCaretShapes(offset, getNaturalBounds());
  }

  public Shape[] getCaretShapes(int offset, Rectangle2D bounds)
  {
    return getCaretShapes(offset, bounds, DEFAULT_CARET_POLICY);
  }

  public Shape[] getCaretShapes(int offset, Rectangle2D bounds,
                                CaretPolicy policy)
  {
    // The RI returns a 2-size array even when there's only one
    // shape in it.
    Shape[] carets = new Shape[2];
    TextHitInfo hit1 = TextHitInfo.afterOffset(offset);
    int caretHit1 = hitToCaret(hit1);
    TextHitInfo hit2 = hit1.getOtherHit();
    int caretHit2 = hitToCaret(hit2);
    if (caretHit1 == caretHit2)
      {
        carets[0] = getCaretShape(hit1);
        carets[1] = null; // The RI returns null in this seldom case.
      }
    else
      {
        Shape caret1 = getCaretShape(hit1);
        Shape caret2 = getCaretShape(hit2);
        TextHitInfo strong = policy.getStrongCaret(hit1, hit2, this);
        if (strong == hit1)
          {
            carets[0] = caret1;
            carets[1] = caret2;
          }
        else
          {
            carets[0] = caret2;
            carets[1] = caret1;
          }
      }
    return carets;
  }

  public int getCharacterCount ()
  {
    return length;
  }

  public byte getCharacterLevel (int index)
  {
    byte level;
    if( bidi == null )
      level = 0;
    else
      level = (byte) bidi.getLevelAt(index);
    return level;
  }

  public float getDescent ()
  {
    return lm.getDescent();
  }

  public TextLayout getJustifiedLayout (float justificationWidth)
  {
    TextLayout newLayout = (TextLayout)clone();

    if( hasWhitespace )
      newLayout.handleJustify( justificationWidth );

    return newLayout;
  }

  public float getLeading ()
  {
    return lm.getLeading();
  }

  public Shape getLogicalHighlightShape (int firstEndpoint, int secondEndpoint)
  {
    return getLogicalHighlightShape( firstEndpoint, secondEndpoint,
                                     getBounds() );
  }

  public Shape getLogicalHighlightShape (int firstEndpoint, int secondEndpoint,
                                         Rectangle2D bounds)
  {
    if( secondEndpoint - firstEndpoint <= 0 )
      return new Rectangle2D.Float(); // Hmm?

    if( firstEndpoint < 0 || secondEndpoint > getCharacterCount())
      return new Rectangle2D.Float();

    Rectangle2D r = null;
    int ri = charIndices[ firstEndpoint ][0];
    int gi = charIndices[ firstEndpoint ][1];

    double advance = 0;

    for( int i = 0; i < ri; i++ )
      advance += runs[i].glyphVector.getLogicalBounds().getWidth();

    for( int i = ri; i <= charIndices[ secondEndpoint - 1 ][0]; i++ )
      {
        Run run = runs[i];
        GlyphVector gv = run.glyphVector;
        int dg; // last index in this run to use.
        if( i == charIndices[ secondEndpoint - 1 ][0] )
          dg = charIndices[ secondEndpoint - 1][1];
        else
          dg = gv.getNumGlyphs() - 1;

        for(; gi <= dg; gi++ )
          {
            Rectangle2D r2 = (gv.getGlyphLogicalBounds( gi )).
              getBounds2D();
            if( r == null )
              r = r2;
            else
              r = r.createUnion(r2);
          }
        gi = 0; // reset glyph index into run for next run.

        advance += gv.getLogicalBounds().getWidth();
      }

    return r;
  }

  public int[] getLogicalRangesForVisualSelection (TextHitInfo firstEndpoint,
                                                   TextHitInfo secondEndpoint)
  {
    // Check parameters.
    checkHitInfo(firstEndpoint);
    checkHitInfo(secondEndpoint);

    // Convert to visual and order correctly.
    int start = hitToCaret(firstEndpoint);
    int end = hitToCaret(secondEndpoint);
    if (start > end)
      {
        // Swap start and end so that end >= start.
        int temp = start;
        start = end;
        end = temp;
      }

    // Now walk through the visual indices and mark the included pieces.
    boolean[] include = new boolean[length];
    for (int i = start; i < end; i++)
      {
        include[visualToLogical[i]] = true;
      }

    // Count included runs.
    int numRuns = 0;
    boolean in = false;
    for (int i = 0; i < length; i++)
      {
        if (include[i] != in) // At each run in/out point we toggle the in var.
          {
            in = ! in;
            if (in) // At each run start we count up.
              numRuns++;
          }
      }

    // Put together the ranges array.
    int[] ranges = new int[numRuns * 2];
    int index = 0;
    in = false;
    for (int i = 0; i < length; i++)
      {
        if (include[i] != in)
          {
            ranges[index] = i;
            index++;
            in = ! in;
          }
      }
    // If the last run ends at the very end, include that last bit too.
    if (in)
      ranges[index] = length;

    return ranges;
  }

  public TextHitInfo getNextLeftHit(int offset)
  {
    return getNextLeftHit(offset, DEFAULT_CARET_POLICY);
  }

  public TextHitInfo getNextLeftHit(int offset, CaretPolicy policy)
  {
    if (policy == null)
      throw new IllegalArgumentException("Null policy not allowed");
    if (offset < 0 || offset > length)
      throw new IllegalArgumentException("Offset out of bounds");

    TextHitInfo hit1 = TextHitInfo.afterOffset(offset);
    TextHitInfo hit2 = hit1.getOtherHit();

    TextHitInfo strong = policy.getStrongCaret(hit1, hit2, this);
    TextHitInfo next = getNextLeftHit(strong);
    TextHitInfo ret = null;
    if (next != null)
      {
        TextHitInfo next2 = getVisualOtherHit(next);
        ret = policy.getStrongCaret(next2, next, this);
      }
    return ret;
  }

  public TextHitInfo getNextLeftHit (TextHitInfo hit)
  {
    checkHitInfo(hit);
    int index = hitToCaret(hit);
    TextHitInfo next = null;
    if (index != 0)
      {
        index--;
        next = caretToHit(index);
      }
    return next;
  }

  public TextHitInfo getNextRightHit(int offset)
  {
    return getNextRightHit(offset, DEFAULT_CARET_POLICY);
  }

  public TextHitInfo getNextRightHit(int offset, CaretPolicy policy)
  {
    if (policy == null)
      throw new IllegalArgumentException("Null policy not allowed");
    if (offset < 0 || offset > length)
      throw new IllegalArgumentException("Offset out of bounds");

    TextHitInfo hit1 = TextHitInfo.afterOffset(offset);
    TextHitInfo hit2 = hit1.getOtherHit();

    TextHitInfo next = getNextRightHit(policy.getStrongCaret(hit1, hit2, this));
    TextHitInfo ret = null;
    if (next != null)
      {
        TextHitInfo next2 = getVisualOtherHit(next);
        ret = policy.getStrongCaret(next2, next, this);
      }
    return ret;
  }

  public TextHitInfo getNextRightHit(TextHitInfo hit)
  {
    checkHitInfo(hit);
    int index = hitToCaret(hit);
    TextHitInfo next = null;
    if (index < length)
      {
        index++;
        next = caretToHit(index);
      }
    return next;
  }

  public Shape getOutline (AffineTransform tx)
  {
    float x = 0f;
    GeneralPath gp = new GeneralPath();
    for(int i = 0; i < runs.length; i++)
      {
        GlyphVector gv = runs[i].glyphVector;
        gp.append( gv.getOutline( x, 0f ), false );
        Rectangle2D r = gv.getLogicalBounds();
        x += r.getWidth();
      }
    if( tx != null )
      gp.transform( tx );
    return gp;
  }

  public float getVisibleAdvance ()
  {
    float totalAdvance = 0f;

    if( runs.length <= 0 )
      return 0f;

    // No trailing whitespace
    if( !Character.isWhitespace( string[offset + length - 1]) )
      return getAdvance();

    // Get length of all runs up to the last
    for(int i = 0; i < runs.length - 1; i++)
      totalAdvance += runs[i].glyphVector.getLogicalBounds().getWidth();

    int lastRun = runs[runs.length - 1].runStart;
    int j = length - 1;
    while( j >= lastRun && Character.isWhitespace( string[j] ) ) j--;

    if( j < lastRun )
      return totalAdvance; // entire last run is whitespace

    int lastNonWSChar = j - lastRun;
    j = 0;
    while( runs[ runs.length - 1 ].glyphVector.getGlyphCharIndex( j )
           <= lastNonWSChar )
      {
        totalAdvance += runs[ runs.length - 1 ].glyphVector
                                               .getGlyphLogicalBounds( j )
                                               .getBounds2D().getWidth();
        j ++;
      }

    return totalAdvance;
  }

  public Shape getVisualHighlightShape (TextHitInfo firstEndpoint,
                                        TextHitInfo secondEndpoint)
  {
    return getVisualHighlightShape( firstEndpoint, secondEndpoint,
                                    getBounds() );
  }

  public Shape getVisualHighlightShape (TextHitInfo firstEndpoint,
                                        TextHitInfo secondEndpoint,
                                        Rectangle2D bounds)
  {
    GeneralPath path = new GeneralPath(GeneralPath.WIND_EVEN_ODD);
    Shape caret1 = getCaretShape(firstEndpoint, bounds);
    path.append(caret1, false);
    Shape caret2 = getCaretShape(secondEndpoint, bounds);
    path.append(caret2, false);
    // Append left (top) bounds to selection if necessary.
    int c1 = hitToCaret(firstEndpoint);
    int c2 = hitToCaret(secondEndpoint);
    if (c1 == 0 || c2 == 0)
      {
        path.append(left(bounds), false);
      }
    // Append right (bottom) bounds if necessary.
    if (c1 == length || c2 == length)
      {
        path.append(right(bounds), false);
      }
    return path.getBounds2D();
  }

  /**
   * Returns the shape that makes up the left (top) edge of this text layout.
   *
   * @param b the bounds
   *
   * @return the shape that makes up the left (top) edge of this text layout
   */
  private Shape left(Rectangle2D b)
  {
    GeneralPath left = new GeneralPath(GeneralPath.WIND_EVEN_ODD);
    left.append(getCaretShape(TextHitInfo.beforeOffset(0)), false);
    if (isVertical())
      {
        float y = (float) b.getMinY();
        left.append(new Line2D.Float((float) b.getMinX(), y,
                                     (float) b.getMaxX(), y), false);
      }
    else
      {
        float x = (float) b.getMinX();
        left.append(new Line2D.Float(x, (float) b.getMinY(),
                                     x, (float) b.getMaxY()), false);
      }
    return left.getBounds2D();
  }

  /**
   * Returns the shape that makes up the right (bottom) edge of this text
   * layout.
   *
   * @param b the bounds
   *
   * @return the shape that makes up the right (bottom) edge of this text
   *         layout
   */
  private Shape right(Rectangle2D b)
  {
    GeneralPath right = new GeneralPath(GeneralPath.WIND_EVEN_ODD);
    right.append(getCaretShape(TextHitInfo.afterOffset(length)), false);
    if (isVertical())
      {
        float y = (float) b.getMaxY();
        right.append(new Line2D.Float((float) b.getMinX(), y,
                                      (float) b.getMaxX(), y), false);
      }
    else
      {
        float x = (float) b.getMaxX();
        right.append(new Line2D.Float(x, (float) b.getMinY(),
                                      x, (float) b.getMaxY()), false);
      }
    return right.getBounds2D();
  }

  public TextHitInfo getVisualOtherHit (TextHitInfo hit)
  {
    checkHitInfo(hit);
    int hitIndex = hit.getCharIndex();

    int index;
    boolean leading;
    if (hitIndex == -1 || hitIndex == length)
      {
        // Boundary case.
        int visual;
        if (isLeftToRight() == (hitIndex == -1))
          visual = 0;
        else
          visual = length - 1;
        index = visualToLogical[visual];
        if (isLeftToRight() == (hitIndex == -1))
          leading = isCharacterLTR(index); // LTR.
        else
          leading = ! isCharacterLTR(index); // RTL.
      }
    else
      {
        // Normal case.
        int visual = logicalToVisual[hitIndex];
        boolean b;
        if (isCharacterLTR(hitIndex) == hit.isLeadingEdge())
          {
            visual--;
            b = false;
          }
        else
          {
            visual++;
            b = true;
          }
        if (visual >= 0 && visual < length)
          {
            index = visualToLogical[visual];
            leading = b == isLeftToRight();
          }
        else
          {
            index = b == isLeftToRight() ? length : -1;
            leading = index == length;
          }
      }
    return leading ? TextHitInfo.leading(index) : TextHitInfo.trailing(index);
  }

  /**
   * This is a protected method of a <code>final</code> class, meaning
   * it exists only to taunt you.
   */
  protected void handleJustify (float justificationWidth)
  {
    // We assume that the text has non-trailing whitespace.
    // First get the change in width to insert into the whitespaces.
    double deltaW = justificationWidth - getVisibleAdvance();
    int nglyphs = 0; // # of whitespace chars

    // determine last non-whitespace char.
    int lastNWS = offset + length - 1;
    while( Character.isWhitespace( string[lastNWS] ) ) lastNWS--;

    // locations of the glyphs.
    int[] wsglyphs = new int[length * 10];
    for(int run = 0; run < runs.length; run++ )
      {
      Run current = runs[run];
      for(int i = 0; i < current.glyphVector.getNumGlyphs(); i++ )
        {
          int cindex = current.runStart
                       + current.glyphVector.getGlyphCharIndex( i );
          if( Character.isWhitespace( string[cindex] ) )
            //        && cindex < lastNWS )
            {
              wsglyphs[ nglyphs * 2 ] = run;
              wsglyphs[ nglyphs * 2 + 1] = i;
              nglyphs++;
            }
        }
      }
    deltaW = deltaW / nglyphs; // Change in width per whitespace glyph
    double w = 0;
    int cws = 0;
    // Shift all characters
    for(int run = 0; run < runs.length; run++ )
      {
        Run current = runs[run];
        for(int i = 0; i < current.glyphVector.getNumGlyphs(); i++ )
          {
            if( wsglyphs[ cws * 2 ] == run && wsglyphs[ cws * 2 + 1 ] == i )
              {
                cws++; // update 'current whitespace'
                w += deltaW; // increment the shift
              }
            Point2D p = current.glyphVector.getGlyphPosition( i );
            p.setLocation( p.getX() + w, p.getY() );
            current.glyphVector.setGlyphPosition( i, p );
          }
      }
  }

  public TextHitInfo hitTestChar (float x, float y)
  {
    return hitTestChar(x, y, getNaturalBounds());
  }

  /**
   * Finds the character hit at the specified point. This 'clips' this
   * text layout against the specified <code>bounds</code> rectangle. That
   * means that in the case where a point is outside these bounds, this method
   * returns the leading edge of the first character or the trailing edge of
   * the last character.
   *
   * @param x the X location to test
   * @param y the Y location to test
   * @param bounds the bounds to test against
   *
   * @return the character hit at the specified point
   */
  public TextHitInfo hitTestChar (float x, float y, Rectangle2D bounds)
  {
    // Check bounds.
    if (isVertical())
      {
        if (y < bounds.getMinY())
          return TextHitInfo.leading(0);
        else if (y > bounds.getMaxY())
          return TextHitInfo.trailing(getCharacterCount() - 1);
      }
    else
      {
        if (x < bounds.getMinX())
          return TextHitInfo.leading(0);
        else if (x > bounds.getMaxX())
          return TextHitInfo.trailing(getCharacterCount() - 1);
      }

    TextHitInfo hitInfo = null;
    if (isVertical())
      {
        // Search for the run at the location.
        // TODO: Perform binary search for maximum efficiency. However, we
        // need the run location laid out statically to do that.
        int numRuns = runs.length;
        Run hitRun = null;
        for (int i = 0; i < numRuns && hitRun == null; i++)
          {
            Run run = runs[i];
            Rectangle2D lBounds = run.glyphVector.getLogicalBounds();
            if (lBounds.getMinY() + run.location <= y
                && lBounds.getMaxY() + run.location >= y)
              hitRun = run;
          }
        // Now we have (hopefully) found a run that hits. Now find the
        // right character.
        if (hitRun != null)
          {
            GlyphVector gv = hitRun.glyphVector;
            for (int i = hitRun.runStart;
                 i < hitRun.runEnd && hitInfo == null; i++)
              {
                int gi = i - hitRun.runStart;
                Rectangle2D lBounds = gv.getGlyphLogicalBounds(gi)
                                      .getBounds2D();
                if (lBounds.getMinY() + hitRun.location <= y
                    && lBounds.getMaxY() + hitRun.location >= y)
                  {
                    // Found hit. Now check if we are leading or trailing.
                    boolean leading = true;
                    if (lBounds.getCenterY() + hitRun.location <= y)
                      leading = false;
                    hitInfo = leading ? TextHitInfo.leading(i)
                                      : TextHitInfo.trailing(i);
                  }
              }
          }
      }
    else
      {
        // Search for the run at the location.
        // TODO: Perform binary search for maximum efficiency. However, we
        // need the run location laid out statically to do that.
        int numRuns = runs.length;
        Run hitRun = null;
        for (int i = 0; i < numRuns && hitRun == null; i++)
          {
            Run run = runs[i];
            Rectangle2D lBounds = run.glyphVector.getLogicalBounds();
            if (lBounds.getMinX() + run.location <= x
                && lBounds.getMaxX() + run.location >= x)
              hitRun = run;
          }
        // Now we have (hopefully) found a run that hits. Now find the
        // right character.
        if (hitRun != null)
          {
            GlyphVector gv = hitRun.glyphVector;
            for (int i = hitRun.runStart;
                 i < hitRun.runEnd && hitInfo == null; i++)
              {
                int gi = i - hitRun.runStart;
                Rectangle2D lBounds = gv.getGlyphLogicalBounds(gi)
                                      .getBounds2D();
                if (lBounds.getMinX() + hitRun.location <= x
                    && lBounds.getMaxX() + hitRun.location >= x)
                  {
                    // Found hit. Now check if we are leading or trailing.
                    boolean leading = true;
                    if (lBounds.getCenterX() + hitRun.location <= x)
                      leading = false;
                    hitInfo = leading ? TextHitInfo.leading(i)
                                      : TextHitInfo.trailing(i);
                  }
              }
          }
      }
    return hitInfo;
  }

  public boolean isLeftToRight ()
  {
    return leftToRight;
  }

  public boolean isVertical ()
  {
    return false; // FIXME: How do you create a vertical layout?
  }

  public int hashCode ()
  {
    // This is implemented in sync to equals().
    if (hash == 0 && runs.length > 0)
      {
        hash = runs.length;
        for (int i = 0; i < runs.length; i++)
          hash ^= runs[i].glyphVector.hashCode();
      }
    return hash;
  }

  public String toString ()
  {
    return "TextLayout [string:"+ new String(string, offset, length)
    +" Rendercontext:"+
      frc+"]";
  }

  /**
   * Returns the natural bounds of that text layout. This is made up
   * of the ascent plus descent and the text advance.
   *
   * @return the natural bounds of that text layout
   */
  private Rectangle2D getNaturalBounds()
  {
    if (naturalBounds == null)
      naturalBounds = new Rectangle2D.Float(0.0F, -getAscent(), getAdvance(),
                                            getAscent() + getDescent());
    return naturalBounds;
  }

  private void checkHitInfo(TextHitInfo hit)
  {
    if (hit == null)
      throw new IllegalArgumentException("Null hit info not allowed");
    int index = hit.getInsertionIndex();
    if (index < 0 || index > length)
      throw new IllegalArgumentException("Hit index out of range");
  }

  private int hitToCaret(TextHitInfo hit)
  {
    int index = hit.getCharIndex();
    int ret;
    if (index < 0)
      ret = isLeftToRight() ? 0 : length;
    else if (index >= length)
      ret = isLeftToRight() ? length : 0;
    else
      {
        ret = logicalToVisual[index];
        if (hit.isLeadingEdge() != isCharacterLTR(index))
          ret++;
      }
    return ret;
  }

  private TextHitInfo caretToHit(int index)
  {
    TextHitInfo hit;
    if (index == 0 || index == length)
      {
        if ((index == length) == isLeftToRight())
          hit = TextHitInfo.leading(length);
        else
          hit = TextHitInfo.trailing(-1);
      }
    else
      {
        int logical = visualToLogical[index];
        boolean leading = isCharacterLTR(logical); // LTR.
        hit = leading ? TextHitInfo.leading(logical)
                      : TextHitInfo.trailing(logical);
      }
    return hit;
  }

  private boolean isCharacterLTR(int index)
  {
    byte level = getCharacterLevel(index);
    return (level & 1) == 0;
  }

  /**
   * Finds the run that holds the specified (logical) character index. This
   * returns <code>null</code> when the index is not inside the range.
   *
   * @param index the index of the character to find
   *
   * @return the run that holds the specified character
   */
  private Run findRunAtIndex(int index)
  {
    Run found = null;
    // TODO: Can we do better than linear searching here?
    for (int i = 0; i < runs.length && found == null; i++)
      {
        Run run = runs[i];
        if (run.runStart <= index && run.runEnd > index)
          found = run;
      }
    return found;
  }

  /**
   * Computes the layout locations for each run.
   */
  private void layoutRuns()
  {
    float loc = 0.0F;
    float lastWidth = 0.0F;
    for (int i = 0; i < runs.length; i++)
      {
        runs[i].location = loc;
        Rectangle2D bounds = runs[i].glyphVector.getLogicalBounds();
        loc += isVertical() ? bounds.getHeight() : bounds.getWidth();
      }
  }

  /**
   * Inner class describing a caret policy
   */
  public static class CaretPolicy
  {
    public CaretPolicy()
    {
    }

    public TextHitInfo getStrongCaret(TextHitInfo hit1,
                                      TextHitInfo hit2,
                                      TextLayout layout)
    {
      byte l1 = layout.getCharacterLevel(hit1.getCharIndex());
      byte l2 = layout.getCharacterLevel(hit2.getCharIndex());
      TextHitInfo strong;
      if (l1 == l2)
        {
          if (hit2.isLeadingEdge() && ! hit1.isLeadingEdge())
            strong = hit2;
          else
            strong = hit1;
        }
      else
        {
          if (l1 < l2)
            strong = hit1;
          else
            strong = hit2;
        }
      return strong;
    }
  }
}
