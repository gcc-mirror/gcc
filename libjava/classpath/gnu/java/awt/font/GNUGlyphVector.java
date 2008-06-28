/* GNUGlyphVector.java -- The GNU implementation of GlyphVector.
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

package gnu.java.awt.font;

import gnu.java.awt.java2d.ShapeWrapper;

import java.awt.Font;
import java.awt.font.FontRenderContext;
import java.awt.font.GlyphMetrics;
import java.awt.font.GlyphJustificationInfo;
import java.awt.font.GlyphVector;

import java.awt.Shape;
import java.awt.geom.AffineTransform;
import java.awt.geom.GeneralPath;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;


/**
 * The GNU implementation of the abstract GlyphVector class, which
 * uses the services provided by a FontDelegate for its functionality.
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class GNUGlyphVector
  extends GlyphVector
{
  private FontDelegate fontDelegate;
  private Font font;
  private FontRenderContext renderContext;
  private int[] glyphs;
  private float fontSize;
  private AffineTransform transform;
  private boolean valid;


  /**
   * The position of each glyph. The horizontal position of the
   * <code>i</code>-th glyph is at <code>pos[i * 2]</code>, its
   * vertical position at <code>pos[i * 2 + 1]</code>. The total
   * advance width of the entire vector is stored at
   * <code>pos[numGlyphs]</code>, the total advance height at
   * <code>pos[numGlyphs + 1]</code>.
   */
  private float[] pos;


  private AffineTransform[] transforms;
  private int layoutFlags;

  /**
   * The cached non-transformed outline of this glyph vector.
   */
  private Shape cleanOutline;

  /**
   * Constructs a new GNUGlyphVector.
   *
   * @param fontDelegate the FontDelegate that creates this vector.
   *
   * @param font the Font that this GlyphVector will return for {@link
   * #getFont()}. That object is also used to determine the point
   * size, which affects the affine transformation used by the font
   * scaler.
   *
   * @param renderContext an object with parameters for font
   * rendering, such as whether anti-aliasing is enabled.
   *
   * @param glyphs the glyphs in this vector.
   */
  public GNUGlyphVector(FontDelegate fontDelegate,
                        Font font,
                        FontRenderContext renderContext,
                        int[] glyphs)
  {
    this.fontDelegate = fontDelegate;
    this.font = font;
    this.renderContext = renderContext;
    this.glyphs = glyphs;
    
    fontSize = font.getSize2D();
    transform = font.getTransform(); // returns a modifiable copy
    //transform.concatenate(renderContext.getTransform());
  }



  /**
   * Returns the font of the glyphs in this GlyphVector.
   */
  public Font getFont()
  {
    return font;
  }


  /**
   * Returns the FontRenderContext that is used to calculate the
   * extent and position of the glyphs.
   */
  public FontRenderContext getFontRenderContext()
  {
    return renderContext;
  }


  /**
   * Moves each glyph in the vector to its default position.
   */
  public void performDefaultLayout()
  {
    float x, y, advanceWidth, advanceHeight;
    int i, p;
    AffineTransform tx;
    Point2D.Float advance = new Point2D.Float();

    pos = new float[(glyphs.length + 1) * 2];
    x = y = 0.0f;
    p = 0;
    for (i = p = 0; i < glyphs.length; i++)
    {
      p += 2;
      
      if ((transforms == null) || (tx = transforms[i]) == null)
        tx = this.transform;
      else
      {
        tx = new AffineTransform(tx);
        tx.concatenate(this.transform);
      }
      
      fontDelegate.getAdvance(glyphs[i], fontSize, tx,
                              renderContext.isAntiAliased(),
                              renderContext.usesFractionalMetrics(),
                              /* horizontal */ true,
                              advance);
      // FIXME: We shouldn't round here, but instead hint the metrics
      // correctly.
      pos[p] = x += Math.round(advance.x);
      pos[p + 1] = y += advance.y;
    }
    valid = true;
  }


  /**
   * Determines the number of glyphs in this GlyphVector.
   */
  public int getNumGlyphs()
  {
    return glyphs.length;
  }


  /**
   * Determines the glyph number by index in this vector.
   * Glyph numbers are specific to each font, so two fonts
   * will likely assign different numbers to the same glyph.
   *
   * @param glyphIndex the index of the glyph whose glyph number is to
   * be retrieved.
   *
   * @throws IndexOutOfBoundsException if <code>glyphIndex</code>
   * is not in the range <code[0 .. getNumGlyphs() - 1]</code>.
   */
  public int getGlyphCode(int glyphIndex)
  {
    /* The exception is thrown automatically if the index is out
     * of the valid bounds.
     */
    return glyphs[glyphIndex];
  }


  /**
   * Returns a slice of this GlyphVector.
   *
   * @param firstGlyphIndex the index of the first glyph in the
   * returned slice.
   *
   * @param numEntries the size of the returned slice.
   *
   * @param outCodes a pre-allocated array for storing the slice,
   * or <code>null</code> to cause allocation of a new array.
   *
   * @return a slice of this GlyphVector. If <code>outCodes</code>
   * is <code>null</code>, the slice will be stored into a freshly
   * allocated array; otherwise, the result will be stored into
   * <code>outCodes</code>.
   */
  public int[] getGlyphCodes(int firstGlyphIndex,
                             int numEntries,
                             int[] outCodes)
  {
    if (numEntries < 0)
      throw new IllegalArgumentException();
    if (outCodes == null)
      outCodes = new int[numEntries];
    System.arraycopy(glyphs, firstGlyphIndex, outCodes, 0, numEntries);
    return outCodes;
  }


  public Rectangle2D getLogicalBounds()
  {
    float ascent, descent;

    validate();

    return new Rectangle2D.Float(0, 0,
                                 pos[pos.length - 2],
                                 getAscent() - getDescent());
  }


  public Rectangle2D getVisualBounds()
  {
    validate();

    // FIXME: Not yet implemented.
    return getLogicalBounds();
  }


  /**
   * Returns the shape of this GlyphVector.
   */
  public Shape getOutline()
  {
    return getOutline(0.0f, 0.0f);
  }


  /**
   * Returns the shape of this GlyphVector, translated to the
   * specified position.
   *
   * @param x the horizontal position for rendering this vector.
   * @param y the vertical position for rendering this vector.
   */
  public Shape getOutline(float x, float y)
  {
    validate();

    Shape outline;
    if (cleanOutline == null)
      {
        GeneralPath path = new GeneralPath();
        int len = glyphs.length;
        for (int i = 0; i < len; i++)
          {
            GeneralPath p = new GeneralPath(getGlyphOutline(i));
            path.append(p, false);
          }
        // Protect the cached instance from beeing modified by application
        // code.
        cleanOutline = new ShapeWrapper(path);
        outline = cleanOutline;
      }
    else
      {
        outline = cleanOutline;
      }
    if (x != 0 || y != 0)
      {
        GeneralPath path = new GeneralPath(outline);
        AffineTransform t = new AffineTransform();
        t.translate(x, y);
        path.transform(t);
        outline = path;
      }
    return outline;
  }

  public Shape getOutline(float x, float y, int type)
  {
    validate();

    GeneralPath outline = new GeneralPath();
    int len = glyphs.length;
    for (int i = 0; i < len; i++)
      {
        GeneralPath p = new GeneralPath(getGlyphOutline(i, type));
        outline.append(p, false);
      }
    AffineTransform t = new AffineTransform();
    t.translate(x, y);
    outline.transform(t);
    return outline;
  }

  /**
   * Determines the shape of the specified glyph.
   *
   * @throws IndexOutOfBoundsException if <code>glyphIndex</code> is
   * not in the range <code[0 .. getNumGlyphs()]</code>.
   */
  public Shape getGlyphOutline(int glyphIndex)
  {
    AffineTransform tx, glyphTx;
    GeneralPath path;

    validate();

    if ((transforms != null)
        && ((glyphTx = transforms[glyphIndex]) != null))
    {
      tx =  new AffineTransform(transform);
      tx.concatenate(glyphTx);
    }
    else
      tx = transform;

    path = fontDelegate.getGlyphOutline(glyphs[glyphIndex], fontSize, tx,
                                        renderContext.isAntiAliased(),
                                        renderContext.usesFractionalMetrics(),
                                        FontDelegate.FLAG_FITTED);

    tx = new AffineTransform();
    tx.translate(pos[glyphIndex * 2], pos[glyphIndex * 2 + 1]);
    path.transform(tx);
    return path;
  }

  public Shape getGlyphOutline(int glyphIndex, int type)
  {
    AffineTransform tx, glyphTx;
    GeneralPath path;

    validate();

    if ((transforms != null)
        && ((glyphTx = transforms[glyphIndex]) != null))
    {
      tx =  new AffineTransform(transform);
      tx.concatenate(glyphTx);
    }
    else
      tx = transform;

    path = fontDelegate.getGlyphOutline(glyphs[glyphIndex], fontSize, tx,
                                        renderContext.isAntiAliased(),
                                        renderContext.usesFractionalMetrics(),
                                        type);

    tx = new AffineTransform();
    tx.translate(pos[glyphIndex * 2], pos[glyphIndex * 2 + 1]);
    path.transform(tx);
    return path;
  }

  /**
   * Determines the position of the specified glyph, or the
   * total advance width and height of the vector.
   *
   * @param glyphIndex the index of the glyph in question.
   * If this value equals <code>getNumGlyphs()</code>, the
   * position <i>after</i> the last glyph will be returned,
   * which is the total advance width and height of the vector.
   *
   * @throws IndexOutOfBoundsException if <code>glyphIndex</code> is
   * not in the range <code[0 .. getNumGlyphs()]</code>.
   */
  public Point2D getGlyphPosition(int glyphIndex)
  {
    validate();
    return new Point2D.Float(pos[glyphIndex * 2],
                             pos[glyphIndex * 2 + 1]);
  }


  /**
   * Moves the specified glyph to a new position, or changes the
   * advance width and height of the entire glyph vector.
   *
   * <p>Note that the position of an individual glyph may also
   * affected by its affine transformation.
   *
   * @param glyphIndex the index of the moved glyph. If
   * <code>glyphIndex</code> equals the total number of glyphs in this
   * vector, the advance width and height of the vector is changed.
   *
   * @param position the new position of the glyph.
   *
   * @throws IndexOutOfBoundsException if <code>glyphIndex</code> is
   * not in the range <code[0 .. getNumGlyphs()]</code>.
   */
  public void setGlyphPosition(int glyphIndex, Point2D position)
  {
    validate();
    pos[glyphIndex * 2] = (float) position.getX();
    pos[glyphIndex * 2 + 1] = (float) position.getY();
  }


  /**
   * Returns the affine transformation that is applied to the
   * glyph at the specified index.
   *
   * @param glyphIndex the index of the glyph whose transformation
   * is to be retrieved.
   *
   * @return an affine transformation, or <code>null</code>
   * for the identity transformation.
   *
   * @throws IndexOutOfBoundsException if <code>glyphIndex</code> is
   * not in the range <code[0 .. getNumGlyphs() - 1]</code>.
   */
  public AffineTransform getGlyphTransform(int glyphIndex)
  {
    if (transforms == null)
      return null;
    else
      return transforms[glyphIndex];
  }


  /**
   * Applies an affine transformation to the glyph at the specified
   * index.
   *
   * @param glyphIndex the index of the glyph to which the
   * transformation is applied.
   *
   * @param transform the affine transformation for the glyph, or
   * <code>null</code> for an identity transformation.
   */
  public void setGlyphTransform(int glyphIndex,
                                AffineTransform transform)
  {
    if (transforms == null)
      transforms = new AffineTransform[glyphs.length];
    transforms[glyphIndex] = transform;

    /* If the GlyphVector has only a transform for a single glyph, and
     * the caller clears its transform, the FLAG_HAS_TRANSFORMS bit
     * should be cleared in layoutFlags.  However, this would require
     * that we keep track of the number of transformed glyphs, or that
     * we count them when a transform is cleared. This would
     * complicate the code quite a bit. Note that the only drawback of
     * wrongly setting FLAG_HAS_TRANSFORMS is that a slower code path
     * might be taken for rendering the vector. Right now, we never
     * really look at the flag, so it does not make any difference.
     */
    if (transform != null)
      layoutFlags |= FLAG_HAS_TRANSFORMS;
    valid = false;
  }


  /**
   * Returns flags that can be used for optimizing the rendering
   * of this GlyphVector.
   *
   * @return a bit mask with the applicable flags set.
   *
   * @since 1.4
   *
   * @see GlyphVector#FLAG_HAS_POSITION_ADJUSTMENTS
   * @see GlyphVector#FLAG_HAS_TRANSFORMS
   * @see GlyphVector#FLAG_RUN_RTL
   * @see GlyphVector#FLAG_COMPLEX_GLYPHS
   * @see GlyphVector#FLAG_MASK
   */
  public int getLayoutFlags()
  {
    return layoutFlags;
  }
  
  
  /**
   * Returns the positions of a range of glyphs in this vector.
   * 
   * @param firstGlyphIndex the index of the first glyph whose
   * position is retrieved.
   *
   * @param numGlyphs the number of glyphs whose positions
   * are retrieved.
   *
   * @param outPositions an array for storing the results
   * (the length must be at least twice <code>numGlyphs</code>),
   * or <code>null</code> for freshly allocating an array.
   *
   * @return an array with the glyph positions. The horizontal
   * position of the <code>i</code>-th glyph is at index <code>2 *
   * i</code>, the vertical position at index <code>2 * i + 1</code>.
   *
   * @throws IllegalArgumentException if <code>numGlyphs</code>
   * is less than zero.
   *
   * @throws IndexOutOfBoundsException if either
   * <code>firstGlyphIndex</code> or <code>(firstGlyphIndex +
   * numGlyphs)</code> is not in the range <code>[0 .. getNumGlyphs() -
   * 1]</code>.
   */
  public float[] getGlyphPositions(int firstGlyphIndex,
                                   int numGlyphs,
                                   float[] outPositions)
  {
    if (numGlyphs < 0)
      throw new IllegalArgumentException();

    validate();
    if (outPositions == null)
      outPositions = new float[numGlyphs * 2];

    System.arraycopy(/*src */ pos, /* srcStart */ firstGlyphIndex * 2,
                     /* dest */ outPositions, /* destStart */ 0,
                     /* length */ numGlyphs * 2);
    return outPositions;
  }

  
  private float getAscent()
  {
    return fontDelegate.getAscent(fontSize, transform,
                                  renderContext.isAntiAliased(),
                                  renderContext.usesFractionalMetrics(),
                                  /* horizontal */ true);
  }


  private float getDescent()
  {
    return fontDelegate.getDescent(fontSize, transform,
                                   renderContext.isAntiAliased(),
                                   renderContext.usesFractionalMetrics(),
                                   /* horizontal */ true);    
  }


  public Shape getGlyphLogicalBounds(int glyphIndex)
  {
    float x, y, ascent;

    validate();
    ascent = getAscent();
    x = pos[glyphIndex * 2];
    y = pos[glyphIndex * 2 + 1];

    return new Rectangle2D.Float(x, y - ascent,
                                 pos[(glyphIndex + 1) * 2] - x,
                                 ascent - getDescent());
  }


  public Shape getGlyphVisualBounds(int glyphIndex)
  {
    return getGlyphOutline(glyphIndex).getBounds2D();
  }


  /**
   * Determines the metrics of the glyph at the specified index.
   *
   * @param glyphIndex the index of the glyph whose metrics is to be
   * retrieved.
   *
   * @throws IndexOutOfBoundsException if <code>glyphIndex</code> is
   * not in the range <code[0 .. getNumGlyphs() - 1]</code>.
   */
  public GlyphMetrics getGlyphMetrics(int glyphIndex)
  {
    // FIXME: Not yet implemented.
    throw new UnsupportedOperationException();
  }


  /**
   * Determines the justification information for the glyph at the
   * specified index.
   *
   * @param glyphIndex the index of the glyph whose justification
   * information is to be retrieved.
   *
   * @throws IndexOutOfBoundsException if <code>glyphIndex</code> is
   * not in the range <code[0 .. getNumGlyphs() - 1]</code>.
   */
  public GlyphJustificationInfo getGlyphJustificationInfo(int glyphIndex)
  {
    // FIXME: Not yet implemented.
    throw new UnsupportedOperationException();
  }


  /**
   * Determines whether another GlyphVector is for the same font and
   * rendering context, uses the same glyphs and positions them to the
   * same location.
   *
   * @param other the GlyphVector to compare with.
   *
   * @return <code>true</code> if the two vectors are equal,
   * <code>false</code> otherwise.
   */
  public boolean equals(GlyphVector other)
  {
    GNUGlyphVector o;
    if (!(other instanceof GNUGlyphVector))
      return false;

    o = (GNUGlyphVector) other;
    if ((this.font != o.font)
        || (this.fontDelegate != o.fontDelegate)
        || (this.renderContext != o.renderContext)
        || (this.glyphs.length != o.glyphs.length))
      return false;

    for (int i = 0; i < glyphs.length; i++)
      if (this.glyphs[i] != o.glyphs[i])
        return false;

    validate();
    o.validate();
    for (int i = 0; i < pos.length; i++)
      if (this.pos[i] != o.pos[i])
        return false;

    return true;
  }

  private void validate()
  {
    if (!valid)
      performDefaultLayout();
  }
}
