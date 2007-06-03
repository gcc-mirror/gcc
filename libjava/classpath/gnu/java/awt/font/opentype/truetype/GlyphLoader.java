/* GlyphLoader.java -- Helper for loading TrueType glyph outlines.
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


package gnu.java.awt.font.opentype.truetype;

import gnu.java.awt.font.opentype.Hinter;

import java.awt.geom.AffineTransform;
import java.nio.ByteBuffer;


/**
 * A class for loading scaled and hinted glyph outlines.
 *
 * <p><b>Lack of Thread Safety:</b> Glyph loaders are intentionally
 * <i>not</i> safe to access from multiple concurrent
 * threads. Synchronization needs to be performed externally. Usually,
 * the font has already obtained a lock before calling the scaler,
 * which in turn calls the GlyphLoader. It would thus be wasteful to
 * acquire additional locks for the GlyphLoader.
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
final class GlyphLoader
{
  /**
   * A helper object for locating glyph data. GlyphLocator is an
   * abstract superclass, and there is a concretization for each glyph
   * location table ('loca') format.
   */
  private final GlyphLocator glyphLocator;


  /**
   * A helper object for measuring the advance width and height of a
   * glyph.
   */
  private final GlyphMeasurer glyphMeasurer;
  
  
  /**
   * The virtual machine for executing TrueType bytecodes.
   */
  private final VirtualMachine vm;


  /**
   * The number of font units in one em. A typical value is 2048,
   * but this depends on the font.
   */
  private final int unitsPerEm;

  private final int[] contourEndPoints;
  private final byte[] pointFlags;


  /**
   * Constructs a GlyphLoader.
   */
  GlyphLoader(GlyphLocator glyphLocator, VirtualMachine vm,
              int unitsPerEm, int maxContours, int maxPoints,
              GlyphMeasurer glyphMeasurer)
  {
    this.glyphLocator = glyphLocator;
    this.glyphMeasurer = glyphMeasurer;
    this.unitsPerEm = unitsPerEm;
    this.vm = vm;

    contourEndPoints = new int[maxContours];
    pointFlags = new byte[maxPoints];
  }


  /**
   * @param glyphIndex the number of the glyph whose outlines are to be
   * retrieved.
   */
  public void loadGlyph(int glyphIndex,
                        double pointSize,
                        AffineTransform transform,
                        boolean antialias,
                        Zone glyphZone, Hinter hinter)
  {
    glyphZone.setNumPoints(4);
    loadSubGlyph(glyphIndex, pointSize, transform, antialias, glyphZone,
                 0, 0, hinter);
  }

  public void loadGlyph(int glyphIndex, AffineTransform transform,
                        Zone glyphZone, Hinter hinter)
  {
    loadGlyph(glyphIndex, unitsPerEm, transform, false, glyphZone, hinter);
  }

  private void loadSubGlyph(int glyphIndex,
                            double pointSize,
                            AffineTransform transform,
                            boolean antialias,
                            Zone glyphZone,
                            int preTranslateX,
                            int preTranslateY,
                            Hinter hinter)
  {
    ByteBuffer glyph;
    int numContours;
    int xMin, yMin, xMax, yMax;
    byte flag;

    glyph = glyphLocator.getGlyphData(glyphIndex);

    if (glyph == null)
    {
      glyphZone.setNumPoints(4);
      setPhantomPoints(glyphIndex, 0, glyphZone);
      glyphZone.transform(pointSize, transform, unitsPerEm,
                          preTranslateX, preTranslateY);
      return;
    }

    numContours = glyph.getShort();
    xMin = glyph.getChar();
    yMin = glyph.getChar();
    xMax = glyph.getChar();
    yMax = glyph.getChar();
    

    if (numContours >= 0)
      loadSimpleGlyph(glyphIndex, pointSize, transform, antialias,
                      numContours, glyph, glyphZone,
                      preTranslateX, preTranslateY, hinter);
    else
      loadCompoundGlyph(glyphIndex, pointSize, transform, antialias,
                        glyph, glyphZone,
                        preTranslateX, preTranslateY, hinter);
  }


  private void loadSimpleGlyph(int glyphIndex,
                               double pointSize, AffineTransform transform,
                               boolean antialias,
                               int numContours, ByteBuffer glyph,
                               Zone glyphZone,
                               int preTranslateX, int preTranslateY,
                               Hinter hinter)
  {
    int numPoints;
    int posInstructions, numInstructions;
    boolean execInstructions;

    execInstructions = vm.setup(pointSize, transform, antialias);

    /* Load the contour end points and determine the number of
     * points.
     */
    for (int i = 0; i < numContours; i++)
      contourEndPoints[i] = glyph.getChar();
    if (numContours > 0)
      numPoints = 1 + contourEndPoints[numContours - 1];
    else
      numPoints = 0;
    glyphZone.setNumPoints(numPoints + 4);

    numInstructions = glyph.getChar();
    posInstructions = glyph.position();
    glyph.position(posInstructions + numInstructions);
    loadFlags(numPoints, glyph);
    loadCoordinates(numPoints, glyph, glyphZone);
    for (int i = 0; i < numContours; i++)
      glyphZone.setContourEnd(contourEndPoints[i], true);

    setPhantomPoints(glyphIndex, numPoints, glyphZone);
    glyphZone.transform(pointSize, transform, unitsPerEm,
                        preTranslateX, preTranslateY);

    if (execInstructions && hinter != null)
      {
        hinter.applyHints(glyphZone);
      }
  }


  private static final short ARGS_ARE_WORDS = 1;
  private static final short ARGS_ARE_XY_VALUES = 2;
  private static final short ROUND_XY_TO_GRID = 4;
  private static final short WE_HAVE_A_SCALE = 8;
  private static final short MORE_COMPONENTS = 32;
  private static final short WE_HAVE_AN_X_AND_Y_SCALE = 64;
  private static final short WE_HAVE_A_TWO_BY_TWO = 128;
  private static final short WE_HAVE_INSTRUCTIONS = 256;
  private static final short USE_MY_METRICS = 512;
  private static final short OVERLAP_COMPOUND = 1024;
  private static final short SCALED_COMPONENT_OFFSET = 2048;
  private static final short UNSCALED_COMPONENT_OFFSET = 4096;

  private void loadCompoundGlyph(int glyphIndex,
                                 double pointSize,
                                 AffineTransform transform,
                                 boolean antialias,
                                 ByteBuffer glyph,
                                 Zone glyphZone,
                                 int preTranslateX, int preTranslateY,
                                 Hinter hinter)
  {
    short flags;
    int subGlyphIndex;
    int metricsGlyphIndex;
    Zone subGlyphZone = new Zone(glyphZone.getCapacity());
    int arg1, arg2;
    double a, b, c, d, e, f;
    AffineTransform componentTransform = new AffineTransform();

    /* By default, use the metrics of the compound glyph. The default
     * is overridden if some component glyph has the USE_MY_METRICS
     * flag set.
     */
    metricsGlyphIndex = glyphIndex;

    do
    {
      flags = glyph.getShort();
      subGlyphIndex = glyph.getChar();

      if ((flags & USE_MY_METRICS) != 0)
        metricsGlyphIndex = subGlyphIndex;

      if ((flags & ARGS_ARE_WORDS) != 0)
      {
        arg1 = glyph.getShort();
        arg2 = glyph.getShort();
      }
      else
      {
        arg1 = glyph.get();
        arg2 = glyph.get();
      }

      if ((flags & WE_HAVE_A_SCALE) != 0)
      {
        a = d = getDouble214(glyph);
        b = c = 0.0; 
      }
      else if ((flags & WE_HAVE_AN_X_AND_Y_SCALE) != 0)
      {
        a = getDouble214(glyph);
        d = getDouble214(glyph);
        b = c = 0.0; 
      }
      else if ((flags & WE_HAVE_A_TWO_BY_TWO) != 0)
      {
        a = getDouble214(glyph);
        b = getDouble214(glyph);
        c = getDouble214(glyph);
        d = getDouble214(glyph);
      }
      else
      {
        a = d = 1.0;
        b = c = 0.0;
      }

      double m = Math.max(Math.abs(a), Math.abs(b));
      double n = Math.max(Math.abs(c), Math.abs(d));

      /* The Apple TrueType specification actually says that m is
       * multiplied by two if
       *
       *        abs(abs(a) - abs(c)) <= 33/65536,
       *
       * but this is probably a typo. On 2003-07-23, Sascha Brawer
       * wrote an e-mail message to applefonts@apple.com, asking
       * whether this might possibly be an error in the specification.
       */
      if (Math.abs(Math.abs(a) - Math.abs(b)) <= 33.0/65536.0)
        m = m * 2;

      if (Math.abs(Math.abs(c) - Math.abs(d)) <= 33.0/65536.0)
        n = n * 2;
      
      if ((flags & ARGS_ARE_XY_VALUES) != 0)
      {
        e = m * arg1;
        f = n * arg2;
      }
      else
        e = f = 0.0;

      componentTransform.setTransform(a, b, c, d, 0.0, 0.0);
      
      // System.out.println("componentTransform = " + componentTransform
      //   + ", e=" + e + ", f=" + f);
      componentTransform.concatenate(transform);

      int pos = glyph.position();
      int lim = glyph.limit();
      
      loadSubGlyph(subGlyphIndex, pointSize, componentTransform,
                   antialias, subGlyphZone,
                   Math.round((float) e + preTranslateX),
                   Math.round(-((float) f + preTranslateY)), hinter);
      glyphZone.combineWithSubGlyph(subGlyphZone, 4);
      glyph.limit(lim).position(pos);
    }
    while ((flags & MORE_COMPONENTS) != 0);

    setPhantomPoints(metricsGlyphIndex, glyphZone.getSize() - 4, glyphZone);
  }


  private double getDouble214(ByteBuffer buf)
  {
    return ((double) buf.getShort()) / (1 << 14);
  }


  /**
   * Loads the per-point flags of a glyph into the
   * <code>pointFlags</code> field.
   */
  private void loadFlags(int numPoints, ByteBuffer glyph)
  {
    byte flag;
    int numRepetitions;

    for (int i = 0; i < numPoints; i++)
    {
      pointFlags[i] = flag = glyph.get();
      if ((flag & 8) != 0)
      {
        numRepetitions = ((int) glyph.get()) & 0xff;
        while (numRepetitions > 0)
        {
          pointFlags[++i] = flag;
          --numRepetitions;
        }
      }
    }
  }


  private void loadCoordinates(int numPoints, ByteBuffer glyph,
                               Zone glyphZone)
  {
    int x, y;
    byte flag;

    x = 0;
    for (int i = 0; i < numPoints; i++)
    {
      flag = pointFlags[i];
      if ((flag & 2) == 0)
      {
        if ((flag & 16) == 0)
          x += glyph.getShort();
      }
      else
      {
        if ((flag & 16) != 0)
          x += (glyph.get() & 0xff);
        else
          x -= (glyph.get() & 0xff);
      }
      glyphZone.setOriginalX(i, x);
      glyphZone.setOnCurve(i, (flag & 1) == 1);
    }

    y = 0;
    for (int i = 0; i < numPoints; i++)
    {
      flag = pointFlags[i];
      if ((flag & 4) == 0)
      {
        if ((flag & 32) == 0)
          y += glyph.getShort();
      }
      else
      {
        if ((flag & 32) != 0)
          y += (glyph.get() & 0xff);
        else
          y -= (glyph.get() & 0xff);
      }
      glyphZone.setOriginalY(i, -y);
    }
  }


  private void setPhantomPoints(int glyphIndex, int numPoints,
                                Zone glyphZone)
  {
    /* Phantom point 0: Character origin. */
    glyphZone.setOriginalX(numPoints, 0);
    glyphZone.setOriginalY(numPoints, 0);

    /* Phantom point 1: Horizontal advance point. */
    glyphZone.setOriginalX(numPoints + 1,
                   glyphMeasurer.getAdvanceWidth(glyphIndex, true));
    glyphZone.setOriginalY(numPoints + 1,
                           glyphMeasurer.getAdvanceHeight(glyphIndex, true));
    
    /* Phantom point 2: Vertical origin. */
    int vertX = glyphMeasurer.getAscent(/* vertical */ false);
    int vertY = glyphMeasurer.getAscent(/* horizontal */ true);
    glyphZone.setOriginalX(numPoints + 2, vertX);
    glyphZone.setOriginalY(numPoints + 2, vertY);

    /* Phantom point 3: Vertical advance point. */
    glyphZone.setOriginalX(numPoints + 3,
                           vertX + glyphMeasurer.getAdvanceWidth(glyphIndex, false));
    glyphZone.setOriginalY(numPoints + 3,
                           vertY + glyphMeasurer.getAdvanceHeight(glyphIndex, false));
  }
}
