/* ScanlineConverter.java -- Rasterizes Shapes
   Copyright (C) 2006, 2007 Free Software Foundation, Inc.

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


package gnu.java.awt.java2d;

import gnu.java.math.Fixed;

import java.awt.RenderingHints;
import java.awt.Shape;
import java.awt.geom.AffineTransform;
import java.awt.geom.PathIterator;

/**
 * Rasterizes {@link Shape} objects on an AbstractGraphics2D.
 */
public final class ScanlineConverter
{

  /**
   * The number of digits to use for fixed point arithmetics.
   */
  private static int FIXED_DIGITS = 6;

  /**
   * The fixed point constant for the number one.
   */
  private static int ONE = Fixed.fixedValue(FIXED_DIGITS, 1);

  /**
   * The number of significant bits for the Y resolution.
   */
  private static int Y_RESOLUTION = 4;

  /**
   * The actual number of scanlines.
   */
  private int numScanlines;

  /**
   * The number of scanlines. This can contain more elements than we have
   * scanlines. The real number of scanlines is stored in
   * {@link #numScanlines}. This can also contain null values for empty
   * scanlines.
   */
  private Scanline[] scanlines;

  /**
   * The upper bounds which correspond to the index 0 in the scanline array.
   *
   * This is a fixed point value.
   */
  private int upperBounds;

  /**
   * The resolution of the scanline converter.
   *
   * This is a fixed point value.
   */
  private int resolution;

  /**
   * One half step according to the resolution. This is stored to avoid
   * unnecessary operations during rendering.
   */
  private int halfStep;

  /**
   * This is used in {@link #addShape(PathIterator, boolean)} to
   * receive the coordinates of the path.
   */
  private float[] coords;

  /**
   * The active edges.
   */
  private ActiveEdges activeEdges;

  private PolyEdge edgePool;
  private PolyEdge edgePoolLast;

  private int minY;
  private int maxY;
  private int minX;
  private int maxX;

  /**
   * Holds and manages information about the pixel coverage.
   */
  private ScanlineCoverage scanlineCoverage;

  /**
   * Create a new ScanlineConverter.
   */
  ScanlineConverter()
  {
    scanlines = new Scanline[10];
    coords = new float[6];
    activeEdges = new ActiveEdges();
    edgePool = new PolyEdge();
    edgePoolLast = edgePool;
    scanlineCoverage = new ScanlineCoverage();
  }

  /**
   * Renders the specified shape using the specified clip and transform.
   *
   * @param p the pixelizer that receives the coverage information
   * @param shape the shape to render
   * @param clip the clip
   * @param trans the transform
   */
  public void renderShape(Pixelizer p, Shape shape, Shape clip,
                          AffineTransform trans, int res, RenderingHints hints)
  {
    // TODO: Do something useful with the rendering hints. Like, adjusting
    // the resolution.

    // Prepare resolution and upper bounds.
    clear();
    setResolution(res);

    boolean haveClip = clip != null;

    // Add shapes.
    float flatness = Fixed.floatValue(FIXED_DIGITS, resolution / 2);
    PathIterator path = shape.getPathIterator(trans, flatness);
    addShape(path, false);
    if (haveClip)
      {
        path= clip.getPathIterator(trans, flatness);
        addShape(path, true);
      }

    setUpperBounds(minY);

    PolyEdge edge = edgePool;
    while (edge != edgePoolLast)
      {
        addEdge(edge);
        edge = edge.poolNext;
      }

    int y = upperBounds;
    int index;
    activeEdges.clear();
    // The render loop...
    Scanline scanline = null;
    int lastRealY = Fixed.intValue(FIXED_DIGITS, y);
    while (y <= maxY)
      {
        // First we put together our list of active edges.
        index = scanlineIndex(y);
        // If we go outside the scanline array we still need to render the
        // remaining edges until they end.
        scanline = index < scanlines.length ? scanlines[index] : null;
        if (scanline != null)
          {
            edge = scanline.getEdges();
            while (edge != null)
              {
                activeEdges.add(edge);
                edge = edge.scanlineNext;
              }
          }

        // Then we intersect all active edges with the current scanline
        // and sort them according to their intersection points.
        activeEdges.intersectSortAndPack(FIXED_DIGITS, y + halfStep);

        // Ok, now we can perform the actual scanlining.
        int realY = Fixed.intValue(FIXED_DIGITS, y + resolution);
        boolean push = lastRealY != realY;
        doScanline(p, y, push, haveClip);

        // Remove obsolete active edges.
        //activeEdges.remove(y + halfStep);
        // Go on with the next line...
        y += resolution;
        lastRealY = realY;

      }
  }

  /**
   * Clears all scanlines.
   */
  private void clear()
  {
    // Reset edge pool.
    edgePoolLast = edgePool;

    // Reset scanlines.
    for (int i = scanlines.length - 1; i >= 0 ; i--)
      {
        Scanline sl = scanlines[i];
        if (sl != null)
          sl.clear();
      }

    // Reset scanline coverage.
    scanlineCoverage.clear();

    // Reset bounds.
    minY = Integer.MAX_VALUE;
    maxY = Integer.MIN_VALUE;
    minX = Integer.MAX_VALUE;
    maxX = Integer.MIN_VALUE;
  }

  /**
   * Performs the scanlining on the current set of active edges.
   *
   * @param p the pixelizer to receive the pixel coverage data
   * @param y the Y coordinate
   * @param push true when the scanline is ready to be pushed to the
   *        pixelizer
   * @param haveClip true when there's a clip, false otherwise
   */
  private void doScanline(Pixelizer p, int y, boolean push,
                          boolean haveClip)
  {
    // First, rewind the scanline coverage.
    scanlineCoverage.rewind();

    // We begin outside the clip and outside the shape. We only draw when
    // we are inside the clip AND inside the shape.
    boolean inClip = ! haveClip;
    boolean inShape = false;
    PolyEdge lastEdge = null;
    int numEdges = activeEdges.getNumActiveEdges();
    for (int i = 0; i < numEdges; i++)
      {
        PolyEdge edge = activeEdges.getActiveEdge(i);
        if (inClip && inShape)
          {
            assert lastEdge != null;
            int x0 = lastEdge.xIntersection;
            int x1 = edge.xIntersection;
            assert x0 <= x1;

            int pix0 = Fixed.intValue(FIXED_DIGITS, x0);
            int pix1 = Fixed.intValue(FIXED_DIGITS, x1);
            int frac0 = ONE - Fixed.trunc(FIXED_DIGITS, x0);
            int frac1 = ONE - Fixed.trunc(FIXED_DIGITS, x1);
            // Only keep the first 4 digits after the point.
            frac0 = frac0 >> (FIXED_DIGITS - Y_RESOLUTION);
            frac1 = frac1 >> (FIXED_DIGITS - Y_RESOLUTION);
            scanlineCoverage.add(pix0, 1 * (1 << Y_RESOLUTION), frac0);
            scanlineCoverage.add(pix1, -1 * (1 << Y_RESOLUTION), -frac1);
          }
        if (edge.isClip)
          inClip = ! inClip;
        else
          inShape = ! inShape;

        lastEdge = edge;
      }

    // Push out the whole scanline to the pixelizer.
    if (push && ! scanlineCoverage.isEmpty())
      {
        p.renderScanline(Fixed.intValue(FIXED_DIGITS, y), scanlineCoverage);
        scanlineCoverage.clear();
      }
  } 


  /**
   * Sets the resolution. A value of 0 rasterizes the shape normally without
   * anti-aliasing. Greater values renders with a resolution of 2 ^ res.
   *
   * @param res the resolution
   */
  private void setResolution(int res)
  {
    int scanlinesPerPixel = 1 << res;
    int one = Fixed.fixedValue(FIXED_DIGITS, 1);
    resolution = one / (scanlinesPerPixel);
    halfStep = resolution / 2;

    scanlineCoverage.setMaxCoverage(scanlinesPerPixel << Y_RESOLUTION);
  }

  /**
   * Sets the vertical bounds of that shape that is beeing rendered.
   *
   * @param y0 the upper bounds
   */
  private void setUpperBounds(int y0)
  {
    upperBounds = fit(y0);
  }

  /**
   * Add a shape to the scanline converter.
   *
   * @param path
   * @param clip
   */
  private void addShape(PathIterator path, boolean clip)
  {
    int startX = 0;
    int startY = 0;
    int lastX = 0;
    int lastY = 0;
    while (! path.isDone())
      {
        int type = path.currentSegment(coords);
        switch (type)
          {
            case PathIterator.SEG_MOVETO:
              startX = lastX = Fixed.fixedValue(FIXED_DIGITS, coords[0]);
              startY = lastY = Fixed.fixedValue(FIXED_DIGITS, coords[1]);
              minY = Math.min(startY, minY);
              maxY = Math.max(startY, maxY);
              minX = Math.min(startX, minX);
              maxX = Math.max(startX, maxX);
              break;
            case PathIterator.SEG_LINETO:
              int x = Fixed.fixedValue(FIXED_DIGITS, coords[0]);
              int y = Fixed.fixedValue(FIXED_DIGITS, coords[1]);
              edgePoolAdd(lastX, lastY, x, y, clip);
              lastX = x;
              lastY = y;
              minY = Math.min(lastY, minY);
              maxY = Math.max(lastY, maxY);
              minX = Math.min(lastX, minX);
              maxX = Math.max(lastX, maxX);
              break;
            case PathIterator.SEG_CLOSE:
              edgePoolAdd(lastX, lastY, startX, startY, clip);
              lastX = startX;
              lastY = startY;
              break;
            case PathIterator.SEG_CUBICTO:
            case PathIterator.SEG_QUADTO:
            default:
              assert false;
          }
        path.next();
      }
  }

  /**
   * Adds an edge into the scanline array.
   */
  private void addEdge(PolyEdge edge)
  {
    // Determine index.
    int upper = Math.min(edge.y0, edge.y1);
    // Fit to raster.
    int index = scanlineIndex(upper);
    // Grow array when necessary.
    if (index >= scanlines.length)
      {
        int oldSize = scanlines.length;
        int newSize = Math.max(oldSize + oldSize / 2 + 1, index + 10);
        Scanline[] newScanlines = new Scanline[newSize];
        System.arraycopy(scanlines, 0, newScanlines, 0, oldSize);
        scanlines = newScanlines;
      }

    // Add edge.
    if (scanlines[index] == null)
      {
        scanlines[index] = new Scanline();
      }
    scanlines[index].addEdge(edge);
  }

  /**
   * Fits an Y coordinate to the grid.
   *
   * @param y the Y coordinate to fit
   *
   * @return the fitted Y coordinate
   */
  private int fit(int y)
  {
    int val1 = Fixed.div(FIXED_DIGITS, y, resolution);
    int rounded = Fixed.round(FIXED_DIGITS, val1);
    return Fixed.mul(FIXED_DIGITS, rounded, resolution);
  }

  /**
   * Calculates the scanline index for the specified y coordinate.
   *
   * @param y the y coordinate as fixed point value
   *
   * @return the scanline index
   */
  private int scanlineIndex(int y)
  {
    int fitted = fit(y);
    // Cleverly skip the fixed point conversions here.
    return (fitted - upperBounds)/ resolution;
  }

  private void edgePoolAdd(int x0, int y0, int x1, int y1, boolean clip)
  {
    // Don't need no horizontal edges.
    if (y0 != y1)
      {
        edgePoolLast.init(FIXED_DIGITS, x0, y0, x1, y1, clip);
        if (edgePoolLast.poolNext == null)
          {
            edgePoolLast.poolNext = new PolyEdge();
          }
        edgePoolLast = edgePoolLast.poolNext;
      }
  }
}
