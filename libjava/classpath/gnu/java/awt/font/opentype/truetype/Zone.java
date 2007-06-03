/* Zone.java -- A collection of points with some additional information.
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

import gnu.java.awt.font.FontDelegate;

import java.awt.geom.AffineTransform;
import java.awt.geom.GeneralPath;
import java.awt.geom.PathIterator;


/**
 * A collection of points with some additional information.
 */
public final class Zone
{
  private Point[] points;
  private int numPoints;

  public double scaleX, scaleY, shearX, shearY;

  public Zone(int maxNumPoints)
  {
    points = new Point[maxNumPoints];
  }

  public int getCapacity()
  {
    return points.length;
  }


  public int getSize()
  {
    return numPoints;
  }


  public int getX(int point)
  {
    return getX(point, FontDelegate.FLAG_FITTED);
  }

  public int getX(int point, int flags)
  {
    int x;
    if ((flags & FontDelegate.FLAG_FITTED) != 0)
      x = points[point].x;
    else
      x = points[point].scaledX;
    return x;
  }


  public void setX(int point, int value, boolean touch)
  {
    points[point].scaledX = value;
    points[point].x = value;
    if (touch)
      points[point].flags |= Point.FLAG_TOUCHED_X;
  }


  public void setY(int point, int value, boolean touch)
  {
    points[point].scaledY = value;
    points[point].y = value;
    if (touch)
      points[point].flags |= Point.FLAG_TOUCHED_Y;
  }

  public int getY(int point)
  {
    return getY(point, FontDelegate.FLAG_FITTED);
  }

  public int getY(int point, int flags)
  {
    int y;
    if ((flags & FontDelegate.FLAG_FITTED) != 0)
      y = points[point].y;
    else
      y = points[point].scaledY;
    return y;
  }


  public int getOriginalX(int point)
  {
    return points[point].origX;
  }


  public int getOriginalY(int point)
  {
    return points[point].origY;
  }


  public void setOriginalX(int point, int x)
  {
    points[point].origX = x;
  }

  public void setOriginalY(int point, int y)
  {
    points[point].origY = y;
  }

  public void setNumPoints(int numPoints)
  {
    for (int i = 0; i < numPoints; i++)
      points[i] = new Point();
    this.numPoints = numPoints;
  }


  public boolean isOnCurve(int point)
  {
    return (points[point].flags & Point.FLAG_ON_CURVE) != 0;
  }


  public void setOnCurve(int point, boolean onCurve)
  {
    if (onCurve)
      points[point].flags |= Point.FLAG_ON_CURVE;
    else
      points[point].flags &= ~Point.FLAG_ON_CURVE;
  }


  public boolean isContourEnd(int point)
  {
    return (points[point].flags & Point.FLAG_CONTOUR_END) != 0;
  }


  public void setContourEnd(int point, boolean segEnd)
  {
    if (segEnd)
      points[point].flags |= Point.FLAG_CONTOUR_END;
    else
      points[point].flags &= ~Point.FLAG_CONTOUR_END;
  }




  void transform(double pointSize, AffineTransform deviceTransform,
                 int unitsPerEm, int preTranslateX, int preTranslateY)
  {
    double factor;

    factor = pointSize / (double) unitsPerEm;
    scaleX = deviceTransform.getScaleX() * factor;
    scaleY = deviceTransform.getScaleY() * factor;
    shearX = deviceTransform.getShearX() * factor;
    shearY = deviceTransform.getShearY() * factor;

    for (int i = 0; i < numPoints; i++)
    {
      int x = points[i].origX + preTranslateX;
      int y = points[i].origY + preTranslateY;

      points[i].scaledX = points[i].x = Fixed.valueOf(scaleX * x
                                                      + shearX * y);
      points[i].scaledY = points[i].y = Fixed.valueOf(shearY * x
                                                      + scaleY * y);
    }
  }



  void combineWithSubGlyph(Zone zone, int numPhantomPoints)
  {
    int offset = this.numPoints - numPhantomPoints;
    int count = zone.numPoints;
    System.arraycopy(zone.points, 0, this.points, offset, count);
    this.numPoints += count - numPhantomPoints;
  }


  private void dump()
  {
    for (int i = 0; i < numPoints; i++)
    {
      System.out.print(" " + i + ": ");
      System.out.print(Fixed.toString(points[i].scaledX, points[i].scaledY));
      System.out.print(' ');
      System.out.print(Fixed.toString(points[i].origX, points[i].origY));
      System.out.print(' ');
      if (isOnCurve(i))
        System.out.print('.');
      else
        System.out.print('c');
      if (isContourEnd(i))      
        System.out.print('E');
      System.out.println();
      if (isContourEnd(i))
        System.out.println();
    }
  }


  public PathIterator getPathIterator(int type)
  {
    return new ZonePathIterator(this, type);
  }


  public GeneralPath getPath(int type)
  {
    GeneralPath p = new GeneralPath(GeneralPath.WIND_NON_ZERO, numPoints);
    p.append(getPathIterator(type), /* connect */ false);
    return p;
  }

  /**
   * Returns the number of contours in this outline.
   *
   * @return the number of contours in this outline
   */
  public int getNumContours()
  {
    int num = 0;
    for (int i = 0; i < numPoints; i++)
      {
        if (isContourEnd(i))
          num++;
      }
    return num;
  }

  public int getContourEnd(int n)
  {
    int idx = -1;
    int num = 0;
    for (int i = 0; i < numPoints; i++)
      {
        if (isContourEnd(i))
          {
            idx = i;
            if (num == n)
              break;
            num++;
          }
      }
    return idx;
  }

  public Point[] getPoints()
  {
    return points;
  }
}
