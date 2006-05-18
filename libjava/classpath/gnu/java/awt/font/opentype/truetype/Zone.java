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

import java.awt.geom.AffineTransform;
import java.awt.geom.GeneralPath;
import java.awt.geom.PathIterator;


/**
 * A collection of points with some additional information.
 */
final class Zone
{
  private final int[] pos;
  private final int[] origPos;
  private final byte[] flags;
  private int numPoints;

  private static final int FLAG_TOUCHED_X = 1;
  private static final int FLAG_TOUCHED_Y = 2;
  private static final int FLAG_ON_CURVE = 4;
  private static final int FLAG_CONTOUR_END = 8;

  public Zone(int maxNumPoints)
  {
    origPos = new int[maxNumPoints * 2];
    pos = new int[maxNumPoints * 2];
    flags = new byte[maxNumPoints];
  }


  public int getCapacity()
  {
    return flags.length;
  }


  public int getSize()
  {
    return numPoints;
  }


  public int getX(int point)
  {
    return pos[2 * point];
  }


  public void setX(int point, int value, boolean touch)
  {
    pos[2 * point] = value;
    if (touch)
      flags[point] |= FLAG_TOUCHED_X;
  }


  public void setY(int point, int value, boolean touch)
  {
    pos[2 * point + 1] = value;
    if (touch)
      flags[point] |= FLAG_TOUCHED_Y;
  }


  public int getY(int point)
  {
    return pos[2 * point + 1];
  }


  public int getOriginalX(int point)
  {
    return origPos[2 * point];
  }


  public int getOriginalY(int point)
  {
    return origPos[2 * point + 1];
  }


  public void setOriginalX(int point, int x)
  {
    origPos[2 * point] = x;
  }

  public void setOriginalY(int point, int y)
  {
    origPos[2 * point + 1] = y;
  }

  public void setNumPoints(int numPoints)
  {
    this.numPoints = numPoints;
    for (int i = 0; i < numPoints; i++)
      flags[i] = 0;
    for (int i = 0; i < 2 * numPoints; i++)
      origPos[i] = pos[i] = 0;
  }


  public boolean isOnCurve(int point)
  {
    return (flags[point] & FLAG_ON_CURVE) != 0;
  }


  public void setOnCurve(int point, boolean onCurve)
  {
    if (onCurve)
      flags[point] |= FLAG_ON_CURVE;
    else
      flags[point] &= ~FLAG_ON_CURVE;
  }


  public boolean isContourEnd(int point)
  {
    return (flags[point] & FLAG_CONTOUR_END) != 0;
  }


  public void setContourEnd(int point, boolean segEnd)
  {
    if (segEnd)
      flags[point] |= FLAG_CONTOUR_END;
    else
      flags[point] &= ~FLAG_CONTOUR_END;
  }




  void transform(double pointSize, AffineTransform deviceTransform,
                 int unitsPerEm, int preTranslateX, int preTranslateY)
  {
    double scaleX, scaleY, shearX, shearY;
    double factor;

    factor = pointSize / (double) unitsPerEm;
    scaleX = deviceTransform.getScaleX() * factor;
    scaleY = deviceTransform.getScaleY() * factor;
    shearX = deviceTransform.getShearX() * factor;
    shearY = deviceTransform.getShearY() * factor;

    for (int i = 0; i < numPoints; i++)
    {
      int x = origPos[2 * i] + preTranslateX;
      int y = origPos[2 * i + 1] + preTranslateY;

      origPos[2*i] = pos[2 * i] = Fixed.valueOf(scaleX * x + shearX * y);
      origPos[2*i+1] = pos[2 * i + 1] = Fixed.valueOf(shearY * x + scaleY * y);
    }
  }



  void combineWithSubGlyph(Zone zone, int numPhantomPoints)
  {
    int offset = this.numPoints - numPhantomPoints;
    int count = zone.numPoints;
    System.arraycopy(zone.origPos, 0, this.origPos, 2 * offset,
                     count * 2);
    System.arraycopy(zone.pos, 0, this.pos, 2 * offset,
                     count * 2);
    System.arraycopy(zone.flags, 0, this.flags, offset, count);
    this.numPoints += count - numPhantomPoints;
  }


  private void dump()
  {
    for (int i = 0; i < numPoints; i++)
    {
      System.out.print(" " + i + ": ");
      System.out.print(Fixed.toString(pos[i*2], pos[i*2+1]));
      System.out.print(' ');
      System.out.print(Fixed.toString(origPos[i*2], origPos[i*2+1]));
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


  public PathIterator getPathIterator()
  {
    return new ZonePathIterator(this);
  }


  public GeneralPath getPath()
  {
    GeneralPath p = new GeneralPath(GeneralPath.WIND_NON_ZERO, numPoints);
    p.append(getPathIterator(), /* connect */ false);
    return p;
  }
}
