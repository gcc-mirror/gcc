/* QPainterPath.java --
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

import java.awt.Shape;
import java.awt.geom.AffineTransform;
import java.awt.geom.PathIterator;
import java.awt.geom.GeneralPath;

/**
 * A simple wrapper class for a QPainterPath,
 * createable from an AWT Shape
 */
public class QPainterPath extends NativeWrapper
{
  QPainterPath()
  {
  }

  public QPainterPath( Shape s )
  {
    PathIterator pi = s.getPathIterator( new AffineTransform() );
    double[] coords = new double[6];

    init( pi.getWindingRule() );

    while( !pi.isDone() )
      {
	switch( pi.currentSegment(coords) )
	  {
	  case PathIterator.SEG_MOVETO:
	    moveTo( coords[0], coords[1] );
	    break;

	  case PathIterator.SEG_CLOSE:
	    close();
	    break;
	  
	  case PathIterator.SEG_LINETO:
	    lineTo( coords[0], coords[1] );
	    break;
	  
	  case PathIterator.SEG_QUADTO:
	    quadTo( coords[0], coords[1], coords[2], coords[3] );
	    break;
	  
	  case PathIterator.SEG_CUBICTO:
	    cubicTo( coords[0], coords[1], 
		     coords[2], coords[3],
		     coords[4], coords[5] );
	    break;
	  }
	pi.next();
      }
  }

  /**
   * Constructor for rectangles.
   */
  public QPainterPath( double x, double y, double w, double h )
  {
    init(PathIterator.WIND_EVEN_ODD);
    moveTo( x, y );
    lineTo( x + w, y );
    lineTo( x + w, y + h );
    lineTo( x , y + h );
    lineTo( x, y );
    close();
  }

  /**
   * Constructor for lines.
   */
  public QPainterPath( double x1, double y1, double x2, double y2, boolean s )
  {
    init(PathIterator.WIND_EVEN_ODD);
    moveTo( x1, y1 );
    lineTo( x2, y2 );
  }

  /**
   * Returns the QPainterPath as a GeneralPath
   */
  public native GeneralPath getPath();

  private native void init(int windingRule);

  private native void moveTo(double x, double y);

  private native void close();

  private native void lineTo(double x, double y);

  private native void quadTo(double x1, double y1, double x2, double y2);

  private native void cubicTo(double x1, double y1, double x2, double y2,
			      double x3, double y3);

  public native void dispose();
  
  public void finalize()
  {
    dispose();
  }
}

