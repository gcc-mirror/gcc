/* PaintContext.java -- the environment for performing a paint operation
   Copyright (C) 2000, 2002 Free Software Foundation

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


package java.awt;

import java.awt.image.ColorModel;
import java.awt.image.Raster;

/**
 * @author Warren Levy (warrenl@cygnus.com)
 * @see Paint
 * @since 1.1
 * @status updated to 1.4
 */
public interface PaintContext
{
  /**
   * Release the resources allocated for the paint.
   */
  void dispose();

  /**
   * Return the color model of this context. It may be different from the
   * hint specified during createContext, as not all contexts can generate
   * color patterns in an arbitrary model.
   *
   * @return the context color model
   */
  ColorModel getColorModel();

  /**
   * Return a raster containing the colors for the graphics operation.
   *
   * @param x the x-coordinate, in device space
   * @param y the y-coordinate, in device space
   * @param w the width, in device space
   * @param h the height, in device space
   * @return a raster for the given area and color
   */
  Raster getRaster(int x, int y, int w, int h);
} // interface PaintContext
