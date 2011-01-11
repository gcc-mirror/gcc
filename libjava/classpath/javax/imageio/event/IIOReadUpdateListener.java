/* IIOReadUpdateListener.java --
   Copyright (C) 2004  Free Software Foundation, Inc.

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


package javax.imageio.event;

import java.awt.image.BufferedImage;
import java.util.EventListener;

import javax.imageio.ImageReader;

public interface IIOReadUpdateListener extends EventListener
{
  /**
   * Reports that a given region of the image has been updated.
   *
   * @param source the <code>ImageReader</code> object calling this method
   * @param image the BufferedImage being updated
   * @param minX the X coordinate of the leftmost updated column of pixels
   * @param minY the Y coordinate of the uppermost updated row of pixels
   * @param width the number of updated pixels horizontally
   * @param height the number of updated pixels vertically
   * @param periodX the horizontal spacing between updated pixels; a value of 1 means no gaps
   * @param periodY the vertical spacing between updated pixels; a value of 1 means no gaps
   * @param bands an array of <code>int</code>s indicating which bands are being updated
   */
  void imageUpdate(ImageReader source, BufferedImage image, int minX,
                   int minY, int width, int height, int periodX, int periodY,
                   int[] bands);

  /**
   * Reports that the current read operation has completed a progressive pass.
   *
   * @param source the <code>ImageReader</code> object calling this method
   * @param image the BufferedImage being updated
   */
  void passComplete(ImageReader source, BufferedImage image);

  /**
   * Reports that the current read operation is about to begin a progressive pass.
   *
   * @param source the <code>ImageReader</code> object calling this method
   * @param image the BufferedImage being updated
   * @param pass the numer of the pass that is about to begin, starting with 0
   * @param minPass the index of the first pass that will be decoded
   * @param maxPass the index of the last pass that will be decoded
   * @param minX the X coordinate of the leftmost updated column of pixels
   * @param minY the Y coordinate of the uppermost updated row of pixels
   * @param periodX the horizontal spacing between updated pixels; a value of 1 means no gaps
   * @param periodY the vertical spacing between updated pixels; a value of 1 means no gaps
   * @param bands an array of <code>int</code>s indicating which bands are being updated
   */
  void passStarted(ImageReader source, BufferedImage image, int pass,
                   int minPass, int maxPass, int minX, int minY, int periodX,
                   int periodY, int[] bands);

  /**
   * Reports that the current thumbnail read operation has completed a progressive pass.
   *
   * @param source the <code>ImageReader</code> object calling this method
   * @param image the BufferedImage being updated
   */
  void thumbnailPassComplete(ImageReader source, BufferedImage image);

  /**
   * Reports that the current thumbnail read operation is about to begin a progressive pass.
   *
   * @param source the <code>ImageReader</code> object calling this method
   * @param image the BufferedImage being updated
   * @param pass the numer of the pass that is about to begin, starting with 0
   * @param minPass the index of the first pass that will be decoded
   * @param maxPass the index of the last pass that will be decoded
   * @param minX the X coordinate of the leftmost updated column of pixels
   * @param minY the Y coordinate of the uppermost updated row of pixels
   * @param periodX the horizontal spacing between updated pixels; a value of 1 means no gaps
   * @param periodY the vertical spacing between updated pixels; a value of 1 means no gaps
   * @param bands an array of <code>int</code>s indicating which bands are being updated
   */
  void thumbnailPassStarted(ImageReader source, BufferedImage image, int pass,
                            int minPass, int maxPass, int minX, int minY,
                            int periodX, int periodY, int[] bands);

  /**
   * Reports that a given region of a thumbnail image has been updated.
   *
   * @param source the <code>ImageReader</code> object calling this method
   * @param image the BufferedImage being updated
   * @param minX the X coordinate of the leftmost updated column of pixels
   * @param minY the Y coordinate of the uppermost updated row of pixels
   * @param width the number of updated pixels horizontally
   * @param height the number of updated pixels vertically
   * @param periodX the horizontal spacing between updated pixels; a value of 1 means no gaps
   * @param periodY the vertical spacing between updated pixels; a value of 1 means no gaps
   * @param bands an array of <code>int</code>s indicating which bands are being updated
   */
  void thumbnailUpdate(ImageReader source, BufferedImage image, int minX,
                       int minY, int width, int height, int periodX,
                       int periodY, int[] bands);
}
