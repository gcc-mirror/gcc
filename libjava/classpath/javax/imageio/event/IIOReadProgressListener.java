/* IIOReadProgressListener.java --
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

import java.util.EventListener;

import javax.imageio.ImageReader;

public interface IIOReadProgressListener extends EventListener
{
  /**
   * Reports that the current image read operation has completed.
   * 
   * @param source the <code>ImageReader</code> object calling this method
   */
  void imageComplete(ImageReader source);

  /**
   * Reports the approximate percentage of completions of this image read
   * operation.
   * 
   * @param source the <code>ImageReader</code> object calling this method
   * @param percentageDone the approximate percentage of encoding completed
   */
  void imageProgress(ImageReader source, float percentageDone);

  /**
   * Reports that the current image read operation has started.
   * 
   * @param source the <code>ImageReader</code> object calling this method
   * @param imageIndex the index of the image to read
   */
  void imageStarted(ImageReader source, int imageIndex);

  /**
   * Reports that a read operation has been aborted.
   * 
   * @param source the <code>ImageReader</code> object calling this method
   */
  void readAborted(ImageReader source);

  /**
   * Reports that a sequence of read operationshas completed.
   * 
   * @param source the <code>ImageReader</code> object calling this method
   */
  void sequenceComplete(ImageReader source);

  /**
   * Reports that a sequence of read operations is beginning.
   * 
   * @param source the <code>ImageReader</code> object calling this method
   * @param minIndex the index of the first image to be read
   */
  void sequenceStarted(ImageReader source, int minIndex);

  /**
   * Reports that a thumbnail read operation has completed.
   * 
   * @param source the <code>ImageReader</code> object calling this method
   */
  void thumbnailComplete(ImageReader source);

  /**
   * Reports the approximate percentage of completion of a thumbnail read
   * operation.
   * 
   * @param source the <code>ImageReader</code> object calling this method
   * @param percentageDone the approximate percentage of encoding completed
   */
  void thumbnailProgress(ImageReader source, float percentageDone);

  /**
   * Reports that a thumbnail read operation is beginning.
   * 
   * @param source the <code>ImageReader</code> object calling this method
   * @param imageIndex the index of the image being read
   * @param thumbnailIndex the index of the thumbnail being read
   */
  void thumbnailStarted(ImageReader source, int imageIndex, int thumbnailIndex);
}
