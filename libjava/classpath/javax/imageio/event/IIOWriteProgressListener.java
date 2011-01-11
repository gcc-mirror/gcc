/* IIOWriteProgressListener.java --
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

import javax.imageio.ImageWriter;

public interface IIOWriteProgressListener extends EventListener
{
  /**
   * Reports that an image write operation has completed.
   *
   * @param source the <code>ImageWriter</code> object calling this method
   */
  void imageComplete(ImageWriter source);

  /**
   * Reports the approximate percentage of completion of an image write
   * operation.
   *
   * @param source the <code>ImageWriter</code> object calling this method
   * @param percentageDone the approximate percentage of decoding completed
   */
  void imageProgress(ImageWriter source, float percentageDone);

  /**
   * Reports that a thumbnail write operation has started.
   *
   * @param source the <code>ImageWriter</code> object calling this method
   * @param imageIndex the index of the image being written
   */
  void imageStarted(ImageWriter source, int imageIndex);

  /**
   * Reports that a thumbnail write operation has completed.
   *
   * @param source the <code>ImageWriter</code> object calling this method
   */
  void thumbnailComplete(ImageWriter source);

  /**
   * Reports the approximate percentage of completion of a thumbnail write
   * operation.
   *
   * @param source the <code>ImageWriter</code> object calling this method
   * @param percentageDone the approximate percentage of decoding completed
   */
  void thumbnailProgress(ImageWriter source, float percentageDone);

  /**
   * Reports that a thumbnail write operation is beginning.
   *
   * @param source the <code>ImageWriter</code> object calling this method
   * @param imageIndex the index of the image being written
   * @param thumnailIndex the index of the thumbnail being written
   */
  void thumbnailStarted(ImageWriter source, int imageIndex, int thumnailIndex);

  /**
   * Reports that an image write operation is aborted.
   *
   * @param source the <code>ImageWriter</code> object calling this method
   */
  void writeAborted(ImageWriter source);
}
