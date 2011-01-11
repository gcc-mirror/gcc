/* GstNativeDataLine.java -- SourceDataLine implementation.
 Copyright (C) 2007 Free Software Foundation, Inc.

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

package gnu.javax.sound.sampled.gstreamer.lines;

import gnu.classpath.Pointer;

import javax.sound.sampled.LineUnavailableException;

public class GstNativeDataLine
{
  public static final GstPipeline createSourcePipeline(int bufferSize)
    throws LineUnavailableException
  {
    GstPipeline pipeline = new GstPipeline(bufferSize);

    pipeline.createForWrite();

    if (!setup_sink_pipeline(pipeline.getNativeClass()))
      throw new LineUnavailableException("Line unavailable");

    return pipeline;
  }

  /* native methods */

  /**
   * Initialize the native peer and enables the object cache.
   * It is meant to be used by the static initializer.
   */
  native static final private void init_id_cache();

  /**
   * Setup a new GStreamer Pipeline
   */
  native static final private boolean setup_sink_pipeline(Pointer pipeline);

  static
  {
    System.loadLibrary("gstreamerpeer"); //$NON-NLS-1$
    init_id_cache();
  }
}
