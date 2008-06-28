/* GstInputStream.java -- Trampoline class for an InputStream, mean to be used
 by native code.
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

package gnu.javax.sound.sampled.gstreamer.io;

import gnu.classpath.Pointer;

import java.io.IOException;
import java.io.InputStream;

/**
 * Encapsulates the functionality of an InputStream Object.
 * 
 * This class is only meant to be used by the native code, to allow reading
 * of the given InputStream as part of a the GStreamer InputStream Source
 * Plugin.
 * 
 * <strong>Note:</strong> this class will be not garbage collected as the
 * native code contains strong references to internal fields.
 * The native layer provides a method that can be called by the C code to
 * free the resources and to let the garbage collected to handle this class
 * when not needed anymore.
 * 
 * @author Mario Torre <neugens@limasoftware.net>
 */
public class GstInputStream
{
  /** The real InputStream on which to perform reading operations. */
  private InputStream istream;
  
  /**
   * Initialized in the native code, don't change without changes 
   * in the native layer.
   */
  private Pointer gstInputStream = null;
  
  public GstInputStream(InputStream istream)
  {
    this.istream = istream;
    init_instance();
  }
  
  public int read(byte[] buf, int off, int len) throws IOException
  {
    return this.istream.read(buf, off, len);
  }
  
  public int available() throws IOException
  {
    return this.istream.available();
  }
  
  /**
   * Return a reference to the GstInputStream native class as a Pointer object.
   * This method is intended as an helper accessor and the returned pointer
   * needs to be casted and used in the native code only. 
   *  
   * @return Pointer to the native GstInputStream class.
   */
  public Pointer getNativeClass()
  {
    return this.gstInputStream;
  }
  
  /* native methods */
  
  /**
   * Initialize the native peer and enables the object cache.
   * It is meant to be used by the class constructor.
   */
  native private final void init_instance();
  
  /**
   * Initialize the native peer and enables the object cache.
   * It is meant to be used by the static initializer.
   */
  native private static final void init_id_cache();

  static
  {
    System.loadLibrary("gstreamerpeer"); //$NON-NLS-1$
    init_id_cache();
  }
}
