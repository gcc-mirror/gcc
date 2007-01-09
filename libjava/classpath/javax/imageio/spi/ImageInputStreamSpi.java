/* ImageInputStreamSpi.java -- Service provider for image input streams.
   Copyright (C) 2004, 2005  Free Software Foundation, Inc.

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


package javax.imageio.spi;

import java.io.File;
import java.io.IOException;

import javax.imageio.stream.ImageInputStream;

/**
 * An abstract superclass for service providers that create
 * {@linkplain javax.imageio.stream.ImageInputStream image input
 * streams} for a file, URL, byte array or any other source.
 *
 * @since 1.4
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public abstract class ImageInputStreamSpi
  extends IIOServiceProvider
{
  /**
   * Indicates which kind of input is processable by the streams
   * created by {@link #createInputStreamInstance(Object)}.
   */
  protected Class<?> inputClass;


  /**
   * Constructs a service provider for image input streams, given no
   * parameters. It is up to the sub-class to set {@link #vendorName},
   * {@link #version} and {@link #inputClass} to non-null values.
   */
  protected ImageInputStreamSpi()
  {
  }


  /**
   * Constructs a service provider for image input streams, given the
   * vendor name and a version string.
   *
   * @throws IllegalArgumentException if <code>vendorName</code>
   * or <code>version</code> is <code>null</code>.
   */
  public ImageInputStreamSpi(String vendorName, String version,
                             Class<?> inputClass)
  {
    super(vendorName, version);
    this.inputClass = inputClass;
  }


  /**
   * Determines which kind of input is processable by the streams
   * created by {@link #createInputStreamInstance(Object)}.
   */
  public Class<?> getInputClass()
  {
    return inputClass;
  }


  /**
   * Determines whether <code>ImageInputStreams</code> created
   * by this service provider benefit from using a cache file.
   *
   * <p>The default behavior is to return <code>false</code>.
   *
   * @return <code>true</code> if the created streams are faster or
   * need less memory when a cache file is being used;
   * <code>false</code> if no positive effect results from the cache
   * file.
   */
  public boolean canUseCacheFile()
  {
    return false;
  }


  /**
   * Determines whether <code>ImageInputStreams</code> created
   * by this service provider require the use of a cache file.
   *
   * <p>The default behavior is to return <code>false</code>.
   *
   * @return <code>true</code> if the created streams can only work
   * when a cache file is being used; <code>false</code> if no cache
   * file is needed.
   */
  public boolean needsCacheFile()
  {
    return false;
  }


  public abstract ImageInputStream createInputStreamInstance(Object input,
                                                             boolean useCache,
                                                             File cacheDir)
    throws IOException;


  public ImageInputStream createInputStreamInstance(Object input)
    throws IOException
  {
    return createInputStreamInstance(input, canUseCacheFile(), null);
  }
}
