/* ImageReaderWriterSpi.java -- Superclass for image reader and writer spis.
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


/**
 * An abstract superclass that contains the common parts of {@link
 * javax.imageio.spi.ImageReaderSpi} and {@link
 * javax.imageio.spi.ImageWriterSpi}.
 *
 * @since 1.4
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public abstract class ImageReaderWriterSpi
  extends IIOServiceProvider
{
  /**
   * The human-readable, localized names of the supported image
   * formats. This value should be non-<code>null</code> after
   * construction.
   *
   * @see #getFormatNames()
   */
  protected String[] names;


  /**
   * The file suffixes of the supported image formats. This value
   * should be non-<code>null</code> after construction.
   *
   * @see #getFileSuffixes()
   */
  protected String[] suffixes;


  /**
   * The MIME types of the supported image formats.  This value
   * should be non-<code>null</code> after construction.
   *
   * @see #getMIMETypes()
   */
  protected String[] MIMETypes;


  /**
   * The fully qualified name of the class that implements the {@link
   * javax.imageio.ImageReader} or {@link javax.imageio.ImageWriter}
   * interface.  This value should be non-<code>null</code> after
   * construction.
   *
   * @see #getPluginClassName()
   */
  protected String pluginClassName;


  /**
   * Indicates whether the per-stream {@linkplain
   * javax.imageio.metadata.IIOMetadata metadata objects} associated
   * with this plug-in support format
   * <code>&#x201c;javax_imageio_1.0&#x201d;</code> in their
   * <code>getAsTree</code> and <code>setAsTree</code> methods.
   *
   * @see #isStandardStreamMetadataFormatSupported()
   */
  protected boolean supportsStandardStreamMetadataFormat;


  /**
   * The name of the format that allows encoding all stream metadata
   * without loss, or <code>null</code> if this plug-in does not
   * provide a format that preserves all stream metadata.
   */
  protected String nativeStreamMetadataFormatName;

  protected String nativeStreamMetadataFormatClassName;


  /**
   * The names of additional formats for encoding stream metadata,
   * other than the {@linkplain
   * #isStandardStreamMetadataFormatSupported() standard} and the
   * {@linkplain #getNativeStreamMetadataFormatName() native} formats,
   * or <code>null</code> if this plug-in does not provide any extra
   * formats.
   */
  protected String[] extraStreamMetadataFormatNames;


  protected String[] extraStreamMetadataFormatClassNames;


  /**
   * Indicates whether the per-image {@linkplain
   * javax.imageio.metadata.IIOMetadata metadata objects} associated
   * with this plug-in support format
   * <code>&#x201c;javax_imageio_1.0&#x201d;</code> in their
   * <code>getAsTree</code> and <code>setAsTree</code> methods.
   *
   * @see #isStandardImageMetadataFormatSupported()
   */
  protected boolean supportsStandardImageMetadataFormat;


  /**
   * The name of the format that allows encoding all image metadata
   * without loss, or <code>null</code> if this plug-in does not
   * provide a format that preserves all image metadata.
   */
  protected String nativeImageMetadataFormatName;

  protected String nativeImageMetadataFormatClassName;


  /**
   * The names of additional formats for encoding image metadata,
   * other than the {@linkplain
   * #isStandardImageMetadataFormatSupported() standard} and the
   * {@linkplain #getNativeImageMetadataFormatName() native} formats,
   * or <code>null</code> if this plug-in does not provide any extra
   * formats.
   */
  protected String[] extraImageMetadataFormatNames;


  protected String[] extraImageMetadataFormatClassNames;


  /**
   * Constructs an <code>ImageReaderWriteSpi</code> instance, without
   * specifying a number of parameters. Constructors of concrete
   * subclasses must ensure that they set all inherited fields to
   * meaningful values.
   */
  public ImageReaderWriterSpi()
  {
  }


  /**
   * Constructs an <code>ImageReaderWriteSpi</code> instance,
   * specifying a number of parameters.
   *
   * @param names the human-readable, localized names of the supported
   * image formats, for example <code>[&#x201c;Tagged Image File
   * Format&#x201d;, &#x201c;Portable Network
   * Graphics&#x201d;]</code>.
   *
   * @param suffixes the file suffixes of the supported image formats,
   * for example <code>[&#x201c;tiff&#x201d;, &#x201c;tif&#x201d;,
   * &#x201c;png&#x201d;]</code>.
   *
   * @param MIMETypes the MIME types of the supported image formats,
   * for example <code>[&#x201c;image/tiff&#x201d;,
   * &#x201c;image/png&#x201d;]</code>.
   *
   * @param pluginClassName the fully qualified name of the class that
   * implements the {@link javax.imageio.ImageReader} or {@link
   * javax.imageio.ImageWriter} interface.
   *
   * @param supportsStandardStreamMetadataFormat whether the
   * per-stream {@linkplain javax.imageio.metadata.IIOMetadata
   * metadata objects} associated with this plug-in support format
   * <code>&#x201c;javax_imageio_1.0&#x201d;</code> in their
   * <code>getAsTree</code> and <code>setAsTree</code> methods.
   *
   * @param nativeStreamMetadataFormatName the name of the format that
   * allows encoding all stream metadata without loss, or
   * <code>null</code> if this plug-in does not provide a format that
   * preserves all stream metadata.
   *
   * @param extraStreamMetadataFormatNames the names of additional
   * formats for encoding stream metadata, other than the {@linkplain
   * #isStandardStreamMetadataFormatSupported() standard} and the
   * {@linkplain #getNativeStreamMetadataFormatName() native} formats,
   * or <code>null</code> if this plug-in does not provide any extra
   * formats.
   *
   * @param supportsStandardImageMetadataFormat whether the per-image
   * {@linkplain javax.imageio.metadata.IIOMetadata metadata objects}
   * associated with this plug-in support format
   * <code>&#x201c;javax_imageio_1.0&#x201d;</code> in their
   * <code>getAsTree</code> and <code>setAsTree</code> methods.
   *
   * @param nativeImageMetadataFormatName the name of the format that
   * allows encoding all image metadata without loss, or
   * <code>null</code> if this plug-in does not provide a format that
   * preserves all image metadata.
   *
   * @param extraImageMetadataFormatNames the names of additional
   * formats for encoding image metadata, other than the {@linkplain
   * #isStandardImageMetadataFormatSupported() standard} and the
   * {@linkplain #getNativeImageMetadataFormatName() native} formats,
   * or <code>null</code> if this plug-in does not provide any extra
   * formats.
   *
   * @throws IllegalArgumentException if <code>vendorName</code>
   * or <code>version</code> is <code>null</code>.
   */
  public ImageReaderWriterSpi(String vendorName, String version,
                              String[] names, String[] suffixes,
                              String[] MIMETypes, String pluginClassName,
                              boolean supportsStandardStreamMetadataFormat,
                              String nativeStreamMetadataFormatName,
                              String nativeStreamMetadataFormatClassName,
                              String[] extraStreamMetadataFormatNames,
                              String[] extraStreamMetadataFormatClassNames,
                              boolean supportsStandardImageMetadataFormat,
                              String nativeImageMetadataFormatName,
                              String nativeImageMetadataFormatClassName,
                              String[] extraImageMetadataFormatNames,
                              String[] extraImageMetadataFormatClassNames)
  {
    /* The inherited constructor will throw IllegalArgumentException
     * if one of its arguments is null.
     */
    super(vendorName, version);

    if (names == null || names.length == 0 || pluginClassName == null)
      throw new IllegalArgumentException();

    this.names = names;
    this.suffixes = suffixes;
    this.MIMETypes = MIMETypes;
    this.pluginClassName = pluginClassName;

    this.supportsStandardStreamMetadataFormat
      = supportsStandardStreamMetadataFormat;

    this.nativeStreamMetadataFormatName
      = nativeStreamMetadataFormatName;

    this.nativeStreamMetadataFormatClassName
      = nativeStreamMetadataFormatClassName;

    this.extraStreamMetadataFormatNames
      = extraStreamMetadataFormatNames;

    this.extraStreamMetadataFormatClassNames
      = extraStreamMetadataFormatClassNames;

    this.supportsStandardImageMetadataFormat
      = supportsStandardImageMetadataFormat;

    this.nativeImageMetadataFormatName
      = nativeImageMetadataFormatName;

    this.nativeImageMetadataFormatClassName
      = nativeImageMetadataFormatClassName;

    this.extraImageMetadataFormatNames
      = extraImageMetadataFormatNames;

    this.extraImageMetadataFormatClassNames
      = extraImageMetadataFormatClassNames;
  }


  /**
   * Returns the human-readable, localized names of the supported
   * image formats. For example, a plug-in might return an array with
   * the elements <code>[&#x201c;Tagged Image File Format&#x201d;,
   * &#x201c;Portable Network Graphics&#x201d;]</code>.
   */
  public String[] getFormatNames()
  {
    return (String[]) names.clone();
  }


  /**
   * Returns the file suffixes of the supported image formats, for
   * example <code>[&#x201c;tiff&#x201d;, &#x201c;tif&#x201d;,
   * &#x201c;png&#x201d;]</code>.
   */
  public String[] getFileSuffixes()
  {
    return suffixes;
  }


  /**
   * Returns the MIME types of the supported image formats, for
   * example <code>[&#x201c;image/tiff&#x201d;,
   * &#x201c;image/png&#x201d;]</code>.
   *
   * @return an array of MIME type strings, or <code>null</code> if
   * none of the supported formats has an associated MIME type.
   */
  public String[] getMIMETypes()
  {
    return MIMETypes;
  }


  /**
   * Returns the fully qualified name of the class that implements the
   * {@link javax.imageio.ImageReader} or {@link
   * javax.imageio.ImageWriter} interface.
   */
  public String getPluginClassName()
  {
    return pluginClassName;
  }


  /**
   * Returns whether the per-stream {@linkplain
   * javax.imageio.metadata.IIOMetadata metadata objects} associated
   * with this plug-in support format
   * <code>&#x201c;javax_imageio_1.0&#x201d;</code> in their
   * <code>getAsTree</code> and <code>setAsTree</code> methods.
   */
  public boolean isStandardStreamMetadataFormatSupported()
  {
    return supportsStandardStreamMetadataFormat;
  }


  /**
   * Returns the name of the format that allows encoding all stream
   * metadata without loss, or <code>null</code> if this plug-in does
   * not provide a format that preserves all stream metadata.
   *
   * @see #getNativeImageMetadataFormatName()
   */
  public String getNativeStreamMetadataFormatName()
  {
    return nativeStreamMetadataFormatName;
  }


  /**
   * Returns the names of additional formats for encoding stream
   * metadata, other than the {@linkplain
   * #isStandardStreamMetadataFormatSupported() standard} and the
   * {@linkplain #getNativeStreamMetadataFormatName() native} formats,
   * or <code>null</code> if this plug-in does not provide any extra
   * formats.
   *
   * @see #getExtraImageMetadataFormatNames()
   */
  public String[] getExtraStreamMetadataFormatNames()
  {
    return extraStreamMetadataFormatNames;
  }


  /**
   * Returns whether the per-image {@linkplain
   * javax.imageio.metadata.IIOMetadata metadata objects} associated
   * with this plug-in support format
   * <code>&#x201c;javax_imageio_1.0&#x201d;</code> in their
   * <code>getAsTree</code> and <code>setAsTree</code> methods.
   */
  public boolean isStandardImageMetadataFormatSupported()
  {
    return supportsStandardImageMetadataFormat;
  }


  /**
   * Returns the name of the format that allows encoding all image
   * metadata without loss, or <code>null</code> if this plug-in does
   * not provide a format that preserves all image metadata.
   *
   * @see #getNativeStreamMetadataFormatName()
   */
  public String getNativeImageMetadataFormatName()
  {
    return nativeImageMetadataFormatName;
  }


  /**
   * Returns the names of additional formats for encoding image
   * metadata, other than the {@linkplain
   * #isStandardImageMetadataFormatSupported() standard} and the
   * {@linkplain #getNativeImageMetadataFormatName() native} formats,
   * or <code>null</code> if this plug-in does not provide any extra
   * formats.
   *
   * @see #getExtraStreamMetadataFormatNames()
   */
  public String[] getExtraImageMetadataFormatNames()
  {
    return extraImageMetadataFormatNames;
  }
}
