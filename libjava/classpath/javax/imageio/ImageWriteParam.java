/* ImageWriteParam.java --
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


package javax.imageio;

import java.awt.Dimension;
import java.util.Locale;

/**
 * DOCUMENT ME
 */
public class ImageWriteParam extends IIOParam
{

  /**
   * Can be passed to setTilingMode, setProgressiveMode and
   * setCompressionMode to disable feature.
   */
  public static final int MODE_DISABLED = 0;

  /**
   * Can be passed to setTilingMode, setProgressiveMode and
   * setCompressionMode to enable feature.
   */
  public static final int MODE_DEFAULT = 1;

  /**
   * Can be passed to setTilingMode, setCompressionMode to disable feature.
   */
  public static final int MODE_EXPLICIT = 2;

  /**
   * Can be passed to setTilingMode, setProgressiveMode and
   * setCompressionMode to enable feature.
   */
  public static final int MODE_COPY_FROM_METADATA = 3;

  /**
   * True if tiling grid offset parameters can be set.
   */
  protected boolean canOffsetTiles;

  /**
   * True if this writer can write images using compression.
   */
  protected boolean canWriteCompressed;

  /**
   * True if images can be written as a progressive sequence
   * of increasing quality.
   */
  protected boolean canWriteProgressive;

  /**
   * True if tile width and height parameters can be set.
   */
  protected boolean canWriteTiles;

  /**
   * Controls compression settings, which must be set to one of the four
   * MODE_* values.
   */
  protected int compressionMode = MODE_COPY_FROM_METADATA;

  /**
   * Contains the current compression quality setting.
   */
  protected float compressionQuality;

  /**
   * Contains the name of the current compression type.
   */
  protected String compressionType;

  /**
   * Array of the names of the available compression types.
   */
  protected String[] compressionTypes;

  /**
   * Localizes compression type names and quality descriptions,
   * or null to use default Locale.
   */
  protected Locale locale;

  /**
   * Preferred tile size range pairs.
   */
  protected Dimension[] preferredTileSizes;

  /**
   * The mode controlling progressive encoding, which must
   * be set to one of the four MODE_* values, except
   * MODE_EXPLICIT.
   */
  protected int progressiveMode = MODE_COPY_FROM_METADATA;

  /**
   * The amount by which the tile grid origin should be offset
   * horizontally from the image origin if tiling has been set.
   */
  protected int tileGridXOffset;

  /**
   * The amount by which the tile grid origin should be offset
   * vertically from the image origin if tiling has been set.
   */
  protected int tileGridYOffset;

  /**
   * The height of each tile if tiling has been set.
   */
  protected int tileHeight;

  /**
   * The width of each tile if tiling has been set.
   */
  protected int tileWidth;

  /**
   * The mode controlling tiling settings, which must be
   * set to one of the four MODE_* values.
   */
  protected int tilingMode;

  /**
   * True if the tiling parameters have been specified.
   */
  protected boolean tilingSet;

  /**
   * Creates an empty <code>ImageWriteParam</code> object.
   * The subclass is responsible to initialize all fields.
   */
  protected ImageWriteParam()
  {
    // Do nothing here.
  }

  /**
   * Creates an <code>ImageWriteParam</code> object with the given locale.
   *
   * @param locale the locale to use for user visible strings
   */
  public ImageWriteParam(Locale locale)
  {
    this.locale = locale;
  }

  public float getBitRate(float quality)
  {
    checkNotExplicitCompression();
    checkCompressionTypesSet();

    return -1.0f;
  }

  private void checkSupportsCompression()
  {
    if (! canWriteCompressed())
      throw new UnsupportedOperationException("compression not supported");
  }

  private void checkNotExplicitCompression()
  {
    if (getCompressionMode() != MODE_EXPLICIT)
      throw new IllegalStateException("compression mode is not MODE_EXPLICIT");
  }

  private void checkCompressionTypesSet()
  {
    if (getCompressionType() == null
        && getCompressionTypes() != null)
      throw new IllegalStateException("no compression type set");
  }

  private void checkSupportsProgressiveEncoding()
  {
    if (! canWriteProgressive())
      throw new UnsupportedOperationException
        ("progressive output not supported");
  }

  private void checkSupportsTiling()
  {
    if (! canWriteTiles())
      throw new UnsupportedOperationException("tiling not supported");
  }

  private void checkNotExplicitTiling()
  {
    if (getTilingMode() != MODE_EXPLICIT)
      throw new IllegalStateException("tiling mode not MODE_EXPLICIT");
  }

  private void checkTilingInitialized()
  {
    if (! tilingSet)
      throw new IllegalStateException("tiling parameters not set");
  }

  private void checkMode(int mode)
  {
    if (mode < MODE_DISABLED || mode > MODE_COPY_FROM_METADATA)
      throw new IllegalArgumentException("mode not supported");
  }

  public boolean canOffsetTiles()
  {
    return canOffsetTiles;
  }

  public boolean canWriteCompressed()
  {
    return canWriteCompressed;
  }

  public boolean canWriteProgressive()
  {
    return canWriteProgressive;
  }

  public boolean canWriteTiles()
  {
    return canWriteTiles;
  }

  public int getCompressionMode()
  {
    checkSupportsCompression();

    return compressionMode;
  }

  public float getCompressionQuality()
  {
    checkNotExplicitCompression();
    checkCompressionTypesSet();

    return compressionQuality;
  }

  public String[] getCompressionQualityDescriptions()
  {
    checkNotExplicitCompression();
    checkCompressionTypesSet();

    return null;
  }

  public float[] getCompressionQualityValues()
  {
    checkNotExplicitCompression();
    checkCompressionTypesSet();

    return null;
  }

  public String getCompressionType()
  {
    checkNotExplicitCompression();

    return compressionType;
  }

  public String[] getCompressionTypes()
  {
    checkSupportsCompression();

    return compressionTypes != null ? (String[]) compressionTypes.clone() : null;
  }

  public Locale getLocale()
  {
    return locale;
  }

  public String getLocalizedCompressionTypeName()
  {
    checkNotExplicitCompression();
    checkCompressionTypesSet();

    return getCompressionType();
  }

  public Dimension[] getPreferredTileSizes()
  {
    checkSupportsTiling();

    return preferredTileSizes;
  }

  public int getProgressiveMode()
  {
    checkSupportsProgressiveEncoding();

    return progressiveMode;
  }

  public int getTileGridXOffset()
  {
    checkNotExplicitTiling();
    checkTilingInitialized();

    return tileGridXOffset;
  }

  public int getTileGridYOffset()
  {
    checkNotExplicitTiling();
    checkTilingInitialized();

    return tileGridYOffset;
  }

  public int getTileHeight()
  {
    checkNotExplicitTiling();
    checkTilingInitialized();

    return tileHeight;
  }

  public int getTileWidth()
  {
    checkNotExplicitTiling();
    checkTilingInitialized();

    return tileWidth;
  }

  public int getTilingMode()
  {
    checkSupportsTiling();

    return tilingMode;
  }

  public boolean isCompressionLossless()
  {
    checkNotExplicitCompression();
    checkCompressionTypesSet();

    return true;
  }

  public void setCompressionMode(int mode)
  {
    checkSupportsCompression();
    checkMode(mode);

    compressionMode = mode;

    if (mode == MODE_EXPLICIT)
      unsetCompression();
  }

  public void setCompressionQuality(float quality)
  {
    checkNotExplicitCompression();
    checkCompressionTypesSet();

    if (quality < 0.0f || quality > 1.0f)
      throw new IllegalArgumentException("quality out of range");

    compressionQuality = quality;
  }

  public void setCompressionType(String compressionType)
  {
    checkNotExplicitCompression();

    String[] types = getCompressionTypes();

    if (types == null)
      throw new UnsupportedOperationException("no settable compression types");

    if (compressionType == null)
      this.compressionType = null;

    for (int i = types.length - 1; i >= 0; --i)
      if (types[i].equals(compressionType))
        {
          this.compressionType = compressionType;
          return;
        }

    throw new IllegalArgumentException("unknown compression type");
  }

  public void setProgressiveMode(int mode)
  {
    checkSupportsProgressiveEncoding();
    checkMode(mode);

    progressiveMode = mode;
  }

  public void setTiling(int tileWidth, int tileHeight,
                        int tileGridXOffset, int tileGridYOffset)
  {
    checkNotExplicitTiling();

    if (! canOffsetTiles
        && tileGridXOffset != 0
        && tileGridYOffset != 0)
      throw new UnsupportedOperationException("tile offsets not supported");

    if (tileWidth < 0 || tileHeight < 0)
      throw new IllegalArgumentException("negative tile dimension");

    if (preferredTileSizes != null)
      {
        boolean found = false;

        for (int i = 0; i < preferredTileSizes.length; i += 2)
          {
            if (tileWidth >= preferredTileSizes[i].width
                && tileWidth <= preferredTileSizes[i + 1].width
                && tileHeight >= preferredTileSizes[i].height
                && tileHeight <= preferredTileSizes[i + 1].height)
              found = true;
          }

        if (! found)
          throw new IllegalArgumentException("illegal tile size");
      }

    this.tilingSet = true;
    this.tileWidth = tileWidth;
    this.tileHeight = tileHeight;
    this.tileGridXOffset = tileGridXOffset;
    this.tileGridYOffset = tileGridYOffset;
  }

  public void setTilingMode(int mode)
  {
    checkSupportsTiling();
    checkMode(mode);
    tilingMode = mode;
  }

  public void unsetCompression()
  {
    checkNotExplicitCompression();

    compressionType = null;
    compressionQuality = 1.0F;
  }

  public void unsetTiling()
  {
    checkNotExplicitTiling();

    tileWidth = 0;
    tileHeight = 0;
    tileGridXOffset = 0;
    tileGridYOffset = 0;
  }
}
