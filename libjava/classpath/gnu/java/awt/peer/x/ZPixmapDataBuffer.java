package gnu.java.awt.peer.x;

import gnu.x11.Display;
import gnu.x11.image.ZPixmap;

import java.awt.GraphicsEnvironment;
import java.awt.image.DataBuffer;

/**
 * A DataBuffer implementation that is based on a ZPixmap. This is used
 * as backing store for BufferedImages.
 */
class ZPixmapDataBuffer
  extends DataBuffer
{

  /**
   * The backing ZPixmap.
   */
  private ZPixmap zpixmap;

  /**
   * Creates a new ZPixmapDataBuffer with a specified width and height.
   *
   * @param d the X display
   * @param w the width
   * @param h the height
   */
  ZPixmapDataBuffer(int w, int h)
  {
    super(TYPE_BYTE, w * h * 3); // TODO: Support non-24-bit-resolutions.
    GraphicsEnvironment env =
      GraphicsEnvironment.getLocalGraphicsEnvironment();
    XGraphicsDevice dev = (XGraphicsDevice) env.getDefaultScreenDevice();
    Display d = dev.getDisplay();
    zpixmap = new ZPixmap(d, w, h, d.default_pixmap_format);
  }

  /**
   * Creates a ZPixmapDataBuffer from an existing ZPixmap.
   *
   * @param zpixmap the ZPixmap to wrap
   */
  ZPixmapDataBuffer(ZPixmap zpixmap)
  {
    super(TYPE_BYTE, zpixmap.get_data_length());
    this.zpixmap = zpixmap;
  }

  @Override
  public int getElem(int bank, int i)
  {
    return 0xff & zpixmap.get_data_element(i);
  }

  @Override
  public void setElem(int bank, int i, int val)
  {
    zpixmap.set_data_element(i, (byte) val);
  }

  ZPixmap getZPixmap()
  {
    return zpixmap;
  }

}
