/* DefaultContentHandlerFactory.java
   Copyright (C) 2004 Free Software Foundation, Inc.

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


package gnu.java.net;

import java.io.IOException;
import java.net.ContentHandler;
import java.net.ContentHandlerFactory;
import java.net.URLConnection;
import java.util.Arrays;
import java.util.HashSet;

/** Content Handler for Image types, using the AWT Toolkit's image decoder. */
class ImageHandler extends ContentHandler
{
  static ImageHandler instance = new ImageHandler();
  
  public Object getContent(URLConnection urlc) throws IOException
  {
    // FIXME: implement using ImageIO
    // ClasspathToolkit tk = (ClasspathToolkit) Toolkit.getDefaultToolkit();
    // java.awt.image.ImageProducer ip = tk.createImageProducer(urlc.getURL());
    // return ip;
    return null;
  }
}

/**
 */
public class DefaultContentHandlerFactory implements ContentHandlerFactory
{
  /** For now, hard code the list of types that we assume should
   *  be supported by the Toolkit. ClasspathToolkit should perhaps provide
   *  an API to express what Image MIME types the Toolkit understands.
   */
  private static String[] known_image_types =
    {
      "image/bmp",
      "image/gif",
      "image/jpeg",
      "image/png",
      "image/tiff",
      "image/x-portable-anymap",
      "image/x-cmu-raster",
      "image/x-xbitmap",
      "image/x-xpixmap"
    };
   
  private static HashSet<String> imageTypes
    = new HashSet<String>(Arrays.asList(known_image_types));

  public ContentHandler createContentHandler(String mimeType)
  {
    if (imageTypes.contains(mimeType))
      return ImageHandler.instance;
    // Currently, only image types are handled.
    return null;
  }
}
