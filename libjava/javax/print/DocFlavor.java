/* DocFlavor.java --
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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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


package javax.print;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

/**
 * @author Michael Koch (konqueror@gmx.de)
 */
public class DocFlavor implements Cloneable, Serializable
{
  /**
   * @author Michael Koch (konqueror@gmx.de)
   */
  public static class BYTE_ARRAY
    extends DocFlavor
  {
    public static final BYTE_ARRAY AUTOSENSE = new BYTE_ARRAY("application/octet-stream");
    public static final BYTE_ARRAY GIF = new BYTE_ARRAY("image/gif");
    public static final BYTE_ARRAY JPEG = new BYTE_ARRAY("image/jpeg");
    public static final BYTE_ARRAY PCL = new BYTE_ARRAY("application/vnd.hp-PCL");
    public static final BYTE_ARRAY PDF = new BYTE_ARRAY("application/pdf");
    public static final BYTE_ARRAY PNG = new BYTE_ARRAY("image/png");
    public static final BYTE_ARRAY POSTSCRIPT = new BYTE_ARRAY("application/postscript");
    public static final BYTE_ARRAY TEXT_HTML_HOST = new BYTE_ARRAY("text/html");
    public static final BYTE_ARRAY TEXT_HTML_US_ASCII = new BYTE_ARRAY("text/html; charset=us-ascii");
    public static final BYTE_ARRAY TEXT_HTML_UTF_16 = new BYTE_ARRAY("text/html; charset=utf-16");
    public static final BYTE_ARRAY TEXT_HTML_UTF_16BE = new BYTE_ARRAY("text/html; charset=utf-16be");
    public static final BYTE_ARRAY TEXT_HTML_UTF_16LE = new BYTE_ARRAY("text/html; charset=utf-16le");
    public static final BYTE_ARRAY TEXT_HTML_UTF_8 = new BYTE_ARRAY("text/html; charset=utf-8");
    public static final BYTE_ARRAY TEXT_PLAIN_HOST = new BYTE_ARRAY("text/plain");
    public static final BYTE_ARRAY TEXT_PLAIN_US_ASCII = new BYTE_ARRAY("text/plain; charset=us-ascii");
    public static final BYTE_ARRAY TEXT_PLAIN_UTF_16 = new BYTE_ARRAY("text/plain; charset=utf-16");
    public static final BYTE_ARRAY TEXT_PLAIN_UTF_16BE = new BYTE_ARRAY("text/plain; charset=utf-16be");
    public static final BYTE_ARRAY TEXT_PLAIN_UTF_16LE = new BYTE_ARRAY("text/plain; charset=utf-16le");
    public static final BYTE_ARRAY TEXT_PLAIN_UTF_8 = new BYTE_ARRAY("text/plain; charset=utf-8");
    
    public BYTE_ARRAY(String mimeType)
    {
      super(mimeType, "[B");
    }
  }
  
  /**
   * @author Michael Koch (konqueror@gmx.de)
   */
  public static class CHAR_ARRAY
    extends DocFlavor
  {
    private static final long serialVersionUID = -8720590903724405128L;
    
    public static final DocFlavor.CHAR_ARRAY TEXT_HTML = new CHAR_ARRAY("text/html; charset=utf-16");
    public static final DocFlavor.CHAR_ARRAY TEXT_PLAIN = new CHAR_ARRAY("text/plain; charset=utf-16");

    public CHAR_ARRAY(String mimeType)
    {
      super(mimeType, "[C");
    }
  }
  
  /**
   * @author Michael Koch (konqueror@gmx.de)
   */
  public static class INPUT_STREAM
    extends DocFlavor
  {
    public static final INPUT_STREAM AUTOSENSE = new INPUT_STREAM("application/octet-stream");
    public static final INPUT_STREAM GIF = new INPUT_STREAM("image/gif");
    public static final INPUT_STREAM JPEG = new INPUT_STREAM("image/jpeg");
    public static final INPUT_STREAM PCL = new INPUT_STREAM("application/vnd.hp-PCL");
    public static final INPUT_STREAM PDF = new INPUT_STREAM("application/pdf");
    public static final INPUT_STREAM PNG = new INPUT_STREAM("image/png");
    public static final INPUT_STREAM POSTSCRIPT = new INPUT_STREAM("application/postscript");
    public static final INPUT_STREAM TEXT_HTML_HOST = new INPUT_STREAM("text/html");
    public static final INPUT_STREAM TEXT_HTML_US_ASCII = new INPUT_STREAM("text/html; charset=us-ascii");
    public static final INPUT_STREAM TEXT_HTML_UTF_16 = new INPUT_STREAM("text/html; charset=utf-16");
    public static final INPUT_STREAM TEXT_HTML_UTF_16BE = new INPUT_STREAM("text/html; charset=utf-16be");
    public static final INPUT_STREAM TEXT_HTML_UTF_16LE = new INPUT_STREAM("text/html; charset=utf-16le");
    public static final INPUT_STREAM TEXT_HTML_UTF_8 = new INPUT_STREAM("text/html; charset=utf-8");
    public static final INPUT_STREAM TEXT_PLAIN_HOST = new INPUT_STREAM("text/plain");
    public static final INPUT_STREAM TEXT_PLAIN_US_ASCII = new INPUT_STREAM("text/plain; charset=us-ascii");
    public static final INPUT_STREAM TEXT_PLAIN_UTF_16 = new INPUT_STREAM("text/plain; charset=utf-16");
    public static final INPUT_STREAM TEXT_PLAIN_UTF_16BE = new INPUT_STREAM("text/plain; charset=utf-16be");
    public static final INPUT_STREAM TEXT_PLAIN_UTF_16LE = new INPUT_STREAM("text/plain; charset=utf-16le");
    public static final INPUT_STREAM TEXT_PLAIN_UTF_8 = new INPUT_STREAM("text/plain; charset=utf-8");
    
    public INPUT_STREAM(String mimeType)
    {
      super(mimeType, "java.io.InputStream");
    }
  }
  
  /**
   * @author Michael Koch (konqueror@gmx.de)
   */
  public static class READER
    extends DocFlavor
  {
    private static final long serialVersionUID = 7100295812579351567L;

    public static final DocFlavor.READER TEXT_HTML = new READER("text/html; charset=utf-16");
    public static final DocFlavor.READER TEXT_PLAIN = new READER("text/plain; charset=utf-16");
    
    public READER(String mimeType)
    {
      super(mimeType, "java.io.Reader");
    }
  }
  
  /**
   * @author Michael Koch (konqueror@gmx.de)
   */
  public static class SERVICE_FORMATTED
    extends DocFlavor
  {
    private static final long serialVersionUID = 6181337766266637256L;

    public static final DocFlavor.SERVICE_FORMATTED PAGEABLE = new SERVICE_FORMATTED("java.awt.print.Pageable");
    public static final DocFlavor.SERVICE_FORMATTED PRINTABLE = new SERVICE_FORMATTED("java.awt.print.Printable");
    public static final DocFlavor.SERVICE_FORMATTED RENDERABLE_IMAGE = new SERVICE_FORMATTED("java.awt.image.renderable.RenderableImage");
    
    public SERVICE_FORMATTED(String className)
    {
      super("application/x-java-jvm-local-objectref", className);
    }
  }
  
  /**
   * @author Michael Koch (konqueror@gmx.de)
   */
  public static class STRING
    extends DocFlavor
  {
    private static final long serialVersionUID = 4414407504887034035L;

    public static final DocFlavor.STRING TEXT_HTML = new STRING("text/html; charset=utf-16");
    public static final DocFlavor.STRING TEXT_PLAIN = new STRING("text/plain; charset=utf-16");
    
    public STRING(String mimeType)
    {
      super(mimeType, "java.lang.String");
    }
  }
  
  /**
   * @author Michael Koch (konqueror@gmx.de)
   */
  public static class URL
    extends DocFlavor
  {
    private static final long serialVersionUID = 2936725788144902062L;

    public static final DocFlavor.URL AUTOSENSE = new URL("application/octet-stream");
    public static final DocFlavor.URL GIF = new URL("image/gif");
    public static final DocFlavor.URL JPEG = new URL("image/jpeg");
    public static final DocFlavor.URL PCL = new URL("application/vnd.hp-PCL");
    public static final DocFlavor.URL PDF = new URL("application/pdf");
    public static final DocFlavor.URL PNG = new URL("image/png");
    public static final DocFlavor.URL POSTSCRIPT = new URL("application/postscript");
    public static final DocFlavor.URL TEXT_HTML_HOST = new URL("text/html");
    public static final DocFlavor.URL TEXT_HTML_US_ASCII = new URL("text/html; charset=us-ascii");
    public static final DocFlavor.URL TEXT_HTML_UTF_16 = new URL("text/html; charset=utf-16");
    public static final DocFlavor.URL TEXT_HTML_UTF_16BE = new URL("text/html; charset=utf-16be");
    public static final DocFlavor.URL TEXT_HTML_UTF_16LE = new URL("text/html; charset=utf-16le");
    public static final DocFlavor.URL TEXT_HTML_UTF_8 = new URL("text/html; charset=utf-8");
    public static final DocFlavor.URL TEXT_PLAIN_HOST = new URL("text/plain");
    public static final DocFlavor.URL TEXT_PLAIN_US_ASCII = new URL("text/plain; charset=us-ascii");
    public static final DocFlavor.URL TEXT_PLAIN_UTF_16 = new URL("text/plain; charset=utf-16");
    public static final DocFlavor.URL TEXT_PLAIN_UTF_16BE = new URL("text/plain; charset=utf-16be");
    public static final DocFlavor.URL TEXT_PLAIN_UTF_16LE = new URL("text/plain; charset=utf-16le");
    public static final DocFlavor.URL TEXT_PLAIN_UTF_8 = new URL("text/plain; charset=utf-8");
    
    public URL(String mimeType)
    {
      super(mimeType, "java.net.URL");
    }
  }
  
  private static final long serialVersionUID = -4512080796965449721L;
  
  // FIXME: Get the host encoding from somewhere. Note that the new String is to make
  // sure the field won't be a compile time constant.
  public static final String hostEncoding = new String("US-ASCII");

  private String mediaSubtype;
  private String mediaType;
  private String className;
  private HashMap params = new HashMap();
  
  public DocFlavor(String mimeType, String className)
  {
    if (mimeType == null || className == null)
      throw new NullPointerException();

    parseMimeType(mimeType);
    this.className = className;
  }

  private void parseMimeType(String mimeType)
  {
    // FIXME: This method is know to be not completely correct, but it works for now.
    
    int pos = mimeType.indexOf(';');

    if (pos != -1)
      {
	String tmp = mimeType.substring(pos + 2);
	mimeType = mimeType.substring(0, pos);
	pos = tmp.indexOf('=');
	params.put(tmp.substring(0, pos), tmp.substring(pos + 1));
      }

    pos = mimeType.indexOf('/');

    if (pos == -1)
      throw new IllegalArgumentException();

    mediaType = mimeType.substring(0, pos);
    mediaSubtype = mimeType.substring(pos + 1);
  }
  
  public boolean equals(Object obj)
  {
    if (! (obj instanceof DocFlavor))
      return false;

    DocFlavor tmp = (DocFlavor) obj;

    return (getMimeType().equals(tmp.getMimeType())
	    && getRepresentationClassName().equals(tmp.getRepresentationClassName()));
  }

  public String getMediaSubtype()
  {
    return mediaSubtype;
  }

  public String getMediaType()
  {
    return mediaType;
  }

  public String getMimeType()
  {
    // FIXME: Check if this algorithm is correct.
    
    String mimeType = getMediaType() + "/" + getMediaSubtype();
    Iterator it = params.entrySet().iterator();

    while (it.hasNext())
      {
	Map.Entry entry = (Map.Entry) it.next();
	mimeType += "; " + entry.getKey() + "=\"" + entry.getValue() + "\"";
      }

    return mimeType;
  }

  public String getParameter(String paramName)
  {
    if (paramName == null)
      throw new NullPointerException();
    
    return (String) params.get(paramName); 
  }

  public String getRepresentationClassName()
  {
    return className;
  }

  public int hashCode()
  {
    return ((mediaType.hashCode()
	     * mediaSubtype.hashCode()
	     * className.hashCode()) ^ params.hashCode());
  }

  public String toString()
  {
    return getMimeType();
  }
}
