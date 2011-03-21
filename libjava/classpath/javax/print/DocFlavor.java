/* DocFlavor.java --
   Copyright (C) 2004, 2006 Free Software Foundation, Inc.

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


package javax.print;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.Serializable;
import java.io.StreamTokenizer;
import java.io.StringReader;
import java.nio.charset.Charset;
import java.util.Iterator;
import java.util.Map;
import java.util.TreeMap;

/**
 * <code>DocFlavor</code> provides a description of the format in which the
 * print data will be supplied in a print job to the print service.
 * <p>
 * A doc flavor consists of two parts:
 * <ul>
 * <li>
 * The MIME type (Multipurpose Internet Mail Extensions types as described
 * in RFC 2045/2046) specifying the media format of the print data.
 * </li><li>
 * The representation class name which is the fully qualified name of the
 * class providing the print data to the print job. For example if the print
 * data is supplied as a byte array the representation class name will be
 * <code>"[B"</code> or for an input stream <code>"java.io.InputStream"</code>.
 * </li>
 * </ul>
 * The <code>DocFlavor</code> class is therefore used in several places in the
 * Java Print Service API. A print service provides its supported document
 * flavors as an array of DocFlavor objects and a print job gets the flavor of
 * its data to print from the <code>Doc</code> object provided as a DocFlavor
 * instance.
 * </p>
 * <p>
 * It has to be differentiated between <b>client formatted</b> and <b>service
 * formatted</b> print data. Client formatted print data is already provided
 * formatted by the client e.g. in an image format or as postscript. For
 * service formatted print data, the Java Print Service instance produces
 * the formatted print data. Here the doc flavor's representation class name
 * does specify an interface instead of the actual print data source. The
 * print service will call the methods of the given implementation of this
 * interface with a special Graphics object capable of producing formatted
 * print data from the graphics routines inside the interface methods.
 * </p>
 * <p>
 * <h3>Client formatted print data document flavors</h3>
 * The print service uses the representation class of the doc flavor to know
 * how to retrieve the print data. If the representation class is a
 * <code>URL</code> it will open the URL to read the print data from it. If it is
 * a <code>byte[]</code> it will directly use the array and send it to the
 * printer. There are predefined doc flavor as inner class for the most common
 * representation class types:
 * <ul>
 * <li>Character arrays (<code>char[]</code>): The characters of the array
 * represent the print data.</li>
 * <li>Character streams (<code>java.io.Reader</code>): The whole characters
 * read from the stream represent the print data.</li>
 * <li>String (<code>java.lang.String</code>): The characters of the String
 * represent the print data.</li>
 * <li>Byte arrays (<code>byte[]</code>): The bytes of the array represent the
 * print data. Encoding if text content is given in the mime type.</li>
 * <li>Byte streams (<code>java.io.InputStream</code>): The whole bytes read
 * from the stream represent the print data. If text content the encoding is
 * specified in the mime type.</li>
 * <li>Uniform Resource Locator (<code>java.net.URL</code>): The bytes read
 * from the stream through opening of the URL represent the print data.
 * If text content the encoding is specified in the mime type.</li></li>
 * </ul>
 * </p>
 * <p>
 * <h3>Service formatted print data document flavors</h3>
 * The print service uses the provided object implementing the interface
 * specified by the representation class to produce the formatted print data.
 * The mime type of service formatted data is always
 * <code>"application/x-java-jvm-local-objectref"</code> to signal the local
 * reference to the print data object implementing the interface. Predefined
 * doc flavor classes exist as an inner class for the three available interface
 * to produce print data:
 * <ul>
 * <li>Pageable object (<code>java.awt.print.Pageable</code>): A pageable object
 * is supplied to the print service. The print service will call the methods of
 * the interface with a Grahics object to produce the formatted print data.</li>
 * <li>Printable object (<code>java.awt.print.Printable</code>): A printable object
 * is supplied to the print service. The print service will call the methods of
 * the interface with a Grahics object to produce the formatted print data.</li>
 * <li>Renderable Image object
 * (<code>java.awt.image.renderable.RenderableImage</code>): A renderable image
 * object is supplied to the print service. The print service calls methods of
 * this interface to obtain the image to be printed.</li>
 * </ul>
 * </p>
 *
 * @author Michael Koch (konqueror@gmx.de)
 * @author Wolfgang Baer (WBaer@gmx.de)
 */
public class DocFlavor implements Cloneable, Serializable
{
  /**
   * Predefined static <code>DocFlavor</code> objects for document
   * types which use a byte array for the print data representation.
   * <p>All the defined doc flavors have a print data representation
   * classname of "[B" (byte array).</p>
   *
   * @author Michael Koch (konqueror@gmx.de)
   */
  public static class BYTE_ARRAY
    extends DocFlavor
  {
    private static final long serialVersionUID = -9065578006593857475L;

    /**
     * Byte array doc flavor with a MIME Type of "application/octet-stream".
     */
    public static final BYTE_ARRAY AUTOSENSE = new BYTE_ARRAY("application/octet-stream");
    /**
     * Byte array doc flavor with a MIME Type of "image/gif".
     */
    public static final BYTE_ARRAY GIF = new BYTE_ARRAY("image/gif");
    /**
     * Byte array doc flavor with a MIME Type of "image/jpeg".
     */
    public static final BYTE_ARRAY JPEG = new BYTE_ARRAY("image/jpeg");
    /**
     * Byte array doc flavor with a MIME Type of "application/vnd.hp-PCL".
     */
    public static final BYTE_ARRAY PCL = new BYTE_ARRAY("application/vnd.hp-PCL");
    /**
     * Byte array doc flavor with a MIME Type of "application/pdf".
     */
    public static final BYTE_ARRAY PDF = new BYTE_ARRAY("application/pdf");
    /**
     * Byte array doc flavor with a MIME Type of "image/png".
     */
    public static final BYTE_ARRAY PNG = new BYTE_ARRAY("image/png");
    /**
     * Byte array doc flavor with a MIME Type of "application/postscript".
     */
    public static final BYTE_ARRAY POSTSCRIPT = new BYTE_ARRAY("application/postscript");
    /**
     * Byte array doc flavor with a MIME Type of "text/html" in the host encoding.
     */
    public static final BYTE_ARRAY TEXT_HTML_HOST = new BYTE_ARRAY("text/html; charset=" + hostEncoding);
    /**
     * Byte array doc flavor with a MIME Type of "text/html; charset=us-ascii".
     */
    public static final BYTE_ARRAY TEXT_HTML_US_ASCII = new BYTE_ARRAY("text/html; charset=us-ascii");
    /**
     * Byte array doc flavor with a MIME Type of "text/html; charset=utf-16".
     */
    public static final BYTE_ARRAY TEXT_HTML_UTF_16 = new BYTE_ARRAY("text/html; charset=utf-16");
    /**
     * Byte array doc flavor with a MIME Type of "text/html; charset=utf-16be".
     */
    public static final BYTE_ARRAY TEXT_HTML_UTF_16BE = new BYTE_ARRAY("text/html; charset=utf-16be");
    /**
     * Byte array doc flavor with a MIME Type of "text/html; charset=utf-16le".
     */
    public static final BYTE_ARRAY TEXT_HTML_UTF_16LE = new BYTE_ARRAY("text/html; charset=utf-16le");
    /**
     * Byte array doc flavor with a MIME Type of "text/html; charset=utf-8".
     */
    public static final BYTE_ARRAY TEXT_HTML_UTF_8 = new BYTE_ARRAY("text/html; charset=utf-8");
    /**
     * Byte array doc flavor with a MIME Type of "text/plain" in the host encoding.
     */
    public static final BYTE_ARRAY TEXT_PLAIN_HOST = new BYTE_ARRAY("text/plain; charset=" + hostEncoding);
    /**
     * Byte array doc flavor with a MIME Type of "text/plain; charset=us-ascii".
     */
    public static final BYTE_ARRAY TEXT_PLAIN_US_ASCII = new BYTE_ARRAY("text/plain; charset=us-ascii");
    /**
     * Byte array doc flavor with a MIME Type of "text/plain; charset=utf-16".
     */
    public static final BYTE_ARRAY TEXT_PLAIN_UTF_16 = new BYTE_ARRAY("text/plain; charset=utf-16");
    /**
     * Byte array doc flavor with a MIME Type of "text/plain; charset=utf-16be".
     */
    public static final BYTE_ARRAY TEXT_PLAIN_UTF_16BE = new BYTE_ARRAY("text/plain; charset=utf-16be");
    /**
     * Byte array doc flavor with a MIME Type of "text/plain; charset=utf-16le".
     */
    public static final BYTE_ARRAY TEXT_PLAIN_UTF_16LE = new BYTE_ARRAY("text/plain; charset=utf-16le");
    /**
     * Byte array doc flavor with a MIME Type of "text/plain; charset=utf-8".
     */
    public static final BYTE_ARRAY TEXT_PLAIN_UTF_8 = new BYTE_ARRAY("text/plain; charset=utf-8");

    /**
     * Constructor for doc flavor objects with the given MIME type
     * and a print data representation class name of "[B".
     *
     * @param mimeType the mime type string
     *
     * @throws NullPointerException if mimeType is <code>null</code>.
     * @throws IllegalArgumentException if mimeType has the wrong syntax.
     */
    public BYTE_ARRAY(String mimeType)
    {
      super(mimeType, "[B");
    }
  }

  /**
   * Predefined static <code>DocFlavor</code> objects for document
   * types which use a char array for the print data representation.
   * <p>All the defined doc flavors have a print data representation
   * classname of "[C" (char array).</p>
   *
   * @author Michael Koch (konqueror@gmx.de)
   */
  public static class CHAR_ARRAY
    extends DocFlavor
  {
    private static final long serialVersionUID = -8720590903724405128L;

    /**
     * Char array doc flavor with a MIME Type of "text/html; charset=utf-16".
     */
    public static final DocFlavor.CHAR_ARRAY TEXT_HTML = new CHAR_ARRAY("text/html; charset=utf-16");
    /**
     * Char array doc flavor with a MIME Type of "text/plain; charset=utf-16".
     */
    public static final DocFlavor.CHAR_ARRAY TEXT_PLAIN = new CHAR_ARRAY("text/plain; charset=utf-16");

    /**
     * Constructor for doc flavor objects with the given MIME type
     * and a print data representation class name of "[C".
     *
     * @param mimeType the mime type string
     *
     * @throws NullPointerException if mimeType is <code>null</code>.
     * @throws IllegalArgumentException if mimeType has the wrong syntax.
     */
    public CHAR_ARRAY(String mimeType)
    {
      super(mimeType, "[C");
    }
  }

  /**
   * Predefined static <code>DocFlavor</code> objects for document
   * types which use an InputStream to retrieve the print data.
   * <p>All the defined doc flavors have a print data representation
   * classname of "java.io.InputStream".</p>
   *
   * @author Michael Koch (konqueror@gmx.de)
   */
  public static class INPUT_STREAM
    extends DocFlavor
  {
    private static final long serialVersionUID = -7045842700749194127L;

    /**
     * InputStream doc flavor with a MIME Type of "application/octet-stream".
     */
    public static final INPUT_STREAM AUTOSENSE = new INPUT_STREAM("application/octet-stream");
    /**
     * InputStream doc flavor with a MIME Type of "image/gif".
     */
    public static final INPUT_STREAM GIF = new INPUT_STREAM("image/gif");
    /**
     * InputStream doc flavor with a MIME Type of "image/jpeg".
     */
    public static final INPUT_STREAM JPEG = new INPUT_STREAM("image/jpeg");
    /**
     * InputStream doc flavor with a MIME Type of "application/vnd.hp-PCL".
     */
    public static final INPUT_STREAM PCL = new INPUT_STREAM("application/vnd.hp-PCL");
    /**
     * InputStream doc flavor with a MIME Type of "application/pdf".
     */
    public static final INPUT_STREAM PDF = new INPUT_STREAM("application/pdf");
    /**
     * InputStream doc flavor with a MIME Type of "image/png".
     */
    public static final INPUT_STREAM PNG = new INPUT_STREAM("image/png");
    /**
     * InputStream doc flavor with a MIME Type of "application/postscript".
     */
    public static final INPUT_STREAM POSTSCRIPT = new INPUT_STREAM("application/postscript");
    /**
     * InputStream doc flavor with a MIME Type of "text/html" in the host encoding.
     */
    public static final INPUT_STREAM TEXT_HTML_HOST = new INPUT_STREAM("text/html; charset=" + hostEncoding);
    /**
     * InputStream doc flavor with a MIME Type of "text/html; charset=us-ascii".
     */
    public static final INPUT_STREAM TEXT_HTML_US_ASCII = new INPUT_STREAM("text/html; charset=us-ascii");
    /**
     * InputStream doc flavor with a MIME Type of "text/html; charset=utf-16".
     */
    public static final INPUT_STREAM TEXT_HTML_UTF_16 = new INPUT_STREAM("text/html; charset=utf-16");
    /**
     * InputStream doc flavor with a MIME Type of "text/html; charset=utf-16be".
     */
    public static final INPUT_STREAM TEXT_HTML_UTF_16BE = new INPUT_STREAM("text/html; charset=utf-16be");
    /**
     * InputStream doc flavor with a MIME Type of "text/html; charset=utf-16le".
     */
    public static final INPUT_STREAM TEXT_HTML_UTF_16LE = new INPUT_STREAM("text/html; charset=utf-16le");
    /**
     * InputStream doc flavor with a MIME Type of "text/html; charset=utf-8".
     */
    public static final INPUT_STREAM TEXT_HTML_UTF_8 = new INPUT_STREAM("text/html; charset=utf-8");
    /**
     * InputStream doc flavor with a MIME Type of "text/plain" in the host encoding.
     */
    public static final INPUT_STREAM TEXT_PLAIN_HOST = new INPUT_STREAM("text/plain; charset=" + hostEncoding);
    /**
     * InputStream doc flavor with a MIME Type of "text/plain; charset=us-ascii".
     */
    public static final INPUT_STREAM TEXT_PLAIN_US_ASCII = new INPUT_STREAM("text/plain; charset=us-ascii");
    /**
     * InputStream doc flavor with a MIME Type of "text/plain; charset=utf-16".
     */
    public static final INPUT_STREAM TEXT_PLAIN_UTF_16 = new INPUT_STREAM("text/plain; charset=utf-16");
    /**
     * InputStream doc flavor with a MIME Type of "text/plain; charset=utf-16be".
     */
    public static final INPUT_STREAM TEXT_PLAIN_UTF_16BE = new INPUT_STREAM("text/plain; charset=utf-16be");
    /**
     * InputStream doc flavor with a MIME Type of "text/plain; charset=utf-16le".
     */
    public static final INPUT_STREAM TEXT_PLAIN_UTF_16LE = new INPUT_STREAM("text/plain; charset=utf-16le");
    /**
     * InputStream doc flavor with a MIME Type of "text/plain; charset=utf-8".
     */
    public static final INPUT_STREAM TEXT_PLAIN_UTF_8 = new INPUT_STREAM("text/plain; charset=utf-8");

    /**
     * Constructor for doc flavor objects with the given MIME type
     * and a print data representation class name of "java.io.InputStream".
     *
     * @param mimeType the mime type string
     *
     * @throws NullPointerException if mimeType is <code>null</code>.
     * @throws IllegalArgumentException if mimeType has the wrong syntax.
     */
    public INPUT_STREAM(String mimeType)
    {
      super(mimeType, "java.io.InputStream");
    }
  }

  /**
   * Predefined static <code>DocFlavor</code> objects for document
   * types which use an Reader to retrieve the print data.
   * <p>All the defined doc flavors have a print data representation
   * classname of "java.io.Reader".</p>
   *
   * @author Michael Koch (konqueror@gmx.de)
   */
  public static class READER
    extends DocFlavor
  {
    private static final long serialVersionUID = 7100295812579351567L;

    /**
     * Reader doc flavor with a MIME Type of "text/html; charset=utf-16".
     */
    public static final DocFlavor.READER TEXT_HTML = new READER("text/html; charset=utf-16");
    /**
     * Reader doc flavor with a MIME Type of "text/plain; charset=utf-16".
     */
    public static final DocFlavor.READER TEXT_PLAIN = new READER("text/plain; charset=utf-16");

    /**
     * Constructor for doc flavor objects with the given MIME type
     * and a print data representation class name of "java.io.Reader".
     *
     * @param mimeType the mime type string
     *
     * @throws NullPointerException if mimeType is <code>null</code>.
     * @throws IllegalArgumentException if mimeType has the wrong syntax.
     */
    public READER(String mimeType)
    {
      super(mimeType, "java.io.Reader");
    }
  }

  /**
   * Predefined static <code>DocFlavor</code> objects for document
   * types which use service formatted print data.
   * <p>All the defined doc flavors have a MIME type of
   * "application/x-java-jvm-local-objectref".</p>
   *
   * @author Michael Koch (konqueror@gmx.de)
   */
  public static class SERVICE_FORMATTED
    extends DocFlavor
  {
    private static final long serialVersionUID = 6181337766266637256L;

    /**
     * Service formatted doc flavor with a representation class of
     * "java.awt.print.Pageable".
     */
    public static final DocFlavor.SERVICE_FORMATTED PAGEABLE = new SERVICE_FORMATTED("java.awt.print.Pageable");
    /**
     * Service formatted doc flavor with a representation class of
     * "java.awt.print.Printable".
     */
    public static final DocFlavor.SERVICE_FORMATTED PRINTABLE = new SERVICE_FORMATTED("java.awt.print.Printable");
    /**
     * Service formatted doc flavor with a representation class of
     * "java.awt.image.renderable.RenderableImage".
     */
    public static final DocFlavor.SERVICE_FORMATTED RENDERABLE_IMAGE = new SERVICE_FORMATTED("java.awt.image.renderable.RenderableImage");

    /**
     * Constructor for doc flavor objects with a MIME type of
     * "application/x-java-jvm-local-objectref" and the given
     * print data representation classname.
     *
     * @param className the representation classname
     *
     * @throws NullPointerException if className is <code>null</code>.
     */
    public SERVICE_FORMATTED(String className)
    {
      super("application/x-java-jvm-local-objectref", className);
    }
  }

  /**
   * Predefined static <code>DocFlavor</code> objects for document
   * types which use a String for the print data representation.
   * <p>All the defined doc flavors have a print data representation
   * classname of "java.lang.String".</p>
   *
   * @author Michael Koch (konqueror@gmx.de)
   */
  public static class STRING
    extends DocFlavor
  {
    private static final long serialVersionUID = 4414407504887034035L;

    /**
     * String doc flavor with a MIME Type of "text/html; charset=utf-16".
     */
    public static final DocFlavor.STRING TEXT_HTML = new STRING("text/html; charset=utf-16");
    /**
     * String doc flavor with a MIME Type of "text/plain; charset=utf-16".
     */
    public static final DocFlavor.STRING TEXT_PLAIN = new STRING("text/plain; charset=utf-16");

    /**
     * Constructor for doc flavor objects with the given MIME type
     * and a print data representation class name of "java.lang.String".
     *
     * @param mimeType the mime type string
     *
     * @throws NullPointerException if mimeType is <code>null</code>.
     * @throws IllegalArgumentException if mimeType has the wrong syntax.
     */
    public STRING(String mimeType)
    {
      super(mimeType, "java.lang.String");
    }
  }

  /**
   * Predefined static <code>DocFlavor</code> objects for document
   * types which have an URL where to retrieve the print data.
   * <p>All the defined doc flavors have a print data representation
   * classname of "java.net.URL".</p>
   *
   * @author Michael Koch (konqueror@gmx.de)
   */
  public static class URL
    extends DocFlavor
  {
    private static final long serialVersionUID = 2936725788144902062L;

    /**
     * URL doc flavor with a MIME Type of "application/octet-stream".
     */
    public static final DocFlavor.URL AUTOSENSE = new URL("application/octet-stream");
    /**
     * URL doc flavor with a MIME Type of "image/gif".
     */
    public static final DocFlavor.URL GIF = new URL("image/gif");
    /**
     * URL doc flavor with a MIME Type of "image/jpeg".
     */
    public static final DocFlavor.URL JPEG = new URL("image/jpeg");
    /**
     * URL doc flavor with a MIME Type of "application/vnd.hp-PCL".
     */
    public static final DocFlavor.URL PCL = new URL("application/vnd.hp-PCL");
    /**
     * URL doc flavor with a MIME Type of "application/pdf".
     */
    public static final DocFlavor.URL PDF = new URL("application/pdf");
    /**
     * URL doc flavor with a MIME Type of "image/png".
     */
    public static final DocFlavor.URL PNG = new URL("image/png");
    /**
     * URL doc flavor with a MIME Type of "application/postscript".
     */
    public static final DocFlavor.URL POSTSCRIPT = new URL("application/postscript");
    /**
     * URL doc flavor with a MIME Type of "text/html" in the host encoding.
     */
    public static final DocFlavor.URL TEXT_HTML_HOST = new URL("text/html; charset=" + hostEncoding);
    /**
     * URL doc flavor with a MIME Type of "text/html; charset=us-ascii".
     */
    public static final DocFlavor.URL TEXT_HTML_US_ASCII = new URL("text/html; charset=us-ascii");
    /**
     * URL doc flavor with a MIME Type of "text/html; charset=utf-16".
     */
    public static final DocFlavor.URL TEXT_HTML_UTF_16 = new URL("text/html; charset=utf-16");
    /**
     * URL doc flavor with a MIME Type of "text/html; charset=utf-16be".
     */
    public static final DocFlavor.URL TEXT_HTML_UTF_16BE = new URL("text/html; charset=utf-16be");
    /**
     * URL doc flavor with a MIME Type of "text/html; charset=utf-16le".
     */
    public static final DocFlavor.URL TEXT_HTML_UTF_16LE = new URL("text/html; charset=utf-16le");
    /**
     * URL doc flavor with a MIME Type of "text/html; charset=utf-8".
     */
    public static final DocFlavor.URL TEXT_HTML_UTF_8 = new URL("text/html; charset=utf-8");
    /**
     * URL doc flavor with a MIME Type of "text/plain" in the host encoding.
     */
    public static final DocFlavor.URL TEXT_PLAIN_HOST = new URL("text/plain; charset=" + hostEncoding);
    /**
     * URL doc flavor with a MIME Type of "text/plain; charset=us-ascii".
     */
    public static final DocFlavor.URL TEXT_PLAIN_US_ASCII = new URL("text/plain; charset=us-ascii");
    /**
     * URL doc flavor with a MIME Type of "text/plain; charset=utf-16".
     */
    public static final DocFlavor.URL TEXT_PLAIN_UTF_16 = new URL("text/plain; charset=utf-16");
    /**
     * URL doc flavor with a MIME Type of "text/plain; charset=utf-16be".
     */
    public static final DocFlavor.URL TEXT_PLAIN_UTF_16BE = new URL("text/plain; charset=utf-16be");
    /**
     * URL doc flavor with a MIME Type of "text/plain; charset=utf-16le".
     */
    public static final DocFlavor.URL TEXT_PLAIN_UTF_16LE = new URL("text/plain; charset=utf-16le");
    /**
     * URL doc flavor with a MIME Type of "text/plain; charset=utf-8".
     */
    public static final DocFlavor.URL TEXT_PLAIN_UTF_8 = new URL("text/plain; charset=utf-8");

    /**
     * Constructor for doc flavor objects with the given MIME type
     * and a print data representation class name of "java.net.URL".
     *
     * @param mimeType the mime type string
     *
     * @throws NullPointerException if mimeType is <code>null</code>.
     * @throws IllegalArgumentException if mimeType has the wrong syntax.
     */
    public URL(String mimeType)
    {
      super(mimeType, "java.net.URL");
    }
  }

  private static final long serialVersionUID = -4512080796965449721L;

  /**
   * The string representing the host encoding. This is the encoding
   * used in the predefined HOST doc flavors
   * (e.g. {@link BYTE_ARRAY#TEXT_HTML_HOST}).
   */
  public static final String hostEncoding = Charset.defaultCharset().name();

  private transient String mediaSubtype;
  private transient String mediaType;
  private transient TreeMap params;

  // name as defined in Serialized Form JDK 1.4
  private String myClassName;

  /**
   * Constructs a <code>DocFlavor</code> object with the given MIME type and
   * representation class name.
   *
   * @param mimeType the MIME type string.
   * @param className the fully-qualified name of the representation class.
   *
   * @throws NullPointerException if mimeType or className are <code>null</code>.
   * @throws IllegalArgumentException if given mimeType has syntax errors.
   */
  public DocFlavor(String mimeType, String className)
  {
    if (mimeType == null || className == null)
      throw new NullPointerException();

    params = new TreeMap();
    parseMimeType(mimeType);

    myClassName = className;
  }

  /**
   * Parses the given string as MIME type.
   * The mediatype, mediasubtype and all parameter/value
   * combinations are extracted, comments are dropped.
   *
   * @param mimeType the string to parse
   * @throws IllegalArgumentException if not conformant.
   */
  private void parseMimeType(String mimeType)
  {
    int MEDIA = 1;
    int MEDIASUB = 2;
    int PARAM_NAME = 3;
    int PARAM_VALUE = 4;
    int COMMENT_START = 5;

    int state = 0;
    int lastState = 0; // keeps track of state before comment
    int tok;

    try
      {
        String paramName = null;
        StreamTokenizer in = new StreamTokenizer(new StringReader(mimeType));
        in.resetSyntax();
        // Allowed characters are anything except:
        // SPACE, CTLs (= Unicode characters U+0000 - U+001F and U+007F)
        // and tspecials ( ) < > @ , ; : \ " / [ ] ? =
        in.whitespaceChars(0x00, 0x20);
        in.whitespaceChars(0x7F, 0x7F);
        in.wordChars('A', 'Z');
        in.wordChars('a', 'z');
        in.wordChars('0', '9');
        in.wordChars(0xA0, 0xFF);
        in.wordChars(0x21, 0x21);
        in.wordChars(0x23, 0x27);
        in.wordChars(0x2A, 0x2B);
        in.wordChars(0x2D, 0x2E);
        in.wordChars(0x5E, 0x60);
        in.wordChars(0x7B, 0x7E);
        in.quoteChar('"');

        while ((tok = in.nextToken()) != StreamTokenizer.TT_EOF)
          {
            switch (tok)
              {
              case StreamTokenizer.TT_WORD:
                if (state == 0)
                  {
                    mediaType = in.sval.toLowerCase();
                    state = MEDIA;
                    break;
                  }
                if (state == MEDIA)
                  {
                    mediaSubtype = in.sval.toLowerCase();
                    state = MEDIASUB;
                    break;
                  }
                // begin of parameters is either after mediasub or a parameter value
                if (state == MEDIASUB || state == PARAM_VALUE)
                  {
                    paramName = in.sval.toLowerCase();
                    state = PARAM_NAME;
                    break;
                  }
                // a parameter always needs to follow a value
                if (state == PARAM_NAME)
                  {
                    String paramValue = in.sval;
                    // if a charset param the value needs to be stored lowercase
                    if (paramName.equals("charset"))
                      paramValue = paramValue.toLowerCase();

                    state = PARAM_VALUE;
                    params.put(paramName, paramValue);
                    break;
                  }
                if (state == COMMENT_START)
                  {
                    // ignore;
                    break;
                  }
                break;
              case '/':
                // may only occur after the mediatype
                if (state != MEDIA)
                  throw new IllegalArgumentException();

                break;
              case '=':
                // may only occur after a parameter
                if (state != PARAM_NAME)
                  throw new IllegalArgumentException();

                break;
              case ';':
                // differentiates mime type and parameters/value combinations
                if (state != MEDIASUB && state != PARAM_VALUE)
                  throw new IllegalArgumentException();

                break;
              case '(': // begin comment
                lastState = state;
                state = COMMENT_START;
                break;
              case ')': // end comment
                state = lastState;
                break;
              // a parameter always needs to follow a value / or quoted value
              case '"':
                if (state == PARAM_NAME)
                  {
                    String paramValue = in.sval;
                    // if a charset param the value needs to be stored lowercase
                    if (paramName.equals("charset"))
                      paramValue = paramValue.toLowerCase();

                    state = PARAM_VALUE;
                    params.put(paramName, paramValue);
                    break;
                  }

                // only values may be quoted
                throw new IllegalArgumentException();
              default:
                // if any other char is observed its not allowed
                throw new IllegalArgumentException();
              }
          }
      }
    catch (IOException e)
      {
        // should not happen as mimetype str cannot be null
        throw new InternalError("IOException during parsing String " + mimeType);
      }
  }

  /**
   * Checks if this doc flavor object is equal to the given object.
   * <p>
   * Two doc flavor objects are considered equal if the provided object is not
   * <code>null</code> and an instance of <code>DocFlavor</code>. The MIME
   * types has to be equal in their media type, media subtype, their
   * paramter/value combinations and the representation classname.
   * </p>
   *
   * @param obj the object to test.
   * @return <code>true</code> if equal, <code>false</code> otherwise.
   */
  public boolean equals(Object obj)
  {
    if (! (obj instanceof DocFlavor))
      return false;

    DocFlavor tmp = (DocFlavor) obj;

    return (getMimeType().equals(tmp.getMimeType())
            && getRepresentationClassName().equals(tmp.getRepresentationClassName()));
  }

  /**
   * Returns the media subtype of this flavor object.
   * A mimetype of "text/html; charset=us-ascii" will
   * return "html" as the media subtype.
   *
   * @return The media subtype.
   */
  public String getMediaSubtype()
  {
    return mediaSubtype;
  }

  /**
   * Returns the media type of this flavor object.
   * A mimetype of "text/html; charset=us-ascii" will
   * return "text" as the media type.
   *
   * @return The media type.
   */
  public String getMediaType()
  {
    return mediaType;
  }

  /**
   * Returns the mime type of this flavor object.
   * The mimetype will have every parameter value
   * enclosed in quotes.
   *
   * @return The mime type.
   */
  public String getMimeType()
  {
    String mimeType = getMediaType() + "/" + getMediaSubtype();
    Iterator it = params.entrySet().iterator();

    while (it.hasNext())
      {
        Map.Entry entry = (Map.Entry) it.next();
        mimeType += "; " + entry.getKey() + "=\"" + entry.getValue() + "\"";
      }

    return mimeType;
  }

  /**
   * Returns the value for an optional parameter of the mime type of this
   * flavor object.
   *
   * @param paramName the name of the parameter
   * @return The value for the parameter, or <code>null</code> if none bound.
   * @throws NullPointerException if paramName is <code>null</code>.
   */
  public String getParameter(String paramName)
  {
    if (paramName == null)
      throw new NullPointerException();

    return (String) params.get(paramName.toLowerCase());
  }

  /**
   * Returns the name of the representation class of this flavor object.
   *
   * @return The representation classname.
   */
  public String getRepresentationClassName()
  {
    return myClassName;
  }

  /**
   * Returns a hash code for this doc flavor object.
   *
   * @return The hashcode.
   */
  public int hashCode()
  {
    return ((mediaType.hashCode()
             * mediaSubtype.hashCode()
             * myClassName.hashCode()) ^ params.hashCode());
  }

  /**
   * Returns a string representation of this doc flavor object.
   * The returned string is of the form
   * getMimeType() + "; class=\"" + getRepresentationClassName() + "\"";
   *
   * @return The constructed string representation.
   */
  public String toString()
  {
    return getMimeType() + "; class=\"" + getRepresentationClassName() + "\"";
  }

  // needs special treatment for serialization
  private void readObject(ObjectInputStream stream)
    throws IOException, ClassNotFoundException
  {
    params = new TreeMap();
    myClassName = (String) stream.readObject();
    parseMimeType((String) stream.readObject());
  }

  private void writeObject(java.io.ObjectOutputStream stream)
    throws IOException
  {
    stream.writeObject(myClassName);
    stream.writeObject(getMimeType());
  }
}
