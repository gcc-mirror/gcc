/* XMLFormatter.java --
   A class for formatting log messages into a standard XML format
   Copyright (C) 2002, 2004 Free Software Foundation, Inc.

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


package java.util.logging;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.ResourceBundle;

/**
 * An <code>XMLFormatter</code> formats LogRecords into
 * a standard XML format.
 *
 * @author Sascha Brawer (brawer@acm.org)
 */
public class XMLFormatter
  extends Formatter
{
  /**
   * Constructs a new XMLFormatter.
   */
  public XMLFormatter()
  {
  }


  /**
   * The character sequence that is used to separate lines in the
   * generated XML stream. Somewhat surprisingly, the Sun J2SE 1.4
   * reference implementation always uses UNIX line endings, even on
   * platforms that have different line ending conventions (i.e.,
   * DOS). The GNU Classpath implementation does not replicates this
   * bug.
   *
   * See also the Sun bug parade, bug #4462871,
   * "java.util.logging.SimpleFormatter uses hard-coded line separator".
   */
  private static final String lineSep = SimpleFormatter.lineSep;

    
  /**
   * A DateFormat for emitting time in the ISO 8601 format.
   * Since the API specification of SimpleDateFormat does not talk
   * about its thread-safety, we cannot share a singleton instance.
   */
  private final SimpleDateFormat iso8601
    = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");


  /**
   * Appends a line consisting of indentation, opening element tag,
   * element content, closing element tag and line separator to
   * a StringBuffer, provided that the element content is
   * actually existing.
   *
   * @param buf the StringBuffer to which the line will be appended.
   *
   * @param indent the indentation level.
   *
   * @param tag the element tag name, for instance <code>method</code>.
   *
   * @param content the element content, or <code>null</code> to
   *        have no output whatsoever appended to <code>buf</code>.
   */
  private static void appendTag(StringBuffer buf, int indent,
                                String tag, String content)
  {
    int i;

    if (content == null)
      return;

    for (i = 0; i < indent * 2; i++)
      buf.append(' ');

    buf.append("<");
    buf.append(tag);
    buf.append('>');

    /* Append the content, but escape for XML by replacing
     * '&', '<', '>' and all non-ASCII characters with
     * appropriate escape sequences.
     * The Sun J2SE 1.4 reference implementation does not
     * escape non-ASCII characters. This is a bug in their
     * implementation which has been reported in the Java
     * bug parade as bug number (FIXME: Insert number here).
     */
    for (i = 0; i < content.length(); i++)
    {
      char c = content.charAt(i);
      switch (c)
      {
      case '&':
	buf.append("&amp;");
	break;

      case '<':
	buf.append("&lt;");
	break;

      case '>':
	buf.append("&gt;");
	break;

      default:
	if (((c >= 0x20) && (c <= 0x7e))
	    || (c == /* line feed */ 10)
	    || (c == /* carriage return */ 13))
	  buf.append(c);
	else
	{
	  buf.append("&#");
	  buf.append((int) c);
	  buf.append(';');
	}
	break;
      } /* switch (c) */
    } /* for i */

    buf.append("</");
    buf.append(tag);
    buf.append(">");
    buf.append(lineSep);
  }


  /**
   * Appends a line consisting of indentation, opening element tag,
   * numeric element content, closing element tag and line separator
   * to a StringBuffer.
   *
   * @param buf the StringBuffer to which the line will be appended.
   *
   * @param indent the indentation level.
   *
   * @param tag the element tag name, for instance <code>method</code>.
   *
   * @param content the element content.
   */
  private static void appendTag(StringBuffer buf, int indent,
                                String tag, long content)
  {
    appendTag(buf, indent, tag, Long.toString(content));
  }


  public String format(LogRecord record)
  {
    StringBuffer    buf = new StringBuffer(400);
    Level           level = record.getLevel();
    long            millis = record.getMillis();
    Object[]        params = record.getParameters();
    ResourceBundle  bundle = record.getResourceBundle();
    String          message;
    
    buf.append("<record>");
    buf.append(lineSep);
    
    
    appendTag(buf, 1, "date", iso8601.format(new Date(millis)));
    appendTag(buf, 1, "millis", millis);
    appendTag(buf, 1, "sequence", record.getSequenceNumber());
    appendTag(buf, 1, "logger", record.getLoggerName());

    if (level.isStandardLevel())
      appendTag(buf, 1, "level", level.toString());
    else
      appendTag(buf, 1, "level", level.intValue());

    appendTag(buf, 1, "class", record.getSourceClassName());
    appendTag(buf, 1, "method", record.getSourceMethodName());
    appendTag(buf, 1, "thread", record.getThreadID());

    /* The Sun J2SE 1.4 reference implementation does not emit the
     * message in localized form. This is in violation of the API
     * specification. The GNU Classpath implementation intentionally
     * replicates the buggy behavior of the Sun implementation, as
     * different log files might be a big nuisance to users.
     */
    try
    {
      record.setResourceBundle(null);
      message = formatMessage(record);
    }
    finally
    {
      record.setResourceBundle(bundle);
    }
    appendTag(buf, 1, "message", message);

    /* The Sun J2SE 1.4 reference implementation does not
     * emit key, catalog and param tags. This is in violation
     * of the API specification.  The Classpath implementation
     * intentionally replicates the buggy behavior of the
     * Sun implementation, as different log files might be
     * a big nuisance to users.
     *
     * FIXME: File a bug report with Sun. Insert bug number here.
     *
     *
     * key = record.getMessage();
     * if (key == null)
     *   key = "";
     *
     * if ((bundle != null) && !key.equals(message))
     * {
     *   appendTag(buf, 1, "key", key);
     *   appendTag(buf, 1, "catalog", record.getResourceBundleName());
     * }
     *
     * if (params != null)
     * {
     *   for (int i = 0; i < params.length; i++)
     *     appendTag(buf, 1, "param", params[i].toString());
     * }
     */

    /* FIXME: We have no way to obtain the stacktrace before free JVMs
     * support the corresponding method in java.lang.Throwable.  Well,
     * it would be possible to parse the output of printStackTrace,
     * but this would be pretty kludgy. Instead, we postpose the
     * implementation until Throwable has made progress.
     */
    Throwable thrown = record.getThrown();
    if (thrown != null)
    {
      buf.append("  <exception>");
      buf.append(lineSep);

      /* The API specification is not clear about what exactly
       * goes into the XML record for a thrown exception: It
       * could be the result of getMessage(), getLocalizedMessage(),
       * or toString(). Therefore, it was necessary to write a
       * Mauve testlet and run it with the Sun J2SE 1.4 reference
       * implementation. It turned out that the we need to call
       * toString().
       *
       * FIXME: File a bug report with Sun, asking for clearer
       * specs.
       */
      appendTag(buf, 2, "message", thrown.toString());

      /* FIXME: The Logging DTD specifies:
       *
       * <!ELEMENT exception (message?, frame+)>
       *
       * However, java.lang.Throwable.getStackTrace() is
       * allowed to return an empty array. So, what frame should
       * be emitted for an empty stack trace? We probably
       * should file a bug report with Sun, asking for the DTD
       * to be changed.
       */

      buf.append("  </exception>");
      buf.append(lineSep);
    }


    buf.append("</record>");
    buf.append(lineSep);

    return buf.toString();
  }


  /**
   * Returns a string that handlers are supposed to emit before
   * the first log record.  The base implementation returns an
   * empty string, but subclasses such as {@link XMLFormatter}
   * override this method in order to provide a suitable header.
   *
   * @return a string for the header.
   *
   * @param h the handler which will prepend the returned
   *     string in front of the first log record.  This method
   *     will inspect certain properties of the handler, for
   *     example its encoding, in order to construct the header.
   */
  public String getHead(Handler h)
  {
    StringBuffer  buf;
    String        encoding;

    buf = new StringBuffer(80);
    buf.append("<?xml version=\"1.0\" encoding=\"");

    encoding = h.getEncoding();

    /* file.encoding is a system property with the Sun JVM, indicating
     * the platform-default file encoding. Unfortunately, the API
     * specification for java.lang.System.getProperties() does not
     * list this property.
     */
    if (encoding == null)
      encoding = System.getProperty("file.encoding");

    /* Since file.encoding is not listed with the API specification of
     * java.lang.System.getProperties(), there might be some VMs that
     * do not define this system property.  Therefore, we use UTF-8 as
     * a reasonable default. Please note that if the platform encoding
     * uses the same codepoints as US-ASCII for the US-ASCII character
     * set (e.g, 65 for A), it does not matter whether we emit the
     * wrong encoding into the XML header -- the GNU Classpath will
     * emit XML escape sequences like &#1234; for any non-ASCII
     * character.  Virtually all character encodings use the same code
     * points as US-ASCII for ASCII characters.  Probably, EBCDIC is
     * the only exception.
     */
    if (encoding == null)
      encoding = "UTF-8";
    
    /* On Windows XP localized for Swiss German (this is one of
     * my [Sascha Brawer's] test machines), the default encoding
     * has the canonical name "windows-1252". The "historical" name
     * of this encoding is "Cp1252" (see the Javadoc for the class
     * java.nio.charset.Charset for the distinction). Now, that class
     * does have a method for mapping historical to canonical encoding
     * names. However, if we used it here, we would be come dependent
     * on java.nio.*, which was only introduced with J2SE 1.4.
     * Thus, we do this little hack here. As soon as Classpath supports
     * java.nio.charset.CharSet, this hack should be replaced by
     * code that correctly canonicalizes the encoding name.
     */
    if ((encoding.length() > 2) && encoding.startsWith("Cp"))
      encoding = "windows-" + encoding.substring(2);

    buf.append(encoding);

    buf.append("\" standalone=\"no\"?>");
    buf.append(lineSep);

    /* SYSTEM is not a fully qualified URL so that validating
     * XML parsers do not need to connect to the Internet in
     * order to read in a log file.  See also the Sun Bug Parade,
     * bug #4372790, "Logging APIs: need to use relative URL for XML
     * doctype".
     */
    buf.append("<!DOCTYPE log SYSTEM \"logger.dtd\">");
    buf.append(lineSep);
    buf.append("<log>");
    buf.append(lineSep);

    return buf.toString();
  }


  public String getTail(Handler h)
  {
    return "</log>" + lineSep;
  }
}
