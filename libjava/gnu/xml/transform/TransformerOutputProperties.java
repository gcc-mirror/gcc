/* TransformerOutputProperties.java -- 
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

package gnu.xml.transform;

import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.Properties;
import java.util.StringTokenizer;
import javax.xml.transform.OutputKeys;

/**
 * Helper class to manage JAXP user setting of output properties.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
class TransformerOutputProperties
  extends Properties
{
  
  final Properties defaultProperties;
  final Stylesheet stylesheet;
  boolean dirty;

  TransformerOutputProperties(Stylesheet stylesheet)
  {
    this.stylesheet = stylesheet;
    defaultProperties = new Properties();
    switch (stylesheet.outputMethod)
      {
      case Stylesheet.OUTPUT_XML:
        defaultProperties.put(OutputKeys.METHOD, "xml");
        break;
      case Stylesheet.OUTPUT_HTML:
        defaultProperties.put(OutputKeys.METHOD, "html");
        break;
      case Stylesheet.OUTPUT_TEXT:
        defaultProperties.put(OutputKeys.METHOD, "text");
        break;
      }
    if (stylesheet.outputVersion != null)
      {
        defaultProperties.put(OutputKeys.VERSION, stylesheet.outputVersion);
      }
    if (stylesheet.outputEncoding != null)
      {
        defaultProperties.put(OutputKeys.ENCODING, stylesheet.outputEncoding);
      }
    defaultProperties.put(OutputKeys.OMIT_XML_DECLARATION,
                          stylesheet.outputOmitXmlDeclaration ? "yes" : "no");
    defaultProperties.put(OutputKeys.STANDALONE,
                          stylesheet.outputStandalone ? "yes" : "no");
    if (stylesheet.outputPublicId != null)
      {
        defaultProperties.put(OutputKeys.DOCTYPE_PUBLIC,
                              stylesheet.outputPublicId);
      }
    if (stylesheet.outputSystemId != null)
      {
        defaultProperties.put(OutputKeys.DOCTYPE_SYSTEM,
                              stylesheet.outputSystemId);
      }
    StringBuffer buf = new StringBuffer();
    for (Iterator i = stylesheet.outputCdataSectionElements.iterator();
         i.hasNext(); )
      {
        if (buf.length() > 0)
          {
            buf.append(' ');
          }
        buf.append((String) i.next());
      }
    defaultProperties.put(OutputKeys.CDATA_SECTION_ELEMENTS, buf.toString());
    defaultProperties.put(OutputKeys.INDENT,
                          stylesheet.outputIndent ? "yes" : "no");
    if (stylesheet.outputMediaType != null)
      {
        defaultProperties.put(OutputKeys.MEDIA_TYPE,
                              stylesheet.outputMediaType);
      }
  }

  public String getProperty(String key)
  {
    String val = super.getProperty(key);
    if (val == null)
      {
        val = defaultProperties.getProperty(key);
      }
    return val;
  }

  public Object put(Object key, Object value)
  {
    Object ret = super.put(key, value);
    dirty = true;
    return ret;
  }

  public void clear()
  {
    super.clear();
    dirty = true;
  }

  /**
   * Applies the current set of properties to the underlying stylesheet.
   */
  void apply()
  {
    if (!dirty)
      {
        return;
      }
    String method = getProperty(OutputKeys.METHOD);
    if ("xml".equals(method))
      {
        stylesheet.outputMethod = Stylesheet.OUTPUT_XML;
      }
    else if ("html".equals(method))
      {
        stylesheet.outputMethod = Stylesheet.OUTPUT_HTML;
      }
    else if ("text".equals(method))
      {
        stylesheet.outputMethod = Stylesheet.OUTPUT_TEXT;
      }
    stylesheet.outputVersion = getProperty(OutputKeys.VERSION);
    stylesheet.outputEncoding = getProperty(OutputKeys.ENCODING);
    stylesheet.outputOmitXmlDeclaration =
      "yes".equals(getProperty(OutputKeys.OMIT_XML_DECLARATION));
    stylesheet.outputStandalone =
      "yes".equals(getProperty(OutputKeys.STANDALONE));
    stylesheet.outputPublicId = getProperty(OutputKeys.DOCTYPE_PUBLIC);
    stylesheet.outputSystemId = getProperty(OutputKeys.DOCTYPE_SYSTEM);
    StringTokenizer st = 
      new StringTokenizer(getProperty(OutputKeys.CDATA_SECTION_ELEMENTS));
    Collection acc = new LinkedHashSet();
    while (st.hasMoreTokens())
      {
        acc.add(st.nextToken());
      }
    stylesheet.outputCdataSectionElements = acc;
    stylesheet.outputIndent = "yes".equals(getProperty(OutputKeys.INDENT));
    stylesheet.outputMediaType = getProperty(OutputKeys.MEDIA_TYPE);
    dirty = false;
  }

}

