/* SAXSource.java -- 
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

package javax.xml.transform.sax;

import java.io.InputStream;
import java.io.Reader;
import javax.xml.transform.Source;
import javax.xml.transform.stream.StreamSource;
import org.xml.sax.InputSource;
import org.xml.sax.XMLReader;

/**
 * Specifies a SAX XML source. This is a tuple of input source and SAX
 * parser.
 *
 * @author (a href='mailto:dog@gnu.org'>Chris Burdess</a)
 */
public class SAXSource
  implements Source
{

  /**
   * Factory feature indicating that SAX sources are supported.
   */
  public static final String FEATURE =
    "http://javax.xml.transform.sax.SAXSource/feature";

  private XMLReader  xmlReader;
  private InputSource  inputSource;

  /**
   * Default constructor.
   */
  public SAXSource()
  {
  }

  /**
   * Constructor with a SAX parser and input source.
   */
  public SAXSource(XMLReader reader, InputSource inputSource)
  {
    xmlReader = reader;
    this.inputSource = inputSource;
  }

  /**
   * Constructor with an input source.
   * The SAX parser will be instantiated by the transformer.
   */
  public SAXSource(InputSource inputSource)
  {
    this.inputSource = inputSource;
  }

  /**
   * Sets the SAX parser to be used by this source.
   * If null, the transformer will instantiate its own parser.
   */
  public void setXMLReader(XMLReader reader)
  {
    xmlReader = reader;
  }

  /**
   * Returns the SAX parser to be used by this source.
   * If null, the transformer will instantiate its own parser.
   */
  public XMLReader getXMLReader()
  {
    return xmlReader;
  }

  /**
   * Sets the input source to parse.
   */
  public void setInputSource(InputSource inputSource)
  {
    this.inputSource = inputSource;
  }

  /**
   * Returns the input source to parse.
   */
  public InputSource getInputSource()
  {
    return inputSource;
  }

  /**
   * Sets the system ID for this source.
   */
  public void setSystemId(String systemId)
  {
    if (inputSource != null)
      {
        inputSource.setSystemId(systemId);
      }
  }

  /**
   * Returns the system ID for this source.
   */
  public String getSystemId()
  {
    if (inputSource != null)
      {
        return inputSource.getSystemId();
      }
    return null;
  }

  /**
   * Converts a source into a SAX input source.
   * This method can use a StreamSource or the system ID.
   * @return an input source or null
   */
  public static InputSource sourceToInputSource(Source source)
  {
    InputSource in = null;
    if (source instanceof SAXSource)
      {
        in = ((SAXSource) source).getInputSource();
      }
    else if (source instanceof StreamSource)
      {
        StreamSource streamSource = (StreamSource) source;
        InputStream inputStream = streamSource.getInputStream();
        if (inputStream != null)
          {
            in = new InputSource(inputStream);
          }
        else
          {
            Reader reader = streamSource.getReader();
            if (reader != null)
              {
                in = new InputSource(reader);
              }
          }
        String publicId = streamSource.getPublicId();
        if (publicId != null && in != null)
          {
            in.setPublicId(publicId);
          }
      }
    String systemId = source.getSystemId();
    if (systemId != null)
      {
        if (in == null)
          {
            in = new InputSource(systemId);
          }
        else
          {
            in.setSystemId(systemId);
          }
      }
    return in;
  }

}
