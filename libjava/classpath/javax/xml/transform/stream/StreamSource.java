/* StreamSource.java --
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

package javax.xml.transform.stream;

import java.io.File;
import java.io.InputStream;
import java.io.IOException;
import java.io.Reader;
import javax.xml.transform.Source;

/**
 * Specifies a stream from which to read the source XML data.
 *
 * @author (a href='mailto:dog@gnu.org'>Chris Burdess</a)
 */
public class StreamSource
  implements Source
{

  /**
   * Factory feature indicating that stream sources are supported.
   */
  public static final String FEATURE =
    "http://javax.xml.transform.stream.StreamSource/feature";

  private String publicId;
  private String systemId;
  private InputStream inputStream;
  private Reader reader;

  /**
   * Default constructor.
   */
  public StreamSource()
  {
  }

  /**
   * Constructor with an input stream.
   */
  public StreamSource(InputStream stream)
  {
    this.inputStream = stream;
  }

  /**
   * Constructor with an input stream and system ID.
   */
  public StreamSource(InputStream stream, String systemId)
  {
    this.inputStream = stream;
    this.systemId = systemId;
  }

  /**
   * Constructor with a reader.
   * Prefer an input stream to a reader, so that the parser can use the
   * character encoding specified in the XML.
   */
  public StreamSource(Reader reader)
  {
    this.reader = reader;
  }

  /**
   * Constructor with a reader and system ID.
   * Prefer an input stream to a reader, so that the parser can use the
   * character encoding specified in the XML.
   */
  public StreamSource(Reader reader, String systemId)
  {
    this.reader = reader;
    this.systemId = systemId;
  }

  /**
   * Constructor with a system ID.
   */
  public StreamSource(String systemId)
  {
    this.systemId = systemId;
  }

  /**
   * Constructor with a system ID specified as a File reference.
   */
  public StreamSource(File file)
  {
    setSystemId(file);
  }

  /**
   * Sets the source input stream.
   */
  public void setInputStream(InputStream stream)
  {
    this.inputStream = stream;
  }

  /**
   * Returns the source input stream.
   */
  public InputStream getInputStream()
  {
    return inputStream;
  }

  /**
   * Sets the source reader.
   * Prefer an input stream to a reader, so that the parser can use the
   * character encoding specified in the XML.
   */
  public void setReader(Reader reader)
  {
    this.reader = reader;
  }

  /**
   * Returns the source reader.
   */
  public Reader getReader()
  {
    return reader;
  }

  /**
   * Sets the public ID for this source.
   */
  public void setPublicId(String publicId)
  {
    this.publicId = publicId;
  }

  /**
   * Returns the public ID for this source.
   */
  public String getPublicId()
  {
    return publicId;
  }

  /**
   * Sets the system ID for this source.
   * If the input stream and reader are absent, the system ID will be used
   * as a readable URL to locate the source data.
   */
  public void setSystemId(String systemId)
  {
    this.systemId = systemId;
  }

  /**
   * Returns the system ID for this source.
   */
  public String getSystemId()
  {
    return systemId;
  }

  /**
   * Sets the system ID using a File reference.
   */
  public void setSystemId(File f)
  {
    try
      {
        this.systemId = f.toURL().toString();
      }
    catch (IOException e)
      {
        throw new RuntimeException(e.getMessage(), e);
      }
  }

}
