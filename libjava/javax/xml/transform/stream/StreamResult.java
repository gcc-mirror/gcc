/* StreamResult.java -- 
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

package javax.xml.transform.stream;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.io.Writer;
import javax.xml.transform.Result;

/**
 * Specifies a stream to which to write the transformation result.
 *
 * @author (a href='mailto:dog@gnu.org'>Chris Burdess</a)
 */
public class StreamResult
  implements Result
{

  /**
   * Factory feature indicating that stream results are supported.
   */
  public static final String FEATURE =
    "http://javax.xml.transform.stream.StreamResult/feature";

  private String systemId;
  private OutputStream outputStream;
  private Writer writer;

  /**
   * Default constructor.
   */
  public StreamResult()
  {
  }

  /**
   * Constructor with an output stream.
   */
  public StreamResult(OutputStream stream)
  {
    this.outputStream = stream;
  }

  /**
   * Constructor with a writer.
   * Prefer to use an output stream rather than a writer, so that the
   * output encoding can be controlled by transformation properties.
   */
  public StreamResult(Writer writer)
  {
    this.writer = writer;
  }

  /**
   * Constructor with a system ID.
   */
  public StreamResult(String systemID)
  {
    this.systemId = systemID;
  }

  /**
   * Constructor with a system ID specified as a File object.
   */
  public StreamResult(File file)
  {
    setSystemId(file);
  }

  /**
   * Sets the target output stream.
   */
  public void setOutputStream(OutputStream outputStream)
  {
    this.outputStream = outputStream;
  }

  /**
   * Returns the target output stream.
   */
  public OutputStream getOutputStream()
  {
    return outputStream;
  }

  /**
   * Sets the target writer.
   * Prefer to use an output stream rather than a writer, so that the
   * output encoding can be controlled by transformation properties.
   */
  public void setWriter(Writer writer)
  {
    this.writer = writer;
  }

  /**
   * Returns the target writer.
   */
  public Writer getWriter()
  {
    return writer;
  }

  /**
   * Sets the system ID.
   * If neither the out stream nor the writer have been specified, the
   * system ID will be treated as a URL for writing to.
   */
  public void setSystemId(String systemID)
  {
    this.systemId = systemID;
  }

  /**
   * Sets the system ID from a File reference.
   */
  public void setSystemId(File f)
  {
    try
      {
        systemId = f.toURL().toString();
      }
    catch (IOException e)
      {
        throw new RuntimeException(e.getMessage(), e);
      }
  }

  /**
   * Returns the system ID.
   */
  public String getSystemId()
  {
    return systemId;
  }

}
