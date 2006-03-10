/* Response.java --
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


package gnu.java.net.protocol.http;

import java.io.InputStream;
import java.util.Date;

/**
 * An HTTP response.
 *
 * @author Chris Burdess (dog@gnu.org)
 */
public class Response
{

  /**
   * The HTTP major version of the server issuing the response.
   */
  protected final int majorVersion;

  /**
   * The HTTP minor version of the server issuing the response.
   */
  protected final int minorVersion;

  /**
   * The HTTP status code of the response.
   */ 
  protected final int code;

  /**
   * Human-readable text of the response.
   */
  protected final String message;

  /**
   * The response headers.
   */
  protected final Headers headers;

  /**
   * An InputStream that returns the body of the response.
   */
  protected final InputStream body;

  /**
   * Constructs a new response with the specified parameters.
   */
  protected Response(int majorVersion, int minorVersion, int code,
                     String message, Headers headers, InputStream body)
  {
    this.majorVersion = majorVersion;
    this.minorVersion = minorVersion;
    this.code = code;
    this.message = message;
    this.headers = headers;
    this.body = body;
  }

  /**
   * Returns the HTTP major version of the server issuing the response.
   * @see #majorVersion
   */
  public int getMajorVersion()
  {
    return majorVersion;
  }

  /**
   * Returns the HTTP minor version of the server issuing the response.
   * @see #minorVersion
   */
  public int getMinorVersion()
  {
    return minorVersion;
  }

  /**
   * Returns the HTTP status code of the response.
   * @see #code
   */ 
  public int getCode()
  {
    return code;
  }

  /**
   * Returns the class of the response.  This is the most significant
   * digit of the status code.
   * <dl>
   * <dt><code>1xx</code></dt> <dd>Informational response</dd>
   * <dt><code>2xx</code></dt> <dd>Success</dd>
   * <dt><code>3xx</code></dt> <dd>Redirection</dd>
   * <dt><code>4xx</code></dt> <dd>Client error</dd>
   * <dt><code>5xx</code></dt> <dd>Server error</dd>
   * </dl>
   */
  public int getCodeClass()
  {
    return code / 100;
  }

  /**
   * Returns the human-readable text of the response.
   * @see #message
   */
  public String getMessage()
  {
    return message;
  }

  /**
   * Returns the headers in the response.
   */
  public Headers getHeaders()
  {
    return headers;
  }

  /**
   * Returns the header value for the specified name.
   * @param name the header name
   */
  public String getHeader(String name)
  {
    return headers.getValue(name);
  }

  /**
   * Returns the header value for the specified name as an integer.
   * @param name the header name
   */
  public int getIntHeader(String name)
  {
    return headers.getIntValue(name);
  }

  /**
   * Returns the header value for the specified name as a long.
   * @param name the header name
   */
  public long getLongHeader(String name)
  {
    return headers.getLongValue(name);
  }

  /**
   * Returns the header value for the specified name as a date.
   * @param name the header name
   */
  public Date getDateHeader(String name)
  {
    return headers.getDateValue(name);
  }
  
  /**
   * Tests whether this response indicates a redirection.
   * 
   * @return <code>true</code> if, <code>false</code> otherwise.
   */
  public boolean isRedirect()
  {
    return (code != 304 && getCodeClass() == 3);
  }
  
  /**
   * Tests whether this response indicates an error.
   * Errors are the response codes <code>4xx</code> - Client error and
   * <code>5xx</code> - Server error.
   * 
   * @return <code>true</code> if, <code>false</code> otherwise.
   */
  public boolean isError()
  {
    return (getCodeClass() == 4 || getCodeClass() == 5);
  }

  /**
   * Returns an InputStream that returns the body of the response.
   *
   * @return the body of the response
   */
  public InputStream getBody()
  {
    return body;
  }
}

