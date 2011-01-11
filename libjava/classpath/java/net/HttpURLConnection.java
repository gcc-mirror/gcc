/* HttpURLConnection.java -- Subclass of communications links using
   Hypertext Transfer Protocol.
   Copyright (C) 1998, 1999, 2000, 2002, 2003  Free Software Foundation

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

package java.net;

import java.io.IOException;
import java.io.InputStream;
import java.io.PushbackInputStream;
import java.security.Permission;


/*
 * Written using on-line Java Platform 1.2 API Specification, as well
 * as "The Java Class Libraries", 2nd edition (Addison-Wesley, 1998).
 * Status:  Believed complete and correct.
 */

/**
 * This class provides a common abstract implementation for those
 * URL connection classes that will connect using the HTTP protocol.
 * In addition to the functionality provided by the URLConnection
 * class, it defines constants for HTTP return code values and
 * methods for setting the HTTP request method and determining whether
 * or not to follow redirects.
 *
 * @since 1.1
 *
 * @author Warren Levy (warrenl@cygnus.com)
 * @author Aaron M. Renn (arenn@urbanophile.com)
 */
public abstract class HttpURLConnection extends URLConnection
{
  /* HTTP Success Response Codes */

  /**
   * Indicates that the client may continue with its request.  This value
   * is specified as part of RFC 2068 but was not included in Sun's JDK, so
   * beware of using this value
   */
  static final int HTTP_CONTINUE = 100;

  /**
   * Indicates the request succeeded.
   */
  public static final int HTTP_OK = 200;

  /**
   * The requested resource has been created.
   */
  public static final int HTTP_CREATED = 201;

  /**
   * The request has been accepted for processing but has not completed.
   * There is no guarantee that the requested action will actually ever
   * be completed succesfully, but everything is ok so far.
   */
  public static final int HTTP_ACCEPTED = 202;

  /**
   * The meta-information returned in the header is not the actual data
   * from the original server, but may be from a local or other copy.
   * Normally this still indicates a successful completion.
   */
  public static final int HTTP_NOT_AUTHORITATIVE = 203;

  /**
   * The server performed the request, but there is no data to send
   * back.  This indicates that the user's display should not be changed.
   */
  public static final int HTTP_NO_CONTENT = 204;

  /**
   * The server performed the request, but there is no data to sent back,
   * however, the user's display should be "reset" to clear out any form
   * fields entered.
   */
  public static final int HTTP_RESET = 205;

  /**
   * The server completed the partial GET request for the resource.
   */
  public static final int HTTP_PARTIAL = 206;

  /* HTTP Redirection Response Codes */

  /**
   * There is a list of choices available for the requested resource.
   */
  public static final int HTTP_MULT_CHOICE = 300;

  /**
   * The resource has been permanently moved to a new location.
   */
  public static final int HTTP_MOVED_PERM = 301;

  /**
   * The resource requested has been temporarily moved to a new location.
   */
  public static final int HTTP_MOVED_TEMP = 302;

  /**
   * The response to the request issued is available at another location.
   */
  public static final int HTTP_SEE_OTHER = 303;

  /**
   * The document has not been modified since the criteria specified in
   * a conditional GET.
   */
  public static final int HTTP_NOT_MODIFIED = 304;

  /**
   * The requested resource needs to be accessed through a proxy.
   */
  public static final int HTTP_USE_PROXY = 305;

  /* HTTP Client Error Response Codes */

  /**
   * The request was misformed or could not be understood.
   */
  public static final int HTTP_BAD_REQUEST = 400;

  /**
   * The request made requires user authorization.  Try again with
   * a correct authentication header.
   */
  public static final int HTTP_UNAUTHORIZED = 401;

  /**
   * Code reserved for future use - I hope way in the future.
   */
  public static final int HTTP_PAYMENT_REQUIRED = 402;

  /**
   * There is no permission to access the requested resource.
   */
  public static final int HTTP_FORBIDDEN = 403;

  /**
   * The requested resource was not found.
   */
  public static final int HTTP_NOT_FOUND = 404;

  /**
   * The specified request method is not allowed for this resource.
   */
  public static final int HTTP_BAD_METHOD = 405;

  /**
   * Based on the input headers sent, the resource returned in response
   * to the request would not be acceptable to the client.
   */
  public static final int HTTP_NOT_ACCEPTABLE = 406;

  /**
   * The client must authenticate with a proxy prior to attempting this
   * request.
   */
  public static final int HTTP_PROXY_AUTH = 407;

  /**
   * The request timed out.
   */
  public static final int HTTP_CLIENT_TIMEOUT = 408;

  /**
   * There is a conflict between the current state of the resource and the
   * requested action.
   */
  public static final int HTTP_CONFLICT = 409;

  /**
   * The requested resource is no longer available.  This ususally indicates
   * a permanent condition.
   */
  public static final int HTTP_GONE = 410;

  /**
   * A Content-Length header is required for this request, but was not
   * supplied.
   */
  public static final int HTTP_LENGTH_REQUIRED = 411;

  /**
   * A client specified pre-condition was not met on the server.
   */
  public static final int HTTP_PRECON_FAILED = 412;

  /**
   * The request sent was too large for the server to handle.
   */
  public static final int HTTP_ENTITY_TOO_LARGE = 413;

  /**
   * The name of the resource specified was too long.
   */
  public static final int HTTP_REQ_TOO_LONG = 414;

  /**
   * The request is in a format not supported by the requested resource.
   */
  public static final int HTTP_UNSUPPORTED_TYPE = 415;

  /* HTTP Server Error Response Codes */

  /**
   * This error code indicates that some sort of server error occurred.
   *
   * @deprecated
   */
  public static final int HTTP_SERVER_ERROR = 500;

  /**
   * The server encountered an unexpected error (such as a CGI script crash)
   * that prevents the request from being fulfilled.
   */
  public static final int HTTP_INTERNAL_ERROR = 500;

  /**
   * The server does not support the requested functionality.
   * @since 1.3
   */
  public static final int HTTP_NOT_IMPLEMENTED = 501;

  /**
   * The proxy encountered a bad response from the server it was proxy-ing for
   */
  public static final int HTTP_BAD_GATEWAY = 502;

  /**
   * The HTTP service is not availalble, such as because it is overloaded
   * and does not want additional requests.
   */
  public static final int HTTP_UNAVAILABLE = 503;

  /**
   * The proxy timed out getting a reply from the remote server it was
   * proxy-ing for.
   */
  public static final int HTTP_GATEWAY_TIMEOUT = 504;

  /**
   * This server does not support the protocol version requested.
   */
  public static final int HTTP_VERSION = 505;

  // Non-HTTP response static variables

  /**
   * Flag to indicate whether or not redirects should be automatically
   * followed by default.
   */
  private static boolean followRedirects = true;

  /**
   * This is a list of valid request methods, separated by "|" characters.
   */
  private static final String valid_methods =
    "|GET|POST|HEAD|OPTIONS|PUT|DELETE|TRACE|";

  // Instance Variables

  /**
   * The requested method in use for this connection. Default is GET.
   */
  protected String method = "GET";

  /**
   * The response code received from the server
   */
  protected int responseCode = -1;

  /**
   * The response message string received from the server.
   */
  protected String responseMessage;

  /**
   * If this instance should follow redirect requests.
   */
  protected boolean instanceFollowRedirects = followRedirects;

  /**
   * Whether we already got a valid response code for this connection.
   * Used by <code>getResponseCode()</code> and
   * <code>getResponseMessage()</code>.
   */
  private boolean gotResponseVals;

  /**
   * Create an HttpURLConnection for the specified URL
   *
   * @param url The URL to create this connection for.
   */
  protected HttpURLConnection(URL url)
  {
    super(url);
  }

  /**
   * Closes the connection to the server.
   */
  public abstract void disconnect();

  /**
   * Returns a boolean indicating whether or not this connection is going
   * through a proxy
   *
   * @return true if through a proxy, false otherwise
   */
  public abstract boolean usingProxy();

  /**
   * Sets whether HTTP redirects (requests with response code 3xx) should be
   * automatically followed by this class. True by default
   *
   * @param set true if redirects should be followed, false otherwis.
   *
   * @exception SecurityException If a security manager exists and its
   * checkSetFactory method doesn't allow the operation
   */
  public static void setFollowRedirects(boolean set)
  {
    // Throw an exception if an extant security mgr precludes
    // setting the factory.
    SecurityManager s = System.getSecurityManager();
    if (s != null)
      s.checkSetFactory();

    followRedirects = set;
  }

  /**
   * Returns a boolean indicating whether or not HTTP redirects will
   * automatically be followed or not.
   *
   * @return true if redirects will be followed, false otherwise
   */
  public static boolean getFollowRedirects()
  {
    return followRedirects;
  }

  /**
   * Returns the value of this HttpURLConnection's instanceFollowRedirects
   * field
   *
   * @return true if following redirects is enabled, false otherwise
   */
  public boolean getInstanceFollowRedirects()
  {
    return instanceFollowRedirects;
  }

  /**
   * Sets the value of this HttpURLConnection's instanceFollowRedirects field
   *
   * @param follow true to enable following redirects, false otherwise
   */
  public void setInstanceFollowRedirects(boolean follow)
  {
    instanceFollowRedirects = follow;
  }

  /**
   * Set the method for the URL request, one of:
   * GET POST HEAD OPTIONS PUT DELETE TRACE are legal
   *
   * @param method the method to use
   *
   * @exception ProtocolException If the method cannot be reset or if the
   * requested method isn't valid for HTTP
   */
  public void setRequestMethod(String method) throws ProtocolException
  {
    if (connected)
      throw new ProtocolException("Already connected");

    method = method.toUpperCase();
    if (valid_methods.indexOf("|" + method + "|") != -1)
      this.method = method;
    else
      throw new ProtocolException("Invalid HTTP request method: " + method);
  }

  /**
   * The request method currently in use for this connection.
   *
   * @return The request method
   */
  public String getRequestMethod()
  {
    return method;
  }

  /**
   * Gets the status code from an HTTP response message, or -1 if
   * the response code could not be determined.
   * Note that all valid response codes have class variables
   * defined for them in this class.
   *
   * @return The response code
   *
   * @exception IOException If an error occurs
   */
  public int getResponseCode() throws IOException
  {
    if (! gotResponseVals)
      getResponseVals();
    return responseCode;
  }

  /**
   * Gets the HTTP response message, if any, returned along with the
   * response code from a server. Null if no response message was set
   * or an error occured while connecting.
   *
   * @return The response message
   *
   * @exception IOException If an error occurs
   */
  public String getResponseMessage() throws IOException
  {
    if (! gotResponseVals)
      getResponseVals();
    return responseMessage;
  }

  private void getResponseVals() throws IOException
  {
    // getHeaderField() will connect for us, but do it here first in
    // order to pick up IOExceptions.
    if (! connected)
      connect();

    gotResponseVals = true;

    // If responseCode not yet explicitly set by subclass
    if (responseCode == -1)
      {
        // Response is the first header received from the connection.
        String respField = getHeaderField(0);

        if (respField == null || ! respField.startsWith("HTTP/"))
          {
            // Set to default values on failure.
            responseCode = -1;
            responseMessage = null;
            return;
          }

        int firstSpc;
        int nextSpc;
        firstSpc = respField.indexOf(' ');
        nextSpc = respField.indexOf(' ', firstSpc + 1);
        responseMessage = respField.substring(nextSpc + 1);
        String codeStr = respField.substring(firstSpc + 1, nextSpc);
        try
          {
            responseCode = Integer.parseInt(codeStr);
          }
        catch (NumberFormatException e)
          {
            // Set to default values on failure.
            responseCode = -1;
            responseMessage = null;
          }
      }
  }

  /**
   * Returns a permission object representing the permission necessary to make
   * the connection represented by this object
   *
   * @return the permission necessary for this connection
   *
   * @exception IOException If an error occurs
   */
  public Permission getPermission() throws IOException
  {
    URL url = getURL();
    String host = url.getHost();
    int port = url.getPort();
    if (port == -1)
      port = 80;

    host = host + ":" + port;

    return new SocketPermission(host, "connect");
  }

  /**
   * This method allows the caller to retrieve any data that might have
   * been sent despite the fact that an error occurred.  For example, the
   * HTML page sent along with a 404 File Not Found error.  If the socket
   * is not connected, or if no error occurred or no data was returned,
   * this method returns <code>null</code>.
   *
   * @return An <code>InputStream</code> for reading error data.
   */
  public InputStream getErrorStream()
  {
    if (! connected)
      return null;

    int code;
    try
      {
        code = getResponseCode();
      }
    catch (IOException e)
      {
        code = -1;
      }

    if (code == -1)
      return null;

    if (((code / 100) != 4) || ((code / 100) != 5))
      return null;

    try
      {
        PushbackInputStream pbis = new PushbackInputStream(getInputStream());

        int i = pbis.read();
        if (i == -1)
          return null;

        pbis.unread(i);
        return pbis;
      }
    catch (IOException e)
      {
        return null;
      }
  }

  /**
   * Returns the value of the named field parsed as date
   *
   * @param key the key of the header field
   * @param value the default value if the header field is not present
   *
   * @return the value of the header field
   */
  public long getHeaderFieldDate(String key, long value)
  {
    // FIXME: implement this correctly
    // http://www.w3.org/Protocols/HTTP-NG/ng-notes.txt
    return super.getHeaderFieldDate(key, value);
  }
}
