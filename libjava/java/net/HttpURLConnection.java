// HttpURLConnection.java - Subclass of communications links using
//			Hypertext Transfer Protocol.

/* Copyright (C) 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.net;

import java.io.*;

/**
 * @author Warren Levy <warrenl@cygnus.com>
 * @date March 29, 1999.
 */

/**
 * Written using on-line Java Platform 1.2 API Specification, as well
 * as "The Java Class Libraries", 2nd edition (Addison-Wesley, 1998).
 * Status:  Believed complete and correct.
 */

public abstract class HttpURLConnection extends URLConnection
{
  /* HTTP Success Response Codes */
  public static final int HTTP_OK		= 200;
  public static final int HTTP_CREATED		= 201;
  public static final int HTTP_ACCEPTED 	= 202;
  public static final int HTTP_NOT_AUTHORITATIVE = 203;
  public static final int HTTP_NO_CONTENT	= 204;
  public static final int HTTP_RESET		= 205;
  public static final int HTTP_PARTIAL		= 206;

  /* HTTP Redirection Response Codes */
  public static final int HTTP_MULT_CHOICE	= 300;
  public static final int HTTP_MOVED_PERM	= 301;
  public static final int HTTP_MOVED_TEMP	= 302;
  public static final int HTTP_SEE_OTHER	= 303;
  public static final int HTTP_NOT_MODIFIED	= 304;
  public static final int HTTP_USE_PROXY	= 305;

  /* HTTP Client Error Response Codes */
  public static final int HTTP_BAD_REQUEST	= 400;
  public static final int HTTP_UNAUTHORIZED	= 401;
  public static final int HTTP_PAYMENT_REQUIRED	= 402;
  public static final int HTTP_FORBIDDEN	= 403;
  public static final int HTTP_NOT_FOUND	= 404;
  public static final int HTTP_BAD_METHOD	= 405;
  public static final int HTTP_NOT_ACCEPTABLE	= 406;
  public static final int HTTP_PROXY_AUTH	= 407;
  public static final int HTTP_CLIENT_TIMEOUT	= 408;
  public static final int HTTP_CONFLICT		= 409;
  public static final int HTTP_GONE		= 410;
  public static final int HTTP_LENGTH_REQUIRED	= 411;
  public static final int HTTP_PRECON_FAILED	= 412;
  public static final int HTTP_ENTITY_TOO_LARGE	= 413;
  public static final int HTTP_REQ_TOO_LONG	= 414;
  public static final int HTTP_UNSUPPORTED_TYPE	= 415;

  /* HTTP Server Error Response Codes */
  public static final int HTTP_SERVER_ERROR	= 500;
  public static final int HTTP_INTERNAL_ERROR	= 501;
  public static final int HTTP_BAD_GATEWAY	= 502;
  public static final int HTTP_UNAVAILABLE	= 503;
  public static final int HTTP_GATEWAY_TIMEOUT	= 504;
  public static final int HTTP_VERSION		= 505;

  protected String method = "GET";
  protected int responseCode = -1;
  protected String responseMessage;

  static boolean followRedirects = true;

  protected HttpURLConnection(URL url)
  {
    super(url);
  }

  public abstract void disconnect();

  public abstract boolean usingProxy();

  public static void setFollowRedirects(boolean set)
  {
    // Throw an exception if an extant security mgr precludes
    // setting the factory.
    SecurityManager s = System.getSecurityManager();
    if (s != null)
      s.checkSetFactory();

    followRedirects = set;
  }

  public static boolean getFollowRedirects()
  {
    return followRedirects;
  }

  public void setRequestMethod(String method) throws ProtocolException
  {
    if (connected)
      throw new ProtocolException("Already connected");

    if (method.equals("GET") || method.equals("POST") ||
	method.equals("HEAD") || method.equals("OPTIONS") ||
	method.equals("PUT") || method.equals("DELETE") ||
	method.equals("TRACE"))
      this.method = method;
    else
      throw new ProtocolException("Invalid HTTP request method");
  }

  public String getRequestMethod()
  {
    return method;
  }

  public int getResponseCode() throws IOException
  {
    getResponseVals();
    return responseCode;
  }

  public String getResponseMessage() throws IOException
  {
    getResponseVals();
    return responseMessage;
  }

  private void getResponseVals() throws IOException
  {
    // Response is the first header received from the connection.
    String respField = getHeaderField(0);
    if (! respField.startsWith("HTTP/"))
      {
	// Set to default values on failure.
        responseCode = -1;
	responseMessage = null;
	return;
      }

    int firstSpc, nextSpc;
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
    if (responseCode == HTTP_NOT_FOUND)
      throw new FileNotFoundException(url.toString());
    else if (responseCode >= 400)
      throw new IOException(url.toString() + " " + respField);
  }

  // TODO12: public Permission getPermission() throws IOException
  // {
  // }

  // TODO12: public InputStream getErrorStream()
  // {
  // }
}
