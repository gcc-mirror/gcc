/* HTTPURLConnection.java --
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


package gnu.java.net.protocol.http;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.ProtocolException;
import java.net.URL;
import java.security.AccessController;
import java.security.PrivilegedAction;
import java.security.cert.Certificate;
import java.util.Collections;
import java.util.Date;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;

import javax.net.ssl.HandshakeCompletedEvent;
import javax.net.ssl.HandshakeCompletedListener;
import javax.net.ssl.HostnameVerifier;
import javax.net.ssl.HttpsURLConnection;
import javax.net.ssl.SSLPeerUnverifiedException;
import javax.net.ssl.SSLSocketFactory;

/**
 * A URLConnection that uses the HTTPConnection class.
 *
 * @author Chris Burdess (dog@gnu.org)
 */
public class HTTPURLConnection
  extends HttpsURLConnection
  implements HandshakeCompletedListener
{

  /**
   * Pool of reusable connections, used if keepAlive is true.
   */
  private static final Map connectionPool = new LinkedHashMap();

  /*
   * The underlying connection.
   */
  private HTTPConnection connection;

  private String proxyHostname;
  private int proxyPort;
  private String agent;
  private boolean keepAlive;
  private int maxConnections;

  private Request request;
  private Headers requestHeaders;
  private ByteArrayOutputStream requestSink;
  private boolean requestMethodSetExplicitly;

  private Response response;
  private ByteArrayInputStream responseSink;
  private ByteArrayInputStream errorSink;

  private HandshakeCompletedEvent handshakeEvent;

  /**
   * Constructor.
   * @param url the URL
   */
  public HTTPURLConnection(URL url)
    throws IOException
  {
    super(url);
    requestHeaders = new Headers();
    AccessController.doPrivileged(this.new GetHTTPPropertiesAction());
  }

  class GetHTTPPropertiesAction
    implements PrivilegedAction
  {

    public Object run()
    {
      proxyHostname = System.getProperty("http.proxyHost");
      if (proxyHostname != null && proxyHostname.length() > 0)
        {
          String port = System.getProperty("http.proxyPort");
          if (port != null && port.length() > 0)
            {
              proxyPort = Integer.parseInt(port);
            }
          else
            {
              proxyHostname = null;
              proxyPort = -1;
            }
        }
      agent = System.getProperty("http.agent");
      String ka = System.getProperty("http.keepAlive");
      keepAlive = !(ka != null && "false".equals(ka));
      String mc = System.getProperty("http.maxConnections");
      maxConnections = (mc != null && mc.length() > 0) ?
        Math.max(Integer.parseInt(mc), 1) : 5;
      return null;
    }

  }

  public void connect()
    throws IOException
  {
    if (connected)
      {
        return;
      }
    String protocol = url.getProtocol();
    boolean secure = "https".equals(protocol);
    String host = url.getHost();
    int port = url.getPort();
    if (port < 0)
      {
        port = secure ? HTTPConnection.HTTPS_PORT :
          HTTPConnection.HTTP_PORT;
      }
    String file = url.getFile();
    String username = url.getUserInfo();
    String password = null;
    if (username != null)
      {
        int ci = username.indexOf(':');
        if (ci != -1)
          {
            password = username.substring(ci + 1);
            username = username.substring(0, ci);
          }
      }
    final Credentials creds = (username == null) ? null :
      new Credentials (username, password);
    
    boolean retry;
    do
      {
        retry = false;
        if (connection == null)
          {
            connection = getConnection(host, port, secure);
            if (secure)
              {
                SSLSocketFactory factory = getSSLSocketFactory();
                HostnameVerifier verifier = getHostnameVerifier();
                if (factory != null)
                  {
                    connection.setSSLSocketFactory(factory);
                  }
                connection.addHandshakeCompletedListener(this);
                // TODO verifier
              }
          }
        if (proxyHostname != null)
          {
            if (proxyPort < 0)
              {
                proxyPort = secure ? HTTPConnection.HTTPS_PORT :
                  HTTPConnection.HTTP_PORT;
              }
            connection.setProxy(proxyHostname, proxyPort);
          }
        request = connection.newRequest(method, file);
        if (!keepAlive)
          {
            request.setHeader("Connection", "close");
          }
        if (agent != null)
          {
            request.setHeader("User-Agent", agent);
          }
        request.getHeaders().putAll(requestHeaders);
        if (requestSink != null)
          {
            byte[] content = requestSink.toByteArray();
            RequestBodyWriter writer = new ByteArrayRequestBodyWriter(content);
            request.setRequestBodyWriter(writer);
          }
        ByteArrayResponseBodyReader reader = new ByteArrayResponseBodyReader();
        request.setResponseBodyReader(reader);
        if (creds != null)
          {
            request.setAuthenticator(new Authenticator() {
              public Credentials getCredentials(String realm, int attempts)
              {
                return (attempts < 2) ? creds : null;
              }
            });
          }
        response = request.dispatch();
        if (response.getCodeClass() == 3 && getInstanceFollowRedirects())
          {
            // Follow redirect
            String location = response.getHeader("Location");
            String connectionUri = connection.getURI();
            int start = connectionUri.length();
            if (location.startsWith(connectionUri) &&
                location.charAt(start) == '/')
              {
                file = location.substring(start);
                retry = true;
              }
            else if (location.startsWith("http:"))
              {
                connection.close();
                connection = null;
                secure = false;
                start = 7;
                int end = location.indexOf('/', start);
                host = location.substring(start, end);
                int ci = host.lastIndexOf(':');
                if (ci != -1)
                  {
                    port = Integer.parseInt(host.substring (ci + 1));
                    host = host.substring(0, ci);
                  }
                else
                  {
                    port = HTTPConnection.HTTP_PORT;
                  }
                file = location.substring(end);
                retry = true;
              }
            else if (location.startsWith("https:"))
              {
                connection.close();
                connection = null;
                secure = true;
                start = 8;
                int end = location.indexOf('/', start);
                host = location.substring(start, end);
                int ci = host.lastIndexOf(':');
                if (ci != -1)
                  {
                    port = Integer.parseInt(host.substring (ci + 1));
                    host = host.substring(0, ci);
                  }
                else
                  {
                    port = HTTPConnection.HTTPS_PORT;
                  }
                file = location.substring(end);
                retry = true;
              }
	    else if (location.length() > 0)
	      {
		// Malformed absolute URI, treat as file part of URI
		if (location.charAt(0) == '/')
		  {
		    // Absolute path
		    file = location;
		  }
		else
		  {
		    // Relative path
		    int lsi = file.lastIndexOf('/');
		    file = (lsi == -1) ? "/" : file.substring(0, lsi + 1);
		    file += location;
		  }
		retry = true;
	      }
          }
        else
          {
            responseSink = new ByteArrayInputStream(reader.toByteArray ());
            if (response.getCode() == 404)
              {
                errorSink = responseSink;
                throw new FileNotFoundException(url.toString());
              }
          }
      }
    while (retry);
    connected = true;
  }

  /**
   * Returns a connection, from the pool if necessary.
   */
  HTTPConnection getConnection(String host, int port, boolean secure)
    throws IOException
  {
    HTTPConnection connection;
    if (keepAlive)
      {
        StringBuffer buf = new StringBuffer(secure ? "https://" : "http://");
        buf.append(host);
        buf.append(':');
        buf.append(port);
        String key = buf.toString();
        synchronized (connectionPool)
          {
            connection = (HTTPConnection) connectionPool.get(key);
            if (connection == null)
              {
                connection = new HTTPConnection(host, port, secure);
                // Good housekeeping
                if (connectionPool.size() == maxConnections)
                  {
                    // maxConnections must always be >= 1
                    Object lru = connectionPool.keySet().iterator().next();
                    connectionPool.remove(lru);
                  }
                connectionPool.put(key, connection);
              }
          }
      }
    else
      {
        connection = new HTTPConnection(host, port, secure);
      }
    return connection;
  }

  public void disconnect()
  {
    if (connection != null)
      {
        try
          {
            connection.close();
          }
        catch (IOException e)
          {
          }
      }
  }

  public boolean usingProxy()
  {
    return (proxyHostname != null);
  }

  /**
   * Overrides the corresponding method in HttpURLConnection to permit
   * arbitrary methods, as long as they're valid ASCII alphabetic
   * characters. This is to permit WebDAV and other HTTP extensions to
   * function.
   * @param method the method
   */
  public void setRequestMethod(String method)
    throws ProtocolException
  {
    if (connected)
      {
        throw new ProtocolException("Already connected");
      }
    // Validate
    method = method.toUpperCase();
    int len = method.length();
    if (len == 0)
      {
        throw new ProtocolException("Empty method name");
      }
    for (int i = 0; i < len; i++)
      {
        char c = method.charAt(i);
        if (c < 0x41 || c > 0x5a)
          {
            throw new ProtocolException("Illegal character '" + c +
                                        "' at index " + i);
          }
      }
    // OK
    this.method = method;
    requestMethodSetExplicitly = true;
  }

  public String getRequestProperty(String key)
  {
    return requestHeaders.getValue(key);
  }

  public Map getRequestProperties()
  {
    return requestHeaders;
  }

  public void setRequestProperty(String key, String value)
  {
    requestHeaders.put(key, value);
  }

  public void addRequestProperty(String key, String value)
  {
    String old = requestHeaders.getValue(key);
    if (old == null)
      {
        requestHeaders.put(key, value);
      }
    else
      {
        requestHeaders.put(key, old + "," + value);
      }
  }

  public OutputStream getOutputStream()
    throws IOException
  {
    if (connected)
      {
        throw new ProtocolException("Already connected");
      }
    if (!doOutput)
      {
        throw new ProtocolException("doOutput is false");
      }
    else if (!requestMethodSetExplicitly)
      {
        /*
         * Silently change the method to POST if no method was set
         * explicitly. This is due to broken applications depending on this
         * behaviour (Apache XMLRPC for one).
         */
        method = "POST";
      }
    if (requestSink == null)
      {
        requestSink = new ByteArrayOutputStream();
      }
    return requestSink;
  }
  
  // -- Response --
  
  public InputStream getInputStream()
    throws IOException
  {
    if (!connected)
      {
        connect();
      }
    if (!doInput)
      {
        throw new ProtocolException("doInput is false");
      }
    return responseSink;
  }

  public InputStream getErrorStream()
  {
    return errorSink;
  }

  public Map getHeaderFields()
  {
    if (!connected)
      {
        try
          {
            connect();
          }
        catch (IOException e)
          {
            return null;
          }
      }
    Map headers = response.getHeaders();
    Map ret = new LinkedHashMap();
    ret.put("", Collections.singletonList(getStatusLine(response)));
    for (Iterator i = headers.entrySet().iterator(); i.hasNext(); )
      {
        Map.Entry entry = (Map.Entry) i.next();
        String key = (String) entry.getKey();
        String value = (String) entry.getValue();
        ret.put(key, Collections.singletonList(value));
      }
    return ret;
  }

  String getStatusLine(Response response)
  {
    return "HTTP/" + response.getMajorVersion() +
      "." + response.getMinorVersion() +
      " " + response.getCode() +
      " " + response.getMessage();
  }
  
  public String getHeaderField(int index)
  {
    if (!connected)
      {
        try
          {
            connect();
          }
        catch (IOException e)
          {
            return null;
          }
      }
    if (index == 0)
      {
        return getStatusLine(response);
      }
    Iterator i = response.getHeaders().entrySet().iterator();
    Map.Entry entry;
    int count = 1;
    do
      {
        if (!i.hasNext())
          {
            return null;
          }
        entry = (Map.Entry) i.next();
        count++;
      }
    while (count <= index);
    return (String) entry.getValue();
  }

  public String getHeaderFieldKey(int index)
  {
    if (!connected)
      {
        try
          {
            connect();
          }
        catch (IOException e)
          {
            return null;
          }
      }
    if (index == 0)
      {
        return null;
      }
    Iterator i = response.getHeaders().entrySet().iterator();
    Map.Entry entry;
    int count = 1;
    do
      {
        entry = (Map.Entry) i.next();
        count++;
      }
    while (count <= index);
    return (String) entry.getKey();
  }

  public String getHeaderField(String name)
  {
    if (!connected)
      {
        try
          {
            connect();
          }
        catch (IOException e)
          {
            return null;
          }
      }
    return (String) response.getHeader(name);
  }

  public long getHeaderFieldDate(String name, long def)
  {
    if (!connected)
      {
        try
          {
            connect();
          }
        catch (IOException e)
          {
            return def;
          }
      }
    Date date = response.getDateHeader(name);
    return (date == null) ? def : date.getTime();
  }

  public String getContentType()
  {
    return getHeaderField("Content-Type");
  }

  public int getResponseCode()
    throws IOException
  {
    if (!connected)
      {
        connect();
      }
    return response.getCode();
  }

  public String getResponseMessage()
    throws IOException
  {
    if (!connected)
      {
        connect();
      }
    return response.getMessage();
  }

  // -- HTTPS specific --

  public String getCipherSuite()
  {
    if (!connected)
      {
        throw new IllegalStateException("not connected");
      }
    return handshakeEvent.getCipherSuite();
  }
  
  public Certificate[] getLocalCertificates()
  {
    if (!connected)
      {
        throw new IllegalStateException("not connected");
      }
    return handshakeEvent.getLocalCertificates();
  }

  public Certificate[] getServerCertificates()
    throws SSLPeerUnverifiedException
  {
    if (!connected)
      {
        throw new IllegalStateException("not connected");
      }
    return handshakeEvent.getPeerCertificates();
  }

  // HandshakeCompletedListener

  public void handshakeCompleted(HandshakeCompletedEvent event)
  {
    handshakeEvent = event;
  }

}

