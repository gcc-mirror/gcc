/* Session.java -- SSL and TLS session data.
   Copyright (C) 2006  Free Software Foundation, Inc.

This file is a part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301
USA

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
exception statement from your version.  */


package gnu.javax.net.ssl.provider;

import java.security.SecureRandom;
import java.security.cert.Certificate;
import java.security.cert.CertificateException;

import java.util.Arrays;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

import javax.net.ssl.SSLPeerUnverifiedException;
import javax.net.ssl.SSLPermission;
import javax.net.ssl.SSLSession;
import javax.net.ssl.SSLSessionBindingEvent;
import javax.net.ssl.SSLSessionBindingListener;
import javax.net.ssl.SSLSessionContext;
import javax.net.ssl.X509KeyManager;
import javax.net.ssl.X509TrustManager;
import javax.security.cert.X509Certificate;

import gnu.javax.net.ssl.SRPTrustManager;

/**
 * A generic SSL session implementation for SSL and TLS.
 */
final class Session implements SSLSession
{

  // Constants and fields.
  // -------------------------------------------------------------------------

  private static final SSLPermission GET_SESSION_CONTEXT_PERMISSION =
    new SSLPermission("getSSLSessionContext");

  private final long creationTime;
  private Date lastAccessedTime;
  ID sessionId;
  Certificate[] localCerts;
  Certificate[] peerCerts;
  X509Certificate[] peerCertChain;
  String peerHost;
  boolean peerVerified;
  SessionContext context;
  HashMap values;
  boolean valid;
  List enabledSuites;
  CipherSuite cipherSuite;
  SortedSet enabledProtocols;
  ProtocolVersion protocol;
  byte[] masterSecret;
  SRPTrustManager srpTrustManager;
  X509TrustManager trustManager;
  X509KeyManager keyManager;
  SecureRandom random;
  SecurityParameters params;
  Alert currentAlert;

  // Constructor.
  // -------------------------------------------------------------------------

  Session()
  {
    this(System.currentTimeMillis());
  }

  Session(long creationTime)
  {
    peerVerified = false;
    valid = true;
    this.creationTime = creationTime;
    lastAccessedTime = new Date(0L);
    values = new HashMap();
    if (("true").equalsIgnoreCase (Util.getSecurityProperty ("jessie.with.jce")))
      params = new JCESecurityParameters();
    else
      params = new GNUSecurityParameters (this);
  }

  // Public instance methods.
  // -------------------------------------------------------------------------

  protected Object clone()
  {
    Session result = new Session(creationTime);
    result.lastAccessedTime = lastAccessedTime;
    result.sessionId = sessionId;
    result.localCerts = (localCerts != null ? (Certificate[]) localCerts.clone() : null);
    result.peerCerts = (peerCerts != null ? (Certificate[]) peerCerts.clone() : null);
    result.peerHost = peerHost;
    result.peerVerified = peerVerified;
    result.context = context;
    result.values = values;
    result.enabledSuites = new ArrayList(enabledSuites);
    result.cipherSuite = cipherSuite;
    result.enabledProtocols = new TreeSet(enabledProtocols);
    result.protocol = protocol;
    result.masterSecret = masterSecret;
    result.keyManager = keyManager;
    result.srpTrustManager = srpTrustManager;
    result.trustManager = trustManager;
    result.random = random;
    return result;
  }

  public String getCipherSuite()
  {
    return cipherSuite.toString();
  }

  public long getCreationTime()
  {
    return creationTime;
  }

  public byte[] getId()
  {
    return (sessionId != null ? sessionId.getId() : null);
  }

  public long getLastAccessedTime()
  {
    return lastAccessedTime.getTime();
  }

  public Certificate[] getLocalCertificates()
  {
    return (Certificate[]) (localCerts != null ? localCerts.clone() : null);
  }

  public Certificate[] getPeerCertificates() throws SSLPeerUnverifiedException
  {
    if (!peerVerified)
      {
        throw new SSLPeerUnverifiedException("peer not verified");
      }
    return (Certificate[]) (peerCerts != null ? peerCerts.clone() : null);
  }

  public X509Certificate[] getPeerCertificateChain()
    throws SSLPeerUnverifiedException
  {
    if (!peerVerified)
      {
        throw new SSLPeerUnverifiedException("peer not verified");
      }
    if (peerCerts == null)
      {
        return null;
      }
    if (peerCertChain != null)
      {
        return (X509Certificate[]) peerCertChain.clone();
      }
    try
      {
        peerCertChain = new X509Certificate[peerCerts.length];
        for (int i = 0; i < peerCerts.length; i++)
          {
            peerCertChain[i] = X509Certificate.getInstance(peerCerts[i].getEncoded());
          }
        return (X509Certificate[]) peerCertChain.clone();
      }
    catch (javax.security.cert.CertificateException ce)
      {
        return null;
      }
    catch (CertificateException ce2)
      {
        return null;
      }
  }

  public String getPeerHost()
  {
    return peerHost;
  }

  public String getProtocol()
  {
    return protocol.toString();
  }

  public SSLSessionContext getSessionContext()
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      {
        sm.checkPermission(GET_SESSION_CONTEXT_PERMISSION);
      }
    return context;
  }

  public String[] getValueNames()
  {
    Set names = values.keySet();
    return (String[]) names.toArray(new String[names.size()]);
  }

  public Object getValue(String name)
  {
    return values.get(name);
  }

  public void putValue(String name, Object value)
  {
    values.put(name, value);
    if (value instanceof SSLSessionBindingListener)
      {
        ((SSLSessionBindingListener) value).valueBound(
          new SSLSessionBindingEvent(this, name));
      }
  }

  public void removeValue(String name)
  {
    Object value = values.remove(name);
    if (value != null && (value instanceof SSLSessionBindingListener))
      {
        ((SSLSessionBindingListener) value).valueUnbound(
          new SSLSessionBindingEvent(this, name));
      }
  }

  public void invalidate()
  {
    if (masterSecret != null)
      {
        for (int i = 0; i < masterSecret.length; i++)
          {
            masterSecret[i] = 0;
          }
        masterSecret = null;
      }
    valid = false;
  }

  synchronized void access()
  {
    lastAccessedTime.setTime(System.currentTimeMillis());
    context.notifyAccess(this);
  }

  void setLastAccessedTime(long lastAccessedTime)
  {
    this.lastAccessedTime.setTime(lastAccessedTime);
  }

  // Inner classes.
  // -------------------------------------------------------------------------

  /**
   * A byte array with appropriate <code>equals()</code>,
   * <code>hashCode()</code>, and <code>compareTo()</code> semantics.
   */
  static final class ID implements Comparable
  {

    // Fields.
    // -----------------------------------------------------------------------

    /** The ID itself. */
    private final byte[] id;

    // Constructor.
    // -----------------------------------------------------------------------

    /**
     * Creates a new ID.
     *
     * @param id The ID. The array is not cloned.
     */
    ID(byte[] id)
    {
      if (id == null)
        {
          throw new IllegalArgumentException();
        }
      this.id = id;
    }

    // Instance methods.
    // -----------------------------------------------------------------------

    public byte[] getId()
    {
      return (byte[]) id.clone();
    }

    public boolean equals(Object other)
    {
      if (other == null || !(other instanceof ID))
        {
          return false;
        }
      return Arrays.equals(id, ((ID) other).id);
    }

    public int hashCode()
    {
      int code = 0;
      for (int i = 0; i < id.length; i++)
        {
          code |= (id[i] & 0xFF) << ((i & 3) << 3);
        }
      return code;
    }

    public int compareTo(Object other)
    {
      if (other == null || !(other instanceof ID))
        {
          return 1;
        }
      byte[] id2 = ((ID) other).id;
      if (id.length != id2.length)
        {
          return (id.length < id2.length) ? -1 : 1;
        }
      for (int i = 0; i < id.length; i++)
        {
          if (id[i] < id2[i])
            {
              return -1;
            }
          else if (id[i] > id2[i])
            {
              return 1;
            }
        }
      return 0;
    }

    public String toString()
    {
      return Util.toHexString(id, ':');
    }
  }
}
