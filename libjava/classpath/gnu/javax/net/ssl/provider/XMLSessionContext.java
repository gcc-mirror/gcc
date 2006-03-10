/* XMLSessionContext.java -- XML-encoded persistent SSL sessions.
   Copyright (C) 2006 Free Software Foundation, Inc.

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

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.IOException;
import java.io.PrintStream;

import java.security.SecureRandom;
import java.security.cert.Certificate;
import java.security.cert.CertificateEncodingException;
import java.security.cert.CertificateFactory;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.TreeSet;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;

import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

import gnu.javax.crypto.mac.IMac;
import gnu.javax.crypto.mac.MacFactory;
import gnu.javax.crypto.mode.IMode;
import gnu.javax.crypto.mode.ModeFactory;
import gnu.javax.crypto.prng.IPBE;
import gnu.java.security.prng.IRandom;
import gnu.java.security.prng.PRNGFactory;

import gnu.javax.net.ssl.Base64;

/**
 * An implementation of session contexts that stores session data on the
 * filesystem in a simple XML-encoded file.
 */
class XMLSessionContext extends SessionContext
{

  // Fields.
  // -------------------------------------------------------------------------

  private final File file;
  private final IRandom pbekdf;
  private final boolean compress;
  private final SecureRandom random;
  private boolean encoding;

  // Constructor.
  // -------------------------------------------------------------------------

  XMLSessionContext() throws IOException, SAXException
  {
    file = new File(Util.getSecurityProperty("jessie.SessionContext.xml.file"));
    String password = Util.getSecurityProperty("jessie.SessionContext.xml.password");
    compress = new Boolean(Util.getSecurityProperty("jessie.SessionContext.xml.compress")).booleanValue();
    if (password == null)
      {
        password = "";
      }
    pbekdf = PRNGFactory.getInstance("PBKDF2-HMAC-SHA1");
    HashMap kdfattr = new HashMap();
    kdfattr.put(IPBE.PASSWORD, password.toCharArray());
    // Dummy salt. This is replaced by a real salt when encoding.
    kdfattr.put(IPBE.SALT, new byte[8]);
    kdfattr.put(IPBE.ITERATION_COUNT, new Integer(1000));
    pbekdf.init(kdfattr);
    encoding = false;
    if (file.exists())
      {
        decode();
      }
    encoding = true;
    random = new SecureRandom ();
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  synchronized boolean addSession(Session.ID sessionId, Session session)
  {
    boolean ret = super.addSession(sessionId, session);
    if (ret && encoding)
      {
        try
          {
            encode();
          }
        catch (IOException ioe)
          {
          }
      }
    return ret;
  }

  synchronized void notifyAccess(Session session)
  {
    try
      {
        encode();
      }
    catch (IOException ioe)
      {
      }
  }

  synchronized boolean removeSession(Session.ID sessionId)
  {
    if (super.removeSession(sessionId))
      {
        try
          {
            encode();
          }
        catch (Exception x)
          {
          }
        return true;
      }
    return false;
  }

  private void decode() throws IOException, SAXException
  {
    SAXParser parser = null;
    try
      {
        parser = SAXParserFactory.newInstance().newSAXParser();
      }
    catch (Exception x)
      {
        throw new Error(x.toString());
      }
    SAXHandler handler = new SAXHandler(this, pbekdf);
    InputStream in = null;
    if (compress)
      in = new GZIPInputStream(new FileInputStream(file));
    else
      in = new FileInputStream(file);
    parser.parse(in, handler);
  }

  private void encode() throws IOException
  {
    IMode cipher = ModeFactory.getInstance("CBC", "AES", 16);
    HashMap cipherAttr = new HashMap();
    IMac mac = MacFactory.getInstance("HMAC-SHA1");
    HashMap macAttr = new HashMap();
    byte[] key = new byte[32];
    byte[] iv = new byte[16];
    byte[] mackey = new byte[20];
    byte[] salt = new byte[8];
    byte[] encryptedSecret = new byte[48];
    cipherAttr.put(IMode.KEY_MATERIAL, key);
    cipherAttr.put(IMode.IV, iv);
    cipherAttr.put(IMode.STATE, new Integer(IMode.ENCRYPTION));
    macAttr.put(IMac.MAC_KEY_MATERIAL, mackey);
    PrintStream out = null;
    if (compress)
      {
        out = new PrintStream(new GZIPOutputStream(new FileOutputStream(file)));
      }
    else
      {
        out = new PrintStream(new FileOutputStream(file));
      }
    out.println("<?xml version=\"1.0\"?>");
    out.println("<!DOCTYPE sessions [");
    out.println("  <!ELEMENT sessions (session*)>");
    out.println("  <!ATTLIST sessions size CDATA \"0\">");
    out.println("  <!ATTLIST sessions timeout CDATA \"86400\">");
    out.println("  <!ELEMENT session (peer, certificates?, secret)>");
    out.println("  <!ATTLIST session id CDATA #REQUIRED>");
    out.println("  <!ATTLIST session protocol (SSLv3|TLSv1|TLSv1.1) #REQUIRED>");
    out.println("  <!ATTLIST session suite CDATA #REQUIRED>");
    out.println("  <!ATTLIST session created CDATA #REQUIRED>");
    out.println("  <!ATTLIST session timestamp CDATA #REQUIRED>");
    out.println("  <!ELEMENT peer (certificates?)>");
    out.println("  <!ATTLIST peer host CDATA #REQUIRED>");
    out.println("  <!ELEMENT certificates (#PCDATA)>");
    out.println("  <!ATTLIST certificates type CDATA \"X.509\">");
    out.println("  <!ELEMENT secret (#PCDATA)>");
    out.println("  <!ATTLIST secret salt CDATA #REQUIRED>");
    out.println("]>");
    out.println();
    out.print("<sessions size=\"");
    out.print(cacheSize);
    out.print("\" timeout=\"");
    out.print(timeout);
    out.println("\">");
    for (Iterator it = sessions.entrySet().iterator(); it.hasNext(); )
      {
        Map.Entry entry = (Map.Entry) it.next();
        Session.ID id = (Session.ID) entry.getKey();
        Session session = (Session) entry.getValue();
        if (!session.valid)
          {
            continue;
          }
        out.print("<session id=\"");
        out.print(Base64.encode(id.getId(), 0));
        out.print("\" suite=\"");
        out.print(session.getCipherSuite());
        out.print("\" protocol=\"");
        out.print(session.getProtocol());
        out.print("\" created=\"");
        out.print(session.getCreationTime());
        out.print("\" timestamp=\"");
        out.print(session.getLastAccessedTime());
        out.println("\">");
        out.print("<peer host=\"");
        out.print(session.getPeerHost());
        out.println("\">");
        Certificate[] certs = session.getPeerCertificates();
        if (certs != null && certs.length > 0)
          {
            out.print("<certificates type=\"");
            out.print(certs[0].getType());
            out.println("\">");
            for (int i = 0; i < certs.length; i++)
              {
                out.println("-----BEGIN CERTIFICATE-----");
                try
                  {
                    out.print(Base64.encode(certs[i].getEncoded(), 70));
                  }
                catch (CertificateEncodingException cee)
                  {
                    throw new IOException(cee.toString());
                  }
                out.println("-----END CERTIFICATE-----");
              }
            out.println("</certificates>");
          }
        out.println("</peer>");
        certs = session.getLocalCertificates();
        if (certs != null && certs.length > 0)
          {
            out.print("<certificates type=\"");
            out.print(certs[0].getType());
            out.println("\">");
            for (int i = 0; i < certs.length; i++)
              {
                out.println("-----BEGIN CERTIFICATE-----");
                try
                  {
                    out.print(Base64.encode(certs[i].getEncoded(), 70));
                  }
                catch (CertificateEncodingException cee)
                  {
                    throw new IOException(cee.toString());
                  }
                out.println("-----END CERTIFICATE-----");
              }
            out.println("</certificates>");
          }
        random.nextBytes (salt);
        pbekdf.init(Collections.singletonMap(IPBE.SALT, salt));
        try
          {
            pbekdf.nextBytes(key, 0, key.length);
            pbekdf.nextBytes(iv, 0, iv.length);
            pbekdf.nextBytes(mackey, 0, mackey.length);
            cipher.reset();
            cipher.init(cipherAttr);
            mac.init(macAttr);
          }
        catch (Exception ex)
          {
            throw new Error(ex.toString());
          }
        for (int i = 0; i < session.masterSecret.length; i += 16)
          {
            cipher.update(session.masterSecret, i, encryptedSecret, i);
          }
        mac.update(encryptedSecret, 0, encryptedSecret.length);
        byte[] macValue = mac.digest();
        out.print("<secret salt=\"");
        out.print(Base64.encode(salt, 0));
        out.println("\">");
        out.print(Base64.encode(Util.concat(encryptedSecret, macValue), 70));
        out.println("</secret>");
        out.println("</session>");
      }
    out.println("</sessions>");
    out.close();
  }

  // Inner class.
  // -------------------------------------------------------------------------

  private class SAXHandler extends DefaultHandler
  {

    // Field.
    // -----------------------------------------------------------------------

    private SessionContext context;
    private Session current;
    private IRandom pbekdf;
    private StringBuffer buf;
    private String certType;
    private int state;
    private IMode cipher;
    private HashMap cipherAttr;
    private IMac mac;
    private HashMap macAttr;
    private byte[] key;
    private byte[] iv;
    private byte[] mackey;

    private static final int START      = 0;
    private static final int SESSIONS   = 1;
    private static final int SESSION    = 2;
    private static final int PEER       = 3;
    private static final int PEER_CERTS = 4;
    private static final int CERTS      = 5;
    private static final int SECRET     = 6;

    // Constructor.
    // -----------------------------------------------------------------------

    SAXHandler(SessionContext context, IRandom pbekdf)
    {
      this.context = context;
      this.pbekdf = pbekdf;
      buf = new StringBuffer();
      state = START;
      cipher = ModeFactory.getInstance("CBC", "AES", 16);
      cipherAttr = new HashMap();
      mac = MacFactory.getInstance("HMAC-SHA1");
      macAttr = new HashMap();
      key = new byte[32];
      iv = new byte[16];
      mackey = new byte[20];
      cipherAttr.put(IMode.KEY_MATERIAL, key);
      cipherAttr.put(IMode.IV, iv);
      cipherAttr.put(IMode.STATE, new Integer(IMode.DECRYPTION));
      macAttr.put(IMac.MAC_KEY_MATERIAL, mackey);
    }

    // Instance methods.
    // -----------------------------------------------------------------------

    public void startElement(String u, String n, String qname, Attributes attr)
      throws SAXException
    {
      qname = qname.toLowerCase();
      switch (state)
        {
        case START:
          if (qname.equals("sessions"))
            {
              try
                {
                  timeout = Integer.parseInt(attr.getValue("timeout"));
                  cacheSize = Integer.parseInt(attr.getValue("size"));
                  if (timeout <= 0 || cacheSize < 0)
                    throw new SAXException("timeout or cache size out of range");
                }
              catch (NumberFormatException nfe)
                {
                  throw new SAXException(nfe);
                }
              state = SESSIONS;
            }
          else
            throw new SAXException("expecting sessions");
          break;

        case SESSIONS:
          if (qname.equals("session"))
            {
              try
                {
                  current = new Session(Long.parseLong(attr.getValue("created")));
                  current.enabledSuites = new ArrayList(SSLSocket.supportedSuites);
                  current.enabledProtocols = new TreeSet(SSLSocket.supportedProtocols);
                  current.context = context;
                  current.sessionId = new Session.ID(Base64.decode(attr.getValue("id")));
                  current.setLastAccessedTime(Long.parseLong(attr.getValue("timestamp")));
                }
              catch (Exception ex)
                {
                  throw new SAXException(ex);
                }
              String prot = attr.getValue("protocol");
              if (prot.equals("SSLv3"))
                current.protocol = ProtocolVersion.SSL_3;
              else if (prot.equals("TLSv1"))
                current.protocol = ProtocolVersion.TLS_1;
              else if (prot.equals("TLSv1.1"))
                current.protocol = ProtocolVersion.TLS_1_1;
              else
                throw new SAXException("bad protocol: " + prot);
              current.cipherSuite = CipherSuite.forName(attr.getValue("suite"));
              state = SESSION;
            }
          else
            throw new SAXException("expecting session");
          break;

        case SESSION:
          if (qname.equals("peer"))
            {
              current.peerHost = attr.getValue("host");
              state = PEER;
            }
          else if (qname.equals("certificates"))
            {
              certType = attr.getValue("type");
              state = CERTS;
            }
          else if (qname.equals("secret"))
            {
              byte[] salt = null;
              try
                {
                  salt = Base64.decode(attr.getValue("salt"));
                }
              catch (IOException ioe)
                {
                  throw new SAXException(ioe);
                }
              pbekdf.init(Collections.singletonMap(IPBE.SALT, salt));
              state = SECRET;
            }
          else
            throw new SAXException("bad element: " + qname);
          break;

        case PEER:
          if (qname.equals("certificates"))
            {
              certType = attr.getValue("type");
              state = PEER_CERTS;
            }
          else
            throw new SAXException("bad element: " + qname);
          break;

        default:
          throw new SAXException("bad element: " + qname);
        }
    }

    public void endElement(String uri, String name, String qname)
      throws SAXException
    {
      qname = qname.toLowerCase();
      switch (state)
        {
        case SESSIONS:
          if (qname.equals("sessions"))
            state = START;
          else
            throw new SAXException("expecting sessions");
          break;

        case SESSION:
          if (qname.equals("session"))
            {
              current.valid = true;
              context.addSession(current.sessionId, current);
              state = SESSIONS;
            }
          else
            throw new SAXException("expecting session");
          break;

        case PEER:
          if (qname.equals("peer"))
            state = SESSION;
          else
            throw new SAXException("unexpected element: " + qname);
          break;

        case PEER_CERTS:
          if (qname.equals("certificates"))
            {
              try
                {
                  CertificateFactory fact = CertificateFactory.getInstance(certType);
                  current.peerCerts = (Certificate[])
                    fact.generateCertificates(new ByteArrayInputStream(
                      buf.toString().getBytes())).toArray(new Certificate[0]);
                }
              catch (Exception ex)
                {
                  throw new SAXException(ex);
                }
              current.peerVerified = true;
              state = PEER;
            }
          else
            throw new SAXException("unexpected element: " + qname);
          break;

        case CERTS:
          if (qname.equals("certificates"))
            {
              try
                {
                  CertificateFactory fact = CertificateFactory.getInstance(certType);
                  current.localCerts = (Certificate[])
                    fact.generateCertificates(new ByteArrayInputStream(
                      buf.toString().getBytes())).toArray(new Certificate[0]);
                }
              catch (Exception ex)
                {
                  throw new SAXException(ex);
                }
              state = SESSION;
            }
          else
            throw new SAXException("unexpected element: " + qname);
          break;

        case SECRET:
          if (qname.equals("secret"))
            {
              byte[] encrypted = null;
              try
                {
                  encrypted = Base64.decode(buf.toString());
                  if (encrypted.length != 68)
                    throw new IOException("encrypted secret not 68 bytes long");
                  pbekdf.nextBytes(key, 0, key.length);
                  pbekdf.nextBytes(iv, 0, iv.length);
                  pbekdf.nextBytes(mackey, 0, mackey.length);
                  cipher.reset();
                  cipher.init(cipherAttr);
                  mac.init(macAttr);
                }
              catch (Exception ex)
                {
                  throw new SAXException(ex);
                }
              mac.update(encrypted, 0, 48);
              byte[] macValue = mac.digest();
              for (int i = 0; i < macValue.length; i++)
                {
                  if (macValue[i] != encrypted[48+i])
                    throw new SAXException("MAC mismatch");
                }
              current.masterSecret = new byte[48];
              for (int i = 0; i < current.masterSecret.length; i += 16)
                {
                  cipher.update(encrypted, i, current.masterSecret, i);
                }
              state = SESSION;
            }
          else
            throw new SAXException("unexpected element: " + qname);
          break;

        default:
          throw new SAXException("unexpected element: " + qname);
        }
      buf.setLength(0);
    }

    public void characters(char[] ch, int off, int len) throws SAXException
    {
      if (state != CERTS && state != PEER_CERTS && state != SECRET)
        {
          throw new SAXException("illegal character data");
        }
      buf.append(ch, off, len);
    }
  }
}
