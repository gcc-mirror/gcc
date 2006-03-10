/* JDBCSessionContext.java -- database persistent sessions.
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

import java.io.ByteArrayOutputStream;
import java.io.InputStream;

import java.security.SecureRandom;
import java.security.cert.Certificate;
import java.security.cert.CertificateFactory;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.Timestamp;
import java.sql.Types;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.Enumeration;
import java.util.TreeSet;
import java.util.Vector;

import javax.net.ssl.SSLSession;

/**
 * The SQL table this class stores sessions in, called <tt>SESSIONS</tt>,
 * looks like this:
 *
 * <blockquote><pre>
 * TABLE SESSIONS (
 *   ID             VARBINARY(32) PRIMARY KEY UNIQUE NOT NULL,
 *   CREATED        TIMESTAMP NOT NULL,
 *   LAST_ACCESSED  TIMESTAMP NOT NULL,
 *   PROTOCOL       VARCHAR(7) NOT NULL,
 *   SUITE          VARCHAR(255) NOT NULL,
 *   PEER_HOST      TEXT NOT NULL,
 *   PEER_CERT_TYPE VARCHAR(32),
 *   PEER_CERTS     BLOB,
 *   CERT_TYPE      VARCHAR(32),
 *   CERTS          BLOB,
 *   SECRET         VARBINARY(48) NOT NULL
 * )
 * </pre></blockquote>
 *
 * <p>Note that the master secret for sessions is not protected before
 * being inserted into the database; it is up to the system to protect
 * the stored data from unauthorized access.
 */
class JDBCSessionContext extends SessionContext
{

  // Fields.
  // -------------------------------------------------------------------------

  protected Connection connection;
  protected PreparedStatement selectById;
  protected PreparedStatement insert;
  protected PreparedStatement selectTimestamp;
  protected PreparedStatement updateTimestamp;
  protected PreparedStatement deleteSession;

  // Constructor.
  // -------------------------------------------------------------------------

  JDBCSessionContext() throws SQLException
  {
    String url = Util.getSecurityProperty("jessie.SessionContext.jdbc.url");
    String user = Util.getSecurityProperty("jessie.SessionContext.jdbc.user");
    String passwd = Util.getSecurityProperty("jessie.SessionContext.jdbc.password");
    if (url == null)
      {
        throw new IllegalArgumentException("no JDBC URL");
      }
    if (user == null || passwd == null)
      {
        connection = DriverManager.getConnection(url);
      }
    else
      {
        connection = DriverManager.getConnection(url, user, passwd);
      }
    selectById =
      connection.prepareStatement("SELECT * FROM SESSIONS WHERE ID = ?");
    insert = connection.prepareStatement("INSERT INTO SESSIONS VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)");
    selectTimestamp =
      connection.prepareStatement("SELECT CREATED FROM SESSIONS WHERE ID = ?");
    updateTimestamp =
      connection.prepareStatement("UPDATE SESSIONS SET LAST_ACCESSED = ? WHERE ID = ?");
    deleteSession =
      connection.prepareStatement("DELETE FROM SESSIONS WHERE ID = ?");
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  public synchronized Enumeration getIds()
  {
    Vector ids = new Vector();
    try
      {
        Statement stmt = connection.createStatement();
        ResultSet rs = stmt.executeQuery("SELECT ID FROM SESSIONS");
        while (rs.next())
          {
            byte[] id = rs.getBytes("ID");
            ids.add(id);
          }
      }
    catch (SQLException sqle)
      {
      }
    return ids.elements();
  }

  public synchronized SSLSession getSession(byte[] sessionId)
  {
    Session session = (Session) super.getSession(sessionId);
    if (session == null)
      {
        try
          {
            selectById.setBytes(1, sessionId);
            ResultSet rs = selectById.executeQuery();
            if (rs.next())
              {
                session = new Session(rs.getTimestamp("CREATED").getTime());
                session.enabledSuites = new ArrayList(SSLSocket.supportedSuites);
                session.enabledProtocols = new TreeSet(SSLSocket.supportedProtocols);
                session.random = new SecureRandom();
                session.context = this;
                session.sessionId = new Session.ID(rs.getBytes("ID"));
                session.setLastAccessedTime(rs.getTimestamp("LAST_ACCESSED").getTime());
                long elapsed = System.currentTimeMillis() - session.getLastAccessedTime();
                if ((int) (elapsed / 1000L) > timeout)
                  {
                    removeSession(session.sessionId);
                    return null;
                  }
                session.peerHost = rs.getString("PEER_HOST");
                String protocol = rs.getString("PROTOCOL");
                if (protocol.equals("SSLv3"))
                  {
                    session.protocol = ProtocolVersion.SSL_3;
                  }
                else if (protocol.equals("TLSv1"))
                  {
                    session.protocol = ProtocolVersion.TLS_1;
                  }
                else if (protocol.equals("TLSv1.1"))
                  {
                    session.protocol = ProtocolVersion.TLS_1_1;
                  }
                else
                  {
                    return null;
                  }
                session.cipherSuite = CipherSuite.forName(rs.getString("SUITE"));
                String type = rs.getString("PEER_CERT_TYPE");
                boolean wasNull = rs.wasNull();
                InputStream certs = null;
                if (!wasNull)
                  {
                    certs = rs.getBinaryStream("PEER_CERTS");
                    wasNull = rs.wasNull();
                  }
                if (!wasNull)
                  {
                    CertificateFactory cf = CertificateFactory.getInstance(type);
                    session.peerCerts = (Certificate[])
                      cf.generateCertificates(certs).toArray(new Certificate[0]);
                    session.peerVerified = true;
                  }
                type = rs.getString("CERT_TYPE");
                wasNull = rs.wasNull();
                if (!wasNull)
                  {
                    certs = rs.getBinaryStream("CERTS");
                    wasNull = rs.wasNull();
                  }
                if (!wasNull)
                  {
                    CertificateFactory cf = CertificateFactory.getInstance(type);
                    session.localCerts = (Certificate[])
                      cf.generateCertificates(certs).toArray(new Certificate[0]);
                  }
                session.masterSecret = rs.getBytes("SECRET");
                if (cacheSize == 0 || sessions.size() < cacheSize)
                  {
                    sessions.put(session.sessionId, session);
                  }
              }
          }
        catch (Exception ex)
          {
          }
      }
    return session;
  }

  synchronized boolean addSession(Session.ID id, Session s)
  {
    if (containsSessionID(id))
      {
        return false;
      }
    try
      {
        insert.setBytes(1, id.getId());
        insert.setTimestamp(2, new Timestamp(s.getCreationTime()));
        insert.setTimestamp(3, new Timestamp(s.getLastAccessedTime()));
        insert.setString(4, s.getProtocol());
        insert.setString(5, s.getCipherSuite());
        insert.setString(6, s.peerHost);
        if (s.peerCerts != null && s.peerCerts.length > 0)
          {
            insert.setString(7, s.peerCerts[0].getType());
            insert.setBytes(8, certs(s.peerCerts));
          }
        else
          {
            insert.setNull(7, Types.VARCHAR);
            insert.setNull(8, Types.LONGVARBINARY);
          }
        if (s.localCerts != null && s.localCerts.length > 0)
          {
            insert.setString(9, s.localCerts[0].getType());
            insert.setBytes(10, certs(s.localCerts));
          }
        else
          {
            insert.setNull(9,  Types.VARCHAR);
            insert.setNull(10, Types.LONGVARBINARY);
          }
        insert.setBytes(11, s.masterSecret);
        insert.executeUpdate();
        super.addSession(id, s);
      }
    catch (SQLException sqle)
      {
        return false;
      }
    return true;
  }

  synchronized boolean containsSessionID(Session.ID sessionId)
  {
    try
      {
        selectTimestamp.setBytes(1, sessionId.getId());
        ResultSet rs = selectTimestamp.executeQuery();
        if (!rs.next())
          {
            return false;
          }
        Timestamp ts = rs.getTimestamp("CREATED");
        if (rs.wasNull())
          {
            return false;
          }
        long elapsed = System.currentTimeMillis() - ts.getTime();
        if ((int) (elapsed / 1000) > timeout)
          {
            removeSession(sessionId);
            return false;
          }
        return true;
      }
    catch (SQLException sqle)
      {
        return false;
      }
  }

  protected boolean removeSession(Session.ID sessionId)
  {
    super.removeSession(sessionId);
    try
      {
        deleteSession.setBytes(1, sessionId.getId());
        return deleteSession.executeUpdate() > 0;
      }
    catch (SQLException sqle)
      {
      }
    return false;
  }

  synchronized void notifyAccess(Session session)
  {
    try
      {
        updateTimestamp.setTimestamp(1, new Timestamp(session.getLastAccessedTime()));
        updateTimestamp.setBytes(2, session.getId());
        updateTimestamp.executeUpdate();
      }
    catch (SQLException sqle)
      {
      }
  }

  private byte[] certs(Certificate[] certs)
  {
    ByteArrayOutputStream out = new ByteArrayOutputStream(2048);
    for (int i = 0; i < certs.length; i++)
      {
        try
          {
            out.write(certs[i].getEncoded());
          }
        catch (Exception x)
          {
          }
      }
    return out.toByteArray();
  }
}
