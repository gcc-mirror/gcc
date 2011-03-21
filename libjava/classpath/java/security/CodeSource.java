/* CodeSource.java -- Code location and certifcates
   Copyright (C) 1998, 2002, 2004  Free Software Foundation, Inc.

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


package java.security;

import gnu.java.lang.CPStringBuilder;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.net.SocketPermission;
import java.net.URL;
// Note that this overrides Certificate in this package.
import java.security.cert.Certificate;
import java.security.cert.CertificateEncodingException;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;

/**
 * This class represents a location from which code is loaded (as
 * represented by a URL), and the list of certificates that are used to
 * check the signatures of signed code loaded from this source.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @author Eric Blake (ebb9@email.byu.edu)
 * @since 1.1
 * @status updated to 1.4
 */
public class CodeSource implements Serializable
{
  /**
   * Compatible with JDK 1.1+.
   */
  private static final long serialVersionUID = 4977541819976013951L;

  /**
   * This is the URL that represents the code base from which code will
   * be loaded.
   *
   * @serial the code location
   */
  private final URL location;

  /** The set of certificates for this code base. */
  private transient HashSet certs;

  /**
   * This creates a new instance of <code>CodeSource</code> that loads code
   * from the specified URL location and which uses the specified certificates
   * for verifying signatures.
   *
   * @param location the location from which code will be loaded
   * @param certs the list of certificates
   */
  public CodeSource(URL location, Certificate[] certs)
  {
    this.location = location;
    if (certs != null)
      this.certs = new HashSet(Arrays.asList(certs));
  }

  /**
   * This method returns a hash value for this object.
   *
   * @return a hash value for this object
   */
  public int hashCode()
  {
    return (location == null ? 0 : location.hashCode())
      ^ (certs == null ? 0 : certs.hashCode());
  }

  /**
   * This method tests the specified <code>Object</code> for equality with
   * this object.  This will be true if and only if the locations are equal
   * and the certificate sets are identical (ignoring order).
   *
   * @param obj the <code>Object</code> to test against
   * @return true if the specified object is equal to this one
   */
  public boolean equals(Object obj)
  {
    if (! (obj instanceof CodeSource))
      return false;
    CodeSource cs = (CodeSource) obj;
    return (certs == null ? cs.certs == null : certs.equals(cs.certs))
      && (location == null ? cs.location == null
          : location.equals(cs.location));
  }

  /**
   * This method returns the URL specifying the location from which code
   * will be loaded under this <code>CodeSource</code>.
   *
   * @return the code location for this <code>CodeSource</code>
   */
  public final URL getLocation()
  {
    return location;
  }

  /**
   * This method returns the list of digital certificates that can be used
   * to verify the signatures of code loaded under this
   * <code>CodeSource</code>.
   *
   * @return the certifcate list for this <code>CodeSource</code>
   */
  public final Certificate[] getCertificates()
  {
    if (certs == null)
      return null;
    Certificate[] c = new Certificate[certs.size()];
    certs.toArray(c);
    return c;
  }

  /**
   * This method tests to see if a specified <code>CodeSource</code> is
   * implied by this object.  Effectively, to meet this test, the specified
   * object must have all the certifcates this object has (but may have more),
   * and must have a location that is a subset of this object's.  In order
   * for this object to imply the specified object, the following must be
   * true:
   *
   * <ol>
   * <li><em>codesource</em> must not be <code>null</code>.</li>
   * <li>If <em>codesource</em> has a certificate list, all of it's
   *     certificates must be present in the certificate list of this
   *     code source.</li>
   * <li>If this object does not have a <code>null</code> location, then
   *     the following addtional tests must be passed.
   *
   *     <ol>
   *     <li><em>codesource</em> must not have a <code>null</code>
   *         location.</li>
   *     <li><em>codesource</em>'s location must be equal to this object's
   *         location, or
   *         <ul>
   *         <li><em>codesource</em>'s location protocol, port, and ref (aka,
   *             anchor) must equal this objects</li>
   *         <li><em>codesource</em>'s location host must imply this object's
   *             location host, as determined by contructing
   *             <code>SocketPermission</code> objects from each with no
   *             action list and using that classes's <code>implies</code>
   *             method</li>
   *         <li>If this object's location file ends with a '/', then the
   *             specified object's location file must start with this
   *             object's location file. Otherwise, the specified object's
   *             location file must start with this object's location file
   *             with the '/' character appended to it.</li>
   *         </ul></li>
   *     </ol></li>
   * </ol>
   *
   * <p>For example, each of these locations imply the location
   * "http://java.sun.com/classes/foo.jar":</p>
   *
   * <pre>
   * http:
   * http://*.sun.com/classes/*
   * http://java.sun.com/classes/-
   * http://java.sun.com/classes/foo.jar
   * </pre>
   *
   * <p>Note that the code source with null location and null certificates implies
   * all other code sources.</p>
   *
   * @param cs the <code>CodeSource</code> to test against this object
   * @return true if this specified <code>CodeSource</code> is implied
   */
  public boolean implies(CodeSource cs)
  {
    if (cs == null)
      return false;
    // First check the certificate list.
    if (certs != null && (cs.certs == null || ! certs.containsAll(cs.certs)))
      return false;
    // Next check the location.
    if (location == null)
      return true;
    if (cs.location == null
        || ! location.getProtocol().equals(cs.location.getProtocol())
        || (location.getPort() != -1
            && location.getPort() != cs.location.getPort())
        || (location.getRef() != null
            && ! location.getRef().equals(cs.location.getRef())))
      return false;
    if (location.getHost() != null)
      {
        String their_host = cs.location.getHost();
        if (their_host == null)
          return false;
        SocketPermission our_sockperm =
          new SocketPermission(location.getHost(), "accept");
        SocketPermission their_sockperm =
          new SocketPermission(their_host, "accept");
        if (! our_sockperm.implies(their_sockperm))
          return false;
      }
    String our_file = location.getFile();
    if (our_file != null)
      {
        if (! our_file.endsWith("/"))
          our_file += "/";
        String their_file = cs.location.getFile();
        if (their_file == null
            || ! their_file.startsWith(our_file))
          return false;
      }
    return true;
  }

  /**
   * This method returns a <code>String</code> that represents this object.
   * The result is in the format <code>"(" + getLocation()</code> followed
   * by a space separated list of certificates (or "&lt;no certificates&gt;"),
   * followed by <code>")"</code>.
   *
   * @return a <code>String</code> for this object
   */
  public String toString()
  {
    CPStringBuilder sb = new CPStringBuilder("(").append(location);
    if (certs == null || certs.isEmpty())
      sb.append(" <no certificates>");
    else
      {
        Iterator iter = certs.iterator();
        for (int i = certs.size(); --i >= 0; )
          sb.append(' ').append(iter.next());
      }
    return sb.append(")").toString();
  }

  /**
   * Reads this object from a serialization stream.
   *
   * @param s the input stream
   * @throws IOException if reading fails
   * @throws ClassNotFoundException if deserialization fails
   * @serialData this reads the location, then expects an int indicating the
   *             number of certificates. Each certificate is a String type
   *             followed by an int encoding length, then a byte[] encoding
   */
  private void readObject(ObjectInputStream s)
    throws IOException, ClassNotFoundException
  {
    s.defaultReadObject();
    int count = s.readInt();
    certs = new HashSet();
    while (--count >= 0)
      {
        String type = (String) s.readObject();
        int bytes = s.readInt();
        byte[] encoded = new byte[bytes];
        for (int i = 0; i < bytes; i++)
          encoded[i] = s.readByte();
        ByteArrayInputStream stream = new ByteArrayInputStream(encoded);
        try
          {
            CertificateFactory factory = CertificateFactory.getInstance(type);
            certs.add(factory.generateCertificate(stream));
          }
        catch (CertificateException e)
          {
            // XXX Should we ignore this certificate?
          }
      }
  }

  /**
   * Writes this object to a serialization stream.
   *
   * @param s the output stream
   * @throws IOException if writing fails
   * @serialData this writes the location, then writes an int indicating the
   *             number of certificates. Each certificate is a String type
   *             followed by an int encoding length, then a byte[] encoding
   */
  private void writeObject(ObjectOutputStream s) throws IOException
  {
    s.defaultWriteObject();
    if (certs == null)
      s.writeInt(0);
    else
      {
        int count = certs.size();
        s.writeInt(count);
        Iterator iter = certs.iterator();
        while (--count >= 0)
          {
            Certificate c = (Certificate) iter.next();
            s.writeObject(c.getType());
            byte[] encoded;
            try
              {
                encoded = c.getEncoded();
              }
            catch (CertificateEncodingException e)
              {
                // XXX Should we ignore this certificate?
                encoded = null;
              }
            if (encoded == null)
              s.writeInt(0);
            else
              {
                s.writeInt(encoded.length);
                for (int i = 0; i < encoded.length; i++)
                  s.writeByte(encoded[i]);
              }
          }
      }
  }
} // class CodeSource
