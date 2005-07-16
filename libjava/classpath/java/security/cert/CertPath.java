/* CertPath.java -- a sequence of certificates
   Copyright (C) 2002, 2005  Free Software Foundation, Inc.

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

package java.security.cert;

import java.io.ByteArrayInputStream;
import java.io.NotSerializableException;
import java.io.ObjectStreamException;
import java.io.Serializable;
import java.util.Iterator;
import java.util.List;

/**
 * This class represents an immutable sequence, or path, of security
 * certificates. The path type must match the type of each certificate in the
 * path, or in other words, for all instances of cert in a certpath object,
 * <code>cert.getType().equals(certpath.getType())</code> will return true.
 *
 * <p>Since this class is immutable, it is thread-safe. During serialization,
 * the path is consolidated into a {@link CertPathRep}, which preserves the
 * data regardless of the underlying implementation of the path.
 *
 * @author Eric Blake (ebb9@email.byu.edu)
 * @since 1.4
 * @status updated to 1.4
 */
public abstract class CertPath implements Serializable
{
  /**
   * The serialized representation of a path.
   *
   * @author Eric Blake (ebb9@email.byu.edu)
   */
  protected static class CertPathRep implements Serializable
  {
    /**
     * Compatible with JDK 1.4+.
     */
    private static final long serialVersionUID = 3015633072427920915L;

    /**
     * The certificate type.
     *
     * @serial the type of the certificate path
     */
    private final String type;

    /**
     * The encoded form of the path.
     *
     * @serial the encoded form
     */
    private final byte[] data;

    /**
     * Create the new serial representation.
     *
     * @param type the path type
     * @param data the encoded path data
     */
    protected CertPathRep(String type, byte[] data)
    {
      this.type = type;
      this.data = data;
    }

    /**
     * Decode the data into an actual {@link CertPath} upon deserialization.
     *
     * @return the replacement object
     * @throws ObjectStreamException if replacement fails
     */
    protected Object readResolve() throws ObjectStreamException
    {
      try
        {
          return CertificateFactory.getInstance(type)
            .generateCertPath(new ByteArrayInputStream(data));
        }
      catch (CertificateException e)
        {
          throw (ObjectStreamException)
            new NotSerializableException("java.security.cert.CertPath: "
                                         + type).initCause(e);
        }
    }
  } // class CertPathRep

  /**
   * Compatible with JDK 1.4+.
   */
  private static final long serialVersionUID = 6068470306649138683L;

  /**
   * The path type.
   *
   * @serial the type of all certificates in this path
   */
  private final String type;

  /**
   * Create a certificate path with the given type. Most code should use
   * {@link CertificateFactory} to create CertPaths.
   *
   * @param type the type of the path
   */
  protected CertPath(String type)
  {
    this.type = type;
  }

  /**
   * Get the (non-null) type of all certificates in the path.
   *
   * @return the path certificate type
   */
  public String getType()
  {
    return type;
  }

  /**
   * Get an immutable iterator over the path encodings (all String names),
   * starting with the default encoding. The iterator will throw an
   * <code>UnsupportedOperationException</code> if an attempt is made to
   * remove items from the list.
   *
   * @return the iterator of supported encodings in the path
   */
  public abstract Iterator getEncodings();

  /**
   * Compares this path to another for semantic equality. To be equal, both
   * must be instances of CertPath, with the same type, and identical
   * certificate lists. Overriding classes must not change this behavior.
   *
   * @param o the object to compare to
   * @return true if the two are equal
   */
  public boolean equals(Object o)
  {
    if (! (o instanceof CertPath))
      return false;
    CertPath cp = (CertPath) o;
    return type.equals(cp.type)
      && getCertificates().equals(cp.getCertificates());
  }

  /**
   * Returns the hashcode of this certificate path. This is defined as:<br>
   * <code>31 * getType().hashCode() + getCertificates().hashCode()</code>.
   *
   * @return the hashcode
   */
  public int hashCode()
  {
    return 31 * type.hashCode() + getCertificates().hashCode();
  }

  public String toString()
  {
    List l = getCertificates();
    int size = l.size();
    int i = 0;
    StringBuffer result = new StringBuffer(type);
    result.append(" Cert Path: length = ").append(size).append(".\n[\n");
    while (--size >= 0)
      result.append(l.get(i++)).append('\n');
    return result.append("\n]").toString();
  }

  /**
   * Returns the encoded form of this path, via the default encoding.
   *
   * @return the encoded form
   * @throws CertificateEncodingException if encoding fails
   */
  public abstract byte[] getEncoded() throws CertificateEncodingException;

  /**
   * Returns the encoded form of this path, via the specified encoding.
   *
   * @param encoding the encoding to use
   * @return the encoded form
   * @throws CertificateEncodingException if encoding fails or does not exist
   */
  public abstract byte[] getEncoded(String encoding)
    throws CertificateEncodingException;

  /**
   * Returns the immutable, thread-safe list of certificates in this path.
   *
   * @return the list of certificates, non-null but possibly empty
   */
  public abstract List getCertificates();

  /**
   * Serializes the path in its encoded form, to ensure reserialization with
   * the appropriate factory object without worrying about list implementation.
   * The result will always be an instance of {@link CertPathRep}.
   *
   * @return the replacement object
   * @throws ObjectStreamException if the replacement creation fails
   */
  protected Object writeReplace() throws ObjectStreamException
  {
    try
      {
        return new CertPathRep(type, getEncoded());
      }
    catch (CertificateEncodingException e)
      {
        throw (ObjectStreamException)
          new NotSerializableException("java.security.cert.CertPath: "
                                       + type).initCause(e);
      }
  }
} // class CertPath
