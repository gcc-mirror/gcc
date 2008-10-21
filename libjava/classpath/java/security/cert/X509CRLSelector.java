/* X509CRLSelector.java -- selects X.509 CRLs by criteria.
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

import gnu.classpath.SystemProperties;
import gnu.java.lang.CPStringBuilder;
import gnu.java.security.der.DERReader;
import gnu.java.security.der.DERValue;

import java.io.IOException;
import java.io.InputStream;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import javax.security.auth.x500.X500Principal;

/**
 * A class for matching X.509 certificate revocation lists by criteria.
 *
 * <p>Use of this class requires extensive knowledge of the Internet
 * Engineering Task Force's Public Key Infrastructure (X.509). The primary
 * document describing this standard is <a
 * href="http://www.ietf.org/rfc/rfc3280.txt">RFC 3280: Internet X.509
 * Public Key Infrastructure Certificate and Certificate Revocation List
 * (CRL) Profile</a>.
 *
 * <p>Note that this class is not thread-safe. If multiple threads will
 * use or modify this class then they need to synchronize on the object.
 *
 * @author Casey Marshall (csm@gnu.org)
 * @since 1.4
 */
public class X509CRLSelector implements CRLSelector, Cloneable
{

  // Fields.
  // -------------------------------------------------------------------------

  private static final String CRL_NUMBER_ID = "2.5.29.20";

  private List issuerNames;
  private BigInteger maxCrlNumber;
  private BigInteger minCrlNumber;
  private Date date;
  private X509Certificate cert;

  // Constructor.
  // -------------------------------------------------------------------------

  /**
   * Creates a new CRL selector with no criteria enabled; i.e., every CRL
   * will be matched.
   */
  public X509CRLSelector()
  {
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  /**
   * Add an issuer name to the set of issuer names criteria, as the DER
   * encoded form.
   *
   * @param name The name to add, as DER bytes.
   * @throws IOException If the argument is not a valid DER-encoding.
   */
  public void addIssuerName(byte[] name) throws IOException
  {
    X500Principal p = null;
    try
      {
        p = new X500Principal(name);
      }
    catch (IllegalArgumentException iae)
      {
        IOException ioe = new IOException("malformed name");
        ioe.initCause(iae);
        throw ioe;
      }
    if (issuerNames == null)
      issuerNames = new LinkedList();
    issuerNames.add(p);
  }

  /**
   * Add an issuer name to the set of issuer names criteria, as a
   * String representation.
   *
   * @param name The name to add.
   * @throws IOException If the argument is not a valid name.
   */
  public void addIssuerName(String name) throws IOException
  {
    X500Principal p = null;
    try
      {
        p = new X500Principal(name);
      }
    catch (IllegalArgumentException iae)
      {
        IOException ioe = new IOException("malformed name: " + name);
        ioe.initCause(iae);
        throw ioe;
      }
    if (issuerNames == null)
      issuerNames = new LinkedList();
    issuerNames.add(p);
  }

  /**
   * Sets the issuer names criterion. Pass <code>null</code> to clear this
   * value. CRLs matched by this selector must have an issuer name in this
   * set.
   *
   * @param names The issuer names.
   * @throws IOException If any of the elements in the collection is not
   *         a valid name.
   */
  public void setIssuerNames(Collection<?> names) throws IOException
  {
    if (names == null)
      {
        issuerNames = null;
        return;
      }
    List l = new ArrayList(names.size());
    for (Iterator it = names.iterator(); it.hasNext(); )
      {
        Object o = it.next();
        if (o instanceof X500Principal)
          l.add(o);
        else if (o instanceof String)
          {
            try
              {
                l.add(new X500Principal((String) o));
              }
            catch (IllegalArgumentException iae)
              {
                IOException ioe = new IOException("malformed name: " + o);
                ioe.initCause(iae);
                throw ioe;
              }
          }
        else if (o instanceof byte[])
          {
            try
              {
                l.add(new X500Principal((byte[]) o));
              }
            catch (IllegalArgumentException iae)
              {
                IOException ioe = new IOException("malformed name");
                ioe.initCause(iae);
                throw ioe;
              }
          }
        else if (o instanceof InputStream)
          {
            try
              {
                l.add(new X500Principal((InputStream) o));
              }
            catch (IllegalArgumentException iae)
              {
                IOException ioe = new IOException("malformed name");
                ioe.initCause(iae);
                throw ioe;
              }
          }
        else
          throw new IOException("not a valid name: " +
                                (o != null ? o.getClass().getName() : "null"));

      }
    issuerNames = l;
  }

  /**
   * Returns the set of issuer names that are matched by this selector,
   * or <code>null</code> if this criteria is not set. The returned
   * collection is not modifiable.
   *
   * @return The set of issuer names.
   */
  public Collection<Object> getIssuerNames()
  {
    if (issuerNames != null)
      return Collections.unmodifiableList(issuerNames);
    else
      return null;
  }

  /**
   * Returns the maximum value of the CRLNumber extension present in
   * CRLs matched by this selector, or <code>null</code> if this
   * criteria is not set.
   *
   * @return The maximum CRL number.
   */
  public BigInteger getMaxCRL()
  {
    return maxCrlNumber;
  }

  /**
   * Returns the minimum value of the CRLNumber extension present in
   * CRLs matched by this selector, or <code>null</code> if this
   * criteria is not set.
   *
   * @return The minimum CRL number.
   */
  public BigInteger getMinCRL()
  {
    return minCrlNumber;
  }

  /**
   * Sets the maximum value of the CRLNumber extension present in CRLs
   * matched by this selector. Specify <code>null</code> to clear this
   * criterion.
   *
   * @param maxCrlNumber The maximum CRL number.
   */
  public void setMaxCRLNumber(BigInteger maxCrlNumber)
  {
    this.maxCrlNumber = maxCrlNumber;
  }

  /**
   * Sets the minimum value of the CRLNumber extension present in CRLs
   * matched by this selector. Specify <code>null</code> to clear this
   * criterion.
   *
   * @param minCrlNumber The minimum CRL number.
   */
  public void setMinCRLNumber(BigInteger minCrlNumber)
  {
    this.minCrlNumber = minCrlNumber;
  }

  /**
   * Returns the date when this CRL must be valid; that is, the date
   * must be after the thisUpdate date, but before the nextUpdate date.
   * Returns <code>null</code> if this criterion is not set.
   *
   * @return The date.
   */
  public Date getDateAndTime()
  {
    return date != null ? (Date) date.clone() : null;
  }

  /**
   * Sets the date at which this CRL must be valid. Specify
   * <code>null</code> to clear this criterion.
   *
   * @param date The date.
   */
  public void setDateAndTime(Date date)
  {
    this.date = date != null ? (Date) date.clone() : null;
  }

  /**
   * Returns the certificate being checked, or <code>null</code> if this
   * value is not set.
   *
   * @return The certificate.
   */
  public X509Certificate getCertificateChecking()
  {
    return cert;
  }

  /**
   * Sets the certificate being checked. This is not a criterion, but
   * info used by certificate store implementations to aid in searching.
   *
   * @param cert The certificate.
   */
  public void setCertificateChecking(X509Certificate cert)
  {
    this.cert = cert;
  }

  /**
   * Returns a string representation of this selector. The string will
   * only describe the enabled criteria, so if none are enabled this will
   * return a string that contains little else besides the class name.
   *
   * @return The string.
   */
  public String toString()
  {
    CPStringBuilder str = new CPStringBuilder(X509CRLSelector.class.getName());
    String nl = SystemProperties.getProperty("line.separator");
    String eol = ";" + nl;

    str.append(" {").append(nl);
    if (issuerNames != null)
      str.append("  issuer names = ").append(issuerNames).append(eol);
    if (maxCrlNumber != null)
      str.append("  max CRL = ").append(maxCrlNumber).append(eol);
    if (minCrlNumber != null)
      str.append("  min CRL = ").append(minCrlNumber).append(eol);
    if (date != null)
      str.append("  date = ").append(date).append(eol);
    if (cert != null)
      str.append("  certificate = ").append(cert).append(eol);
    str.append("}").append(nl);
    return str.toString();
  }

  /**
   * Checks a CRL against the criteria of this selector, returning
   * <code>true</code> if the given CRL matches all the criteria.
   *
   * @param _crl The CRL being checked.
   * @return True if the CRL matches, false otherwise.
   */
  public boolean match(CRL _crl)
  {
    if (!(_crl instanceof X509CRL))
      return false;
    X509CRL crl = (X509CRL) _crl;
    if (issuerNames != null)
      {
        if (!issuerNames.contains(crl.getIssuerX500Principal()))
          return false;
      }
    BigInteger crlNumber = null;
    if (maxCrlNumber != null)
      {
        byte[] b = crl.getExtensionValue(CRL_NUMBER_ID);
        if (b == null)
          return false;
        try
          {
            DERValue val = DERReader.read(b);
            if (!(val.getValue() instanceof BigInteger))
              return false;
            crlNumber = (BigInteger) val.getValue();
          }
        catch (IOException ioe)
          {
            return false;
          }
        if (maxCrlNumber.compareTo(crlNumber) < 0)
          return false;
      }
    if (minCrlNumber != null)
      {
        if (crlNumber == null)
          {
            byte[] b = crl.getExtensionValue(CRL_NUMBER_ID);
            if (b == null)
              return false;
            try
              {
                DERValue val = DERReader.read(b);
                if (!(val.getValue() instanceof BigInteger))
                  return false;
                crlNumber = (BigInteger) val.getValue();
              }
            catch (IOException ioe)
              {
                return false;
              }
          }
        if (minCrlNumber.compareTo(crlNumber) > 0)
          return false;
      }
    if (date != null)
      {
        if (date.compareTo(crl.getThisUpdate()) < 0 ||
            date.compareTo(crl.getNextUpdate()) > 0)
          return false;
      }
    return true;
  }

  /**
   * Returns a copy of this object.
   *
   * @return The copy.
   */
  public Object clone()
  {
    try
      {
        return super.clone();
      }
    catch (CloneNotSupportedException shouldNotHappen)
      {
        throw new Error(shouldNotHappen);
      }
  }
}
