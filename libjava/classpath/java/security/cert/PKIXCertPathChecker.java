/* PKIXCertPathChecker.java -- checks X.509 certificate paths.
   Copyright (C) 2003 Free Software Foundation, Inc.

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

import java.util.Collection;
import java.util.Set;

/**
 * A validator for X.509 certificates when approving certificate chains.
 *
 * <p>Concrete subclasses can be passed to the {@link
 * PKIXParameters#setCertPathCheckers(java.util.List)} and {@link
 * PKIXParameters#addCertPathChecker(java.security.cert.PKIXCertPathChecker)}
 * methods, which are then used to set up PKIX certificate chain
 * builders or validators. These classes then call the {@link
 * #check(java.security.cert.Certificate,java.util.Collection)} method
 * of this class, performing whatever checks on the certificate,
 * throwing an exception if any check fails.
 *
 * <p>Subclasses of this must be able to perform their checks in the
 * backward direction -- from the most-trusted certificate to the target
 * -- and may optionally support forward checking -- from the target to
 * the most-trusted certificate.
 *
 * @see PKIXParameters
 * @since 1.4
 */
public abstract class PKIXCertPathChecker implements Cloneable
{

  // Constructor.
  // ------------------------------------------------------------------------

  /** Default constructor. */
  protected PKIXCertPathChecker()
  {
    super();
  }

  // Cloneable interface.
  // ------------------------------------------------------------------------

  public Object clone()
  {
    try
      {
        return super.clone();
      }
    catch (CloneNotSupportedException cnse)
      {
        throw new InternalError(cnse.getMessage());
      }
  }

  // Abstract methods.
  // ------------------------------------------------------------------------

  /**
   * Initialize this PKIXCertPathChecker. If subclasses support forward
   * checking, a value of true can be passed to this method, and
   * certificates can be validated from the target certificate to the
   * most-trusted certifcate.
   *
   * @param forward The direction of this PKIXCertPathChecker.
   * @throws CertPathValidatorException If <i>forward</i> is true and
   *         this class does not support forward checking.
   */
  public abstract void init(boolean forward) throws CertPathValidatorException;

  /**
   * Returns whether or not this class supports forward checking.
   *
   * @return Whether or not this class supports forward checking.
   */
  public abstract boolean isForwardCheckingSupported();

  /**
   * Returns an immutable set of X.509 extension object identifiers (OIDs)
   * supported by this PKIXCertPathChecker.
   *
   * @return An immutable set of Strings of the supported X.509 OIDs, or
   *         null if no extensions are supported.
   */
  public abstract Set<String> getSupportedExtensions();

  /**
   * Checks a certificate, removing any critical extensions that are
   * resolved in this check.
   *
   * @param cert               The certificate to check.
   * @param unresolvedCritExts The (mutable) collection of as-of-yet
   *        unresolved critical extensions, as OID strings.
   * @throws CertPathValidatorException If this certificate fails this
   *         check.
   */
  public abstract void check(Certificate cert, Collection<String> unresolvedCritExts)
  throws CertPathValidatorException;
}
