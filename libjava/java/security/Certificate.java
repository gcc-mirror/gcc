/* Certificate.java -- Interface for modeling digital certificates
   Copyright (C) 1998 Free Software Foundation, Inc.

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

package java.security;

import java.io.InputStream;
import java.io.OutputStream;
import java.io.IOException;

/**
 * This interface models a digital certificate which verifies the 
 * authenticity of a party.  This class simply allows certificate
 * information to be queried, it does not guarantee that the certificate
 * is valid.
 * <p>
 * This class is deprecated in favor of the new java.security.cert package.
 * It exists for backward compatibility only.
 * 
 * @deprecated
 *
 * @version 0.0
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 */
public interface Certificate
{

  /**
   * This method returns the <code>Principal</code> that is guaranteeing
   * this certificate.
   *
   * @return The <code>Principal</code> guaranteeing the certificate
   */
  public abstract Principal getGuarantor();

  /**
   * This method returns the <code>Principal</code> being guaranteed by
   * this certificate.
   *
   * @return The <code>Principal</code> guaranteed by this certificate.
   */
  public abstract Principal getPrincipal();

  /**
   * This method returns the public key for the <code>Principal</code> that
   * is being guaranteed.
   *
   * @return The <code>PublicKey</code> of the <code>Principal</code> being guaranteed
   */
  public abstract PublicKey getPublicKey();

  /**
   * This method returns the encoding format of the certificate (e.g., "PGP",
   * "X.509").  This format is used by the <code>encode</code. and
   * <code>decode</code> methods.
   *
   * @return The encoding format being used
   */
  public abstract String getFormat();

  /**
   * This method writes the certificate to an <code>OutputStream</code> in
   * a format that can be understood by the <code>decode</code> method.
   *
   * @param out The <code>OutputStream</code> to write to.
   *
   * @exception KeyException If there is a problem with the internals of this certificate
   * @exception IOException If an error occurs writing to the stream.
   */
  public abstract void
    encode(OutputStream out) throws KeyException, IOException;

  /**
   * This method reads an encoded certificate from an <code>InputStream</code>.
   *
   * @param in The <code>InputStream</code> to read from.
   *
   * @param KeyException If there is a problem with the certificate data
   * @param IOException If an error occurs reading from the stream.
   */
  public abstract void
    decode(InputStream in) throws KeyException, IOException;

  /**
   * This method returns a <code>String</code> representation of the contents
   * of this certificate.
   *
   * @param detail <code>true</code> to provided detailed information about this certificate, <code>false</code> otherwise
   */
  public abstract String toString(boolean detail);
}
