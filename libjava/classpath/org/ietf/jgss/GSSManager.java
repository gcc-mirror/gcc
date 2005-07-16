/* GSSManager.java -- manager class for the GSS-API.
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
exception statement from your version.

   The documentation comments of this class are derived from the text
   of RFC 2853:  Generic Security Service API Version 2: Java Bindings.
   That document is covered under the following license notice:

Copyright (C) The Internet Society (2000).  All Rights Reserved.

This document and translations of it may be copied and furnished to
others, and derivative works that comment on or otherwise explain it
or assist in its implementation may be prepared, copied, published and
distributed, in whole or in part, without restriction of any kind,
provided that the above copyright notice and this paragraph are
included on all such copies and derivative works.  However, this
document itself may not be modified in any way, such as by removing
the copyright notice or references to the Internet Society or other
Internet organizations, except as needed for the purpose of developing
Internet standards in which case the procedures for copyrights defined
in the Internet Standards process must be followed, or as required to
translate it into languages other than English.

The limited permissions granted above are perpetual and will not be
revoked by the Internet Society or its successors or assigns.

This document and the information contained herein is provided on an
"AS IS" basis and THE INTERNET SOCIETY AND THE INTERNET ENGINEERING
TASK FORCE DISCLAIMS ALL WARRANTIES, EXPRESS OR IMPLIED, INCLUDING BUT
NOT LIMITED TO ANY WARRANTY THAT THE USE OF THE INFORMATION HEREIN
WILL NOT INFRINGE ANY RIGHTS OR ANY IMPLIED WARRANTIES OF
MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. */


package org.ietf.jgss;

import java.security.Provider;
import java.security.Security;

/**
 * <p>The GSSManager class is an abstract class that serves as a factory
 * for three GSS interfaces: {@link GSSName}, {@link GSSCredential}, and
 * {@link GSSContext}. It also provides methods for applications to determine
 * what mechanisms are available from the GSS implementation and what
 * nametypes these mechanisms support. An instance of the default GSSManager
 * subclass may be obtained through the static method {@link #getInstance()},
 * but applications are free to instantiate other subclasses of GSSManager.</p>
 *
 * <p>All but one method in this class are declared abstract. This means
 * that subclasses have to provide the complete implementation for those
 * methods. The only exception to this is the static method {@link
 * #getInstance()} which will have platform specific code to return an
 * instance of the default subclass.</p>
 *
 * <p>Platform providers of GSS are required not to add any constructors to
 * this class, private, public, or protected. This will ensure that all
 * subclasses invoke only the default constructor provided to the base
 * class by the compiler.</p>
 *
 * <p>A subclass extending the GSSManager abstract class may be implemented
 * as a modular provider based layer that utilizes some well known
 * service provider specification. The GSSManager API provides the
 * application with methods to set provider preferences on such an
 * implementation. These methods also allow the implementation to throw
 * a well-defined exception in case provider based configuration is not
 * supported. Applications that expect to be portable should be aware of
 * this and recover cleanly by catching the exception.</p>
 *
 * <p>It is envisioned that there will be three most common ways in which
 * providers will be used:</p>
 *
 * <ol>
 * <li>The application does not care about what provider is used (the
 * default case).</li>
 *
 * <li>The application wants a particular provider to be used
 * preferentially, either for a particular mechanism or all the
 * time, irrespective of mechanism.</li>
 *
 * <li>The application wants to use the locally configured providers
 * as far as possible but if support is missing for one or more
 * mechanisms then it wants to fall back on its own provider.</li>
 * </ol>
 *
 * <p>The GSSManager class has two methods that enable these modes of
 * usage: {@link #addProviderAtFront(java.security.Provider,org.ietf.jgss.Oid)}
 * and {@link #addProviderAtEnd(java.security.Provider,org.ietf.jgss.Oid)}.
 * These methods have the effect of creating an ordered list of
 * (<i>provider</i>, <i>oid</i>) pairs where each pair indicates a preference
 * of provider for a given oid.</p>
 *
 * <p>The use of these methods does not require any knowledge of whatever
 * service provider specification the GSSManager subclass follows. It is
 * hoped that these methods will serve the needs of most applications.
 * Additional methods may be added to an extended GSSManager that could
 * be part of a service provider specification that is standardized
 * later.</p>
 *
 * <h3>Example Code</h3>
 *
 * <pre>
GSSManager mgr = GSSManager.getInstance();

// What mechs are available to us?
Oid[] supportedMechs = mgr.getMechs();

// Set a preference for the provider to be used when support is needed
// for the mechanisms "1.2.840.113554.1.2.2" and "1.3.6.1.5.5.1.1".

Oid krb = new Oid("1.2.840.113554.1.2.2");
Oid spkm1 = new Oid("1.3.6.1.5.5.1.1");

Provider p = (Provider) (new com.foo.security.Provider());

mgr.addProviderAtFront(p, krb);
mgr.addProviderAtFront(p, spkm1);

// What name types does this spkm implementation support?
Oid[] nameTypes = mgr.getNamesForMech(spkm1);
</pre>
 */
public abstract class GSSManager
{

  // Constructor.
  // -------------------------------------------------------------------------

  public GSSManager()
  {
  }

  // Class method.
  // -------------------------------------------------------------------------

  /**
   * Returns the default GSSManager implementation.
   *
   * @return The default GSSManager implementation.
   */
  public static synchronized GSSManager getInstance()
  {
    String impl = Security.getProperty("org.ietf.jgss.GSSManager");
    if (impl == null)
      impl = "gnu.crypto.gssapi.GSSManagerImpl";
    try
      {
        ClassLoader loader = GSSManager.class.getClassLoader();
        if (loader == null)
          loader = ClassLoader.getSystemClassLoader();
        Class c = loader.loadClass(impl);
        return (GSSManager) c.newInstance();
      }
    catch (Exception x)
      {
        throw new RuntimeException(x.toString());
      }
  }

  // Abstract methods.
  // -------------------------------------------------------------------------

  /**
   * <p>This method is used to indicate to the GSSManager that the
   * application would like a particular provider to be used if no other
   * provider can be found that supports the given mechanism. When a value
   * of null is used instead of an Oid for the mechanism, the GSSManager
   * must use the indicated provider for any mechanism.</p>
   *
   * <p>Calling this method repeatedly preserves the older settings but
   * raises them above newer ones in preference thus forming an ordered
   * list of providers and Oid pairs that grows at the bottom. Thus the
   * older provider settings will be utilized first before this one is.</p>
   *
   * <p>If there are any previously existing preferences that conflict with
   * the preference being set here, then the GSSManager should ignore this
   * request.</p>
   *
   * <p>If the GSSManager implementation does not support an SPI with a
   * pluggable provider architecture it should throw a GSSException with
   * the status code {@link GSSException#UNAVAILABLE} to indicate that the
   * operation is unavailable.</p>
   *
   * @param p    The provider instance that should be used whenever
   *             support is needed for <i>mech</i>.
   * @param mech The mechanism for which the provider is being set.
   * @throws GSSException If this service is unavailable.
   */
  public abstract void addProviderAtEnd(Provider p, Oid mech)
    throws GSSException;

  /**
   * <p>This method is used to indicate to the GSSManager that the
   * application would like a particular provider to be used ahead of all
   * others when support is desired for the given mechanism. When a value
   * of null is used instead of an Oid for the mechanism, the GSSManager
   * must use the indicated provider ahead of all others no matter what
   * the mechanism is. Only when the indicated provider does not support
   * the needed mechanism should the GSSManager move on to a different
   * provider.</p>
   *
   * <p>Calling this method repeatedly preserves the older settings but
   * lowers them in preference thus forming an ordered list of provider
   * and Oid pairs that grows at the top.</p>
   *
   * <p>Calling addProviderAtFront with a null Oid will remove all previous
   * preferences that were set for this provider in the GSSManager
   * instance. Calling addProviderAtFront with a non-null Oid will remove
   * any previous preference that was set using this mechanism and this
   * provider together.</p>
   *
   * <p>If the GSSManager implementation does not support an SPI with a
   * pluggable provider architecture it should throw a GSSException with
   * the status code {@link GSSException#UNAVAILABLE} to indicate that the
   * operation is unavailable.</p>
   *
   * @param p    The provider instance that should be used whenever
   *             support is needed for <i>mech</i>.
   * @param mech The mechanism for which the provider is being set.
   * @throws GSSException If this service is unavailable.
   */
  public abstract void addProviderAtFront(Provider p, Oid mech)
    throws GSSException;

  /**
   * Factory method for creating a previously exported context.  The
   * context properties will be determined from the input token and can't
   * be modified through the set methods.
   *
   * @param interProcessToken The token previously emitted from the
   *                          export method.
   * @return The context.
   * @throws GSSException If this operation fails.
   */
  public abstract GSSContext createContext(byte[] interProcessToken)
    throws GSSException;

  /**
   * Factory method for creating a context on the acceptor' side.  The
   * context's properties will be determined from the input token supplied
   * to the accept method.
   *
   * @param myCred Credentials for the acceptor.  Use <code>null</code> to
   *               act as a default acceptor principal.
   * @return The context.
   * @throws GSSException If this operation fails.
   */
  public abstract GSSContext createContext(GSSCredential myCred)
    throws GSSException;

  /**
   * Factory method for creating a context on the initiator's side.
   * Context flags may be modified through the mutator methods prior to
   * calling {@link
   * GSSContext#initSecContext(java.io.InputStream,java.io.OutputStream)}.
   *
   * @param peer     Name of the target peer.
   * @param mech     Oid of the desired mechanism.  Use <code>null</code>
   *                 to request default mechanism.
   * @param myCred   Credentials of the initiator.  Use <code>null</code>
   *                 default initiator principal.
   * @param lifetime The request lifetime, in seconds, for the context.
   *                 Use {@link GSSContext#INDEFINITE_LIFETIME} and
   *                 {@link GSSContext#DEFAULT_LIFETIME} to request
   *                 indefinite or default context lifetime.
   * @return The context.
   * @throws GSSException If this operation fails.
   */
  public abstract GSSContext createContext(GSSName peer, Oid mech,
                                           GSSCredential myCred, int lifetime)
    throws GSSException;

  /**
   * Factory method for acquiring default credentials.  This will cause
   * the GSS-API to use system specific defaults for the set of
   * mechanisms, name, and a DEFAULT lifetime.
   *
   * @param usage The intended usage for this credential object.  The
   *              value of this parameter must be one of:
   *              {@link GSSCredential#ACCEPT_AND_INITIATE},
   *              {@link GSSCredential#ACCEPT_ONLY},
   *              {@link GSSCredential#INITIATE_ONLY}.
   * @return The credential.
   * @throws GSSException If this operation fails.
   */
  public abstract GSSCredential createCredential(int usage) throws GSSException;

  /**
   * Factory method for acquiring a single mechanism credential.
   *
   * @param aName    Name of the principal for whom this credential is to
   *                 be acquired.  Use <code>null</code> to specify the
   *                 default principal.
   * @param lifetime The number of seconds that credentials should remain
   *                 valid.  Use {@link GSSCredential#INDEFINITE_LIFETIME}
   *                 to request that the credentials have the maximum
   *                 permitted lifetime.  Use {@link
   *                 GSSCredential#DEFAULT_LIFETIME} to request default
   *                 credential lifetime.
   * @param mech     The oid of the desired mechanism.  Use <code>null</code>
   *                 to request the default mechanism(s).
   * @param usage    The intended usage for this credential object.  The
   *                 value of this parameter must be one of:
   *                 {@link GSSCredential#ACCEPT_AND_INITIATE},
   *                 {@link GSSCredential#ACCEPT_ONLY},
   *                 {@link GSSCredential#INITIATE_ONLY}.
   * @return The credential.
   * @throws GSSException If this operation fails.
   */
  public abstract GSSCredential createCredential(GSSName aName, int lifetime,
                                                 Oid mech, int usage)
    throws GSSException;

  /**
   * Factory method for acquiring credentials over a set of mechanisms.
   * Acquires credentials for each of the mechanisms specified in the
   * array called mechs.  To determine the list of mechanisms' for which
   * the acquisition of credentials succeeded, the caller should use the
   * {@link GSSCredential#getMechs()} method.
   *
   * @param aName    Name of the principal for whom this credential is to
   *                 be acquired.  Use <code>null</code> to specify the
   *                 default principal.
   * @param lifetime The number of seconds that credentials should remain
   *                 valid.  Use {@link GSSCredential#INDEFINITE_LIFETIME}
   *                 to request that the credentials have the maximum
   *                 permitted lifetime.  Use {@link
   *                 GSSCredential#DEFAULT_LIFETIME} to request default
   *                 credential lifetime.
   * @param mechs    The array of mechanisms over which the credential is
   *                 to be acquired.  Use <code>null</code> for requesting
   *                 a system specific default set of mechanisms.
   * @param usage    The intended usage for this credential object.  The
   *                 value of this parameter must be one of:
   *                 {@link GSSCredential#ACCEPT_AND_INITIATE},
   *                 {@link GSSCredential#ACCEPT_ONLY},
   *                 {@link GSSCredential#INITIATE_ONLY}.
   * @return The credential.
   * @throws GSSException If this operation fails.
   */
  public abstract GSSCredential createCredential(GSSName aName, int lifetime,
                                                 Oid[] mechs, int usage)
    throws GSSException;

  /**
   * Factory method to convert a contiguous byte array containing a name
   * from the specified namespace to a {@link GSSName} object.  In general,
   * the {@link GSSName} object created will not be an MN; two examples that
   * are exceptions to this are when the namespace type parameter indicates
   * {@link GSSName#NT_EXPORT_NAME} or when the GSS-API implementation is not
   * multi-mechanism.
   *
   * @param name     The byte array containing the name to create.
   * @param nameType The Oid specifying the namespace of the name supplied
   *                 in the byte array.  Note that nameType serves to
   *                 describe and qualify the interpretation of the input
   *                 name byte array, it does not necessarily imply a type
   *                 for the output GSSName implementation. "null" value
   *                 can be used to specify that a mechanism specific
   *                 default syntax should be assumed by each mechanism
   *                 that examines the byte array.
   * @return The name.
   * @throws GSSException If this operation fails.
   */
  public abstract GSSName createName(byte[] name, Oid nameType)
    throws GSSException;

  /**
   * Factory method to convert a contiguous byte array containing a name
   * from the specified namespace to a GSSName object that is an MN.  In
   * other words, this method is a utility that does the equivalent of two
   * steps: {@link #createName(byte[],org.ietf.jgss.Oid)} and then also
   * {@link GSSName#canonicalize(org.ietf.jgss.Oid)}.
   *
   * @param name     The byte array representing the name to create.
   * @param nameType The Oid specifying the namespace of the name supplied
   *                 in the byte array.  Note that nameType serves to
   *                 describe and qualify the interpretation of the input
   *                 name byte array, it does not necessarily imply a type
   *                 for the output GSSName implementation. "null" value
   *                 can be used to specify that a mechanism specific
   *                 default syntax should be assumed by each mechanism
   *                 that examines the byte array.
   * @param mech     Oid specifying the mechanism for which this name
   *                 should be created.
   * @return The name.
   * @throws GSSException If this operation fails.
   */
  public abstract GSSName createName(byte[] name, Oid nameType, Oid mech)
    throws GSSException;

  /**
   * Factory method to convert a contiguous string name from the specified
   * namespace to a {@link GSSName} object.  In general, the {@link GSSName}
   * object created will not be an MN; two examples that are exceptions to
   * this are when the namespace type parameter indicates {@link
   * GSSName#NT_EXPORT_NAME} or when the GSS-API implementation is not
   * multi-mechanism.
   *
   * @param nameStr  The string representing a printable form of the name
   *                 to create.
   * @param nameType The Oid specifying the namespace of the printable name
   *                 supplied. Note that nameType serves to describe and
   *                 qualify the interpretation of the input nameStr, it
   *                 does not necessarily imply a type for the output
   *                 GSSName implementation. "null" value can be used to
   *                 specify that a mechanism specific default printable
   *                 syntax should be assumed by each mechanism that
   *                 examines nameStr.
   * @return The name.
   * @throws GSSException If this operation fails.
   */
  public abstract GSSName createName(String nameStr, Oid nameType)
    throws GSSException;

  /**
   * Factory method to convert a contiguous string name from the specified
   * namespace to an GSSName object that is a mechanism name (MN).  In
   * other words, this method is a utility that does the equivalent of two
   * steps: the {@link #createName(java.lang.String,org.ietf.jgss.Oid)}
   * and then also {@link GSSName#canonicalize(org.ietf.jgss.Oid)}.
   *
   * @param nameStr  The string representing a printable form of the name
   *                 to create.
   * @param nameType The Oid specifying the namespace of the printable name
   *                 supplied.  Note that nameType serves to describe and
   *                 qualify the interpretation of the input nameStr, it
   *                 does not necessarily imply a type for the output
   *                 GSSName implementation. "null" value can be used to
   *                 specify that a mechanism specific default printable
   *                 syntax should be assumed when the mechanism examines
   *                 nameStr.
   * @param mech     Oid specifying the mechanism for which this name
   *                 should be created.
   * @return The name.
   * @throws GSSException If this operation fails.
   */
  public abstract GSSName createName(String nameStr, Oid nameType, Oid mech)
    throws GSSException;

  /**
   * Returns an array of {@link Oid} objects indicating mechanisms available
   * to GSS-API callers.  A <code>null</code> value is returned when no
   * mechanism are available (an example of this would be when mechanism are
   * dynamically configured, and currently no mechanisms are installed).
   *
   * @return The array of available mechanisms, or <code>null</code>.
   */
  public abstract Oid[] getMechs();

  /**
   * Returns an array of {@link Oid} objects corresponding to the mechanisms
   * that support the specific name type. <code>null</code> is returned when
   * no mechanisms are found to support the specified name type.
   *
   * @param name The Oid object for the name type.
   * @return The array of mechanisms, or <code>null</code>.
   */
  public abstract Oid[] getMechsForName(Oid name);

  /**
   * Returns name type Oid's supported by the specified mechanism.
   *
   * @param mechanism The Oid object for the mechanism to query.
   * @return The name type Oid's supported by the mechanism.
   * @throws GSSException If this operation fails.
   */
  public abstract Oid[] getNamesForMech(Oid mechanism) throws GSSException;
}
