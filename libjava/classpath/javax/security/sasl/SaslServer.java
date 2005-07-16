/* SasServer.java
   Copyright (C) 2003, Free Software Foundation, Inc.

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


package javax.security.sasl;

/**
 * <p>Performs SASL authentication as a server.</p>
 *
 * <p>A server such as an LDAP server gets an instance of this class in order to
 * perform authentication defined by a specific SASL mechanism. Invoking methods
 * on the <code>SaslServer</code> instance generates challenges corresponding to
 * the SASL mechanism implemented by the <code>SaslServer</code> instance. As
 * the authentication proceeds, the instance encapsulates the state of a SASL
 * server's authentication exchange.</p>
 *
 * <p>Here's an example of how an LDAP server might use a <code>SaslServer</code>
 * instance. It first gets an instance of a <code>SaslServer</code> for the SASL
 * mechanism requested by the client:</p>
 *
 * <pre>
 *SaslServer ss =
 *      Sasl.createSaslServer(mechanism, "ldap", myFQDN, props, callbackHandler);
 * </pre>
 *
 * <p>It can then proceed to use the server for authentication. For example,
 * suppose the LDAP server received an LDAP BIND request containing the name of
 * the SASL mechanism and an (optional) initial response. It then might use the
 * server as follows:</p>
 *
 * <pre>
 *while (!ss.isComplete()) {
 *   try {
 *      byte[] challenge = ss.evaluateResponse(response);
 *      if (ss.isComplete()) {
 *         status = ldap.sendBindResponse(mechanism, challenge, SUCCESS);
 *      } else {
 *         status = ldap.sendBindResponse(mechanism, challenge, SASL_BIND_IN_PROGRESS);
 *         response = ldap.readBindRequest();
 *      }
 *   } catch (SaslException x) {
 *      status = ldap.sendErrorResponse(x);
 *      break;
 *   }
 *}
 *if (ss.isComplete() && (status == SUCCESS)) {
 *   String qop = (String) sc.getNegotiatedProperty(Sasl.QOP);
 *   if (qop != null
 *         && (qop.equalsIgnoreCase("auth-int")
 *            || qop.equalsIgnoreCase("auth-conf"))) {
 *      // Use SaslServer.wrap() and SaslServer.unwrap() for future
 *      // communication with client
 *      ldap.in = new SecureInputStream(ss, ldap.in);
 *      ldap.out = new SecureOutputStream(ss, ldap.out);
 *   }
 *}
 * </pre>
 *
 * @see Sasl
 * @see SaslServerFactory
 */
public interface SaslServer
{

  /**
   * Returns the IANA-registered mechanism name of this SASL server (e.g.
   * "CRAM-MD5", "GSSAPI").
   *
   * @return a non-null string representing the IANA-registered mechanism name.
   */
  String getMechanismName();

  /**
   * Evaluates the response data and generates a challenge. If a response is
   * received from the client during the authentication process, this method is
   * called to prepare an appropriate next challenge to submit to the client.
   * The challenge is <code>null</code> if the authentication has succeeded and
   * no more challenge data is to be sent to the client. It is non-null if the
   * authentication must be continued by sending a challenge to the client, or
   * if the authentication has succeeded but challenge data needs to be
   * processed by the client. {@link #isComplete()} should be called after each
   * call to <code>evaluateResponse()</code>,to determine if any further
   * response is needed from the client.
   *
   * @param response the non-null (but possibly empty) response sent by the
   * client.
   * @return the possibly <code>null</code> challenge to send to the client.
   * It is <code>null</code> if the authentication has succeeded and there is
   * no more challenge data to be sent to the client.
   * @throws SaslException if an error occurred while processing the response
   * or generating a challenge.
   */
  byte[] evaluateResponse(byte[] response) throws SaslException;

  /**
   * Determines if the authentication exchange has completed. This method is
   * typically called after each invocation of {@link #evaluateResponse(byte[])}
   * to determine whether the authentication has completed successfully or
   * should be continued.
   *
   * @return <code>true</code> if the authentication exchange has completed;
   * <code>false</code> otherwise.
   */
  boolean isComplete();

  /**
   * Reports the authorization ID in effect for the client of this session This
   * method can only be called if {@link #isComplete()} returns <code>true</code>.
   *
   * @return the authorization ID of the client.
   * @throws IllegalStateException if this authentication session has not
   * completed.
   */
  String getAuthorizationID();

  /**
   * <p>Unwraps a byte array received from the client. This method can be called
   * only after the authentication exchange has completed (i.e., when
   * {@link #isComplete()} returns <code>true</code>) and only if the
   * authentication exchange has negotiated integrity and/or privacy as the
   * quality of protection; otherwise, an {@link IllegalStateException} is
   * thrown.</p>
   *
   * <p><code>incoming</code> is the contents of the SASL buffer as defined in
   * RFC 2222 without the leading four octet field that represents the length.
   * <code>offset</code> and <code>len</code> specify the portion of incoming
   * to use.</p>
   *
   * @param incoming a non-null byte array containing the encoded bytes from
   * the client.
   * @param offset the starting position at <code>incoming</code> of the bytes
   * to use.
   * @param len the number of bytes from <code>incoming</code> to use.
   * @return a non-null byte array containing the decoded bytes.
   * @throws SaslException if <code>incoming</code> cannot be successfully
   * unwrapped.
   * @throws IllegalStateException if the authentication exchange has not
   * completed, or if the negotiated quality of protection has neither
   * integrity nor privacy.
   */
  byte[] unwrap(byte[] incoming, int offset, int len) throws SaslException;

  /**
   * <p>Wraps a byte array to be sent to the client. This method can be called
   * only after the authentication exchange has completed (i.e., when
   * {@link #isComplete()} returns <code>true</code>) and only if the
   * authentication exchange has negotiated integrity and/or privacy as the
   * quality of protection; otherwise, an {@link IllegalStateException} is
   * thrown.</p>
   *
   * <p>The result of this method will make up the contents of the SASL buffer
   * as defined in RFC 2222 without the leading four octet field that
   * represents the length. <code>offset</code> and <code>len</code> specify
   * the portion of <code>outgoing</code> to use.
   *
   * @param outgoing a non-null byte array containing the bytes to encode.
   * @param offset the starting position at <code>outgoing</code> of the bytes
   * to use.
   * @param len the number of bytes from <code>outgoing</code> to use.
   * @return a non-null byte array containing the encoded bytes.
   * @throws SaslException if <code>outgoing</code> cannot be successfully
   * wrapped.
   * @throws IllegalStateException if the authentication exchange has not
   * completed, or if the negotiated quality of protection has neither
   * integrity nor privacy.
   */
  byte[] wrap(byte[] outgoing, int offset, int len) throws SaslException;

  /**
   * Retrieves the negotiated property. This method can be called only after
   * the authentication exchange has completed (i.e., when
   * {@link #isComplete()} returns <code>true</code>); otherwise, an
   * {@link IllegalStateException} is thrown.
   *
   * @return the value of the negotiated property. If <code>null</code>, the
   * property was not negotiated or is not applicable to this mechanism.
   * @throws IllegalStateException if this authentication exchange has not
   * completed.
   */
  Object getNegotiatedProperty(String propName) throws SaslException;

  /**
   * Disposes of any system resources or security-sensitive information the
   * <code>SaslServer</code> might be using. Invoking this method invalidates
   * the <code>SaslServer</code> instance. This method is idempotent.
   *
   * @throws SaslException if a problem was encountered while disposing of the
   * resources.
   */
  void dispose() throws SaslException;
}
