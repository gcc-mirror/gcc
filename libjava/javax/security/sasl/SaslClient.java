/* SaslClient.java --
   Copyright (C) 2003, 2004 Free Software Foundation, Inc.

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
 * <p>Performs SASL authentication as a client.</p>
 *
 * <p>A protocol library such as one for LDAP gets an instance of this class in
 * order to perform authentication defined by a specific SASL mechanism.
 * Invoking methods on the <code>SaslClient</code> instance process challenges
 * and create responses according to the SASL mechanism implemented by the
 * <code>SaslClient</code>. As the authentication proceeds, the instance
 * encapsulates the state of a SASL client's authentication exchange.</p>
 *
 * <p>Here's an example of how an LDAP library might use a <code>SaslClient</code>.
 * It first gets an instance of a SaslClient:</p>
 * <pre>
 *SaslClient sc =
 *      Sasl.createSaslClient(mechanisms, authorizationID, protocol,
 *                            serverName, props, callbackHandler);
 * </pre>
 *
 * <p>It can then proceed to use the client for authentication. For example, an
 * LDAP library might use the client as follows:</p>
 * <pre>
 * // Get initial response and send to server
 *byte[] response = sc.hasInitialResponse()
 *      ? sc.evaluateChallenge(new byte[0]) : null;
 *LdapResult res = ldap.sendBindRequest(dn, sc.getName(), response);
 *while (!sc.isComplete()
 *       && ((res.status == SASL_BIND_IN_PROGRESS) || (res.status == SUCCESS))) {
 *   response = sc.evaluateChallenge( res.getBytes() );
 *   if (res.status == SUCCESS) {
 *      // we're done; don't expect to send another BIND
 *      if ( response != null ) {
 *         throw new SaslException(
 *               "Protocol error: attempting to send response after completion");
 *      }
 *      break;
 *   }
 *   res = ldap.sendBindRequest(dn, sc.getName(), response);
 *}
 *if (sc.isComplete() && (res.status == SUCCESS) ) {
 *   String qop = (String)sc.getNegotiatedProperty(Sasl.QOP);
 *   if ((qop != null)
 *         && (qop.equalsIgnoreCase("auth-int")
 *            || qop.equalsIgnoreCase("auth-conf"))) {
 *      // Use SaslClient.wrap() and SaslClient.unwrap() for future
 *      // communication with server
 *      ldap.in = new SecureInputStream(sc, ldap.in);
 *      ldap.out = new SecureOutputStream(sc, ldap.out);
 *   }
 *}
 * </pre>
 *
 * <p>If the mechanism has an initial response, the library invokes
 * {@link #evaluateChallenge(byte[])} with an empty challenge to get the initial
 * response. Protocols such as IMAP4, which do not include an initial response
 * with their first authentication command to the server, initiate the
 * authentication without first calling {@link #hasInitialResponse()} or
 * {@link #evaluateChallenge(byte[])}. When the server responds to the command,
 * it sends an initial challenge. For a SASL mechanism in which the client sends
 * data first, the server should have issued a challenge with no data. This will
 * then result in a call (on the client) to {@link #evaluateChallenge(byte[])}
 * with an empty challenge.</p>
 *
 * @see Sasl
 * @see SaslClientFactory
 */
public interface SaslClient
{

  /**
   * Returns the IANA-registered mechanism name of this SASL client. (e.g.
   * "CRAM-MD5", "GSSAPI").
   *
   * @return a non-null string representing the IANA-registered mechanism name.
   */
  String getMechanismName();

  /**
   * Determines if this mechanism has an optional initial response. If
   * <code>true</code>, caller should call {@link #evaluateChallenge(byte[])}
   * with an empty array to get the initial response.
   *
   * @return <code>true</code> if this mechanism has an initial response.
   */
  boolean hasInitialResponse();

  /**
   * Evaluates the challenge data and generates a response. If a challenge is
   * received from the server during the authentication process, this method is
   * called to prepare an appropriate next response to submit to the server.
   *
   * @param challenge the non-null challenge sent from the server. The
   * challenge array may have zero length.
   * @return the possibly <code>null</code> reponse to send to the server. It
   * is <code>null</code> if the challenge accompanied a "SUCCESS" status and
   * the challenge only contains data for the client to update its state and no
   * response needs to be sent to the server. The response is a zero-length
   * byte array if the client is to send a response with no data.
   * @throws SaslException if an error occurred while processing the challenge
   * or generating a response.
   */
  byte[] evaluateChallenge(byte[] challenge) throws SaslException;

  /**
   * Determines if the authentication exchange has completed. This method may
   * be called at any time, but typically, it will not be called until the
   * caller has received indication from the server (in a protocol-specific
   * manner) that the exchange has completed.
   *
   * @return <code>true</code> if the authentication exchange has completed;
   * <code>false</code> otherwise.
   */
  boolean isComplete();

  /**
   * <p>Unwraps a byte array received from the server. This method can be
   * called only after the authentication exchange has completed (i.e., when
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
   * the server.
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
   * <p>Wraps a byte array to be sent to the server. This method can be called
   * only after the authentication exchange has completed (i.e., when
   * {@link #isComplete()} returns <code>true</code>) and only if the
   * authentication exchange has negotiated integrity and/or privacy as the
   * quality of protection; otherwise, an {@link IllegalStateException} is
   * thrown.</p>
   *
   * <p>The result of this method will make up the contents of the SASL buffer
   * as defined in RFC 2222 without the leading four octet field that
   * represents the length. <code>offset</code> and <code>len</code> specify
   * the portion of <code>outgoing</code> to use.</p>
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
   * the authentication exchange has completed (i.e., when {@link #isComplete()}
   * returns <code>true</code>); otherwise, an {@link IllegalStateException} is
   * thrown.
   *
   * @param propName the non-null property name.
   * @return the value of the negotiated property. If <code>null</code>, the
   * property was not negotiated or is not applicable to this mechanism.
   * @throws IllegalStateException if this authentication exchange has not
   * completed.
   */
  Object getNegotiatedProperty(String propName) throws SaslException;

  /**
   * Disposes of any system resources or security-sensitive information the
   * <code>SaslClient</code> might be using. Invoking this method invalidates
   * the <code>SaslClient</code> instance. This method is idempotent.
   *
   * @throws SaslException if a problem was encountered while disposing of the
   * resources.
   */
  void dispose() throws SaslException;
}
