/* GSSContext.java -- The GSS context interface.
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


package org.ietf.jgss;

import java.io.InputStream;
import java.io.OutputStream;

/**
 * <p>This interface encapsulates the GSS-API security context and provides
 * the security services ({@link #wrap(byte[],int,int,org.ietf.jgss.MessageProp)},
 * {@link #unwrap(byte[],int,int,org.ietf.jgss.MessageProp)}, {@link
 * #getMIC(byte[],int,int,org.ietf.jgss.MessageProp)}, {@link
 * #verifyMIC(byte[],int,int,byte[],int,int,org.ietf.jgss.MessageProp)}) that
 * are available over the context.  Security contexts are established
 * between peers using locally acquired credentials.  Multiple contexts
 * may exist simultaneously between a pair of peers, using the same or
 * different set of credentials.  GSS-API functions in a manner
 * independent of the underlying transport protocol and depends on its
 * calling application to transport its tokens between peers.</p>
 *
 * <p>Before the context establishment phase is initiated, the context
 * initiator may request specific characteristics desired of the
 * established context.  These can be set using the set methods.  After
 * the context is established, the caller can check the actual
 * characteristic and services offered by the context using the query
 * methods.</p>
 *
 * <p>The context establishment phase begins with the first call to the
 * init method by the context initiator.  During this phase the
 * {@link #initSecContext(byte[],int,int)} and {@link
 * #acceptSecContext(byte[],int,int)} methods will produce GSS-API
 * authentication tokens which the calling application needs to send to
 * its peer.  If an error occurs at any point, an exception will get
 * thrown and the code will start executing in a catch block.  If not,
 * the normal flow of code continues and the application can make a call
 * to the {@link #isEstablished()} method. If this method returns false it
 * indicates that a token is needed from its peer in order to continue
 * the context establishment phase.  A return value of true signals that
 * the local end of the context is established.  This may still require
 * that a token be sent to the peer, if one is produced by GSS-API.
 * During the context establishment phase, the {@link #isProtReady()}
 * method may be called to determine if the context can be used for the
 * per-message operations.  This allows applications to use per-message
 * operations on contexts which aren't fully established.</p>
 *
 * <p>After the context has been established or the {@link #isProtReady()}
 * method returns <code>true</code>, the query routines can be invoked to
 * determine the actual characteristics and services of the established
 * context.  The application can also start using the per-message methods
 * of {@link #wrap(byte[],int,int,org.ietf.jgss.MessageProp)} and
 * {@link #getMIC(byte[],int,int,org.ietf.jgss.MessageProp)} to obtain
 * cryptographic operations on application supplied data.</p>
 *
 * <p>When the context is no longer needed, the application should call
 * {@link dispose()} to release any system resources the context may be
 * using.</p>
 *
 * <h3>Example Code</h3>
 *
 * <pre>
GSSManager mgr = GSSManager.getInstance();

// start by creating the name for a service entity
GSSName targetName = mgr.createName("service@host",
                                    GSSName.NT_HOSTBASED_SERVICE);

// create a context using default credentials for the above entity
// and the implementation specific default mechanism
GSSContext context = mgr.createContext(targetName,
                                       null,   // default mechanism
                                       null,   // default credentials
                                       GSSContext.INDEFINITE_LIFETIME);

// set desired context options - all others are false by default
context.requestConf(true);
context.requestMutualAuth(true);
context.requestReplayDet(true);
context.requestSequenceDet(true);

// establish a context between peers - using byte arrays
byte []inTok = new byte[0];

try
  {
    do
      {
        byte[] outTok = context.initSecContext(inTok, 0,
                                               inTok.length);

        // send the token if present
        if (outTok != null)
          sendToken(outTok);

        // check if we should expect more tokens
        if (context.isEstablished())
          break;

        // another token expected from peer
        inTok = readToken();

      }
    while (true);
  }
catch (GSSException e)
  {
    print("GSSAPI error: " + e.getMessage());
  }

// display context information
print("Remaining lifetime in seconds = " + context.getLifetime());
print("Context mechanism = " + context.getMech().toString());
print("Initiator = " + context.getSrcName().toString());
print("Acceptor = " + context.getTargName().toString());

if (context.getConfState())
  print("Confidentiality security service available");

if (context.getIntegState())
  print("Integrity security service available");

// perform wrap on an application supplied message, appMsg,
// using QOP = 0, and requesting privacy service
byte[] appMsg ...
MessageProp mProp = new MessageProp(0, true);
byte[] tok = context.wrap(appMsg, 0, appMsg.length, mProp);

if (mProp.getPrivacy())
  print("Message protected with privacy.");

sendToken(tok);


// release the local-end of the context
context.dispose();
 * </pre>
 */
public interface GSSContext
{

  // Constants.
  // -------------------------------------------------------------------------

  /**
   * A lifetime constant representing the default context lifetime.
   */
  int DEFAULT_LIFETIME = 0;

  /**
   * A lifetime constant representing indefinite context lifetime.
   */
  int INDEFINITE_LIFETIME = Integer.MAX_VALUE;

  // Methods.
  // -------------------------------------------------------------------------

  /**
   * <p>Called by the context initiator to start the context creation
   * process.  This is equivalent to the stream based method except that
   * the token buffers are handled as byte arrays instead of using stream
   * objects.  This method may return an output token which the
   * application will need to send to the peer for processing by the
   * accept call.  Typically, the application would do so by calling the
   * {@link OutputStream#flush()} method on an OutputStream that
   * encapsulates the connection between the two peers.  The application
   * can call {@link #isEstablished()} to determine if the context
   * establishment phase is complete for this peer.  A return value of
   * <code>false</code> from {@link #isEstablished()} indicates that more
   * tokens are expected to be supplied to the initSecContext() method. Note
   * that it is possible that the initSecContext() method return a token for
   * the peer, and {@link #isEstablished()} to return <code>true</code> also.
   * This indicates that the token needs to be sent to the peer, but the local
   * end of the context is now fully established.</p>
   *
   * <p>Upon completion of the context establishment, the available context
   * options may be queried through the get methods.</p>
   *
   * @param inputBuf Token generated by the peer. This parameter is ignored
   *                 on the first call.
   * @param offset   The offset within the <i>inputBuf</i> where the token
   *                 begins.
   * @param len      The length of the token within the <i>inputBuf</i>
   *                 (starting at the offset).
   * @return The output token, if any.
   * @throws GSSException If this operation fails.
   */
  byte[] initSecContext(byte[] inputBuf, int offset, int len)
    throws GSSException;

  /**
   * <p>Called by the context initiator to start the context creation
   * process.  This is equivalent to the byte array based method.  This
   * method may write an output token to the <i>outStream</i>, which the
   * application will need to send to the peer for processing by the
   * accept call. Typically, the application would do so by calling the
   * {@link OutputStream#flush()} method on an OutputStream that encapsulates
   * the connection between the two peers. The application can call {@link
   * #isEstablished()} to determine if the context establishment phase is
   * complete for this peer. A return value of <code>false</code> from
   * isEstablished indicates that more tokens are expected to be supplied
   * to the initSecContext() method. Note that it is possible that the
   * initSecContext() method return a token for the peer, and {@link
   * #isEstablished() return <code>true</code> also. This indicates that
   * the token needs to be sent to the peer, but the local end of the context
   * is now fully established.</p>
   *
   * <p>The GSS-API authentication tokens contain a definitive start and end.
   * This method will attempt to read one of these tokens per invocation,
   * and may block on the stream if only part of the token is available.</p>
   *
   * <p>Upon completion of the context establishment, the available context
   * options may be queried through the get methods.</p>
   *
   * @param inStream  Contains the token generated by the peer. This
   *                  parameter is ignored on the first call.
   * @param outStream Output stream where the output token will be written.
   *                  During the final stage of context establishment, there
   *                  may be no bytes written.
   * @return The number of bytes written to <i>outStream</i>, or 0 if no
   *         token is written.
   * @throws GSSException If this operation fails.
   */
  int initSecContext(InputStream inStream, OutputStream outStream)
    throws GSSException;

  /**
   * <p>Called by the context acceptor upon receiving a token from the peer.
   * This call is equivalent to the stream based method except that the
   * token buffers are handled as byte arrays instead of using stream
   * objects.</p>
   *
   * <p>This method may return an output token which the application will
   * need to send to the peer for further processing by the init call.</p>
   *
   * <p><code>null</code> return value indicates that no token needs to be
   * sent to the peer. The application can call {@link #isEstablished()}
   * to determine if the context establishment phase is complete for this
   * peer. A return value of <code>false</code> from {@link #isEstablished()}
   * indicates that more tokens are expected to be supplied to this
   * method.</p>
   *
   * <p>Note that it is possible that acceptSecContext() return a token for
   * the peer, and isEstablished() return <code>true</code> also. This
   * indicates that the token needs to be sent to the peer, but the local
   * end of the context is now fully established.</p>
   *
   * <p>Upon completion of the context establishment, the available context
   * options may be queried through the get methods.</p>
   *
   * @param inTok  Token generated by the peer.
   * @param offset The offset within the <i>inTok</i> where the token begins.
   * @param len    The length of the token within the <i>inTok</i> (starting
   *               at the offset).
   * @return The output token, if any.
   * @throws GSSException If this operation fails.
   */
  byte[] acceptSecContext(byte[] inTok, int offset, int len)
    throws GSSException;

  /**
   * <p>Called by the context acceptor upon receiving a token from the peer.
   * This call is equivalent to the byte array method.  It may write an
   * output token to the outStream, which the application will need to
   * send to the peer for processing by its initSecContext method.
   * Typically, the application would do so by calling the {@link
   * OutputStream#flush()} method on an OutputStream that encapsulates the
   * connection between the two peers. The application can call {@link
   * #isEstablished()} to determine if the context establishment phase is
   * complete for this peer. A return value of <code>false</code> from
   * {@link #isEstablished()} indicates that more tokens are expected to be
   * supplied to this method.</p>
   *
   * <p>Note that it is possible that acceptSecContext() return a token for
   * the peer, and isEstablished() return <code>true</code> also. This
   * indicates that the token needs to be sent to the peer, but the local
   * end of the context is now fully established.</p>
   *
   * <p>The GSS-API authentication tokens contain a definitive start and end.
   * This method will attempt to read one of these tokens per invocation,
   * and may block on the stream if only part of the token is available.</p>
   *
   * <p>Upon completion of the context establishment, the available context
   * options may be queried through the get methods.</p>
   *
   * @param inStream  Contains the token generated by the peer.
   * @param outStream Output stream where the output token will be written.
   *                  During the final stage of context establishment, there
   *                  may be no bytes written.
   * @return The number of bytes written, or 0 if no token is written.
   * @throws GSSException If this operation fails.
   */
  void acceptSecContext(InputStream inStream, OutputStream outStream)
    throws GSSException;

  /**
   * Used during context establishment to determine the state of the
   * context. Returns <code>true</code> if this is a fully established
   * context on the caller's side and no more tokens are needed from the
   * peer. Should be called after a call to {@link
   * #initSecContext(byte[],int,int)} or {@link
   * #acceptSecContext(byte[],int,int)} when no {@link GSSException}
   * is thrown.
   *
   * @return True of this context is fully established on this side.
   */
  boolean isEstablished();

  /**
   * Releases any system resources and cryptographic information stored in
   * the context object. This will invalidate the context.
   *
   * @throws GSSException If this operation fails.
   */
  void dispose() throws GSSException;

  /**
   * <p>Returns the maximum message size that, if presented to the
   * {@link #wrap(byte[],int,int,org.ietf.jgss.MessageProp)} method with
   * the same <i>confReq</i> and <i>qop</i> parameters, will result in an
   * output token containing no more than the <i>maxTokenSize</i> bytes.</p>
   *
   * <p>This call is intended for use by applications that communicate over
   * protocols that impose a maximum message size.  It enables the
   * application to fragment messages prior to applying protection.</p>
   *
   * <p>GSS-API implementations are recommended but not required to detect
   * invalid QOP values when getWrapSizeLimit is called.  This routine
   * guarantees only a maximum message size, not the availability of
   * specific QOP values for message protection.</p>
   *
   * <p>Successful completion of this call does not guarantee that wrap will
   * be able to protect a message of the computed length, since this
   * ability may depend on the availability of system resources at the
   * time that wrap is called.  However, if the implementation itself
   * imposes an upper limit on the length of messages that may be
   * processed by wrap, the implementation should not return a value that
   * is greater than this length.</p>
   *
   * @param qop          Indicates the level of protection wrap will be asked
   *                     to provide.
   * @param confReq      Indicates if wrap will be asked to provide privacy
   *                     service.
   * @param maxTokenSize The desired maximum size of the token emitted
   *                     by {@link #wrap(byte[],int,int,org.ietf.jgss.MessageProp)}.
   * @return The maximum wrapped output size.
   * @throws GSSException If this operation fails.
   */
  int getWrapSizeLimit(int qop, boolean confReq, int maxTokenSize)
    throws GSSException;

  /**
   * <p>Applies per-message security services over the established security
   * context.  The method will return a token with a cryptographic MIC and
   * may optionally encrypt the specified <i>inBuf</i>. This method is
   * equivalent in functionality to its stream counterpart. The returned
   * byte array will contain both the MIC and the message.</p>
   *
   * <p>The {@link MessageProp} object is instantiated by the application
   * and used to specify a QOP value which selects cryptographic algorithms,
   * and a privacy service to optionally encrypt the message. The underlying
   * mechanism that is used in the call may not be able to provide the
   * privacy service.  It sets the actual privacy service that it does
   * provide in this {@link MessageProp} object which the caller should then
   * query upon return. If the mechanism is not able to provide the
   * requested QOP, it throws a {@link GSSException} with the {@link
   * GSSException#BAD_QOP} code.</p>
   *
   * <p>Since some application-level protocols may wish to use tokens emitted
   * by wrap to provide "secure framing", implementations should support
   * the wrapping of zero-length messages.</p>
   *
   * <p>The application will be responsible for sending the token to the
   * peer.</p>
   *
   * @param inBuf   Application data to be protected.
   * @param offset  The offset within the inBuf where the data begins.
   * @param len     The length of the data within the inBuf (starting at
   *                the offset).
   * @param msgProp Instance of {@link MessageProp} that is used by the
   *                application to set the desired QOP and privacy state.
   *                Set the desired QOP to 0 to request the default QOP.
   *                Upon return from this method, this object will contain
   *                the the actual privacy state that was applied to the
   *                message by the underlying mechanism.
   * @return The wrapped data.
   * @throws GSSException If this operation fails.
   */
  byte[] wrap(byte[] inBuf, int offset, int len, MessageProp msgProp)
    throws GSSException;

  /**
   * <p>Allows to apply per-message security services over the established
   * security context.  The method will produce a token with a
   * cryptographic MIC and may optionally encrypt the message in inStream.
   * The outStream will contain both the MIC and the message.</p>
   *
   * <p>The {@link MessageProp} object is instantiated by the application and
   * used to specify a QOP value which selects cryptographic algorithms, and
   * a privacy service to optionally encrypt the message.  The underlying
   * mechanism that is used in the call may not be able to provide the
   * privacy service.  It sets the actual privacy service that it does
   * provide in this MessageProp object which the caller should then query
   * upon return.  If the mechanism is not able to provide the requested
   * QOP, it throws a {@link GSSException} with the {@link
   * GSSException#BAD_QOP} code.</p>
   *
   * <p>Since some application-level protocols may wish to use tokens emitted
   * by wrap to provide "secure framing", implementations should support
   * the wrapping of zero-length messages.</p>
   *
   * <p>The application will be responsible for sending the token to the
   * peer.</p>
   *
   * @param inStream  Input stream containing the application data to be
   *                  protected.
   * @param outStream The output stream to write the protected message to.
   *                  The application is responsible for sending this to the
   *                  other peer for processing in its unwrap method.
   * @param msgProp   Instance of {@link MessageProp} that is used by the
   *                  application to set the desired QOP and privacy state.
   *                  Set the desired QOP to 0 to request the default QOP.
   *                  Upon return from this method, this object will contain
   *                  the the actual privacy state that was applied to the
   *                  message by the underlying mechanism.
   * @throws GSSException If this operation fails.
   */
  void wrap(InputStream inStream, OutputStream outStream, MessageProp msgProp)
    throws GSSException;

  /**
   * <p>Used by the peer application to process tokens generated with the
   * wrap call. This call is equal in functionality to its stream
   * counterpart. The method will return the message supplied in the peer
   * application to the wrap call, verifying the embedded MIC.</p>
   *
   * <p>The {@link MessageProp} object is instantiated by the application and
   * is used by the underlying mechanism to return information to the caller
   * such as the QOP, whether confidentiality was applied to the message, and
   * other supplementary message state information.</p>
   *
   * <p>Since some application-level protocols may wish to use tokens emitted
   * by wrap to provide "secure framing", implementations should support
   * the wrapping and unwrapping of zero-length messages.</p>
   *
   * @param inBuf   GSS-API wrap token received from peer.
   * @param offset  The offset within the inBuf where the token begins.
   * @param len     The length of the token within the inBuf (starting at
   *                the offset).
   * @param msgProp Upon return from the method, this object will contain
   *                the applied QOP, the privacy state of the message, and
   *                supplementary information stating whether the token was
   *                a duplicate, old, out of sequence or arriving after a gap.
   * @return The unwrapped token.
   * @throws GSSException If this operation fails.
   */
  byte[] unwrap(byte[] inBuf, int offset, int len, MessageProp msgProp)
    throws GSSException;

  /**
   * <p>Used by the peer application to process tokens generated with the
   * wrap call.  This call is equal in functionality to its byte array
   * counterpart.  It will produce the message supplied in the peer
   * application to the wrap call, verifying the embedded MIC.</p>
   *
   * <p>The {@link MessageProp} object is instantiated by the application
   * and is used by the underlying mechanism to return information to the
   * caller such as the QOP, whether confidentiality was applied to the
   * message, and other supplementary message state information.</p>
   *
   * <p>Since some application-level protocols may wish to use tokens emitted
   * by wrap to provide "secure framing", implementations should support
   * the wrapping and unwrapping of zero-length messages.</p>
   *
   * @param inStream  Input stream containing the GSS-API wrap token
   *                  received from the peer.
   * @param outStream The output stream to write the application message to.
   * @param msgProp   Upon return from the method, this object will contain
   *                  the applied QOP, the privacy state of the message, and
   *                  supplementary information stating whether the token was
   *                  a duplicate, old, out of sequence or arriving after a gap.
   * @throws GSSException If this operation fails.
   */
  void unwrap(InputStream inStream, OutputStream outStream, MessageProp msgProp)
    throws GSSException;

  /**
   * <p>Returns a token containing a cryptographic MIC for the supplied
   * message, for transfer to the peer application.  Unlike wrap, which
   * encapsulates the user message in the returned token, only the message
   * MIC is returned in the output token.  This method is identical in
   * functionality to its stream counterpart.</p>
   *
   * <p>Note that privacy can only be applied through the wrap call.</p>
   *
   * <p>Since some application-level protocols may wish to use tokens emitted
   * by getMIC to provide "secure framing", implementations should support
   * derivation of MICs from zero-length messages.</p>
   *
   * @param inMsg   Message to generate MIC over.
   * @param offset  The offset within the inMsg where the token begins.
   * @param len     The length of the token within the inMsg (starting at
   *                the offset).
   * @param msgProp Instance of MessageProp that is used by the
   *                application to set the desired QOP.  Set the desired
   *                QOP to 0 in msgProp to request the default QOP.
   *                Alternatively pass in <code>null</code> for msgProp to
   *                request default QOP.
   * @return The MIC.
   * @throws GSSException If this operation fails.
   */
  byte[] getMIC(byte[] inMsg, int offset, int len, MessageProp msgProp)
    throws GSSException;

  /**
   * <p>Produces a token containing a cryptographic MIC for the supplied
   * message, for transfer to the peer application.  Unlike wrap, which
   * encapsulates the user message in the returned token, only the message
   * MIC is produced in the output token.  This method is identical in
   * functionality to its byte array counterpart.</p>
   *
   * <p>Note that privacy can only be applied through the wrap call.</p>
   *
   * <p>Since some application-level protocols may wish to use tokens emitted
   * by getMIC to provide "secure framing", implementations should support
   * derivation of MICs from zero-length messages.</p>
   *
   * @param inStream  Input stream containing the message to generate
   *                  the MIC over.
   * @param outStream Output stream to write the GSS-API output token to.
   * @param mgProp    Instance of MessageProp that is used by the
   *                  application to set the desired QOP.  Set the desired
   *                  QOP to 0 in msgProp to request the default QOP.
   *                  Alternatively pass in <code>null</code> for msgProp
   *                  to request default QOP.
   * @throws GSSException If this operation fails.
   */
  void getMIC(InputStream inStream, OutputStream outStream, MessageProp mgProp)
    throws GSSException;

  /**
   * <p>Verifies the cryptographic MIC, contained in the token parameter,
   * over the supplied message.  This method is equivalent in
   * functionality to its stream counterpart.</p>
   *
   * <p>The MessageProp object is instantiated by the application and is used
   * by the underlying mechanism to return information to the caller such
   * as the QOP indicating the strength of protection that was applied to
   * the message and other supplementary message state information.</p>
   *
   * <p>Since some application-level protocols may wish to use tokens emitted
   * by getMIC to provide "secure framing", implementations should support
   * the calculation and verification of MICs over zero-length messages.</p>
   *
   * @param inTok     Token generated by peer's getMIC method.
   * @param tokOffset The offset within the inTok where the token begins.
   * @param tokLen    The length of the token within the inTok (starting at
   *                  the offset).
   * @param inMsg     Application message to verify the cryptographic MIC
   *                  over.
   * @param msgOffset The offset within the inMsg where the message begins.
   * @param msgLen    The length of the message within the inMsg (starting
   *                  at the offset).
   * @param msgProp   Upon return from the method, this object will contain
   *                  the applied QOP and supplementary information
   *                  stating whether the token was a duplicate, old, out
   *                  of sequence or arriving after a gap.  The
   *                  confidentiality state will be set to <code>false</code>.
   * @throws GSSException If this operation fails.
   */
  void verifyMIC(byte[] inTok, int tokOffset, int tokLen, byte[] inMsg,
                 int msgOffset, int msgLen, MessageProp msgProp)
    throws GSSException;

  /**
   * <p>Verifies the cryptographic MIC, contained in the token parameter,
   * over the supplied message.  This method is equivalent in
   * functionality to its byte array counterpart.</p>
   *
   * <p>The MessageProp object is instantiated by the application and is used
   * by the underlying mechanism to return information to the caller such
   * as the QOP indicating the strength of protection that was applied to
   * the message and other supplementary message state information.</p>
   *
   * <p>Since some application-level protocols may wish to use tokens emitted
   * by getMIC to provide "secure framing", implementations should support
   * the calculation and verification of MICs over zero-length messages.</p>
   *
   * @param tokStream Input stream containing the token generated by peer's
   *                  getMIC method.
   * @param msgStream Input stream containing the application message to
   *                  verify the cryptographic MIC over.
   * @param msgProp   Upon return from the method, this object will contain
   *                  the applied QOP and supplementary information
   *                  stating whether the token was a duplicate, old, out of
   *                  sequence or arriving after a gap.  The confidentiality
   *                  state will be set to <code>false</code>.
   * @throws GSSException If this operation fails.
   */
  void verifyMIC(InputStream tokStream, InputStream msgStream, MessageProp msgProp)
    throws GSSException;

  /**
   * <p>Provided to support the sharing of work between multiple processes.
   * This routine will typically be used by the context-acceptor, in an
   * application where a single process receives incoming connection
   * requests and accepts security contexts over them, then passes the
   * established context to one or more other processes for message
   * exchange.</p>
   *
   * <p>This method deactivates the security context and creates an
   * interprocess token which, when passed to the byte array constructor
   * of the GSSContext interface in another process, will re-activate the
   * context in the second process.  Only a single instantiation of a
   * given context may be active at any one time; a subsequent attempt by
   * a context exporter to access the exported security context will fail.</p>
   *
   * <p>The implementation may constrain the set of processes by which the
   * interprocess token may be imported, either as a function of local
   * security policy, or as a result of implementation decisions.  For
   * example, some implementations may constrain contexts to be passed
   * only between processes that run under the same account, or which are
   * part of the same process group.</p>
   *
   * <p>The interprocess token may contain security-sensitive information
   * (for example cryptographic keys).  While mechanisms are encouraged to
   * either avoid placing such sensitive information within interprocess
   * tokens, or to encrypt the token before returning it to the
   * application, in a typical GSS-API implementation this may not be
   * possible.  Thus the application must take care to protect the
   * interprocess token, and ensure that any process to which the token is
   * transferred is trustworthy.</p>
   *
   * @return The exported context.
   * @throws GSSException If this operation fails.
   */
  byte[] export() throws GSSException;

  /**
   * <p>Sets the request state of the mutual authentication flag for the
   * context.  This method is only valid before the context creation
   * process begins and only for the initiator.</p>
   *
   * @param state Boolean representing if mutual authentication should
   *              be requested during context establishment.
   * @throws GSSException If this operation fails.
   */
  void requestMutualAuth(boolean state) throws GSSException;

  /**
   * <p>Sets the request state of the replay detection service for the
   * context.  This method is only valid before the context creation
   * process begins and only for the initiator.</p>
   *
   * @param state Boolean representing if replay detection is desired
   *              over the established context.
   * @throws GSSException If this operation fails.
   */
  void requestReplayDet(boolean state) throws GSSException;

  /**
   * <p>Sets the request state for the sequence checking service of the
   * context.  This method is only valid before the context creation
   * process begins and only for the initiator.</p>
   *
   * @param state Boolean representing if sequence detection is desired
   *              over the established context.
   * @throws GSSException If this operation fails.
   */
  void requestSequenceDet(boolean state) throws GSSException;

  /**
   * <p>Sets the request state for the credential delegation flag for the
   * context.  This method is only valid before the context creation
   * process begins and only for the initiator.</p>
   *
   * @param state Boolean representing if credential delegation is
   *              desired.
   * @throws GSSException If this operation fails.
   */
  void requestCredDeleg(boolean state) throws GSSException;

  /**
   * <p>Requests anonymous support over the context.  This method is only
   * valid before the context creation process begins and only for the
   * initiator.</p>
   *
   * @param state Boolean representing if anonymity support is requested.
   * @throws GSSException If this operation fails.
   */
  void requestAnonymity(boolean state) throws GSSException;

  /**
   * <p>Requests that confidentiality service be available over the context.
   * This method is only valid before the context creation process begins
   * and only for the initiator.</p>
   *
   * @param state Boolean indicating if confidentiality services are to
   *              be requested for the context.
   * @throws GSSException If this operation fails.
   */
  void requestConf(boolean state) throws GSSException;

  /**
   * <p>Requests that integrity services be available over the context. This
   * method is only valid before the context creation process begins and
   * only for the initiator.</p>
   *
   * @param state Boolean indicating if integrity services are to be
   *              requested for the context.
   * @throws GSSException If this operation fails.
   */
  void requestInteg(boolean state) throws GSSException;

  /**
   * <p>Sets the desired lifetime for the context in seconds. This method is
   * only valid before the context creation process begins and only for
   * the initiator. Use {@link #INDEFINITE_LIFETIME} and {@link
   * #DEFAULT_LIFETIME} to request indefinite or default context lifetime.</p>
   *
   * @param lifetime The desired context lifetime in seconds.
   * @throws GSSException If this operation fails.
   */
  void requestLifetime(int lifetime) throws GSSException;

  /**
   * <p>Sets the channel bindings to be used during context establishment.
   * This method is only valid before the context creation process begins.</p>
   *
   * @param cb Channel bindings to be used.
   * @throws GSSException If this operation fails.
   */
  void setChannelBinding(ChannelBinding cb) throws GSSException;

  /**
   * <p>Returns the state of the delegated credentials for the context.
   * When issued before context establishment is completed or when the
   * isProtReady method returns "false", it returns the desired state,
   * otherwise it will indicate the actual state over the established
   * context.</p>
   *
   * @return The state of the delegated credentials for the context.
   */
  boolean getCredDelegState();

  /**
   * <p>Returns the state of the mutual authentication option for the
   * context.  When issued before context establishment completes or when
   * the isProtReady method returns "false", it returns the desired state,
   * otherwise it will indicate the actual state over the established
   * context.</p>
   *
   * @return The state of the mutual authentication option.
   */
  boolean getMutualAuthState();

  /**
   * <p>Returns the state of the replay detection option for the context.
   * When issued before context establishment completes or when the
   * isProtReady method returns "false", it returns the desired state,
   * otherwise it will indicate the actual state over the established
   * context.</p>
   *
   * @return The state of the replay detection option.
   */
  boolean getReplayDetState();

  /**
   * <p>Returns the state of the sequence detection option for the context.
   * When issued before context establishment completes or when the
   * isProtReady method returns "false", it returns the desired state,
   * otherwise it will indicate the actual state over the established
   * context.</p>
   *
   * @return The state of the sequence detection option.
   */
  boolean getSequenceDetState();

  /**
   * <p>Returns "true" if this is an anonymous context. When issued before
   * context establishment completes or when the isProtReady method
   * returns "false", it returns the desired state, otherwise it will
   * indicate the actual state over the established context.</p>
   *
   * @return True if this is an anonymous context.
   */
  boolean getAnonymityState();

  /**
   * <p>Returns "true" if the context is transferable to other processes
   * through the use of the {@link #export()} method. This call is only
   * valid on fully established contexts.</p>
   *
   * @return True if the context is transferable.
   * @throws GSSException If this operation fails.
   */
  boolean isTransferable() throws GSSException;

  /**
   * <p>Returns "true" if the per message operations can be applied over
   * the context.  Some mechanisms may allow the usage of per-message
   * operations before the context is fully established.  This will also
   * indicate that the get methods will return actual context state
   * characteristics instead of the desired ones.</p>
   *
   * @return True if the per message operations can be applied over
   *         the context.
   */
  boolean isProtReady();

  /**
   * <p>Returns the confidentiality service state over the context. When
   * issued before context establishment completes or when the isProtReady
   * method returns "false", it returns the desired state, otherwise it
   * will indicate the actual state over the established context.</p>
   *
   * @return True the confidentiality service state.
   */
  boolean getConfState();

  /**
   * <p>Returns the integrity service state over the context. When issued
   * before context establishment completes or when the isProtReady method
   * returns "false", it returns the desired state, otherwise it will
   * indicate the actual state over the established context.</p>
   *
   * @return The integrity service state.
   */
  boolean getIntegState();

  /**
   * <p>Returns the context lifetime in seconds. When issued before context
   * establishment completes or when the isProtReady method returns
   * "false", it returns the desired lifetime, otherwise it will indicate
   * the remaining lifetime for the context.</p>
   *
   * @return The lifetime.
   */
  int getLifetime();

  /**
   * <p>Returns the name of the context initiator. This call is valid only
   * after the context is fully established or the isProtReady method
   * returns "true".  It is guaranteed to return an MN.</p>
   *
   * @return The name of the context initiator.
   * @throws GSSException If this operation fails.
   */
  GSSName getSrcName() throws GSSException;

  /**
   * <p>Returns the name of the context target (acceptor).  This call is
   * valid only after the context is fully established or the isProtReady
   * method returns "true".  It is guaranteed to return an MN.</p>
   *
   * @return The name of the context target.
   * @throws GSSException If this operation fails.
   */
  GSSName getTargName() throws GSSException;

  /**
   * <p>Returns the mechanism oid for this context. This method may be called
   * before the context is fully established, but the mechanism returned
   * may change on successive calls in negotiated mechanism case.</p>
   *
   * @return The mechanism OID.
   * @throws GSSException If this operation fails.
   */
  Oid getMech() throws GSSException;

  /**
   * <p>Returns the delegated credential object on the acceptor's side.
   * To check for availability of delegated credentials call
   * {@link #getDelegCredState()}. This call is only valid on fully
   * established contexts.</p>
   *
   * @return The delegated credential object.
   * @throws GSSException If this operation fails.
   */
  GSSCredential getDelegCred() throws GSSException;

  /**
   * <p>Returns "true" if this is the initiator of the context. This call is
   * only valid after the context creation process has started.</p>
   *
   * @return True if this is the initiator.
   * @throws GSSException If this operation fails.
   */
  boolean isInitiator() throws GSSException;
}
