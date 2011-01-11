/* AbstractHandshake.java -- abstract handshake handler.
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

import gnu.classpath.debug.Component;
import gnu.classpath.debug.SystemLogger;
import gnu.java.security.action.GetSecurityPropertyAction;
import gnu.java.security.prng.IRandom;
import gnu.java.security.prng.LimitReachedException;
import gnu.java.security.util.ByteArray;
import gnu.javax.security.auth.callback.CertificateCallback;
import gnu.javax.security.auth.callback.DefaultCallbackHandler;

import java.nio.ByteBuffer;
import java.security.AccessController;
import java.security.DigestException;
import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.KeyManagementException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.PrivilegedExceptionAction;
import java.security.SecureRandom;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.zip.Deflater;
import java.util.zip.Inflater;

import javax.crypto.Cipher;
import javax.crypto.KeyAgreement;
import javax.crypto.Mac;
import javax.crypto.NoSuchPaddingException;
import javax.crypto.SecretKey;
import javax.crypto.interfaces.DHPrivateKey;
import javax.crypto.interfaces.DHPublicKey;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;
import javax.net.ssl.SSLEngineResult;
import javax.net.ssl.SSLException;
import javax.net.ssl.X509TrustManager;
import javax.net.ssl.SSLEngineResult.HandshakeStatus;
import javax.security.auth.callback.Callback;
import javax.security.auth.callback.CallbackHandler;
import javax.security.auth.callback.ConfirmationCallback;

/**
 * The base interface for handshake implementations. Concrete
 * subclasses of this class (one for the server, one for the client)
 * handle the HANDSHAKE content-type in communications.
 */
public abstract class AbstractHandshake
{
  protected static final SystemLogger logger = SystemLogger.SYSTEM;

  /**
   * "server finished" -- TLS 1.0 and later
   */
  protected static final byte[] SERVER_FINISHED
    = new byte[] {
      115, 101, 114, 118, 101, 114,  32, 102, 105, 110, 105, 115,
      104, 101, 100
    };

  /**
   * "client finished" -- TLS 1.0 and later
   */
  protected static final byte[] CLIENT_FINISHED
    = new byte[] {
       99, 108, 105, 101, 110, 116,  32, 102, 105, 110, 105, 115,
      104, 101, 100
    };

  /**
   * "key expansion" -- TLS 1.0 and later
   */
  private static final byte[] KEY_EXPANSION =
    new byte[] { 107, 101, 121,  32, 101, 120, 112,
                  97, 110, 115, 105, 111, 110 };

  /**
   * "master secret" -- TLS 1.0 and later
   */
  private static final byte[] MASTER_SECRET
    = new byte[] {
      109,  97, 115, 116, 101, 114,  32, 115, 101,  99, 114, 101, 116
    };

  /**
   * "client write key" -- TLS 1.0 exportable whitener.
   */
  private static final byte[] CLIENT_WRITE_KEY
    = new byte[] {
       99, 108, 105, 101, 110, 116,  32, 119, 114, 105, 116, 101,  32, 107,
      101, 121
    };

  /**
   * "server write key" -- TLS 1.0 exportable whitener.
   */
  private static final byte[] SERVER_WRITE_KEY
    = new byte[] {
      115, 101, 114, 118, 101, 114,  32, 119, 114, 105, 116, 101,  32, 107,
      101, 121
    };

  private static final byte[] IV_BLOCK
    = new byte[] {
       73,  86,  32,  98, 108, 111,  99, 107
    };

  /**
   * SSL 3.0; the string "CLNT"
   */
  private static final byte[] SENDER_CLIENT
    = new byte[] { 0x43, 0x4C, 0x4E, 0x54 };

  /**
   * SSL 3.0; the string "SRVR"
   */
  private static final byte[] SENDER_SERVER
    = new byte[] { 0x53, 0x52, 0x56, 0x52 };

  /**
   * SSL 3.0; the value 0x36 40 (for SHA-1 hashes) or 48 (for MD5 hashes)
   * times.
   */
  protected static final byte[] PAD1 = new byte[48];

  /**
   * SSL 3.0; the value 0x5c 40 (for SHA-1 hashes) or 48 (for MD5 hashes)
   * times.
   */
  protected static final byte[] PAD2 = new byte[48];

  static
  {
    Arrays.fill(PAD1, SSLHMac.PAD1);
    Arrays.fill(PAD2, SSLHMac.PAD2);
  }

  /**
   * The currently-read handshake messages. There may be zero, or
   * multiple, handshake messages in this buffer.
   */
  protected ByteBuffer handshakeBuffer;

  /**
   * The offset into `handshakeBuffer' where the first unread
   * handshake message resides.
   */
  protected int handshakeOffset;

  protected MessageDigest sha;
  protected MessageDigest md5;

  protected final SSLEngineImpl engine;
  protected KeyAgreement keyAgreement;
  protected byte[] preMasterSecret;
  protected InputSecurityParameters inParams;
  protected OutputSecurityParameters outParams;
  protected LinkedList<DelegatedTask> tasks;
  protected Random serverRandom;
  protected Random clientRandom;
  protected CompressionMethod compression;

  protected AbstractHandshake(SSLEngineImpl engine)
    throws NoSuchAlgorithmException
  {
    this.engine = engine;
    sha = MessageDigest.getInstance("SHA-1");
    md5 = MessageDigest.getInstance("MD5");
    tasks = new LinkedList<DelegatedTask>();
  }

  /**
   * Handles the next input message in the handshake. This is called
   * in response to a call to {@link javax.net.ssl.SSLEngine#unwrap}
   * for a message with content-type HANDSHAKE.
   *
   * @param record The input record. The callee should not assume that
   * the record's buffer is writable, and should not try to use it for
   * output or temporary storage.
   * @return An {@link SSLEngineResult} describing the result.
   */
  public final HandshakeStatus handleInput (ByteBuffer fragment)
    throws SSLException
  {
    if (!tasks.isEmpty())
      return HandshakeStatus.NEED_TASK;

    HandshakeStatus status = status();
    if (status != HandshakeStatus.NEED_UNWRAP)
      return status;

    // Try to read another...
    if (!pollHandshake(fragment))
      return HandshakeStatus.NEED_UNWRAP;

    while (hasMessage() && status != HandshakeStatus.NEED_WRAP)
      {
        int pos = handshakeOffset;
        status = implHandleInput();
        int len = handshakeOffset - pos;
        if (len == 0)
          {
            // Don't bother; the impl is just telling us to go around
            // again.
            continue;
          }
        if (doHash())
          {
            if (Debug.DEBUG)
              logger.logv(Component.SSL_HANDSHAKE, "hashing output\n{0}",
                          Util.hexDump((ByteBuffer) handshakeBuffer
                                       .duplicate().position(pos)
                                       .limit(pos+len), " >> "));
            sha.update((ByteBuffer) handshakeBuffer.duplicate()
                       .position(pos).limit(pos+len));
            md5.update((ByteBuffer) handshakeBuffer.duplicate()
                       .position(pos).limit(pos+len));
          }
      }
    return status;
  }

  /**
   * Called to process more handshake data. This method will be called
   * repeatedly while there is remaining handshake data, and while the
   * status is
   * @return
   * @throws SSLException
   */
  protected abstract HandshakeStatus implHandleInput()
    throws SSLException;

  /**
   * Produce more handshake output. This is called in response to a
   * call to {@link javax.net.ssl.SSLEngine#wrap}, when the handshake
   * is still in progress.
   *
   * @param record The output record; the callee should put its output
   * handshake message (or a part of it) in the argument's
   * <code>fragment</code>, and should set the record length
   * appropriately.
   * @return An {@link SSLEngineResult} describing the result.
   */
  public final HandshakeStatus handleOutput (ByteBuffer fragment)
    throws SSLException
  {
    if (!tasks.isEmpty())
      return HandshakeStatus.NEED_TASK;

    int orig = fragment.position();
    SSLEngineResult.HandshakeStatus status = implHandleOutput(fragment);
    if (doHash())
      {
        if (Debug.DEBUG)
          logger.logv(Component.SSL_HANDSHAKE, "hashing output:\n{0}",
                      Util.hexDump((ByteBuffer) fragment.duplicate().flip().position(orig), " >> "));
        sha.update((ByteBuffer) fragment.duplicate().flip().position(orig));
        md5.update((ByteBuffer) fragment.duplicate().flip().position(orig));
      }
    return status;
  }

  /**
   * Called to implement the underlying output handling. The callee should
   * attempt to fill the given buffer as much as it can; this can include
   * multiple, and even partial, handshake messages.
   *
   * @param fragment The buffer the callee should write handshake messages to.
   * @return The new status of the handshake.
   * @throws SSLException If an error occurs processing the output message.
   */
  protected abstract SSLEngineResult.HandshakeStatus implHandleOutput (ByteBuffer fragment)
    throws SSLException;

  /**
   * Return a new instance of input security parameters, initialized with
   * the session key. It is, of course, only valid to invoke this method
   * once the handshake is complete, and the session keys established.
   *
   * <p>In the presence of a well-behaving peer, this should be called once
   * the <code>ChangeCipherSpec</code> message is recieved.
   *
   * @return The input parameters for the newly established session.
   * @throws SSLException If the handshake is not complete.
   */
  final InputSecurityParameters getInputParams() throws SSLException
  {
    checkKeyExchange();
    return inParams;
  }

  /**
   * Return a new instance of output security parameters, initialized with
   * the session key. This should be called after the
   * <code>ChangeCipherSpec</code> message is sent to the peer.
   *
   * @return The output parameters for the newly established session.
   * @throws SSLException If the handshake is not complete.
   */
  final OutputSecurityParameters getOutputParams() throws SSLException
  {
    checkKeyExchange();
    return outParams;
  }

  /**
   * Fetch a delegated task waiting to run, if any.
   *
   * @return The task.
   */
  final Runnable getTask()
  {
    if (tasks.isEmpty())
      return null;
    return tasks.removeFirst();
  }

  /**
   * Used by the skeletal code to query the current status of the handshake.
   * This <em>should</em> be the same value as returned by the previous call
   * to {@link #implHandleOutput(ByteBuffer)} or {@link
   *  #implHandleInput(ByteBuffer)}.
   *
   * @return The current handshake status.
   */
  abstract HandshakeStatus status();

  /**
   * Check if the key exchange completed successfully, throwing an exception
   * if not.
   *
   * <p>Note that we assume that the caller of our SSLEngine is correct, and
   * that they did run the delegated tasks that encapsulate the key exchange.
   * What we are primarily checking, therefore, is that no error occurred in the
   * key exchange operation itself.
   *
   * @throws SSLException If the key exchange did not complete successfully.
   */
  abstract void checkKeyExchange() throws SSLException;

  /**
   * Handle an SSLv2 client hello. This is only used by SSL servers.
   *
   * @param hello The hello message.
   */
  abstract void handleV2Hello(ByteBuffer hello) throws SSLException;

  /**
   * Attempt to read the next handshake message from the given
   * record. If only a partial handshake message is available, then
   * this method saves the incoming bytes and returns false. If a
   * complete handshake is read, or if there was one buffered in the
   * handshake buffer, this method returns true, and `handshakeBuffer'
   * can be used to read the handshake.
   *
   * @param record The input record.
   * @return True if a complete handshake is present in the buffer;
   * false if only a partial one.
   */
  protected boolean pollHandshake (final ByteBuffer fragment)
  {
    // Allocate space for the new fragment.
    if (handshakeBuffer == null
        || handshakeBuffer.remaining() < fragment.remaining())
      {
        // We need space for anything still unread in the handshake
        // buffer...
        int len = ((handshakeBuffer == null) ? 0
                   : handshakeBuffer.position() - handshakeOffset);

        // Plus room for the incoming record.
        len += fragment.remaining();
        reallocateBuffer(len);
      }

    if (Debug.DEBUG)
      logger.logv(Component.SSL_HANDSHAKE, "inserting {0} into {1}",
                  fragment, handshakeBuffer);

    // Put the fragment into the buffer.
    handshakeBuffer.put(fragment);

    return hasMessage();
  }

  protected boolean doHash()
  {
    return true;
  }

  /**
   * Tell if the handshake buffer currently has a full handshake
   * message.
   */
  protected boolean hasMessage()
  {
    if (handshakeBuffer == null)
      return false;
    ByteBuffer tmp = handshakeBuffer.duplicate();
    tmp.flip();
    tmp.position(handshakeOffset);
    if (Debug.DEBUG)
      logger.logv(Component.SSL_HANDSHAKE, "current buffer: {0}; test buffer {1}",
                  handshakeBuffer, tmp);
    if (tmp.remaining() < 4)
      return false;
    Handshake handshake = new Handshake(tmp.slice());
    if (Debug.DEBUG)
      logger.logv(Component.SSL_HANDSHAKE, "handshake len:{0} remaining:{1}",
                  handshake.length(), tmp.remaining());
    return (handshake.length() <= tmp.remaining() - 4);
  }

  /**
   * Reallocate the handshake buffer so it can hold `totalLen'
   * bytes. The smallest buffer allocated is 1024 bytes, and the size
   * doubles from there until the buffer is sufficiently large.
   */
  private void reallocateBuffer (final int totalLen)
  {
    int len = handshakeBuffer == null ? -1
                                      : handshakeBuffer.capacity() - (handshakeBuffer.limit() - handshakeOffset);
    if (len >= totalLen)
      {
        // Big enough; no need to reallocate; but maybe shift the contents
        // down.
        if (handshakeOffset > 0)
          {
            handshakeBuffer.flip().position(handshakeOffset);
            handshakeBuffer.compact();
            handshakeOffset = 0;
          }
        return;
      }

    // Start at 1K (probably the system's page size). Double the size
    // from there.
    len = 1024;
    while (len < totalLen)
      len = len << 1;
    ByteBuffer newBuf = ByteBuffer.allocate (len);

    // Copy the unread bytes from the old buffer.
    if (handshakeBuffer != null)
      {
        handshakeBuffer.flip ();
        handshakeBuffer.position(handshakeOffset);
        newBuf.put(handshakeBuffer);
      }
    handshakeBuffer = newBuf;

    // We just put only unread handshake messages in the new buffer;
    // the offset of the next one is now zero.
    handshakeOffset = 0;
  }

  /**
   * Generate a certificate verify message for SSLv3. In SSLv3, a different
   * algorithm was used to generate this value was subtly different than
   * that used in TLSv1.0 and later. In TLSv1.0 and later, this value is
   * just the digest over the handshake messages.
   *
   * <p>SSLv3 uses the algorithm:
   *
   * <pre>
CertificateVerify.signature.md5_hash
  MD5(master_secret + pad_2 +
      MD5(handshake_messages + master_secret + pad_1));
Certificate.signature.sha_hash
  SHA(master_secret + pad_2 +
      SHA(handshake_messages + master_secret + pad_1));</pre>
   *
   * @param md5 The running MD5 hash of the handshake.
   * @param sha The running SHA-1 hash of the handshake.
   * @param session The current session being negotiated.
   * @return The computed to-be-signed value.
   */
  protected byte[] genV3CertificateVerify(MessageDigest md5,
                                          MessageDigest sha,
                                          SessionImpl session)
  {
    byte[] md5value = null;
    if (session.suite.signatureAlgorithm() == SignatureAlgorithm.RSA)
      {
        md5.update(session.privateData.masterSecret);
        md5.update(PAD1, 0, 48);
        byte[] tmp = md5.digest();
        md5.reset();
        md5.update(session.privateData.masterSecret);
        md5.update(PAD2, 0, 48);
        md5.update(tmp);
        md5value = md5.digest();
      }

    sha.update(session.privateData.masterSecret);
    sha.update(PAD1, 0, 40);
    byte[] tmp = sha.digest();
    sha.reset();
    sha.update(session.privateData.masterSecret);
    sha.update(PAD2, 0, 40);
    sha.update(tmp);
    byte[] shavalue = sha.digest();

    if (md5value != null)
      return Util.concat(md5value, shavalue);

    return shavalue;
  }

  /**
   * Generate the session keys from the computed master secret.
   *
   * @param clientRandom The client's nonce.
   * @param serverRandom The server's nonce.
   * @param session The session being established.
   * @return The derived keys.
   */
  protected byte[][] generateKeys(Random clientRandom, Random serverRandom,
                                  SessionImpl session)
  {
    int maclen = 20; // SHA-1.
    if (session.suite.macAlgorithm() == MacAlgorithm.MD5)
      maclen = 16;
    int ivlen = 0;
    if (session.suite.cipherAlgorithm() == CipherAlgorithm.DES
        || session.suite.cipherAlgorithm() == CipherAlgorithm.DESede)
      ivlen = 8;
    if (session.suite.cipherAlgorithm() == CipherAlgorithm.AES)
      ivlen = 16;
    int keylen = session.suite.keyLength();

    byte[][] keys = new byte[6][];
    keys[0] = new byte[maclen]; // client_write_MAC_secret
    keys[1] = new byte[maclen]; // server_write_MAC_secret
    keys[2] = new byte[keylen]; // client_write_key
    keys[3] = new byte[keylen]; // server_write_key
    keys[4] = new byte[ivlen];  // client_write_iv
    keys[5] = new byte[ivlen];  // server_write_iv

    IRandom prf = null;
    if (session.version == ProtocolVersion.SSL_3)
      {
        byte[] seed = new byte[clientRandom.length()
                               + serverRandom.length()];
        serverRandom.buffer().get(seed, 0, serverRandom.length());
        clientRandom.buffer().get(seed, serverRandom.length(),
                                  clientRandom.length());
        prf = new SSLRandom();
        HashMap<String,byte[]> attr = new HashMap<String,byte[]>(2);
        attr.put(SSLRandom.SECRET, session.privateData.masterSecret);
        attr.put(SSLRandom.SEED, seed);
        prf.init(attr);
      }
    else
      {
        byte[] seed = new byte[KEY_EXPANSION.length
                               + clientRandom.length()
                               + serverRandom.length()];
        System.arraycopy(KEY_EXPANSION, 0, seed, 0, KEY_EXPANSION.length);
        serverRandom.buffer().get(seed, KEY_EXPANSION.length,
                                  serverRandom.length());
        clientRandom.buffer().get(seed, (KEY_EXPANSION.length
                                         + serverRandom.length()),
                                  clientRandom.length());

        prf = new TLSRandom();
        HashMap<String,byte[]> attr = new HashMap<String,byte[]>(2);
        attr.put(TLSRandom.SECRET, session.privateData.masterSecret);
        attr.put(TLSRandom.SEED, seed);
        prf.init(attr);
      }

    try
      {
        prf.nextBytes(keys[0], 0, keys[0].length);
        prf.nextBytes(keys[1], 0, keys[1].length);
        prf.nextBytes(keys[2], 0, keys[2].length);
        prf.nextBytes(keys[3], 0, keys[3].length);

        if (session.suite.isExportable())
          {
            if (session.version == ProtocolVersion.SSL_3)
              {
                MessageDigest md5 = MessageDigest.getInstance("MD5");
                md5.update(clientRandom.buffer());
                md5.update(serverRandom.buffer());
                byte[] d = md5.digest();
                System.arraycopy(d, 0, keys[4], 0, keys[4].length);

                md5.reset();
                md5.update(serverRandom.buffer());
                md5.update(clientRandom.buffer());
                d = md5.digest();
                System.arraycopy(d, 0, keys[5], 0, keys[5].length);

                md5.reset();
                md5.update(keys[2]);
                md5.update(clientRandom.buffer());
                md5.update(serverRandom.buffer());
                keys[2] = Util.trim(md5.digest(), 8);

                md5.reset();
                md5.update(keys[3]);
                md5.update(serverRandom.buffer());
                md5.update(clientRandom.buffer());
                keys[3] = Util.trim(md5.digest(), 8);
              }
            else
              {
                TLSRandom prf2 = new TLSRandom();
                HashMap<String,byte[]> attr = new HashMap<String,byte[]>(2);
                attr.put(TLSRandom.SECRET, keys[2]);
                byte[] seed = new byte[CLIENT_WRITE_KEY.length +
                                       clientRandom.length() +
                                       serverRandom.length()];
                System.arraycopy(CLIENT_WRITE_KEY, 0, seed, 0,
                                 CLIENT_WRITE_KEY.length);
                clientRandom.buffer().get(seed, CLIENT_WRITE_KEY.length,
                                          clientRandom.length());
                serverRandom.buffer().get(seed, CLIENT_WRITE_KEY.length
                                          + clientRandom.length(),
                                          serverRandom.length());
                attr.put(TLSRandom.SEED, seed);
                prf2.init(attr);
                keys[2] = new byte[8];
                prf2.nextBytes(keys[2], 0, keys[2].length);

                attr.put(TLSRandom.SECRET, keys[3]);
                seed = new byte[SERVER_WRITE_KEY.length +
                                serverRandom.length() +
                                clientRandom.length()];
                System.arraycopy(SERVER_WRITE_KEY, 0, seed, 0,
                                 SERVER_WRITE_KEY.length);
                serverRandom.buffer().get(seed, SERVER_WRITE_KEY.length,
                                          serverRandom.length());
                clientRandom.buffer().get(seed, SERVER_WRITE_KEY.length
                                          + serverRandom.length(),
                                          + clientRandom.length());
                attr.put(TLSRandom.SEED, seed);
                prf2.init(attr);
                keys[3] = new byte[8];
                prf2.nextBytes(keys[3], 0, keys[3].length);

                attr.put(TLSRandom.SECRET, new byte[0]);
                seed = new byte[IV_BLOCK.length +
                                clientRandom.length() +
                                serverRandom.length()];
                System.arraycopy(IV_BLOCK, 0, seed, 0, IV_BLOCK.length);
                clientRandom.buffer().get(seed, IV_BLOCK.length,
                                          clientRandom.length());
                serverRandom.buffer().get(seed, IV_BLOCK.length
                                          + clientRandom.length(),
                                          serverRandom.length());
                attr.put(TLSRandom.SEED, seed);
                prf2.init(attr);
                prf2.nextBytes(keys[4], 0, keys[4].length);
                prf2.nextBytes(keys[5], 0, keys[5].length);
              }
          }
        else
          {
            prf.nextBytes(keys[4], 0, keys[4].length);
            prf.nextBytes(keys[5], 0, keys[5].length);
          }
      }
    catch (LimitReachedException lre)
      {
        // Won't happen with our implementation.
        throw new Error(lre);
      }
    catch (NoSuchAlgorithmException nsae)
      {
        throw new Error(nsae);
      }

    if (Debug.DEBUG_KEY_EXCHANGE)
      logger.logv(Component.SSL_KEY_EXCHANGE,
                  "keys generated;\n  [0]: {0}\n  [1]: {1}\n  [2]: {2}\n" +
                  "  [3]: {3}\n  [4]: {4}\n  [5]: {5}",
                  Util.toHexString(keys[0], ':'),
                  Util.toHexString(keys[1], ':'),
                  Util.toHexString(keys[2], ':'),
                  Util.toHexString(keys[3], ':'),
                  Util.toHexString(keys[4], ':'),
                  Util.toHexString(keys[5], ':'));
    return keys;
  }

  /**
   * Generate a "finished" message. The hashes passed in are modified
   * by this function, so they should be clone copies of the digest if
   * the hash function needs to be used more.
   *
   * @param md5 The MD5 computation.
   * @param sha The SHA-1 computation.
   * @param isClient Whether or not the client-side finished message is
   *  being computed.
   * @param session The current session.
   * @return A byte buffer containing the computed finished message.
   */
  protected ByteBuffer generateFinished(MessageDigest md5,
                                        MessageDigest sha,
                                        boolean isClient,
                                        SessionImpl session)
  {
    ByteBuffer finishedBuffer = null;
    if (session.version.compareTo(ProtocolVersion.TLS_1) >= 0)
      {
        finishedBuffer = ByteBuffer.allocate(12);
        TLSRandom prf = new TLSRandom();
        byte[] md5val = md5.digest();
        byte[] shaval = sha.digest();
        if (Debug.DEBUG)
          logger.logv(Component.SSL_HANDSHAKE, "finished md5:{0} sha:{1}",
                      Util.toHexString(md5val, ':'),
                      Util.toHexString(shaval, ':'));
        byte[] seed = new byte[CLIENT_FINISHED.length
                               + md5val.length
                               + shaval.length];
        if (isClient)
          System.arraycopy(CLIENT_FINISHED, 0, seed, 0, CLIENT_FINISHED.length);
        else
          System.arraycopy(SERVER_FINISHED, 0, seed, 0, SERVER_FINISHED.length);
        System.arraycopy(md5val, 0,
                         seed, CLIENT_FINISHED.length,
                         md5val.length);
        System.arraycopy(shaval, 0,
                         seed, CLIENT_FINISHED.length + md5val.length,
                         shaval.length);
        HashMap<String, Object> params = new HashMap<String, Object>(2);
        params.put(TLSRandom.SECRET, session.privateData.masterSecret);
        params.put(TLSRandom.SEED, seed);
        prf.init(params);
        byte[] buf = new byte[12];
        prf.nextBytes(buf, 0, buf.length);
        finishedBuffer.put(buf).position(0);
      }
    else
      {
        // The SSLv3 algorithm is:
        //
        //   enum { client(0x434C4E54), server(0x53525652) } Sender;
        //
        //   struct {
        //     opaque md5_hash[16];
        //     opaque sha_hash[20];
        //   } Finished;
        //
        //   md5_hash       MD5(master_secret + pad2 +
        //                      MD5(handshake_messages + Sender +
        //                          master_secret + pad1));
        //   sha_hash        SHA(master_secret + pad2 +
        //                       SHA(handshake_messages + Sender +
        //                           master_secret + pad1));
        //

        finishedBuffer = ByteBuffer.allocate(36);

        md5.update(isClient ? SENDER_CLIENT : SENDER_SERVER);
        md5.update(session.privateData.masterSecret);
        md5.update(PAD1);

        byte[] tmp = md5.digest();
        md5.reset();
        md5.update(session.privateData.masterSecret);
        md5.update(PAD2);
        md5.update(tmp);
        finishedBuffer.put(md5.digest());

        sha.update(isClient ? SENDER_CLIENT : SENDER_SERVER);
        sha.update(session.privateData.masterSecret);
        sha.update(PAD1, 0, 40);

        tmp = sha.digest();
        sha.reset();
        sha.update(session.privateData.masterSecret);
        sha.update(PAD2, 0, 40);
        sha.update(tmp);
        finishedBuffer.put(sha.digest()).position(0);
      }
    return finishedBuffer;
  }

  protected void initDiffieHellman(DHPrivateKey dhKey, SecureRandom random)
    throws SSLException
  {
    try
      {
        keyAgreement = KeyAgreement.getInstance("DH");
        keyAgreement.init(dhKey, random);
      }
    catch (InvalidKeyException ike)
      {
        throw new SSLException(ike);
      }
    catch (NoSuchAlgorithmException nsae)
      {
        throw new SSLException(nsae);
      }
  }

  protected void generateMasterSecret(Random clientRandom,
                                      Random serverRandom,
                                      SessionImpl session)
    throws SSLException
  {
    assert(clientRandom != null);
    assert(serverRandom != null);
    assert(session != null);

    if (Debug.DEBUG_KEY_EXCHANGE)
      logger.logv(Component.SSL_KEY_EXCHANGE, "preMasterSecret:\n{0}",
                  new ByteArray(preMasterSecret));

    if (session.version == ProtocolVersion.SSL_3)
      {
        try
          {
            MessageDigest _md5 = MessageDigest.getInstance("MD5");
            MessageDigest _sha = MessageDigest.getInstance("SHA");
            session.privateData.masterSecret = new byte[48];

            _sha.update((byte) 'A');
            _sha.update(preMasterSecret);
            _sha.update(clientRandom.buffer());
            _sha.update(serverRandom.buffer());
            _md5.update(preMasterSecret);
            _md5.update(_sha.digest());
            _md5.digest(session.privateData.masterSecret, 0, 16);

            _sha.update((byte) 'B');
            _sha.update((byte) 'B');
            _sha.update(preMasterSecret);
            _sha.update(clientRandom.buffer());
            _sha.update(serverRandom.buffer());
            _md5.update(preMasterSecret);
            _md5.update(_sha.digest());
            _md5.digest(session.privateData.masterSecret, 16, 16);

            _sha.update((byte) 'C');
            _sha.update((byte) 'C');
            _sha.update((byte) 'C');
            _sha.update(preMasterSecret);
            _sha.update(clientRandom.buffer());
            _sha.update(serverRandom.buffer());
            _md5.update(preMasterSecret);
            _md5.update(_sha.digest());
            _md5.digest(session.privateData.masterSecret, 32, 16);
          }
        catch (DigestException de)
          {
            throw new SSLException(de);
          }
        catch (NoSuchAlgorithmException nsae)
          {
            throw new SSLException(nsae);
          }
      }
    else // TLSv1.0 and later
      {
        byte[] seed = new byte[clientRandom.length()
                               + serverRandom.length()
                               + MASTER_SECRET.length];
        System.arraycopy(MASTER_SECRET, 0, seed, 0, MASTER_SECRET.length);
        clientRandom.buffer().get(seed, MASTER_SECRET.length,
                                  clientRandom.length());
        serverRandom.buffer().get(seed,
                                  MASTER_SECRET.length + clientRandom.length(),
                                  serverRandom.length());
        TLSRandom prf = new TLSRandom();
        HashMap<String,byte[]> attr = new HashMap<String,byte[]>(2);
        attr.put(TLSRandom.SECRET, preMasterSecret);
        attr.put(TLSRandom.SEED, seed);
        prf.init(attr);

        session.privateData.masterSecret = new byte[48];
        prf.nextBytes(session.privateData.masterSecret, 0, 48);
      }

    if (Debug.DEBUG_KEY_EXCHANGE)
      logger.log(Component.SSL_KEY_EXCHANGE, "master_secret: {0}",
                 new ByteArray(session.privateData.masterSecret));

    // Wipe out the preMasterSecret.
    for (int i = 0; i < preMasterSecret.length; i++)
      preMasterSecret[i] = 0;
  }

  protected void setupSecurityParameters(byte[][] keys, boolean isClient,
                                         SSLEngineImpl engine,
                                         CompressionMethod compression)
    throws SSLException
  {
    assert(keys.length == 6);
    assert(engine != null);
    assert(compression != null);

    try
      {
        CipherSuite s = engine.session().suite;
        Cipher inCipher = s.cipher();
        Mac inMac = s.mac(engine.session().version);
        Inflater inflater = (compression == CompressionMethod.ZLIB
                             ? new Inflater() : null);
        inCipher.init(Cipher.DECRYPT_MODE,
                      new SecretKeySpec(keys[isClient ? 3 : 2],
                                        s.cipherAlgorithm().toString()),
                      new IvParameterSpec(keys[isClient ? 5 : 4]));
        inMac.init(new SecretKeySpec(keys[isClient ? 1 : 0],
                                     inMac.getAlgorithm()));
        inParams = new InputSecurityParameters(inCipher, inMac,
                                               inflater,
                                               engine.session(), s);

        Cipher outCipher = s.cipher();
        Mac outMac = s.mac(engine.session().version);
        Deflater deflater = (compression == CompressionMethod.ZLIB
                             ? new Deflater() : null);
        outCipher.init(Cipher.ENCRYPT_MODE,
                       new SecretKeySpec(keys[isClient ? 2 : 3],
                                         s.cipherAlgorithm().toString()),
                       new IvParameterSpec(keys[isClient ? 4 : 5]));
        outMac.init(new SecretKeySpec(keys[isClient ? 0 : 1],
                                      outMac.getAlgorithm()));
        outParams = new OutputSecurityParameters(outCipher, outMac,
                                                 deflater,
                                                 engine.session(), s);
      }
    catch (InvalidAlgorithmParameterException iape)
      {
        throw new SSLException(iape);
      }
    catch (InvalidKeyException ike)
      {
        throw new SSLException(ike);
      }
    catch (NoSuchAlgorithmException nsae)
      {
        throw new SSLException(nsae);
      }
    catch (NoSuchPaddingException nspe)
      {
        throw new SSLException(nspe);
      }
  }

  protected void generatePSKSecret(String identity, byte[] otherkey,
                                   boolean isClient)
    throws SSLException
  {
    SecretKey key = null;
    try
      {
        key = engine.contextImpl.pskManager.getKey(identity);
      }
    catch (KeyManagementException kme)
      {
      }
    if (key != null)
      {
        byte[] keyb = key.getEncoded();
        if (otherkey == null)
          {
            otherkey = new byte[keyb.length];
          }
        preMasterSecret = new byte[otherkey.length + keyb.length + 4];
        preMasterSecret[0] = (byte) (otherkey.length >>> 8);
        preMasterSecret[1] = (byte)  otherkey.length;
        System.arraycopy(otherkey, 0, preMasterSecret, 2, otherkey.length);
        preMasterSecret[otherkey.length + 2]
          = (byte) (keyb.length >>> 8);
        preMasterSecret[otherkey.length + 3]
          = (byte)  keyb.length;
        System.arraycopy(keyb, 0, preMasterSecret,
                         otherkey.length + 4, keyb.length);
      }
    else
      {
        // Generate a random, fake secret.
        preMasterSecret = new byte[8];
        preMasterSecret[1] = 2;
        preMasterSecret[5] = 2;
        preMasterSecret[6] = (byte) engine.session().random().nextInt();
        preMasterSecret[7] = (byte) engine.session().random().nextInt();
      }

    if (Debug.DEBUG_KEY_EXCHANGE)
      logger.logv(Component.SSL_KEY_EXCHANGE, "PSK identity {0} key {1}",
                  identity, key);

    generateMasterSecret(clientRandom, serverRandom,
                         engine.session());
    byte[][] keys = generateKeys(clientRandom, serverRandom,
                                 engine.session());
    setupSecurityParameters(keys, isClient, engine, compression);
  }

  protected class DHPhase extends DelegatedTask
  {
    private final DHPublicKey key;
    private final boolean full;

    protected DHPhase(DHPublicKey key)
    {
      this(key, true);
    }

    protected DHPhase(DHPublicKey key, boolean full)
    {
      this.key = key;
      this.full = full;
    }

    protected void implRun() throws InvalidKeyException, SSLException
    {
      keyAgreement.doPhase(key, true);
      preMasterSecret = keyAgreement.generateSecret();
      if (full)
        {
          generateMasterSecret(clientRandom, serverRandom, engine.session());
          byte[][] keys = generateKeys(clientRandom, serverRandom, engine.session());
          setupSecurityParameters(keys, engine.getUseClientMode(), engine, compression);
        }
    }
  }

  protected class CertVerifier extends DelegatedTask
  {
    private final boolean clientSide;
    private final X509Certificate[] chain;
    private boolean verified;

    protected CertVerifier(boolean clientSide, X509Certificate[] chain)
    {
      this.clientSide = clientSide;
      this.chain = chain;
    }

    boolean verified()
    {
      return verified;
    }

    protected void implRun()
    {
      X509TrustManager tm = engine.contextImpl.trustManager;
      if (clientSide)
        {
          try
            {
              tm.checkServerTrusted(chain, null);
              verified = true;
            }
          catch (CertificateException ce)
            {
              if (Debug.DEBUG)
                logger.log(Component.SSL_DELEGATED_TASK, "cert verify", ce);
              // For client connections, ask the user if the certificate is OK.
              CallbackHandler verify = new DefaultCallbackHandler();
              GetSecurityPropertyAction gspa
                = new GetSecurityPropertyAction("jessie.certificate.handler");
              String clazz = AccessController.doPrivileged(gspa);
              try
                {
                  ClassLoader cl =
                    AccessController.doPrivileged(new PrivilegedExceptionAction<ClassLoader>()
                      {
                        public ClassLoader run() throws Exception
                        {
                          return ClassLoader.getSystemClassLoader();
                        }
                      });
                  verify = (CallbackHandler) cl.loadClass(clazz).newInstance();
                }
              catch (Exception x)
                {
                  // Ignore.
                  if (Debug.DEBUG)
                    logger.log(Component.SSL_DELEGATED_TASK,
                               "callback handler loading", x);
                }
              // XXX Internationalize
              CertificateCallback confirm =
                new CertificateCallback(chain[0],
                "The server's certificate could not be verified. There is no proof " +
                "that this server is who it claims to be, or that their certificate " +
                "is valid. Do you wish to continue connecting? ");

              try
                {
                  verify.handle(new Callback[] { confirm });
                  verified = confirm.getSelectedIndex() == ConfirmationCallback.YES;
                }
              catch (Exception x)
                {
                  if (Debug.DEBUG)
                    logger.log(Component.SSL_DELEGATED_TASK,
                               "callback handler exception", x);
                  verified = false;
                }
            }
        }
      else
        {
          try
            {
              tm.checkClientTrusted(chain, null);
            }
          catch (CertificateException ce)
            {
              verified = false;
            }
        }

      if (verified)
        engine.session().setPeerVerified(true);
    }
  }

  protected class DHE_PSKGen extends DelegatedTask
  {
    private final DHPublicKey dhKey;
    private final SecretKey psKey;
    private final boolean isClient;

    protected DHE_PSKGen(DHPublicKey dhKey, SecretKey psKey, boolean isClient)
    {
      this.dhKey = dhKey;
      this.psKey = psKey;
      this.isClient = isClient;
    }

    /* (non-Javadoc)
     * @see gnu.javax.net.ssl.provider.DelegatedTask#implRun()
     */
    @Override protected void implRun() throws Throwable
    {
      keyAgreement.doPhase(dhKey, true);
      byte[] dhSecret = keyAgreement.generateSecret();
      byte[] psSecret = null;
      if (psKey != null)
        psSecret = psKey.getEncoded();
      else
        {
          psSecret = new byte[8];
          engine.session().random().nextBytes(psSecret);
        }

      preMasterSecret = new byte[dhSecret.length + psSecret.length + 4];
      preMasterSecret[0] = (byte) (dhSecret.length >>> 8);
      preMasterSecret[1] = (byte)  dhSecret.length;
      System.arraycopy(dhSecret, 0, preMasterSecret, 2, dhSecret.length);
      preMasterSecret[dhSecret.length + 2] = (byte) (psSecret.length >>> 8);
      preMasterSecret[dhSecret.length + 3] = (byte)  psSecret.length;
      System.arraycopy(psSecret, 0, preMasterSecret, dhSecret.length + 4,
                       psSecret.length);

      generateMasterSecret(clientRandom, serverRandom, engine.session());
      byte[][] keys = generateKeys(clientRandom, serverRandom, engine.session());
      setupSecurityParameters(keys, isClient, engine, compression);
    }
  }
}
