/* SRPClient.java -- 
   Copyright (C) 2003, 2006 Free Software Foundation, Inc.

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


package gnu.javax.crypto.sasl.srp;

import gnu.java.security.Configuration;
import gnu.java.security.Registry;
import gnu.java.security.hash.MD5;
import gnu.java.security.util.PRNG;
import gnu.java.security.util.Util;
import gnu.javax.crypto.assembly.Direction;
import gnu.javax.crypto.cipher.CipherFactory;
import gnu.javax.crypto.cipher.IBlockCipher;
import gnu.javax.crypto.key.IKeyAgreementParty;
import gnu.javax.crypto.key.IncomingMessage;
import gnu.javax.crypto.key.KeyAgreementException;
import gnu.javax.crypto.key.KeyAgreementFactory;
import gnu.javax.crypto.key.OutgoingMessage;
import gnu.javax.crypto.key.srp6.SRP6KeyAgreement;
import gnu.javax.crypto.sasl.ClientMechanism;
import gnu.javax.crypto.sasl.IllegalMechanismStateException;
import gnu.javax.crypto.sasl.InputBuffer;
import gnu.javax.crypto.sasl.IntegrityException;
import gnu.javax.crypto.sasl.OutputBuffer;
import gnu.javax.security.auth.Password;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.math.BigInteger;
import java.security.NoSuchAlgorithmException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.StringTokenizer;
import java.util.logging.Logger;

import javax.security.auth.DestroyFailedException;
import javax.security.auth.callback.Callback;
import javax.security.auth.callback.NameCallback;
import javax.security.auth.callback.PasswordCallback;
import javax.security.auth.callback.UnsupportedCallbackException;
import javax.security.sasl.AuthenticationException;
import javax.security.sasl.SaslClient;
import javax.security.sasl.SaslException;

/**
 * The SASL-SRP client-side mechanism.
 */
public class SRPClient
    extends ClientMechanism
    implements SaslClient
{
  private static final Logger log = Logger.getLogger(SRPClient.class.getName());
  private String uid; // the unique key for this type of client
  private String U; // the authentication identity
  BigInteger N, g, A, B;
  private Password password; // the authentication credentials
  private byte[] s; // the user's salt
  private byte[] cIV, sIV; // client+server IVs, when confidentiality is on
  private byte[] M1, M2; // client+server evidences
  private byte[] cn, sn; // client's and server's nonce
  private SRP srp; // SRP algorithm instance used by this client
  private byte[] sid; // session ID when re-used
  private int ttl; // session time-to-live in seconds
  private byte[] sCB; // the peer's channel binding data
  private String L; // available options
  private String o;
  private String chosenIntegrityAlgorithm;
  private String chosenConfidentialityAlgorithm;
  private int rawSendSize = Registry.SASL_BUFFER_MAX_LIMIT;
  private byte[] K; // shared session key
  private boolean replayDetection = true; // whether Replay Detection is on
  private int inCounter = 0; // messages sequence numbers
  private int outCounter = 0;
  private IALG inMac, outMac; // if !null, use for integrity
  private CALG inCipher, outCipher; // if !null, use for confidentiality
  private IKeyAgreementParty clientHandler =
      KeyAgreementFactory.getPartyAInstance(Registry.SRP_SASL_KA);
  /** Our default source of randomness. */
  private PRNG prng = null;

  public SRPClient()
  {
    super(Registry.SASL_SRP_MECHANISM);
  }

  protected void initMechanism() throws SaslException
  {
    // we shall keep track of the sid (and the security context of this SRP
    // client) based on the initialisation parameters of an SRP session.
    // we shall compute a unique key for those parameters and key the sid
    // (and the security context) accordingly.
    // 1. compute the mapping key. use MD5 (the fastest) for this purpose
    final MD5 md = new MD5();
    byte[] b;
    b = authorizationID.getBytes();
    md.update(b, 0, b.length);
    b = serverName.getBytes();
    md.update(b, 0, b.length);
    b = protocol.getBytes();
    md.update(b, 0, b.length);
    if (channelBinding.length > 0)
      md.update(channelBinding, 0, channelBinding.length);

    uid = Util.toBase64(md.digest());
    if (ClientStore.instance().isAlive(uid))
      {
        final SecurityContext ctx = ClientStore.instance().restoreSession(uid);
        srp = SRP.instance(ctx.getMdName());
        sid = ctx.getSID();
        K = ctx.getK();
        cIV = ctx.getClientIV();
        sIV = ctx.getServerIV();
        replayDetection = ctx.hasReplayDetection();
        inCounter = ctx.getInCounter();
        outCounter = ctx.getOutCounter();
        inMac = ctx.getInMac();
        outMac = ctx.getOutMac();
        inCipher = ctx.getInCipher();
        outCipher = ctx.getOutCipher();
      }
    else
      {
        sid = new byte[0];
        ttl = 0;
        K = null;
        cIV = null;
        sIV = null;
        cn = null;
        sn = null;
      }
  }

  protected void resetMechanism() throws SaslException
  {
    try
      {
        password.destroy();
      }
    catch (DestroyFailedException dfe)
      {
        SaslException se = new SaslException("resetMechanism()");
        se.initCause(dfe);
        throw se;
      }
    password = null;
    M1 = null;
    K = null;
    cIV = null;
    sIV = null;
    inMac = outMac = null;
    inCipher = outCipher = null;
    sid = null;
    ttl = 0;
    cn = null;
    sn = null;
  }

  public boolean hasInitialResponse()
  {
    return true;
  }

  public byte[] evaluateChallenge(final byte[] challenge) throws SaslException
  {
    switch (state)
      {
      case 0:
        state++;
        return sendIdentities();
      case 1:
        state++;
        final byte[] result = sendPublicKey(challenge);
        try
          {
            password.destroy(); //don't need further this session
          }
        catch (DestroyFailedException x)
          {
            SaslException se = new SaslException("sendPublicKey()");
            se.initCause(se);
            throw se;
          }
        return result;
      case 2: // should only occur if session re-use was rejected
        if (! complete)
          {
            state++;
            return receiveEvidence(challenge);
          }
      // else fall through
      default:
        throw new IllegalMechanismStateException("evaluateChallenge()");
      }
  }

  protected byte[] engineUnwrap(final byte[] incoming, final int offset,
                                final int len) throws SaslException
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "engineUnwrap");
    if (inMac == null && inCipher == null)
      throw new IllegalStateException("connection is not protected");
    // at this point one, or both, of confidentiality and integrity protection
    // services are active.
    final byte[] result;
    try
      {
        if (inMac != null)
          { // integrity bytes are at the end of the stream
            final int macBytesCount = inMac.length();
            final int payloadLength = len - macBytesCount;
            final byte[] received_mac = new byte[macBytesCount];
            System.arraycopy(incoming, offset + payloadLength, received_mac, 0,
                             macBytesCount);
            if (Configuration.DEBUG)
              log.fine("Got C (received MAC): " + Util.dumpString(received_mac));
            inMac.update(incoming, offset, payloadLength);
            if (replayDetection)
              {
                inCounter++;
                if (Configuration.DEBUG)
                  log.fine("inCounter=" + inCounter);
                inMac.update(new byte[] {
                    (byte)(inCounter >>> 24),
                    (byte)(inCounter >>> 16),
                    (byte)(inCounter >>> 8),
                    (byte) inCounter });
              }
            final byte[] computed_mac = inMac.doFinal();
            if (Configuration.DEBUG)
              log.fine("Computed MAC: " + Util.dumpString(computed_mac));
            if (! Arrays.equals(received_mac, computed_mac))
              throw new IntegrityException("engineUnwrap()");
            // deal with the payload, which can be either plain or encrypted
            if (inCipher != null)
              result = inCipher.doFinal(incoming, offset, payloadLength);
            else
              {
                result = new byte[len - macBytesCount];
                System.arraycopy(incoming, offset, result, 0, result.length);
              }
          }
        else // no integrity protection; just confidentiality
          result = inCipher.doFinal(incoming, offset, len);
      }
    catch (IOException x)
      {
        if (x instanceof SaslException)
          throw (SaslException) x;
        throw new SaslException("engineUnwrap()", x);
      }
    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "engineUnwrap");
    return result;
  }

  protected byte[] engineWrap(final byte[] outgoing, final int offset,
                              final int len) throws SaslException
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "engineWrap");
    if (outMac == null && outCipher == null)
      throw new IllegalStateException("connection is not protected");
    // at this point one, or both, of confidentiality and integrity protection
    // services are active.
    byte[] result;
    try
      {
        final ByteArrayOutputStream out = new ByteArrayOutputStream();
        // Process the data
        if (outCipher != null)
          {
            result = outCipher.doFinal(outgoing, offset, len);
            if (Configuration.DEBUG)
              log.fine("Encoding c (encrypted plaintext): "
                       + Util.dumpString(result));
            out.write(result);
            if (outMac != null)
              {
                outMac.update(result);
                if (replayDetection)
                  {
                    outCounter++;
                    if (Configuration.DEBUG)
                      log.fine("outCounter=" + outCounter);
                    outMac.update(new byte[] {
                        (byte)(outCounter >>> 24),
                        (byte)(outCounter >>> 16),
                        (byte)(outCounter >>> 8),
                        (byte) outCounter });
                  }
                final byte[] C = outMac.doFinal();
                out.write(C);
                if (Configuration.DEBUG)
                  log.fine("Encoding C (integrity checksum): " + Util.dumpString(C));
              }
            // else confidentiality only; do nothing
          }
        else // no confidentiality; just integrity [+ replay detection]
          {
            if (Configuration.DEBUG)
              log.fine("Encoding p (plaintext): "
                       + Util.dumpString(outgoing, offset, len));
            out.write(outgoing, offset, len);
            outMac.update(outgoing, offset, len);
            if (replayDetection)
              {
                outCounter++;
                if (Configuration.DEBUG)
                  log.fine("outCounter=" + outCounter);
                outMac.update(new byte[] {
                    (byte)(outCounter >>> 24),
                    (byte)(outCounter >>> 16),
                    (byte)(outCounter >>> 8),
                    (byte) outCounter });
              }
            final byte[] C = outMac.doFinal();
            out.write(C);
            if (Configuration.DEBUG)
              log.fine("Encoding C (integrity checksum): " + Util.dumpString(C));
          }
        result = out.toByteArray();
      }
    catch (IOException x)
      {
        if (x instanceof SaslException)
          throw (SaslException) x;
        throw new SaslException("engineWrap()", x);
      }
    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "engineWrap");
    return result;
  }

  protected String getNegotiatedQOP()
  {
    if (inMac != null)
      {
        if (inCipher != null)
          return Registry.QOP_AUTH_CONF;
        return Registry.QOP_AUTH_INT;
      }
    return Registry.QOP_AUTH;
  }

  protected String getNegotiatedStrength()
  {
    if (inMac != null)
      {
        if (inCipher != null)
          return Registry.STRENGTH_HIGH;
        return Registry.STRENGTH_MEDIUM;
      }
    return Registry.STRENGTH_LOW;
  }

  protected String getNegotiatedRawSendSize()
  {
    return String.valueOf(rawSendSize);
  }

  protected String getReuse()
  {
    return Registry.REUSE_TRUE;
  }

  private byte[] sendIdentities() throws SaslException
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "sendIdentities");
    // If necessary, prompt the client for the username and password
    getUsernameAndPassword();
    if (Configuration.DEBUG)
      {
        log.fine("Password: \"" + new String(password.getPassword()) + "\"");
        log.fine("Encoding U (username): \"" + U + "\"");
        log.fine("Encoding I (userid): \"" + authorizationID + "\"");
      }
    // if session re-use generate new 16-byte nonce
    if (sid.length != 0)
      {
        cn = new byte[16];
        getDefaultPRNG().nextBytes(cn);
      }
    else
      cn = new byte[0];
    final OutputBuffer frameOut = new OutputBuffer();
    try
      {
        frameOut.setText(U);
        frameOut.setText(authorizationID);
        frameOut.setEOS(sid); // session ID to re-use
        frameOut.setOS(cn); // client nonce
        frameOut.setEOS(channelBinding);
      }
    catch (IOException x)
      {
        if (x instanceof SaslException)
          throw (SaslException) x;
        throw new AuthenticationException("sendIdentities()", x);
      }
    final byte[] result = frameOut.encode();
    if (Configuration.DEBUG)
      {
        log.fine("C: " + Util.dumpString(result));
        log.fine("  U = " + U);
        log.fine("  I = " + authorizationID);
        log.fine("sid = " + new String(sid));
        log.fine(" cn = " + Util.dumpString(cn));
        log.fine("cCB = " + Util.dumpString(channelBinding));
        log.exiting(this.getClass().getName(), "sendIdentities");
      }
    return result;
  }

  private byte[] sendPublicKey(final byte[] input) throws SaslException
  {
    if (Configuration.DEBUG)
      {
        log.entering(this.getClass().getName(), "sendPublicKey");
        log.fine("S: " + Util.dumpString(input));
      }
    // Server sends [00], N, g, s, B, L
    // or [FF], sn, sCB
    final InputBuffer frameIn = new InputBuffer(input);
    final int ack;
    try
      {
        ack = (int) frameIn.getScalar(1);
        if (ack == 0x00) // new session
          {
            N = frameIn.getMPI();
            if (Configuration.DEBUG)
              log.fine("Got N (modulus): " + Util.dump(N));
            g = frameIn.getMPI();
            if (Configuration.DEBUG)
              log.fine("Got g (generator): " + Util.dump(g));
            s = frameIn.getOS();
            if (Configuration.DEBUG)
              log.fine("Got s (salt): " + Util.dumpString(s));
            B = frameIn.getMPI();
            if (Configuration.DEBUG)
              log.fine("Got B (server ephermeral public key): " + Util.dump(B));
            L = frameIn.getText();
            if (Configuration.DEBUG)
              log.fine("Got L (available options): \"" + L + "\"");
          }
        else if (ack == 0xFF) // session re-use
          {
            sn = frameIn.getOS();
            if (Configuration.DEBUG)
              log.fine("Got sn (server nonce): " + Util.dumpString(sn));
            sCB = frameIn.getEOS();
            if (Configuration.DEBUG)
              log.fine("Got sCB (server channel binding): " + Util.dumpString(sCB));
          }
        else // unexpected scalar
          throw new SaslException("sendPublicKey(): Invalid scalar (" + ack
                                  + ") in server's request");
      }
    catch (IOException x)
      {
        if (x instanceof SaslException)
          throw (SaslException) x;
        throw new SaslException("sendPublicKey()", x);
      }
    if (ack == 0x00)
      { // new session ---------------------------------------
        o = createO(L.toLowerCase()); // do this first to initialise the SRP hash
        final byte[] pBytes; // use ASCII encoding to inter-operate w/ non-java
        pBytes = password.getBytes();
        // ----------------------------------------------------------------------
        final HashMap mapA = new HashMap();
        mapA.put(SRP6KeyAgreement.HASH_FUNCTION, srp.getAlgorithm());
        mapA.put(SRP6KeyAgreement.USER_IDENTITY, U);
        mapA.put(SRP6KeyAgreement.USER_PASSWORD, pBytes);
        try
          {
            clientHandler.init(mapA);
            clientHandler.processMessage(null);
          }
        catch (KeyAgreementException x)
          {
            throw new SaslException("sendPublicKey()", x);
          }
        // -------------------------------------------------------------------
        try
          {
            OutgoingMessage out = new OutgoingMessage();
            out.writeMPI(N);
            out.writeMPI(g);
            out.writeMPI(new BigInteger(1, s));
            out.writeMPI(B);
            IncomingMessage in = new IncomingMessage(out.toByteArray());
            out = clientHandler.processMessage(in);
            in = new IncomingMessage(out.toByteArray());
            A = in.readMPI();
            K = clientHandler.getSharedSecret();
          }
        catch (KeyAgreementException x)
          {
            throw new SaslException("sendPublicKey()", x);
          }
        // -------------------------------------------------------------------
        if (Configuration.DEBUG)
          {
            log.fine("K: " + Util.dumpString(K));
            log.fine("Encoding A (client ephemeral public key): " + Util.dump(A));
          }
        try
          {
            M1 = srp.generateM1(N, g, U, s, A, B, K, authorizationID, L, cn,
                                channelBinding);
          }
        catch (UnsupportedEncodingException x)
          {
            throw new AuthenticationException("sendPublicKey()", x);
          }
        if (Configuration.DEBUG)
          {
            log.fine("Encoding o (client chosen options): \"" + o + "\"");
            log.fine("Encoding cIV (client IV): \"" + Util.dumpString(cIV) + "\"");
          }
        final OutputBuffer frameOut = new OutputBuffer();
        try
          {
            frameOut.setMPI(A);
            frameOut.setOS(M1);
            frameOut.setText(o);
            frameOut.setOS(cIV);
          }
        catch (IOException x)
          {
            if (x instanceof SaslException)
              throw (SaslException) x;
            throw new AuthenticationException("sendPublicKey()", x);
          }
        final byte[] result = frameOut.encode();
        if (Configuration.DEBUG)
          {
            log.fine("New session, or session re-use rejected...");
            log.fine("C: " + Util.dumpString(result));
            log.fine("  A = 0x" + A.toString(16));
            log.fine(" M1 = " + Util.dumpString(M1));
            log.fine("  o = " + o);
            log.fine("cIV = " + Util.dumpString(cIV));
            log.exiting(this.getClass().getName(), "sendPublicKey");
          }
        return result;
      }
    else // session re-use accepted -------------------------------------------
      {
        setupSecurityServices(true);
        if (Configuration.DEBUG)
          {
            log.fine("Session re-use accepted...");
            log.exiting(this.getClass().getName(), "sendPublicKey");
          }
        return null;
      }
  }

  private byte[] receiveEvidence(byte[] input) throws SaslException
  {
    if (Configuration.DEBUG)
      {
        log.entering(this.getClass().getName(), "receiveEvidence");
        log.fine("S: " + Util.dumpString(input));
      }
    // Server send M2, sIV, sCB, sid, ttl
    final InputBuffer frameIn = new InputBuffer(input);
    try
      {
        M2 = frameIn.getOS();
        if (Configuration.DEBUG)
          log.fine("Got M2 (server evidence): " + Util.dumpString(M2));
        sIV = frameIn.getOS();
        if (Configuration.DEBUG)
          log.fine("Got sIV (server IV): " + Util.dumpString(sIV));
        sid = frameIn.getEOS();
        if (Configuration.DEBUG)
          log.fine("Got sid (session ID): " + new String(sid));
        ttl = (int) frameIn.getScalar(4);
        if (Configuration.DEBUG)
          log.fine("Got ttl (session time-to-live): " + ttl + "sec.");
        sCB = frameIn.getEOS();
        if (Configuration.DEBUG)
          log.fine("Got sCB (server channel binding): " + Util.dumpString(sCB));
      }
    catch (IOException x)
      {
        if (x instanceof SaslException)
          throw (SaslException) x;
        throw new AuthenticationException("receiveEvidence()", x);
      }

    final byte[] expected;
    try
      {
        expected = srp.generateM2(A, M1, K, U, authorizationID, o, sid, ttl,
                                  cIV, sIV, sCB);
      }
    catch (UnsupportedEncodingException x)
      {
        throw new AuthenticationException("receiveEvidence()", x);
      }
    if (Configuration.DEBUG)
      log.fine("Expected: " + Util.dumpString(expected));
    if (! Arrays.equals(M2, expected))
      throw new AuthenticationException("M2 mismatch");
    setupSecurityServices(false);
    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "receiveEvidence");
    return null;
  }

  private void getUsernameAndPassword() throws AuthenticationException
  {
    try
      {
        if ((! properties.containsKey(Registry.SASL_USERNAME))
            && (! properties.containsKey(Registry.SASL_PASSWORD)))
          {
            final NameCallback nameCB;
            final String defaultName = System.getProperty("user.name");
            if (defaultName == null)
              nameCB = new NameCallback("username: ");
            else
              nameCB = new NameCallback("username: ", defaultName);
            final PasswordCallback pwdCB = new PasswordCallback("password: ",
                                                                false);
            handler.handle(new Callback[] { nameCB, pwdCB });
            U = nameCB.getName();
            password = new Password(pwdCB.getPassword());
          }
        else
          {
            if (properties.containsKey(Registry.SASL_USERNAME))
              this.U = (String) properties.get(Registry.SASL_USERNAME);
            else
              {
                final NameCallback nameCB;
                final String defaultName = System.getProperty("user.name");
                if (defaultName == null)
                  nameCB = new NameCallback("username: ");
                else
                  nameCB = new NameCallback("username: ", defaultName);
                this.handler.handle(new Callback[] { nameCB });
                this.U = nameCB.getName();
              }

            if (properties.containsKey(Registry.SASL_PASSWORD))
              {
                Object pw = properties.get(Registry.SASL_PASSWORD);
                if (pw instanceof char[])
                  password = new Password((char[]) pw);
                else if (pw instanceof Password)
                  password = (Password) pw;
                else if (pw instanceof String)
                  password = new Password(((String) pw).toCharArray());
                else
                  throw new IllegalArgumentException(pw.getClass().getName()
                                                     + "is not a valid password class");
              }
            else
              {
                final PasswordCallback pwdCB = new PasswordCallback("password: ",
                                                                    false);
                this.handler.handle(new Callback[] { pwdCB });
                password = new Password(pwdCB.getPassword());
              }
          }

        if (U == null)
          throw new AuthenticationException("null username supplied");
        if (password == null)
          throw new AuthenticationException("null password supplied");
      }
    catch (UnsupportedCallbackException x)
      {
        throw new AuthenticationException("getUsernameAndPassword()", x);
      }
    catch (IOException x)
      {
        throw new AuthenticationException("getUsernameAndPassword()", x);
      }
  }

  // We go through the list of available services and for each available one
  // we decide whether or not we want it enabled, based on properties passed
  // to us by the client.
  private String createO(final String aol) throws AuthenticationException
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "createO", aol);
    boolean replaydetectionAvailable = false;
    boolean integrityAvailable = false;
    boolean confidentialityAvailable = false;
    String option, mandatory = SRPRegistry.DEFAULT_MANDATORY;
    int i;

    String mdName = SRPRegistry.SRP_DEFAULT_DIGEST_NAME;
    final StringTokenizer st = new StringTokenizer(aol, ",");
    while (st.hasMoreTokens())
      {
        option = st.nextToken();
        if (option.startsWith(SRPRegistry.OPTION_SRP_DIGEST + "="))
          {
            option = option.substring(option.indexOf('=') + 1);
            if (Configuration.DEBUG)
              log.fine("mda: <" + option + ">");
            for (i = 0; i < SRPRegistry.INTEGRITY_ALGORITHMS.length; i++)
              if (SRPRegistry.SRP_ALGORITHMS[i].equals(option))
                {
                  mdName = option;
                  break;
                }
          }
        else if (option.equals(SRPRegistry.OPTION_REPLAY_DETECTION))
          replaydetectionAvailable = true;
        else if (option.startsWith(SRPRegistry.OPTION_INTEGRITY + "="))
          {
            option = option.substring(option.indexOf('=') + 1);
            if (Configuration.DEBUG)
              log.fine("ialg: <" + option + ">");
            for (i = 0; i < SRPRegistry.INTEGRITY_ALGORITHMS.length; i++)
              if (SRPRegistry.INTEGRITY_ALGORITHMS[i].equals(option))
                {
                  chosenIntegrityAlgorithm = option;
                  integrityAvailable = true;
                  break;
                }
          }
        else if (option.startsWith(SRPRegistry.OPTION_CONFIDENTIALITY + "="))
          {
            option = option.substring(option.indexOf('=') + 1);
            if (Configuration.DEBUG)
              log.fine("calg: <" + option + ">");
            for (i = 0; i < SRPRegistry.CONFIDENTIALITY_ALGORITHMS.length; i++)
              if (SRPRegistry.CONFIDENTIALITY_ALGORITHMS[i].equals(option))
                {
                  chosenConfidentialityAlgorithm = option;
                  confidentialityAvailable = true;
                  break;
                }
          }
        else if (option.startsWith(SRPRegistry.OPTION_MANDATORY + "="))
          mandatory = option.substring(option.indexOf('=') + 1);
        else if (option.startsWith(SRPRegistry.OPTION_MAX_BUFFER_SIZE + "="))
          {
            final String maxBufferSize = option.substring(option.indexOf('=') + 1);
            try
              {
                rawSendSize = Integer.parseInt(maxBufferSize);
                if (rawSendSize > Registry.SASL_BUFFER_MAX_LIMIT
                    || rawSendSize < 1)
                  throw new AuthenticationException(
                      "Illegal value for 'maxbuffersize' option");
              }
            catch (NumberFormatException x)
              {
                throw new AuthenticationException(
                    SRPRegistry.OPTION_MAX_BUFFER_SIZE + "=" + maxBufferSize, x);
              }
          }
      }
    String s;
    Boolean flag;
    s = (String) properties.get(SRPRegistry.SRP_REPLAY_DETECTION);
    flag = Boolean.valueOf(s);
    replayDetection = replaydetectionAvailable && flag.booleanValue();
    s = (String) properties.get(SRPRegistry.SRP_INTEGRITY_PROTECTION);
    flag = Boolean.valueOf(s);
    boolean integrity = integrityAvailable && flag.booleanValue();
    s = (String) properties.get(SRPRegistry.SRP_CONFIDENTIALITY);
    flag = Boolean.valueOf(s);
    boolean confidentiality = confidentialityAvailable && flag.booleanValue();
    // make sure we do the right thing
    if (SRPRegistry.OPTION_REPLAY_DETECTION.equals(mandatory))
      {
        replayDetection = true;
        integrity = true;
      }
    else if (SRPRegistry.OPTION_INTEGRITY.equals(mandatory))
      integrity = true;
    else if (SRPRegistry.OPTION_CONFIDENTIALITY.equals(mandatory))
      confidentiality = true;

    if (replayDetection)
      {
        if (chosenIntegrityAlgorithm == null)
          throw new AuthenticationException(
              "Replay detection is required but no integrity protection "
              + "algorithm was chosen");
      }
    if (integrity)
      {
        if (chosenIntegrityAlgorithm == null)
          throw new AuthenticationException(
              "Integrity protection is required but no algorithm was chosen");
      }
    if (confidentiality)
      {
        if (chosenConfidentialityAlgorithm == null)
          throw new AuthenticationException(
              "Confidentiality protection is required but no algorithm was chosen");
      }
    // 1. check if we'll be using confidentiality; if not set IV to 0-byte
    if (chosenConfidentialityAlgorithm == null)
      cIV = new byte[0];
    else
      {
        // 2. get the block size of the cipher
        final IBlockCipher cipher = CipherFactory.getInstance(chosenConfidentialityAlgorithm);
        if (cipher == null)
          throw new AuthenticationException("createO()",
                                            new NoSuchAlgorithmException());
        final int blockSize = cipher.defaultBlockSize();
        // 3. generate random iv
        cIV = new byte[blockSize];
        getDefaultPRNG().nextBytes(cIV);
      }
    srp = SRP.instance(mdName);
    // Now create the options list specifying which of the available options
    // we have chosen.

    // For now we just select the defaults. Later we need to add support for
    // properties (perhaps in a file) where a user can specify the list of
    // algorithms they would prefer to use.
    final StringBuffer sb = new StringBuffer();
    sb.append(SRPRegistry.OPTION_SRP_DIGEST)
      .append("=").append(mdName).append(",");
    if (replayDetection)
      sb.append(SRPRegistry.OPTION_REPLAY_DETECTION).append(",");
    if (integrity)
      sb.append(SRPRegistry.OPTION_INTEGRITY)
        .append("=").append(chosenIntegrityAlgorithm).append(",");
    if (confidentiality)
      sb.append(SRPRegistry.OPTION_CONFIDENTIALITY)
        .append("=").append(chosenConfidentialityAlgorithm).append(",");

    final String result = sb.append(SRPRegistry.OPTION_MAX_BUFFER_SIZE)
                            .append("=").append(Registry.SASL_BUFFER_MAX_LIMIT)
                            .toString();
    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "createO", result);
    return result;
  }

  private void setupSecurityServices(final boolean sessionReUse)
      throws SaslException
  {
    complete = true; // signal end of authentication phase
    if (! sessionReUse)
      {
        outCounter = inCounter = 0;
        // instantiate cipher if confidentiality protection filter is active
        if (chosenConfidentialityAlgorithm != null)
          {
            if (Configuration.DEBUG)
              log.fine("Activating confidentiality protection filter");
            inCipher = CALG.getInstance(chosenConfidentialityAlgorithm);
            outCipher = CALG.getInstance(chosenConfidentialityAlgorithm);
          }
        // instantiate hmacs if integrity protection filter is active
        if (chosenIntegrityAlgorithm != null)
          {
            if (Configuration.DEBUG)
              log.fine("Activating integrity protection filter");
            inMac = IALG.getInstance(chosenIntegrityAlgorithm);
            outMac = IALG.getInstance(chosenIntegrityAlgorithm);
          }
      }
    else // same session new Keys
      K = srp.generateKn(K, cn, sn);

    final KDF kdf = KDF.getInstance(K);
    // initialise in/out ciphers if confidentiality protection is used
    if (inCipher != null)
      {
        inCipher.init(kdf, sIV, Direction.REVERSED);
        outCipher.init(kdf, cIV, Direction.FORWARD);
      }
    // initialise in/out macs if integrity protection is used
    if (inMac != null)
      {
        inMac.init(kdf);
        outMac.init(kdf);
      }
    if (sid != null && sid.length != 0)
      { // update the security context and save in map
        if (Configuration.DEBUG)
          log.fine("Updating security context for UID = " + uid);
        ClientStore.instance().cacheSession(uid,
                                            ttl,
                                            new SecurityContext(srp.getAlgorithm(),
                                                                sid,
                                                                K,
                                                                cIV,
                                                                sIV,
                                                                replayDetection,
                                                                inCounter,
                                                                outCounter,
                                                                inMac, outMac,
                                                                inCipher,
                                                                outCipher));
      }
  }

  private PRNG getDefaultPRNG()
  {
    if (prng == null)
      prng = PRNG.getInstance();
    return prng;
  }
}
