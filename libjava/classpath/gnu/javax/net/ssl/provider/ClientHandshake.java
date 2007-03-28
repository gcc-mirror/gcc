/* ClientHandshake.java -- 
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
exception statement from your version. */


package gnu.javax.net.ssl.provider;

import static gnu.javax.net.ssl.provider.ClientHandshake.State.*;
import static gnu.javax.net.ssl.provider.KeyExchangeAlgorithm.*;

import gnu.classpath.debug.Component;
import gnu.java.security.action.GetSecurityPropertyAction;
import gnu.javax.crypto.key.dh.GnuDHPublicKey;
import gnu.javax.net.ssl.AbstractSessionContext;
import gnu.javax.net.ssl.Session;
import gnu.javax.net.ssl.provider.Alert.Description;
import gnu.javax.net.ssl.provider.Alert.Level;
import gnu.javax.net.ssl.provider.CertificateRequest.ClientCertificateType;
import gnu.javax.net.ssl.provider.ServerNameList.NameType;
import gnu.javax.net.ssl.provider.ServerNameList.ServerName;

import java.nio.ByteBuffer;
import java.security.AccessController;
import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.KeyManagementException;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.SignatureException;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.security.interfaces.RSAPublicKey;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.zip.Deflater;
import java.util.zip.Inflater;

import javax.crypto.BadPaddingException;
import javax.crypto.Cipher;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.NoSuchPaddingException;
import javax.crypto.SecretKey;
import javax.crypto.interfaces.DHPrivateKey;
import javax.crypto.interfaces.DHPublicKey;
import javax.crypto.spec.DHParameterSpec;
import javax.net.ssl.SSLException;
import javax.net.ssl.SSLPeerUnverifiedException;
import javax.net.ssl.X509ExtendedKeyManager;
import javax.net.ssl.SSLEngineResult.HandshakeStatus;
import javax.security.auth.x500.X500Principal;

/**
 * @author Casey Marshall (csm@gnu.org)
 */
public class ClientHandshake extends AbstractHandshake
{
  static enum State
  {
    WRITE_CLIENT_HELLO (false, true),
    READ_SERVER_HELLO (true, false),
    READ_CERTIFICATE (true, false),
    READ_SERVER_KEY_EXCHANGE (true, false),
    READ_CERTIFICATE_REQUEST (true, false),
    READ_SERVER_HELLO_DONE (true, false),
    WRITE_CERTIFICATE (false, true),
    WRITE_CLIENT_KEY_EXCHANGE (false, true),
    WRITE_CERTIFICATE_VERIFY (false, true),
    WRITE_FINISHED (false, true),
    READ_FINISHED (true, false),
    DONE (false, false);
    
    private final boolean isWriteState;
    private final boolean isReadState;
    
    private State(boolean isReadState, boolean isWriteState)
    {
      this.isReadState = isReadState;
      this.isWriteState = isWriteState;
    }
    
    boolean isReadState()
    {
      return isReadState;
    }
    
    boolean isWriteState()
    {
      return isWriteState;
    }
  }
  
  private State state;
  private ByteBuffer outBuffer;
  private boolean continuedSession;
  private SessionImpl continued;
  private KeyPair dhPair;
  private String keyAlias;
  private PrivateKey privateKey;
  private MaxFragmentLength maxFragmentLengthSent;
  private boolean truncatedHMacSent;
  private ProtocolVersion sentVersion;
  
  // Delegated tasks.
  private CertVerifier certVerifier;
  private ParamsVerifier paramsVerifier;
  private DelegatedTask keyExchange;
  private CertLoader certLoader;
  private GenCertVerify genCertVerify;
  
  public ClientHandshake(SSLEngineImpl engine) throws NoSuchAlgorithmException
  {
    super(engine);
    state = WRITE_CLIENT_HELLO;
    continuedSession = false;
  }

  /* (non-Javadoc)
   * @see gnu.javax.net.ssl.provider.AbstractHandshake#implHandleInput()
   */
  @Override protected HandshakeStatus implHandleInput() throws SSLException
  {
    if (state == DONE)
      return HandshakeStatus.FINISHED;

    if (state.isWriteState()
        || (outBuffer != null && outBuffer.hasRemaining()))
      return HandshakeStatus.NEED_WRAP;
    
    // Copy the current buffer, and prepare it for reading.
    ByteBuffer buffer = handshakeBuffer.duplicate ();
    buffer.flip();
    buffer.position(handshakeOffset);

    Handshake handshake = new Handshake(buffer.slice(),
                                        engine.session().suite,
                                        engine.session().version);
        
    if (Debug.DEBUG)
      logger.logv(Component.SSL_HANDSHAKE, "processing in state {0}:\n{1}",
                  state, handshake);

    switch (state)
      {
        // Server Hello.
        case READ_SERVER_HELLO:
        {
          if (handshake.type() != Handshake.Type.SERVER_HELLO)
            throw new AlertException(new Alert(Alert.Level.FATAL,
                                               Alert.Description.UNEXPECTED_MESSAGE));
          ServerHello hello = (ServerHello) handshake.body();
          serverRandom = hello.random().copy();
          engine.session().suite = hello.cipherSuite();
          engine.session().version = hello.version();
          compression = hello.compressionMethod();
          Session.ID serverId = new Session.ID(hello.sessionId());
          if (continued != null
              && continued.id().equals(serverId))
            {
              continuedSession = true;
              engine.setSession(continued);
            }
          else if (engine.getEnableSessionCreation())
            {
              ((AbstractSessionContext) engine.contextImpl
                  .engineGetClientSessionContext()).put(engine.session());
            }
          ExtensionList extensions = hello.extensions();
          if (extensions != null)
            {
              for (Extension extension : extensions)
                {
                  Extension.Type type = extension.type();
                  if (type == null)
                    continue;
                  switch (type)
                    {
                      case MAX_FRAGMENT_LENGTH:
                        MaxFragmentLength mfl
                          = (MaxFragmentLength) extension.value();
                        if (maxFragmentLengthSent == mfl)
                          engine.session().setApplicationBufferSize(mfl.maxLength());
                        break;

                      case TRUNCATED_HMAC:
                        if (truncatedHMacSent)
                          engine.session().setTruncatedMac(true);
                        break;
                    }
                }
            }

          KeyExchangeAlgorithm kex = engine.session().suite.keyExchangeAlgorithm();
          if (continuedSession)
            {
              byte[][] keys = generateKeys(clientRandom, serverRandom,
                                           engine.session());
              setupSecurityParameters(keys, true, engine, compression);
              state = READ_FINISHED;
            }
          else if (kex == RSA || kex == DH_DSS || kex == DH_RSA
                   || kex == DHE_DSS || kex == DHE_RSA || kex == RSA_PSK)
            state = READ_CERTIFICATE;
          else if (kex == DH_anon || kex == PSK || kex == DHE_PSK)
            state = READ_SERVER_KEY_EXCHANGE;
          else
            state = READ_CERTIFICATE_REQUEST;
        }
        break;
        
        // Server Certificate.
        case READ_CERTIFICATE:
        {
          if (handshake.type() != Handshake.Type.CERTIFICATE)
            {
              // We need a certificate for non-anonymous suites.
              if (engine.session().suite.signatureAlgorithm() != SignatureAlgorithm.ANONYMOUS)
                throw new AlertException(new Alert(Level.FATAL,
                                                   Description.UNEXPECTED_MESSAGE));
              state = READ_SERVER_KEY_EXCHANGE;
            }
          Certificate cert = (Certificate) handshake.body();
          X509Certificate[] chain = null;
          try
            {
              chain = cert.certificates().toArray(new X509Certificate[0]);
            }
          catch (CertificateException ce)
            {
              throw new AlertException(new Alert(Level.FATAL,
                                                 Description.BAD_CERTIFICATE),
                                       ce);
            }
          catch (NoSuchAlgorithmException nsae)
            {
              throw new AlertException(new Alert(Level.FATAL,
                                                 Description.UNSUPPORTED_CERTIFICATE),
                                       nsae);
            }
          engine.session().setPeerCertificates(chain);
          certVerifier = new CertVerifier(true, chain);
          tasks.add(certVerifier);
          
          // If we are doing an RSA key exchange, generate our parameters.
          KeyExchangeAlgorithm kea = engine.session().suite.keyExchangeAlgorithm();
          if (kea == RSA || kea == RSA_PSK)
            {
              keyExchange = new RSAGen(kea == RSA);
              tasks.add(keyExchange);
              if (kea == RSA)
                state = READ_CERTIFICATE_REQUEST;
              else
                state = READ_SERVER_KEY_EXCHANGE;
            }
          else
            state = READ_SERVER_KEY_EXCHANGE;
        }
        break;
        
        // Server Key Exchange.
        case READ_SERVER_KEY_EXCHANGE:
        {
          CipherSuite s = engine.session().suite;
          KeyExchangeAlgorithm kexalg = s.keyExchangeAlgorithm();
          // XXX also SRP.
          if (kexalg != DHE_DSS && kexalg != DHE_RSA && kexalg != DH_anon
              && kexalg != DHE_PSK && kexalg != PSK && kexalg != RSA_PSK)
            throw new AlertException(new Alert(Level.FATAL,
                                               Description.UNEXPECTED_MESSAGE));
          
          if (handshake.type() != Handshake.Type.SERVER_KEY_EXCHANGE)
            {
              if (kexalg != RSA_PSK && kexalg != PSK)
                throw new AlertException(new Alert(Level.FATAL,
                                                   Description.UNEXPECTED_MESSAGE));
              state = READ_CERTIFICATE_REQUEST;
              return HandshakeStatus.NEED_UNWRAP;
            }

          ServerKeyExchange skex = (ServerKeyExchange) handshake.body();
          ByteBuffer paramsBuffer = null;
          if (kexalg == DHE_DSS || kexalg == DHE_RSA || kexalg == DH_anon)
            {
              ServerDHParams dhParams = (ServerDHParams) skex.params();
              ByteBuffer b = dhParams.buffer();
              paramsBuffer = ByteBuffer.allocate(b.remaining());
              paramsBuffer.put(b);
            }
          
          if (s.signatureAlgorithm() != SignatureAlgorithm.ANONYMOUS)
            {
              byte[] signature = skex.signature().signature();
              paramsVerifier = new ParamsVerifier(paramsBuffer, signature);
              tasks.add(paramsVerifier);
            }
          
          if (kexalg == DHE_DSS || kexalg == DHE_RSA || kexalg == DH_anon)
            {
              ServerDHParams dhParams = (ServerDHParams) skex.params();
              DHPublicKey serverKey = new GnuDHPublicKey(null,
                                                         dhParams.p(),
                                                         dhParams.g(),
                                                         dhParams.y());
              DHParameterSpec params = new DHParameterSpec(dhParams.p(),
                                                           dhParams.g());
              keyExchange = new ClientDHGen(serverKey, params, true);
              tasks.add(keyExchange);
            }
          if (kexalg == DHE_PSK)
            {
              ServerDHE_PSKParameters pskParams = (ServerDHE_PSKParameters)
                skex.params();
              ServerDHParams dhParams = pskParams.params();
              DHPublicKey serverKey = new GnuDHPublicKey(null,
                                                         dhParams.p(),
                                                         dhParams.g(),
                                                         dhParams.y());
              DHParameterSpec params = new DHParameterSpec(dhParams.p(),
                                                           dhParams.g());
              keyExchange = new ClientDHGen(serverKey, params, false);
              tasks.add(keyExchange);
            }
          state = READ_CERTIFICATE_REQUEST;
        }
        break;
        
        // Certificate Request.
        case READ_CERTIFICATE_REQUEST:
        {
          if (handshake.type() != Handshake.Type.CERTIFICATE_REQUEST)
            {
              state = READ_SERVER_HELLO_DONE;
              return HandshakeStatus.NEED_UNWRAP;
            }
          
          CertificateRequest req = (CertificateRequest) handshake.body();
          ClientCertificateTypeList types = req.types();
          LinkedList<String> typeList = new LinkedList<String>();
          for (ClientCertificateType t : types)
            typeList.add(t.name());
          
          X500PrincipalList issuers = req.authorities();
          LinkedList<X500Principal> issuerList = new LinkedList<X500Principal>();
          for (X500Principal p : issuers)
            issuerList.add(p);
          
          certLoader = new CertLoader(typeList, issuerList);
          tasks.add(certLoader);
        }
        break;
        
        // Server Hello Done.
        case READ_SERVER_HELLO_DONE:
        {
          if (handshake.type() != Handshake.Type.SERVER_HELLO_DONE)
            throw new AlertException(new Alert(Level.FATAL,
                                               Description.UNEXPECTED_MESSAGE));
          state = WRITE_CERTIFICATE;
        }
        break;
        
        // Finished.
        case READ_FINISHED:
        {
          if (handshake.type() != Handshake.Type.FINISHED)
            throw new AlertException(new Alert(Level.FATAL,
                                               Description.UNEXPECTED_MESSAGE));

          Finished serverFinished = (Finished) handshake.body();
          MessageDigest md5copy = null;
          MessageDigest shacopy = null;
          try
            {
              md5copy = (MessageDigest) md5.clone();
              shacopy = (MessageDigest) sha.clone();
            }
          catch (CloneNotSupportedException cnse)
            {
              // We're improperly configured to use a non-cloneable
              // md5/sha-1, OR there's a runtime bug.
              throw new SSLException(cnse);
            }
          Finished clientFinished =
            new Finished(generateFinished(md5copy, shacopy,
                                          false, engine.session()),
                                          engine.session().version);

          if (Debug.DEBUG)
            logger.logv(Component.SSL_HANDSHAKE, "clientFinished: {0}",
                        clientFinished);
          
          if (engine.session().version == ProtocolVersion.SSL_3)
            {
              if (!Arrays.equals(clientFinished.md5Hash(),
                                 serverFinished.md5Hash())
                  || !Arrays.equals(clientFinished.shaHash(),
                                    serverFinished.shaHash()))
                {
                  engine.session().invalidate();
                  throw new SSLException("session verify failed");
                }
            }
          else
            {
              if (!Arrays.equals(clientFinished.verifyData(),
                                 serverFinished.verifyData()))
                {
                  engine.session().invalidate();
                  throw new SSLException("session verify failed");
                }
            }

          if (continuedSession)
            {
              engine.changeCipherSpec();
              state = WRITE_FINISHED;
            }
          else
            state = DONE;
        }
        break;
        
        default:
          throw new IllegalStateException("invalid state: " + state);
      }
    
    handshakeOffset += handshake.length() + 4;
    
    if (!tasks.isEmpty())
      return HandshakeStatus.NEED_TASK;
    if (state.isWriteState()
        || (outBuffer != null && outBuffer.hasRemaining()))
      return HandshakeStatus.NEED_WRAP;
    if (state.isReadState())
      return HandshakeStatus.NEED_UNWRAP;

    return HandshakeStatus.FINISHED;
  }

  /* (non-Javadoc)
   * @see gnu.javax.net.ssl.provider.AbstractHandshake#implHandleOutput(java.nio.ByteBuffer)
   */
  @Override protected HandshakeStatus implHandleOutput(ByteBuffer fragment)
    throws SSLException
  {
    if (Debug.DEBUG)
      logger.logv(Component.SSL_HANDSHAKE, "output to {0}; state:{1}; outBuffer:{2}",
                  fragment, state, outBuffer);

    // Drain the output buffer, if it needs it.
    if (outBuffer != null && outBuffer.hasRemaining())
      {
        int l = Math.min(fragment.remaining(), outBuffer.remaining());
        fragment.put((ByteBuffer) outBuffer.duplicate().limit(outBuffer.position() + l));
        outBuffer.position(outBuffer.position() + l);
      }
    
    if (!fragment.hasRemaining())
      {
        if (state.isWriteState() || outBuffer.hasRemaining())
          return HandshakeStatus.NEED_WRAP;
        else
          return HandshakeStatus.NEED_UNWRAP;
      }

outer_loop:
    while (fragment.remaining() >= 4 && state.isWriteState())
      {
        if (Debug.DEBUG)
          logger.logv(Component.SSL_HANDSHAKE, "loop state={0}", state);

        switch (state)
          {
            case WRITE_CLIENT_HELLO:
            {
              ClientHelloBuilder hello = new ClientHelloBuilder();
              AbstractSessionContext ctx = (AbstractSessionContext)
                engine.contextImpl.engineGetClientSessionContext();
              continued = (SessionImpl) ctx.getSession(engine.getPeerHost(),
                                                       engine.getPeerPort());
              engine.session().setId(new Session.ID(new byte[0]));
              Session.ID sid = engine.session().id();
              // If we have a session that we may want to continue, send
              // that ID.
              if (continued != null)
                sid = continued.id();
              
              hello.setSessionId(sid.id());
              sentVersion = chooseVersion();
              hello.setVersion(sentVersion);
              hello.setCipherSuites(getSuites());
              hello.setCompressionMethods(getCompressionMethods());
              Random r = hello.random();
              r.setGmtUnixTime(Util.unixTime());
              byte[] nonce = new byte[28];
              engine.session().random().nextBytes(nonce);
              r.setRandomBytes(nonce);
              clientRandom = r.copy();
              if (enableExtensions())
                {
                  List<Extension> extensions = new LinkedList<Extension>();
                  MaxFragmentLength fraglen = maxFragmentLength();
                  if (fraglen != null)
                    {
                      extensions.add(new Extension(Extension.Type.MAX_FRAGMENT_LENGTH,
                                                   fraglen));
                      maxFragmentLengthSent = fraglen;
                    }

                  String host = engine.getPeerHost();
                  if (host != null)
                    {
                      ServerName name
                        = new ServerName(NameType.HOST_NAME, host);
                      ServerNameList names
                        = new ServerNameList(Collections.singletonList(name));
                      extensions.add(new Extension(Extension.Type.SERVER_NAME,
                                                   names));
                    }
                  
                  if (truncatedHMac())
                    {
                      extensions.add(new Extension(Extension.Type.TRUNCATED_HMAC,
                                                   new TruncatedHMAC()));
                      truncatedHMacSent = true;
                    }

                  ExtensionList elist = new ExtensionList(extensions);
                  hello.setExtensions(elist.buffer());
                }
              else
                hello.setDisableExtensions(true);
              
              if (Debug.DEBUG)
                logger.logv(Component.SSL_HANDSHAKE, "{0}", hello);

              fragment.putInt((Handshake.Type.CLIENT_HELLO.getValue() << 24)
                              | (hello.length() & 0xFFFFFF));
              outBuffer = hello.buffer();
              int l = Math.min(fragment.remaining(), outBuffer.remaining());
              fragment.put((ByteBuffer) outBuffer.duplicate()
                           .limit(outBuffer.position() + l));
              outBuffer.position(outBuffer.position() + l);

              state = READ_SERVER_HELLO;
            }
            break;
            
            case WRITE_CERTIFICATE:
            {
              java.security.cert.Certificate[] chain
                = engine.session().getLocalCertificates();
              if (chain != null)
                {
                  CertificateBuilder cert
                    = new CertificateBuilder(CertificateType.X509);
                  try
                    {
                      cert.setCertificates(Arrays.asList(chain));
                    }
                  catch (CertificateException ce)
                    {
                      throw new AlertException(new Alert(Level.FATAL,
                                                         Description.INTERNAL_ERROR),
                                               ce);
                    }
                  
                  outBuffer = cert.buffer();
                  
                  fragment.putInt((Handshake.Type.CERTIFICATE.getValue() << 24)
                                  | (cert.length() & 0xFFFFFF));
                  
                  int l = Math.min(fragment.remaining(), outBuffer.remaining());
                  fragment.put((ByteBuffer) outBuffer.duplicate()
                               .limit(outBuffer.position() + l));
                  outBuffer.position(outBuffer.position() + l);
                }
              state = WRITE_CLIENT_KEY_EXCHANGE;
            }
            break;
            
            case WRITE_CLIENT_KEY_EXCHANGE:
            {
              KeyExchangeAlgorithm kea = engine.session().suite.keyExchangeAlgorithm();
              ClientKeyExchangeBuilder ckex
                = new ClientKeyExchangeBuilder(engine.session().suite,
                                               engine.session().version);
              if (kea == DHE_DSS || kea == DHE_RSA || kea == DH_anon
                  || kea == DH_DSS || kea == DH_RSA)
                {
                  assert(dhPair != null);
                  DHPublicKey pubkey = (DHPublicKey) dhPair.getPublic();
                  ClientDiffieHellmanPublic pub
                    = new ClientDiffieHellmanPublic(pubkey.getY());
                  ckex.setExchangeKeys(pub.buffer());
                }
              if (kea == RSA || kea == RSA_PSK)
                {
                  assert(keyExchange instanceof RSAGen);
                  assert(keyExchange.hasRun());
                  if (keyExchange.thrown() != null)
                    throw new AlertException(new Alert(Level.FATAL,
                                                       Description.HANDSHAKE_FAILURE),
                                             keyExchange.thrown());
                  EncryptedPreMasterSecret epms
                    = new EncryptedPreMasterSecret(((RSAGen) keyExchange).encryptedSecret(),
                                                   engine.session().version);
                  if (kea == RSA)
                    ckex.setExchangeKeys(epms.buffer());
                  else
                    {
                      String identity = getPSKIdentity();
                      if (identity == null)
                        throw new SSLException("no pre-shared-key identity;"
                                               + " set the security property"
                                               + " \"jessie.client.psk.identity\"");
                      ClientRSA_PSKParameters params =
                        new ClientRSA_PSKParameters(identity, epms.buffer());
                      ckex.setExchangeKeys(params.buffer());
                      generatePSKSecret(identity, preMasterSecret, true);
                    }
                }
              if (kea == DHE_PSK)
                {
                  assert(keyExchange instanceof ClientDHGen);
                  assert(dhPair != null);
                  String identity = getPSKIdentity();
                  if (identity == null)
                    throw new SSLException("no pre-shared key identity; set"
                                           + " the security property"
                                           + " \"jessie.client.psk.identity\"");
                  DHPublicKey pubkey = (DHPublicKey) dhPair.getPublic();
                  ClientDHE_PSKParameters params =
                    new ClientDHE_PSKParameters(identity,
                                                new ClientDiffieHellmanPublic(pubkey.getY()));
                  ckex.setExchangeKeys(params.buffer());
                  generatePSKSecret(identity, preMasterSecret, true);
                }
              if (kea == PSK)
                {
                  String identity = getPSKIdentity();
                  if (identity == null)
                    throw new SSLException("no pre-shared key identity; set"
                                           + " the security property"
                                           + " \"jessie.client.psk.identity\"");
                  generatePSKSecret(identity, null, true);
                  ClientPSKParameters params = new ClientPSKParameters(identity);
                  ckex.setExchangeKeys(params.buffer());
                }
              if (kea == NONE)
                {
                  Inflater inflater = null;
                  Deflater deflater = null;
                  if (compression == CompressionMethod.ZLIB)
                    {
                      inflater = new Inflater();
                      deflater = new Deflater();
                    }
                  inParams = new InputSecurityParameters(null, null, inflater,
                                                         engine.session(),
                                                         engine.session().suite);
                  outParams = new OutputSecurityParameters(null, null, deflater,
                                                           engine.session(),
                                                           engine.session().suite);
                  engine.session().privateData.masterSecret = new byte[0];
                }
              
              if (Debug.DEBUG)
                logger.logv(Component.SSL_HANDSHAKE, "{0}", ckex);
              
              outBuffer = ckex.buffer();
              if (Debug.DEBUG)
                logger.logv(Component.SSL_HANDSHAKE, "client kex buffer {0}", outBuffer);
              fragment.putInt((Handshake.Type.CLIENT_KEY_EXCHANGE.getValue() << 24)
                              | (ckex.length() & 0xFFFFFF));
              int l = Math.min(fragment.remaining(), outBuffer.remaining());
              fragment.put((ByteBuffer) outBuffer.duplicate().limit(outBuffer.position() + l));
              outBuffer.position(outBuffer.position() + l);

              if (privateKey != null)
                {
                  genCertVerify = new GenCertVerify(md5, sha);
                  tasks.add(genCertVerify);
                  state = WRITE_CERTIFICATE_VERIFY;
                }
              else
                {
                  engine.changeCipherSpec();
                  state = WRITE_FINISHED;
                }
            }
            // Both states terminate in a NEED_TASK, or a need to change cipher
            // specs; so we can't write any more messages here.
            break outer_loop;
            
            case WRITE_CERTIFICATE_VERIFY:
            {
              assert(genCertVerify != null);
              assert(genCertVerify.hasRun());
              CertificateVerify verify = new CertificateVerify(genCertVerify.signed(),
                                                               engine.session().suite.signatureAlgorithm());
              
              outBuffer = verify.buffer();
              fragment.putInt((Handshake.Type.CERTIFICATE_VERIFY.getValue() << 24)
                              | (verify.length() & 0xFFFFFF));
              int l = Math.min(fragment.remaining(), outBuffer.remaining());
              fragment.put((ByteBuffer) outBuffer.duplicate().limit(outBuffer.position() + l));
              outBuffer.position(outBuffer.position() + l);
              
              // XXX This is a potential problem: we may not have drained
              // outBuffer, but set the changeCipherSpec toggle.
              engine.changeCipherSpec();
              state = WRITE_FINISHED;
            }
            break outer_loop;
            
            case WRITE_FINISHED:
            {
              MessageDigest md5copy = null;
              MessageDigest shacopy = null;
              try
                {
                  md5copy = (MessageDigest) md5.clone();
                  shacopy = (MessageDigest) sha.clone();
                }
              catch (CloneNotSupportedException cnse)
                {
                  // We're improperly configured to use a non-cloneable
                  // md5/sha-1, OR there's a runtime bug.
                  throw new SSLException(cnse);
                }
              outBuffer
                = generateFinished(md5copy, shacopy, true,
                                   engine.session());
              
              fragment.putInt((Handshake.Type.FINISHED.getValue() << 24)
                              | outBuffer.remaining() & 0xFFFFFF);
              
              int l = Math.min(outBuffer.remaining(), fragment.remaining());
              fragment.put((ByteBuffer) outBuffer.duplicate().limit(outBuffer.position() + l));
              outBuffer.position(outBuffer.position() + l);

              if (continuedSession)
                state = DONE;
              else
                state = READ_FINISHED;              
            }
            break;
            
            default:
              throw new IllegalStateException("invalid state: " + state);
          }
      }

    if (!tasks.isEmpty())
      return HandshakeStatus.NEED_TASK;
    if (state.isWriteState() ||
        (outBuffer != null && outBuffer.hasRemaining()))
      return HandshakeStatus.NEED_WRAP;
    if (state.isReadState())
      return HandshakeStatus.NEED_UNWRAP;

    return HandshakeStatus.FINISHED;
  }

  /* (non-Javadoc)
   * @see gnu.javax.net.ssl.provider.AbstractHandshake#status()
   */
  @Override HandshakeStatus status()
  {
    if (state.isReadState())
      return HandshakeStatus.NEED_UNWRAP;
    if (state.isWriteState())
      return HandshakeStatus.NEED_WRAP;
    return HandshakeStatus.FINISHED;
  }
  
  @Override void checkKeyExchange() throws SSLException
  {
    // XXX implement.
  }

  /* (non-Javadoc)
   * @see gnu.javax.net.ssl.provider.AbstractHandshake#handleV2Hello(java.nio.ByteBuffer)
   */
  @Override void handleV2Hello(ByteBuffer hello) throws SSLException
  {
    throw new SSLException("this should be impossible");
  }
  
  private ProtocolVersion chooseVersion() throws SSLException
  {
    // Select the highest enabled version, for our initial key exchange.
    ProtocolVersion version = null;
    for (String ver : engine.getEnabledProtocols())
      {
        try
          {
            ProtocolVersion v = ProtocolVersion.forName(ver);
            if (version == null || version.compareTo(v) < 0)
              version = v;
          }
        catch (Exception x)
          {
            continue;
          }
      }
    
    if (version == null)
      throw new SSLException("no suitable enabled versions");
    
    return version;
  }
  
  private List<CipherSuite> getSuites() throws SSLException
  {
    List<CipherSuite> suites = new LinkedList<CipherSuite>();
    for (String s : engine.getEnabledCipherSuites())
      {
        CipherSuite suite = CipherSuite.forName(s);
        if (suite != null)
          suites.add(suite);
      }
    if (suites.isEmpty())
      throw new SSLException("no cipher suites enabled");
    return suites;
  }
  
  private List<CompressionMethod> getCompressionMethods()
  {
    List<CompressionMethod> methods = new LinkedList<CompressionMethod>();
    GetSecurityPropertyAction gspa = new GetSecurityPropertyAction("jessie.enable.compression");
    if (Boolean.valueOf(AccessController.doPrivileged(gspa)))
      methods.add(CompressionMethod.ZLIB);
    methods.add(CompressionMethod.NULL);
    return methods;
  }
  
  private boolean enableExtensions()
  {
    GetSecurityPropertyAction action
      = new GetSecurityPropertyAction("jessie.client.enable.extensions");
    return Boolean.valueOf(AccessController.doPrivileged(action));
  }
  
  private MaxFragmentLength maxFragmentLength()
  {
    GetSecurityPropertyAction action
      = new GetSecurityPropertyAction("jessie.client.maxFragmentLength");
    String s = AccessController.doPrivileged(action);
    if (s != null)
      {
        try
          {
            int len = Integer.parseInt(s);
            switch (len)
              {
                case 9:
                case (1 <<  9): return MaxFragmentLength.LEN_2_9;
                case 10:
                case (1 << 10): return MaxFragmentLength.LEN_2_10;
                case 11:
                case (1 << 11): return MaxFragmentLength.LEN_2_11;
                case 12:
                case (1 << 12): return MaxFragmentLength.LEN_2_12;
              }
          }
        catch (NumberFormatException nfe)
          {
          }
      }
    return null;
  }
  
  private boolean truncatedHMac()
  {
    GetSecurityPropertyAction action
      = new GetSecurityPropertyAction("jessie.client.truncatedHMac");
    return Boolean.valueOf(AccessController.doPrivileged(action));
  }
  
  private String getPSKIdentity()
  {
    GetSecurityPropertyAction action
      = new GetSecurityPropertyAction("jessie.client.psk.identity");
    return AccessController.doPrivileged(action);
  }
  
  // Delegated tasks.
  
  class ParamsVerifier extends DelegatedTask
  {
    private final ByteBuffer paramsBuffer;
    private final byte[] signature;
    private boolean verified;
    
    ParamsVerifier(ByteBuffer paramsBuffer, byte[] signature)
    {
      this.paramsBuffer = paramsBuffer;
      this.signature = signature;
    }
    
    public void implRun()
      throws InvalidKeyException, NoSuchAlgorithmException,
             SSLPeerUnverifiedException, SignatureException
    {
      java.security.Signature s
        = java.security.Signature.getInstance(engine.session().suite
                                              .signatureAlgorithm().algorithm());
      s.initVerify(engine.session().getPeerCertificates()[0]);
      s.update(paramsBuffer);
      verified = s.verify(signature);
      synchronized (this)
        {
          notifyAll();
        }
    }
    
    boolean verified()
    {
      return verified;
    }
  }
  
  class ClientDHGen extends DelegatedTask
  {
    private final DHPublicKey serverKey;
    private final DHParameterSpec params;
    private final boolean full;
    
    ClientDHGen(DHPublicKey serverKey, DHParameterSpec params, boolean full)
    {
      this.serverKey = serverKey;
      this.params = params;
      this.full = full;
    }
    
    public void implRun()
      throws InvalidAlgorithmParameterException, NoSuchAlgorithmException,
             SSLException
    {
      if (Debug.DEBUG)
        logger.log(Component.SSL_DELEGATED_TASK, "running client DH phase");
      if (paramsVerifier != null)
        {
          synchronized (paramsVerifier)
            {
              try
                {
                  while (!paramsVerifier.hasRun())
                    paramsVerifier.wait(500);
                }
              catch (InterruptedException ie)
                {
                  // Ignore.
                }
            }
        }
      KeyPairGenerator gen = KeyPairGenerator.getInstance("DH");
      gen.initialize(params, engine.session().random());
      dhPair = gen.generateKeyPair();
      if (Debug.DEBUG_KEY_EXCHANGE)
        logger.logv(Component.SSL_KEY_EXCHANGE,
                    "client keys public:{0} private:{1}", dhPair.getPublic(),
                    dhPair.getPrivate());

      initDiffieHellman((DHPrivateKey) dhPair.getPrivate(), engine.session().random());

      // We have enough info to do the full key exchange; so let's do it.
      DHPhase phase = new DHPhase(serverKey, full);
      phase.run();
      if (phase.thrown() != null)
        throw new SSLException(phase.thrown());
    }
    
    DHPublicKey serverKey()
    {
      return serverKey;
    }
  }
  
  class CertLoader extends DelegatedTask
  {
    private final List<String> keyTypes;
    private final List<X500Principal> issuers;
    
    CertLoader(List<String> keyTypes, List<X500Principal> issuers)
    {
      this.keyTypes = keyTypes;
      this.issuers = issuers;
    }
    
    public void implRun()
    {
      X509ExtendedKeyManager km = engine.contextImpl.keyManager;
      if (km == null)
        return;
      keyAlias = km.chooseEngineClientAlias(keyTypes.toArray(new String[keyTypes.size()]),
                                            issuers.toArray(new X500Principal[issuers.size()]),
                                            engine);
      engine.session().setLocalCertificates(km.getCertificateChain(keyAlias));
      privateKey = km.getPrivateKey(keyAlias);
    }
  }

  class RSAGen extends DelegatedTask
  {
    private byte[] encryptedPreMasterSecret;
    private final boolean full;
    
    RSAGen()
    {
      this(true);
    }
    
    RSAGen(boolean full)
    {
      this.full = full;
    }
    
    public void implRun()
      throws BadPaddingException, IllegalBlockSizeException, InvalidKeyException,
             NoSuchAlgorithmException, NoSuchPaddingException,
             SSLException
    {
      if (certVerifier != null)
        {
          synchronized (certVerifier)
            {
              try
                {
                  while (!certVerifier.hasRun())
                    certVerifier.wait(500);
                }
              catch (InterruptedException ie)
                {
                  // Ignore.
                }
            }
        }
      preMasterSecret = new byte[48];
      engine.session().random().nextBytes(preMasterSecret);
      preMasterSecret[0] = (byte) sentVersion.major();
      preMasterSecret[1] = (byte) sentVersion.minor();
      Cipher rsa = Cipher.getInstance("RSA");
      java.security.cert.Certificate cert
        = engine.session().getPeerCertificates()[0];
      if (cert instanceof X509Certificate)
        {
          boolean[] keyUsage = ((X509Certificate) cert).getKeyUsage();
          if (keyUsage != null && !keyUsage[2])
            throw new InvalidKeyException("certificate's keyUsage does not permit keyEncipherment");
        }
      rsa.init(Cipher.ENCRYPT_MODE, cert.getPublicKey());
      encryptedPreMasterSecret = rsa.doFinal(preMasterSecret);
      
      // Generate our session keys, because we can.
      if (full)
        {
          generateMasterSecret(clientRandom, serverRandom, engine.session());
          byte[][] keys = generateKeys(clientRandom, serverRandom, engine.session());
          setupSecurityParameters(keys, true, engine, compression);
        }
    }
    
    byte[] encryptedSecret()
    {
      return encryptedPreMasterSecret;
    }
  }
  
  class GenCertVerify extends DelegatedTask
  {
    private final MessageDigest md5, sha;
    private byte[] signed;
    
    GenCertVerify(MessageDigest md5, MessageDigest sha)
    {
      try
        {
          this.md5 = (MessageDigest) md5.clone();
          this.sha = (MessageDigest) sha.clone();
        }
      catch (CloneNotSupportedException cnse)
        {
          // Our message digests *should* be cloneable.
          throw new Error(cnse);
        }
    }

    public void implRun()
      throws InvalidKeyException, NoSuchAlgorithmException, SignatureException
    {
      byte[] toSign;
      if (engine.session().version == ProtocolVersion.SSL_3)
        {
          toSign = genV3CertificateVerify(md5, sha, engine.session());
        }
      else
        {
          if (engine.session().suite.signatureAlgorithm() == SignatureAlgorithm.RSA)
            toSign = Util.concat(md5.digest(), sha.digest());
          else
            toSign = sha.digest();
        }
      
      java.security.Signature sig =
        java.security.Signature.getInstance(engine.session().suite.signatureAlgorithm().name());
      sig.initSign(privateKey);
      sig.update(toSign);
      signed = sig.sign();
    }
    
    byte[] signed()
    {
      return signed;
    }
  }
}
