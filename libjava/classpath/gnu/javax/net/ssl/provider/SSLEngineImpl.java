/* SSLEngineImpl.java -- implementation of SSLEngine.
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

import gnu.java.security.util.ByteBufferOutputStream;
import gnu.javax.net.ssl.Session;
import gnu.javax.net.ssl.SSLRecordHandler;

import java.nio.BufferOverflowException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;

import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.List;
import java.util.zip.DataFormatException;

import javax.crypto.IllegalBlockSizeException;
import javax.crypto.ShortBufferException;
import javax.net.ssl.SSLEngine;
import javax.net.ssl.SSLEngineResult;
import javax.net.ssl.SSLException;
import javax.net.ssl.SSLSession;
import javax.net.ssl.SSLEngineResult.HandshakeStatus;
import javax.net.ssl.SSLEngineResult.Status;

public final class SSLEngineImpl extends SSLEngine
{
  final SSLContextImpl contextImpl;
  private SSLRecordHandler[] handlers;
  private static final SystemLogger logger = SystemLogger.SYSTEM;
  private SessionImpl session;
  private InputSecurityParameters insec;
  private OutputSecurityParameters outsec;
  private boolean inClosed;
  private boolean outClosed;
  private boolean createSessions;
  private boolean needClientAuth;
  private boolean wantClientAuth;
  private boolean initialHandshakeDone;
  private AbstractHandshake handshake;
  private Alert lastAlert;
  private SSLEngineResult.HandshakeStatus handshakeStatus;
  private boolean changeCipherSpec;

  private String[] enabledSuites;
  private String[] enabledProtocols;

  /**
   * We can receive any message chunked across multiple records,
   * including alerts, even though all alert messages are only two
   * bytes long. Handshake messages are de-chunked in the handshake
   * handler, change-cipher-spec messages are always empty, and we
   * don't care about chunking of application messages.
   *
   * This buffer will hold the incomplete alert that we receive, if
   * any.
   */
  private final ByteBuffer alertBuffer;

  private Mode mode;

  private enum Mode { SERVER, CLIENT }

  SSLEngineImpl (SSLContextImpl contextImpl, String host, int port)
  {
    super(host, port);
    this.contextImpl = contextImpl;
    handlers = new SSLRecordHandler[256];
    session = new SessionImpl();
    session.suite = CipherSuite.TLS_NULL_WITH_NULL_NULL;
    session.version = ProtocolVersion.TLS_1_1;
    byte[] sid = new byte[32];
    contextImpl.random.nextBytes(sid);
    session.setId(new Session.ID(sid));
    session.setRandom(contextImpl.random);

    if (Debug.DEBUG)
      logger.logv(Component.SSL_RECORD_LAYER, "generated session ID {0} with random {1}",
                  session.id(), contextImpl.random);

    // Begin with no encryption.
    insec = new InputSecurityParameters (null, null, null, session,
                                         CipherSuite.TLS_NULL_WITH_NULL_NULL);
    outsec = new OutputSecurityParameters (null, null, null, session,
                                           CipherSuite.TLS_NULL_WITH_NULL_NULL);
    inClosed = false;
    outClosed = false;
    needClientAuth = false;
    wantClientAuth = false;
    createSessions = true;
    initialHandshakeDone = false;
    alertBuffer = ByteBuffer.wrap (new byte[2]);
    mode = null;
    lastAlert = null;
    handshakeStatus = SSLEngineResult.HandshakeStatus.NOT_HANDSHAKING;
    changeCipherSpec = false;

    // Set up default protocols and suites.
    enabledProtocols = new String[] {
      ProtocolVersion.TLS_1_1.toString(),
      ProtocolVersion.TLS_1.toString(),
      ProtocolVersion.SSL_3.toString()
    };
    enabledSuites = defaultSuites();
  }

  static String[] defaultSuites()
  {
    return new String[] {
      CipherSuite.TLS_DHE_DSS_WITH_AES_256_CBC_SHA.toString(),
      CipherSuite.TLS_DHE_RSA_WITH_AES_256_CBC_SHA.toString(),
      CipherSuite.TLS_DH_DSS_WITH_AES_256_CBC_SHA.toString(),
      CipherSuite.TLS_DH_RSA_WITH_AES_256_CBC_SHA.toString(),
      CipherSuite.TLS_RSA_WITH_AES_256_CBC_SHA.toString(),
      CipherSuite.TLS_DHE_DSS_WITH_AES_128_CBC_SHA.toString(),
      CipherSuite.TLS_DHE_RSA_WITH_AES_128_CBC_SHA.toString(),
      CipherSuite.TLS_DH_DSS_WITH_AES_128_CBC_SHA.toString(),
      CipherSuite.TLS_DH_RSA_WITH_AES_128_CBC_SHA.toString(),
      CipherSuite.TLS_RSA_WITH_AES_128_CBC_SHA.toString(),
      CipherSuite.TLS_DHE_DSS_WITH_3DES_EDE_CBC_SHA.toString(),
      CipherSuite.TLS_DHE_RSA_WITH_3DES_EDE_CBC_SHA.toString(),
      CipherSuite.TLS_DH_DSS_WITH_3DES_EDE_CBC_SHA.toString(),
      CipherSuite.TLS_DH_RSA_WITH_3DES_EDE_CBC_SHA.toString(),
      CipherSuite.TLS_RSA_WITH_3DES_EDE_CBC_SHA.toString(),
      CipherSuite.TLS_RSA_WITH_RC4_128_MD5.toString(),
      CipherSuite.TLS_RSA_WITH_RC4_128_SHA.toString(),
      CipherSuite.TLS_DHE_DSS_WITH_DES_CBC_SHA.toString(),
      CipherSuite.TLS_DHE_RSA_WITH_DES_CBC_SHA.toString(),
      CipherSuite.TLS_DH_DSS_WITH_DES_CBC_SHA.toString(),
      CipherSuite.TLS_DH_RSA_WITH_DES_CBC_SHA.toString(),
      CipherSuite.TLS_RSA_WITH_DES_CBC_SHA.toString(),
      CipherSuite.TLS_DH_DSS_EXPORT_WITH_DES40_CBC_SHA.toString(),
      CipherSuite.TLS_DH_RSA_EXPORT_WITH_DES40_CBC_SHA.toString(),
      CipherSuite.TLS_RSA_EXPORT_WITH_DES40_CBC_SHA.toString(),
      CipherSuite.TLS_RSA_EXPORT_WITH_RC4_40_MD5.toString(),
      CipherSuite.TLS_DHE_DSS_EXPORT_WITH_DES40_CBC_SHA.toString(),
      CipherSuite.TLS_DHE_RSA_EXPORT_WITH_DES40_CBC_SHA.toString(),
      CipherSuite.TLS_RSA_WITH_NULL_MD5.toString(),
      CipherSuite.TLS_RSA_WITH_NULL_SHA.toString()
    };
  }

  // XXX implement?
  /*public void registerHandler (final int contentType,
                               SSLRecordHandler handler)
    throws SSLException
  {
    if (type.equals (ContentType.CHANGE_CIPHER_SPEC)
        || type.equals (ContentType.ALERT)
        || type.equals (ContentType.HANDSHAKE)
        || type.equals (ContentType.APPLICATION_DATA))
      throw new SSLException ("can't override handler for content type " + type);
    int i = type.getValue ();
    if (i < 0 || i > 255)
      throw new SSLException ("illegal content type: " + type);
    handlers[i] = handler;
  }*/

  @Override
  public void beginHandshake () throws SSLException
  {
    if (Debug.DEBUG)
      logger.log(Component.SSL_HANDSHAKE, "{0} handshake begins", mode);

    if (mode == null)
      throw new IllegalStateException("setUseClientMode was never used");

    switch (mode)
      {
      case SERVER:
        if (getHandshakeStatus() != SSLEngineResult.HandshakeStatus.NOT_HANDSHAKING)
          throw new SSLException("handshake already in progress");
        try
          {
            handshake = new ServerHandshake(initialHandshakeDone, this);
          }
        catch (NoSuchAlgorithmException nsae)
          {
            throw new SSLException(nsae);
          }
        break;

      case CLIENT:
        try
          {
            handshake = new ClientHandshake(this);
          }
        catch (NoSuchAlgorithmException nsae)
          {
            throw new SSLException(nsae);
          }
        break;
      }
  }

  @Override
  public void closeInbound()
  {
    inClosed = true;
  }

  @Override
  public void closeOutbound()
  {
    lastAlert = new Alert(Alert.Level.WARNING, Alert.Description.CLOSE_NOTIFY);
  }

  @Override
  public Runnable getDelegatedTask()
  {
    if (handshake == null)
      return null;
    return handshake.getTask();
  }

  @Override
  public String[] getEnabledCipherSuites()
  {
    return (String[]) enabledSuites.clone();
  }

  @Override
  public String[] getEnabledProtocols()
  {
    return (String[]) enabledProtocols.clone();
  }

  @Override
  public boolean getEnableSessionCreation()
  {
    return createSessions;
  }

  @Override
  public HandshakeStatus getHandshakeStatus()
  {
    if (handshake == null)
      return HandshakeStatus.NOT_HANDSHAKING;
    return handshake.status();
  }

  @Override
  public boolean getNeedClientAuth()
  {
    return needClientAuth;
  }

  @Override
  public SSLSession getSession()
  {
    return session;
  }

  @Override
  public boolean getUseClientMode ()
  {
    return (mode == Mode.CLIENT);
  }

  @Override
  public boolean getWantClientAuth()
  {
    return wantClientAuth;
  }

  @Override
  public boolean isInboundDone()
  {
    return inClosed;
  }

  @Override
  public boolean isOutboundDone()
  {
    return outClosed;
  }

  @Override
  public void setEnableSessionCreation(final boolean createSessions)
  {
    this.createSessions = createSessions;
  }

  @Override
  public void setEnabledCipherSuites(final String[] suites)
  {
    if (suites.length == 0)
      throw new IllegalArgumentException("need at least one suite");
    enabledSuites = (String[]) suites.clone();
  }

  @Override
  public void setEnabledProtocols(final String[] protocols)
  {
    if (protocols.length == 0)
      throw new IllegalArgumentException("need at least one protocol");
    enabledProtocols = (String[]) protocols.clone();
  }

  @Override
  public String[] getSupportedCipherSuites()
  {
    // XXX if we ever want to support "pluggable" cipher suites, we'll need
    // to figure this out.

    return CipherSuite.availableSuiteNames().toArray(new String[0]);
  }

  @Override
  public String[] getSupportedProtocols()
  {
    return new String[] { ProtocolVersion.SSL_3.toString(),
                          ProtocolVersion.TLS_1.toString(),
                          ProtocolVersion.TLS_1_1.toString() };
  }

  @Override
  public void setNeedClientAuth(final boolean needClientAuth)
  {
    this.needClientAuth = needClientAuth;
  }

  @Override
  public void setUseClientMode (final boolean clientMode)
  {
    if (clientMode)
      mode = Mode.CLIENT;
    else
      mode = Mode.SERVER;
  }

  public @Override void setWantClientAuth(final boolean wantClientAuth)
  {
    this.wantClientAuth = wantClientAuth;
  }

  public @Override SSLEngineResult unwrap (final ByteBuffer source,
                                           final ByteBuffer[] sinks,
                                           final int offset, final int length)
    throws SSLException
  {
    if (mode == null)
      throw new IllegalStateException ("setUseClientMode was never called");

    if (inClosed)
      return new SSLEngineResult(SSLEngineResult.Status.CLOSED,
                                 handshakeStatus, 0, 0);

    if (source.remaining() < 5)
      {
        return new SSLEngineResult(SSLEngineResult.Status.BUFFER_UNDERFLOW,
                                   handshakeStatus, 0, 0);
      }

    Record record = null;
    boolean helloV2 = false;

    // XXX: messages may be chunked across multiple records; does this
    // include the SSLv2 message? I don't think it does, but we should
    // make sure.
    if (!getUseClientMode() && (source.get(source.position()) & 0x80) == 0x80)
      {
        if (handshake == null)
          beginHandshake();
        int hellolen = source.getShort(source.position()) & 0x7FFF;
        this.handshake.handleV2Hello(source.slice());
        if (!insec.cipherSuite().equals (CipherSuite.TLS_NULL_WITH_NULL_NULL))
          throw new SSLException ("received SSLv2 client hello in encrypted "
                                  + "session; this is invalid.");
        if (Debug.DEBUG)
          logger.log (Component.SSL_RECORD_LAYER,
                      "converting SSLv2 client hello to version 3 hello");

        source.getShort(); // skip length
        ClientHelloV2 v2 = new ClientHelloV2(source.slice());

        if (Debug.DEBUG)
          logger.log(Component.SSL_RECORD_LAYER, "v2 hello: {0}", v2);

        List<CipherSuite> suites = v2.cipherSpecs();

        ClientHelloBuilder hello = new ClientHelloBuilder();
        hello.setVersion(v2.version ());

        Random random = hello.random();
        byte[] challenge = v2.challenge();
        if (challenge.length < 32)
          {
            byte[] b = new byte[32];
            System.arraycopy(challenge, 0, b, b.length - challenge.length,
                             challenge.length);
            challenge = b;
          }
        random.setGmtUnixTime((challenge[0] & 0xFF) << 24
                              | (challenge[1] & 0xFF) << 16
                              | (challenge[2] & 0xFF) <<  8
                              | (challenge[3] & 0xFF));
        random.setRandomBytes(challenge, 4);

        byte[] sessionId = v2.sessionId();
        hello.setSessionId(sessionId, 0, sessionId.length);
        hello.setCipherSuites(suites);
        ArrayList<CompressionMethod> comps = new ArrayList<CompressionMethod>(1);
        comps.add(CompressionMethod.NULL);
        hello.setCompressionMethods(comps);

        record = new Record(ByteBuffer.allocate(hello.length() + 9));
        record.setContentType(ContentType.HANDSHAKE);
        record.setVersion(v2.version());
        record.setLength(hello.length() + 4);

        Handshake handshake = new Handshake(record.fragment());
        handshake.setLength(hello.length());
        handshake.setType(Handshake.Type.CLIENT_HELLO);

        handshake.bodyBuffer().put(hello.buffer());
        source.position(source.position() + hellolen);
        helloV2 = true;
      }
    else
      record = new Record(source);

    ContentType type = record.contentType ();

    if (Debug.DEBUG)
      logger.log(Component.SSL_RECORD_LAYER, "input record:\n{0}", record);

    if (record.length() > session.getPacketBufferSize() - 5)
      {
        lastAlert = new Alert(Alert.Level.FATAL,
                              Alert.Description.RECORD_OVERFLOW);
        throw new AlertException(lastAlert);
      }

    ByteBufferOutputStream sysMsg = null;
    ByteBuffer msg = null;

    int produced = 0;
    try
      {
        // Application data will get decrypted directly into the user's
        // output buffers.
        if (record.contentType() == ContentType.APPLICATION_DATA)
          produced = insec.decrypt(record, sinks, offset, length);
        else
          {
            if (insec.cipherSuite() == CipherSuite.TLS_NULL_WITH_NULL_NULL)
              msg = record.fragment();
            else
              {
                sysMsg = new ByteBufferOutputStream();
                insec.decrypt(record, sysMsg);
              }
          }

        // Advance the input buffer past the record we just read.
        if (!helloV2)
          source.position(source.position() + record.length() + 5);
      }
    catch (BufferOverflowException boe)
      {
        // We throw this if the output buffers are not large enough; signal
        // the caller about this.
        logger.log(Component.SSL_RECORD_LAYER, "buffer overflow when decrypting", boe);
        return new SSLEngineResult(SSLEngineResult.Status.BUFFER_OVERFLOW,
                                   handshakeStatus, 0, 0);
      }
    catch (IllegalBlockSizeException ibse)
      {
        lastAlert = new Alert(Alert.Level.FATAL,
                              Alert.Description.BAD_RECORD_MAC);
        throw new AlertException(lastAlert, ibse);
      }
    catch (DataFormatException dfe)
      {
        lastAlert = new Alert(Alert.Level.FATAL,
                              Alert.Description.DECOMPRESSION_FAILURE);
        throw new AlertException(lastAlert, dfe);
      }
    catch (MacException me)
      {
        lastAlert = new Alert(Alert.Level.FATAL,
                              Alert.Description.BAD_RECORD_MAC);
        throw new AlertException(lastAlert, me);
      }
    catch (ShortBufferException sbe)
      {
        // We've messed up if this happens.
        lastAlert = new Alert(Alert.Level.FATAL,
                              Alert.Description.INTERNAL_ERROR);
        throw new AlertException(lastAlert, sbe);
      }

    SSLEngineResult result = null;

    // If we need to handle the output here, do it. Otherwise, the output
    // has been stored in the supplied output buffers.
    if (sysMsg != null)
      {
        if (Debug.DEBUG)
          logger.logv(Component.SSL_RECORD_LAYER, "sysmessage {0}", sysMsg);
        msg = sysMsg.buffer();
      }

    if (type == ContentType.CHANGE_CIPHER_SPEC)
      {
        // We *may* get a partial message, even though the message is only
        // one byte long.
        if (msg.remaining() == 0)
          {
            result = new SSLEngineResult (SSLEngineResult.Status.OK,
                                          handshakeStatus,
                                          record.length() + 5, 0);
          }
        else
          {
            byte b = msg.get();
            if (b != 1)
              throw new SSLException ("unknown ChangeCipherSpec value: " + (b & 0xFF));
            InputSecurityParameters params = handshake.getInputParams();
            logger.log (Component.SSL_RECORD_LAYER,
                        "switching to input security parameters {0}",
                        params.cipherSuite());
            insec = params;
            result = new SSLEngineResult (SSLEngineResult.Status.OK,
                                          handshakeStatus,
                                          record.length() + 5, 0);
          }
      }
    else if (type == ContentType.ALERT)
      {
        int len = 0;
        if (alertBuffer.position() > 0)
          {
            alertBuffer.put(msg.get());
            len = 1;
          }
        if (Debug.DEBUG)
          logger.logv(Component.SSL_RECORD_LAYER, "processing alerts {0}",
                      Util.wrapBuffer(msg));
        len += msg.remaining() / 2;
        Alert[] alerts = new Alert[len];
        int i = 0;
        if (alertBuffer.position() > 0)
          {
            alertBuffer.flip();
            alerts[0] = new Alert(alertBuffer);
            i++;
          }
        while (i < alerts.length)
          {
            alerts[i++] = new Alert(msg.duplicate());
            msg.position(msg.position() + 2);
          }
        if (Debug.DEBUG)
          logger.logv(Component.SSL_RECORD_LAYER, "alerts: {0}", alerts.length);

        for (i = 0; i < alerts.length; i++)
          {
            if (alerts[i].level() == Alert.Level.FATAL)
              throw new AlertException(alerts[i], false);
            if (alerts[i].description() != Alert.Description.CLOSE_NOTIFY)
              logger.log(java.util.logging.Level.WARNING,
                         "received alert: {0}", alerts[i]);
            if (alerts[i].description() == Alert.Description.CLOSE_NOTIFY)
              inClosed = true;
          }

        if (msg.hasRemaining())
          alertBuffer.position(0).limit(2);

        result = new SSLEngineResult (SSLEngineResult.Status.OK,
                                      handshakeStatus,
                                      record.length() + 5, 0);
      }
    else if (type == ContentType.HANDSHAKE)
      {
        if (handshake == null)
          beginHandshake();
        try
          {
            handshakeStatus = handshake.handleInput(msg);
          }
        catch (AlertException ae)
          {
            lastAlert = ae.alert();
            return new SSLEngineResult(SSLEngineResult.Status.OK,
                                       SSLEngineResult.HandshakeStatus.NEED_WRAP,
                                       0, 0);
          }
        if (Debug.DEBUG)
          logger.logv(Component.SSL_HANDSHAKE, "handshake status {0}", handshakeStatus);
        result = new SSLEngineResult(SSLEngineResult.Status.OK,
                                     handshakeStatus,
                                     record.length() + 5,
                                     0);
        if (handshakeStatus == HandshakeStatus.FINISHED)
          {
            handshake = null;
            handshakeStatus = HandshakeStatus.NOT_HANDSHAKING;
          }
      }
    else if (type == ContentType.APPLICATION_DATA)
      {
        // Do nothing more; the application data has been put into
        // the output buffers.
        result = new SSLEngineResult(SSLEngineResult.Status.OK,
                                     handshakeStatus,
                                     record.length() + 5,
                                     produced);
      }
    else
      {
        SSLRecordHandler handler = handlers[type.getValue()];
        if (handler != null)
          {
            result = new SSLEngineResult(SSLEngineResult.Status.OK,
                                         handshakeStatus,
                                         record.length() + 5,
                                         0);
          }
        else
          throw new SSLException ("unknown content type: " + type);
      }

    if (Debug.DEBUG)
      logger.logv(Component.SSL_RECORD_LAYER, "return result: {0}", result);

    return result;
  }

  public @Override SSLEngineResult wrap (ByteBuffer[] sources, int offset, int length,
                                         ByteBuffer sink)
    throws SSLException
  {
    if (mode == null)
      throw new IllegalStateException ("setUseClientMode was never called");

    if (outClosed)
      return new SSLEngineResult(SSLEngineResult.Status.CLOSED,
                                 handshakeStatus, 0, 0);

    ContentType type = null;
    ByteBuffer sysMessage = null;
    if (Debug.DEBUG)
      logger.logv(Component.SSL_RECORD_LAYER, "wrap {0} {1} {2} {3} / {4}",
                  sources, offset, length, sink, getHandshakeStatus());
    if (lastAlert != null)
      {
        type = ContentType.ALERT;
        sysMessage = ByteBuffer.allocate(2);
        Alert alert = new Alert(sysMessage);
        alert.setDescription(lastAlert.description());
        alert.setLevel(lastAlert.level());
        if (lastAlert.description() == Alert.Description.CLOSE_NOTIFY)
          outClosed = true;
      }
    else if (changeCipherSpec)
      {
        type = ContentType.CHANGE_CIPHER_SPEC;
        sysMessage = ByteBuffer.allocate(1);
        sysMessage.put(0, (byte) 1);
      }
    else if (getHandshakeStatus() == SSLEngineResult.HandshakeStatus.NEED_WRAP)
      {
        // If we are not encrypting, optimize the handshake to fill
        // the buffer directly.
        if (outsec.suite() == CipherSuite.TLS_NULL_WITH_NULL_NULL)
          {
            int orig = sink.position();
            sink.order(ByteOrder.BIG_ENDIAN);
            sink.put((byte) ContentType.HANDSHAKE.getValue());
            sink.putShort((short) session.version.rawValue());
            sink.putShort((short) 0);
            handshakeStatus = handshake.handleOutput(sink);
            int produced = sink.position() - orig;
            sink.putShort(orig + 3, (short) (produced - 5));
            if (Debug.DEBUG)
              logger.logv(Component.SSL_RECORD_LAYER, "emitting record:\n{0}",
                          new Record((ByteBuffer) sink.duplicate().position(orig)));
            SSLEngineResult result = new SSLEngineResult(SSLEngineResult.Status.OK,
                                                         handshakeStatus, 0, produced);

            // Note, this will only happen if we transition from
            // TLS_NULL_WITH_NULL_NULL *to* TLS_NULL_WITH_NULL_NULL, which
            // doesn't make a lot of sense, but we support it anyway.
            if (handshakeStatus == HandshakeStatus.FINISHED)
              {
                handshake = null; // finished with it.
                handshakeStatus = HandshakeStatus.NOT_HANDSHAKING;
              }
            return result;
          }

        // Rough guideline; XXX.
        sysMessage = ByteBuffer.allocate(sink.remaining() - 2048);
        type = ContentType.HANDSHAKE;
        try
          {
            handshakeStatus = handshake.handleOutput(sysMessage);
          }
        catch (AlertException ae)
          {
            lastAlert = ae.alert();
            return new SSLEngineResult(Status.OK,
                                       HandshakeStatus.NEED_WRAP, 0, 0);
          }
        sysMessage.flip();
        if (Debug.DEBUG)
          logger.logv(Component.SSL_HANDSHAKE, "handshake status {0}",
                      handshakeStatus);
      }

    int produced = 0;
    int consumed = 0;

    try
      {
        int orig = sink.position();
        int[] inout = null;
        if (sysMessage != null)
          {
            if (Debug.DEBUG)
              logger.logv(Component.SSL_RECORD_LAYER, "encrypt system message {0} to {1}", sysMessage, sink);
            inout = outsec.encrypt(new ByteBuffer[] { sysMessage }, 0, 1,
                                   type, sink);
            produced = inout[1];
          }
        else
          {
            inout = outsec.encrypt(sources, offset, length,
                                   ContentType.APPLICATION_DATA, sink);
            consumed = inout[0];
            produced = inout[1];
          }

        if (Debug.DEBUG)
          logger.logv(Component.SSL_RECORD_LAYER, "emitting record:\n{0}",
                      new Record((ByteBuffer) sink.duplicate().position(orig).limit(produced)));
      }
    catch (ShortBufferException sbe)
      {
        // We don't expect this to happen, except for bugs; signal an
        // internal error.
        lastAlert = new Alert(Alert.Level.FATAL, Alert.Description.INTERNAL_ERROR);
        return new SSLEngineResult(SSLEngineResult.Status.OK, handshakeStatus, 0, 0);
      }
    catch (IllegalBlockSizeException ibse)
      {
        // We don't expect this to happen, except for bugs; signal an
        // internal error.
        lastAlert = new Alert(Alert.Level.FATAL, Alert.Description.INTERNAL_ERROR);
        return new SSLEngineResult(SSLEngineResult.Status.OK, handshakeStatus, 0, 0);
      }
    catch (DataFormatException dfe)
      {
        // We don't expect this to happen; signal an internal error.
        lastAlert = new Alert(Alert.Level.FATAL, Alert.Description.INTERNAL_ERROR);
        return new SSLEngineResult(SSLEngineResult.Status.OK, handshakeStatus, 0, 0);
      }

    if (lastAlert != null && lastAlert.level() == Alert.Level.FATAL)
      {
        AlertException ae = new AlertException(lastAlert);
        lastAlert = null;
        throw ae;
      }

    if (changeCipherSpec)
      {
        outsec = handshake.getOutputParams();
        changeCipherSpec = false;
      }
    SSLEngineResult result
      = new SSLEngineResult(outClosed ? SSLEngineResult.Status.CLOSED
                                      : SSLEngineResult.Status.OK,
                            handshakeStatus, consumed, produced);
    if (handshakeStatus == HandshakeStatus.FINISHED)
      {
        handshake = null; // done with it.
        handshakeStatus = HandshakeStatus.NOT_HANDSHAKING;
      }
    return result;
  }

  // Package-private methods.

  SessionImpl session ()
  {
    return session;
  }

  void setSession(SessionImpl session)
  {
    this.session = session;
  }

  void changeCipherSpec()
  {
    changeCipherSpec = true;
  }
}
