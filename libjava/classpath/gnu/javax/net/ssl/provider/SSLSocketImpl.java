/* SSLSocketImpl.java -- implementation of an SSL client socket.
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

import gnu.classpath.debug.Component;
import gnu.classpath.debug.SystemLogger;

import java.io.DataInputStream;
import java.io.EOFException;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.Socket;
import java.net.SocketAddress;
import java.net.SocketException;
import java.nio.ByteBuffer;
import java.nio.channels.SocketChannel;
import java.util.HashSet;
import java.util.Set;

import javax.net.ssl.HandshakeCompletedEvent;
import javax.net.ssl.HandshakeCompletedListener;
import javax.net.ssl.SSLEngineResult;
import javax.net.ssl.SSLException;
import javax.net.ssl.SSLSession;
import javax.net.ssl.SSLSocket;
import javax.net.ssl.SSLEngineResult.HandshakeStatus;
import javax.net.ssl.SSLEngineResult.Status;

/**
 * @author Casey Marshall (csm@gnu.org)
 */
public class SSLSocketImpl extends SSLSocket
{
  private class SocketOutputStream extends OutputStream
  {
    private final ByteBuffer buffer;
    private final OutputStream out;

    SocketOutputStream() throws IOException
    {
      buffer = ByteBuffer.wrap(new byte[getSession().getPacketBufferSize()]);
      if (underlyingSocket != null)
        out = underlyingSocket.getOutputStream();
      else
        out = SSLSocketImpl.super.getOutputStream();
    }

    @Override public void write(byte[] buf, int off, int len) throws IOException
    {
      if (!initialHandshakeDone
          || engine.getHandshakeStatus() != HandshakeStatus.NOT_HANDSHAKING)
        {
          doHandshake();
          if (handshakeException != null)
            throw handshakeException;
        }

      int k = 0;
      while (k < len)
        {
          synchronized (engine)
            {
              int l = Math.min(len-k, getSession().getApplicationBufferSize());
              ByteBuffer in = ByteBuffer.wrap(buf, off+k, l);
              SSLEngineResult result = engine.wrap(in, buffer);
              if (result.getStatus() == Status.CLOSED)
                return;
              if (result.getStatus() != Status.OK)
                throw new SSLException("unexpected SSL state " + result.getStatus());
              buffer.flip();
              out.write(buffer.array(), 0, buffer.limit());
              k += result.bytesConsumed();
              buffer.clear();
            }
        }
    }

    @Override public void write(int b) throws IOException
    {
      write(new byte[] { (byte) b });
    }

    @Override public void close() throws IOException
    {
      SSLSocketImpl.this.close();
    }
  }

  private class SocketInputStream extends InputStream
  {
    private final ByteBuffer inBuffer;
    private final ByteBuffer appBuffer;
    private final DataInputStream in;

    SocketInputStream() throws IOException
    {
      inBuffer = ByteBuffer.wrap(new byte[getSession().getPacketBufferSize()]);
      inBuffer.limit(0);
      appBuffer = ByteBuffer.allocate(getSession().getApplicationBufferSize());
      appBuffer.flip();
      if (underlyingSocket != null)
        in = new DataInputStream(underlyingSocket.getInputStream());
      else
        in = new DataInputStream(SSLSocketImpl.super.getInputStream());
    }

    @Override public int read(byte[] buf, int off, int len) throws IOException
    {
      if (!initialHandshakeDone ||
          engine.getHandshakeStatus() != HandshakeStatus.NOT_HANDSHAKING)
        {
          doHandshake();
          if (handshakeException != null)
            throw handshakeException;
        }

      if (!appBuffer.hasRemaining())
        {
          int x = in.read();
          if (x == -1)
            return -1;
          inBuffer.clear();
          inBuffer.put((byte) x);
          inBuffer.putInt(in.readInt());
          int reclen = inBuffer.getShort(3) & 0xFFFF;
          in.readFully(inBuffer.array(), 5, reclen);
          inBuffer.position(0).limit(reclen + 5);
          synchronized (engine)
            {
              appBuffer.clear();
              SSLEngineResult result = engine.unwrap(inBuffer, appBuffer);
              Status status = result.getStatus();
              if (status == Status.CLOSED && result.bytesProduced() == 0)
                return -1;
            }
          inBuffer.compact();
          appBuffer.flip();
        }
      int l = Math.min(len, appBuffer.remaining());
      appBuffer.get(buf, off, l);
      return l;
    }

    @Override public int read() throws IOException
    {
      byte[] b = new byte[1];
      if (read(b) == -1)
        return -1;
      return b[0] & 0xFF;
    }
  }

  private static final SystemLogger logger = SystemLogger.getSystemLogger();

  private SSLEngineImpl engine;
  private Set<HandshakeCompletedListener> listeners;
  private Socket underlyingSocket;
  private boolean isHandshaking;
  private IOException handshakeException;
  private boolean initialHandshakeDone = false;
  private final boolean autoClose;

  public SSLSocketImpl(SSLContextImpl contextImpl, String host, int port)
  {
    this(contextImpl, host, port, new Socket(), true);
  }

  public SSLSocketImpl(SSLContextImpl contextImpl, String host, int port,
                       Socket underlyingSocket, boolean autoClose)
  {
    engine = new SSLEngineImpl(contextImpl, host, port);
    engine.setUseClientMode(true); // default to client mode
    listeners = new HashSet<HandshakeCompletedListener>();
    this.underlyingSocket = underlyingSocket;
    this.autoClose = autoClose;
  }

  /* (non-Javadoc)
   * @see javax.net.ssl.SSLSocket#addHandshakeCompletedListener(javax.net.ssl.HandshakeCompletedListener)
   */
  @Override
  public void addHandshakeCompletedListener(HandshakeCompletedListener listener)
  {
    listeners.add(listener);
  }

  /* (non-Javadoc)
   * @see javax.net.ssl.SSLSocket#getEnableSessionCreation()
   */
  @Override public boolean getEnableSessionCreation()
  {
    return engine.getEnableSessionCreation();
  }

  /* (non-Javadoc)
   * @see javax.net.ssl.SSLSocket#getEnabledCipherSuites()
   */
  @Override public String[] getEnabledCipherSuites()
  {
    return engine.getEnabledCipherSuites();
  }

  /* (non-Javadoc)
   * @see javax.net.ssl.SSLSocket#getEnabledProtocols()
   */
  @Override public String[] getEnabledProtocols()
  {
    return engine.getEnabledProtocols();
  }

  /* (non-Javadoc)
   * @see javax.net.ssl.SSLSocket#getNeedClientAuth()
   */
  @Override public boolean getNeedClientAuth()
  {
    return engine.getNeedClientAuth();
  }

  /* (non-Javadoc)
   * @see javax.net.ssl.SSLSocket#getSession()
   */
  @Override public SSLSession getSession()
  {
    return engine.getSession();
  }

  /* (non-Javadoc)
   * @see javax.net.ssl.SSLSocket#getSupportedCipherSuites()
   */
  @Override public String[] getSupportedCipherSuites()
  {
    return engine.getSupportedCipherSuites();
  }

  /* (non-Javadoc)
   * @see javax.net.ssl.SSLSocket#getSupportedProtocols()
   */
  @Override public String[] getSupportedProtocols()
  {
    return engine.getSupportedProtocols();
  }

  /* (non-Javadoc)
   * @see javax.net.ssl.SSLSocket#getUseClientMode()
   */
  @Override public boolean getUseClientMode()
  {
    return engine.getUseClientMode();
  }

  /* (non-Javadoc)
   * @see javax.net.ssl.SSLSocket#getWantClientAuth()
   */
  @Override public boolean getWantClientAuth()
  {
    return engine.getWantClientAuth();
  }

  /* (non-Javadoc)
   * @see javax.net.ssl.SSLSocket#removeHandshakeCompletedListener(javax.net.ssl.HandshakeCompletedListener)
   */
  @Override
  public void removeHandshakeCompletedListener(HandshakeCompletedListener listener)
  {
    listeners.remove(listener);
  }

  /* (non-Javadoc)
   * @see javax.net.ssl.SSLSocket#setEnableSessionCreation(boolean)
   */
  @Override public void setEnableSessionCreation(boolean enable)
  {
    engine.setEnableSessionCreation(enable);
  }

  /* (non-Javadoc)
   * @see javax.net.ssl.SSLSocket#setEnabledCipherSuites(java.lang.String[])
   */
  @Override public void setEnabledCipherSuites(String[] suites)
  {
    engine.setEnabledCipherSuites(suites);
  }

  /* (non-Javadoc)
   * @see javax.net.ssl.SSLSocket#setEnabledProtocols(java.lang.String[])
   */
  @Override public void setEnabledProtocols(String[] protocols)
  {
    engine.setEnabledProtocols(protocols);
  }

  /* (non-Javadoc)
   * @see javax.net.ssl.SSLSocket#setNeedClientAuth(boolean)
   */
  @Override public void setNeedClientAuth(boolean needAuth)
  {
    engine.setNeedClientAuth(needAuth);
  }

  /* (non-Javadoc)
   * @see javax.net.ssl.SSLSocket#setUseClientMode(boolean)
   */
  @Override public void setUseClientMode(boolean clientMode)
  {
    engine.setUseClientMode(clientMode);
  }

  /* (non-Javadoc)
   * @see javax.net.ssl.SSLSocket#setWantClientAuth(boolean)
   */
  @Override public void setWantClientAuth(boolean wantAuth)
  {
    engine.setWantClientAuth(wantAuth);
  }

  /* (non-Javadoc)
   * @see javax.net.ssl.SSLSocket#startHandshake()
   */
  @Override public void startHandshake() throws IOException
  {
    if (isHandshaking)
      return;

    if (handshakeException != null)
      throw handshakeException;

    Thread t = new Thread(new Runnable()
    {
      public void run()
      {
        try
          {
            doHandshake();
          }
        catch (IOException ioe)
          {
            handshakeException = ioe;
          }
      }
    }, "HandshakeThread@" + System.identityHashCode(this));
    t.start();
  }

  void doHandshake() throws IOException
  {
    synchronized (engine)
      {
        if (isHandshaking)
          {
            try
              {
                engine.wait();
              }
            catch (InterruptedException ie)
              {
              }
            return;
          }
        isHandshaking = true;
      }

    if (initialHandshakeDone)
      throw new SSLException("rehandshaking not yet implemented");

    long now = -System.currentTimeMillis();
    engine.beginHandshake();

    HandshakeStatus status = engine.getHandshakeStatus();
    assert(status != HandshakeStatus.NOT_HANDSHAKING);

    ByteBuffer inBuffer = ByteBuffer.wrap(new byte[getSession().getPacketBufferSize()]);
    inBuffer.position(inBuffer.limit());
    ByteBuffer outBuffer = ByteBuffer.wrap(new byte[getSession().getPacketBufferSize()]);
    ByteBuffer emptyBuffer = ByteBuffer.allocate(0);
    SSLEngineResult result = null;

    DataInputStream sockIn = new DataInputStream(underlyingSocket.getInputStream());
    OutputStream sockOut = underlyingSocket.getOutputStream();

    try
      {
        while (status != HandshakeStatus.NOT_HANDSHAKING
               && status != HandshakeStatus.FINISHED)
          {
            logger.logv(Component.SSL_HANDSHAKE, "socket processing state {0}",
                        status);

            if (inBuffer.capacity() != getSession().getPacketBufferSize())
              {
                ByteBuffer b
                  = ByteBuffer.wrap(new byte[getSession().getPacketBufferSize()]);
                if (inBuffer.hasRemaining())
                  b.put(inBuffer).flip();
                inBuffer = b;
              }
            if (outBuffer.capacity() != getSession().getPacketBufferSize())
              outBuffer
              = ByteBuffer.wrap(new byte[getSession().getPacketBufferSize()]);

            switch (status)
              {
                case NEED_UNWRAP:
                  // Read in a single SSL record.
                  inBuffer.clear();
                  int i = sockIn.read();
                  if (i == -1)
                    throw new EOFException();
                  if ((i & 0x80) == 0x80) // SSLv2 client hello.
                    {
                      inBuffer.put((byte) i);
                      int v2len = (i & 0x7f) << 8;
                      i = sockIn.read();
                      v2len = v2len | (i & 0xff);
                      inBuffer.put((byte) i);
                      sockIn.readFully(inBuffer.array(), 2, v2len);
                      inBuffer.position(0).limit(v2len + 2);
                    }
                  else
                    {
                      inBuffer.put((byte) i);
                      inBuffer.putInt(sockIn.readInt());
                      int reclen = inBuffer.getShort(3) & 0xFFFF;
                      sockIn.readFully(inBuffer.array(), 5, reclen);
                      inBuffer.position(0).limit(reclen + 5);
                    }
                  result = engine.unwrap(inBuffer, emptyBuffer);
                  status = result.getHandshakeStatus();
                  if (result.getStatus() != Status.OK)
                    throw new SSLException("unexpected SSL status "
                                           + result.getStatus());
                  break;

                case NEED_WRAP:
                {
                  outBuffer.clear();
                  result = engine.wrap(emptyBuffer, outBuffer);
                  status = result.getHandshakeStatus();
                  if (result.getStatus() != Status.OK)
                    throw new SSLException("unexpected SSL status "
                                           + result.getStatus());
                  outBuffer.flip();
                  sockOut.write(outBuffer.array(), outBuffer.position(),
                                outBuffer.limit());
                }
                break;

                case NEED_TASK:
                {
                  Runnable task;
                  while ((task = engine.getDelegatedTask()) != null)
                    task.run();
                  status = engine.getHandshakeStatus();
                }
                break;

                case FINISHED:
                  break;
              }
          }

        initialHandshakeDone = true;

        HandshakeCompletedEvent hce = new HandshakeCompletedEvent(this, getSession());
        for (HandshakeCompletedListener l : listeners)
          {
            try
              {
                l.handshakeCompleted(hce);
              }
            catch (ThreadDeath td)
              {
                throw td;
              }
            catch (Throwable x)
              {
                logger.log(Component.WARNING,
                           "HandshakeCompletedListener threw exception", x);
              }
          }

        now += System.currentTimeMillis();
        if (Debug.DEBUG)
          logger.logv(Component.SSL_HANDSHAKE,
                      "handshake completed in {0}ms in thread {1}", now,
                      Thread.currentThread().getName());
      }
    catch (SSLException ssle)
      {
        handshakeException = ssle;
        throw ssle;
      }
    finally
      {
        synchronized (engine)
          {
            isHandshaking = false;
            engine.notifyAll();
          }
      }
  }

  // Methods overriding Socket.

  @Override public void bind(SocketAddress bindpoint) throws IOException
  {
    underlyingSocket.bind(bindpoint);
  }

  @Override public void connect(SocketAddress endpoint) throws IOException
  {
    underlyingSocket.connect(endpoint);
  }

  @Override public void connect(SocketAddress endpoint, int timeout)
    throws IOException
  {
    underlyingSocket.connect(endpoint, timeout);
  }

  @Override public InetAddress getInetAddress()
  {
    return underlyingSocket.getInetAddress();
  }

  @Override public InetAddress getLocalAddress()
  {
    return underlyingSocket.getLocalAddress();
  }

  @Override public int getPort()
  {
    return underlyingSocket.getPort();
  }

  @Override public int getLocalPort()
  {
    return underlyingSocket.getLocalPort();
  }

  @Override public SocketAddress getRemoteSocketAddress()
  {
    return underlyingSocket.getRemoteSocketAddress();
  }

  public SocketAddress getLocalSocketAddress()
  {
    return underlyingSocket.getLocalSocketAddress();
  }

  @Override public SocketChannel getChannel()
  {
    throw new UnsupportedOperationException("use javax.net.ssl.SSLEngine for NIO");
  }

  @Override public InputStream getInputStream() throws IOException
  {
    return new SocketInputStream();
  }

  @Override public OutputStream getOutputStream() throws IOException
  {
    return new SocketOutputStream();
  }

  @Override public void setTcpNoDelay(boolean on) throws SocketException
  {
    underlyingSocket.setTcpNoDelay(on);
  }

  @Override public boolean getTcpNoDelay() throws SocketException
  {
    return underlyingSocket.getTcpNoDelay();
  }

  @Override public void setSoLinger(boolean on, int linger) throws SocketException
  {
    underlyingSocket.setSoLinger(on, linger);
  }

  public int getSoLinger() throws SocketException
  {
    return underlyingSocket.getSoLinger();
  }

  @Override public void sendUrgentData(int x) throws IOException
  {
    throw new UnsupportedOperationException("not supported");
  }

  @Override public void setOOBInline(boolean on) throws SocketException
  {
    underlyingSocket.setOOBInline(on);
  }

  @Override public boolean getOOBInline() throws SocketException
  {
    return underlyingSocket.getOOBInline();
  }

  @Override public void setSoTimeout(int timeout) throws SocketException
  {
    underlyingSocket.setSoTimeout(timeout);
  }

  @Override public int getSoTimeout() throws SocketException
  {
    return underlyingSocket.getSoTimeout();
  }

  @Override public void setSendBufferSize(int size) throws SocketException
  {
    underlyingSocket.setSendBufferSize(size);
  }

  @Override public int getSendBufferSize() throws SocketException
  {
    return underlyingSocket.getSendBufferSize();
  }

  @Override public void setReceiveBufferSize(int size) throws SocketException
  {
    underlyingSocket.setReceiveBufferSize(size);
  }

  @Override public int getReceiveBufferSize() throws SocketException
  {
    return underlyingSocket.getReceiveBufferSize();
  }

  @Override public void setKeepAlive(boolean on) throws SocketException
  {
    underlyingSocket.setKeepAlive(on);
  }

  @Override public boolean getKeepAlive() throws SocketException
  {
    return underlyingSocket.getKeepAlive();
  }

  @Override public void setTrafficClass(int tc) throws SocketException
  {
    underlyingSocket.setTrafficClass(tc);
  }

  @Override public int getTrafficClass() throws SocketException
  {
    return underlyingSocket.getTrafficClass();
  }

  @Override public void setReuseAddress(boolean reuseAddress)
    throws SocketException
  {
    underlyingSocket.setReuseAddress(reuseAddress);
  }

  @Override public boolean getReuseAddress() throws SocketException
  {
    return underlyingSocket.getReuseAddress();
  }

  @Override public void close() throws IOException
  {
    // XXX closure alerts.
    if (autoClose)
      underlyingSocket.close();
  }

  @Override public void shutdownInput() throws IOException
  {
    underlyingSocket.shutdownInput();
  }

  @Override public void shutdownOutput() throws IOException
  {
    underlyingSocket.shutdownOutput();
  }

  @Override public boolean isConnected()
  {
    return underlyingSocket.isConnected();
  }

  @Override public boolean isBound()
  {
    return underlyingSocket.isBound();
  }

  @Override public boolean isClosed()
  {
    return underlyingSocket.isClosed();
  }

  @Override public boolean isInputShutdown()
  {
    return underlyingSocket.isInputShutdown();
  }

  @Override public boolean isOutputShutdown()
  {
    return underlyingSocket.isOutputShutdown();
  }
}
