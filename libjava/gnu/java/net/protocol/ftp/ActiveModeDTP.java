/* ActiveModeDTP.java --
   Copyright (C) 2003, 2004  Free Software Foundation, Inc.

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


package gnu.java.net.protocol.ftp;

import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;

/**
 * An active mode FTP data transfer process.
 * This starts a server on the specified port listening for a data
 * connection. It converts the socket input into a file stream for reading.
 *
 * @author Chris Burdess (dog@gnu.org)
 */
final class ActiveModeDTP
  implements DTP, Runnable
{

  ServerSocket server;
  Socket socket;
  DTPInputStream in;
  DTPOutputStream out;
  boolean completed;
  boolean inProgress;
  int transferMode;
  IOException exception;
  Thread acceptThread;
  int connectionTimeout;

  ActiveModeDTP(InetAddress localhost, int port,
                int connectionTimeout, int timeout)
    throws IOException
  {
    completed = false;
    inProgress = false;
    server = new ServerSocket(port, 1, localhost);
    if (timeout > 0)
      {
        server.setSoTimeout(timeout);
      }
    if (connectionTimeout <= 0)
      {
        connectionTimeout = 20000;
      }
    this.connectionTimeout = connectionTimeout;
    acceptThread = new Thread(this, "ActiveModeDTP");
    acceptThread.start();
  }

  /**
   * Start listening.
   */
  public void run()
  {
    try
      {
        socket = server.accept();
        //System.err.println("Accepted connection from "+socket.getInetAddress()+":"+socket.getPort());
      }
    catch (IOException e)
      {
        exception = e;
      }
  }

  /**
   * Waits until a client has connected.
   */
  public void waitFor()
    throws IOException
  {
    try
      {
        acceptThread.join(connectionTimeout);
      }
    catch (InterruptedException e)
      {
      }
    if (exception != null)
      {
        throw exception;
      }
    if (socket == null)
      {
        server.close();
        throw new IOException("client did not connect before timeout");
      }
    acceptThread = null;
  }
  
  /**
   * Returns an input stream from which a remote file can be read.
   */
  public InputStream getInputStream()
    throws IOException
  {
    if (inProgress)
      {
        throw new IOException("Transfer in progress");
      }
    if (acceptThread != null)
      {
        waitFor();
      }
    switch (transferMode)
      {
      case FTPConnection.MODE_STREAM:
        in = new StreamInputStream(this, socket.getInputStream());
        break;
      case FTPConnection.MODE_BLOCK:
        in = new BlockInputStream(this, socket.getInputStream());
        break;
      case FTPConnection.MODE_COMPRESSED:
        in = new CompressedInputStream(this, socket.getInputStream());
        break;
      default:
        throw new IllegalStateException("invalid transfer mode");
      }
    in.setTransferComplete(false);
    return in;
  }

  /**
   * Returns an output stream to which a local file can be written for
   * upload.
   */
  public OutputStream getOutputStream() throws IOException
  {
    if (inProgress)
      {
        throw new IOException("Transfer in progress");
      }
    if (acceptThread != null)
      {
        waitFor();
      }
    switch (transferMode)
      {
      case FTPConnection.MODE_STREAM:
        out = new StreamOutputStream(this, socket.getOutputStream());
        break;
      case FTPConnection.MODE_BLOCK:
        out = new BlockOutputStream(this, socket.getOutputStream());
        break;
      case FTPConnection.MODE_COMPRESSED:
        out = new CompressedOutputStream(this, socket.getOutputStream());
        break;
      default:
        throw new IllegalStateException("invalid transfer mode");
      }
    out.setTransferComplete(false);
    return out;
  }

  public void setTransferMode(int mode)
  {
    transferMode = mode;
  }

  public void complete()
  {
    completed = true;
    if (!inProgress)
      {
        transferComplete();
      }
  }

  public boolean abort()
  {
    completed = true;
    transferComplete();
    return inProgress;
  }
  
  public void transferComplete()
  {
    if (socket == null)
      {
        return;
      }
    if (in != null)
      {
        in.setTransferComplete(true);
      }
    if (out != null)
      {
        out.setTransferComplete(true);
      }
    completed = completed || (transferMode == FTPConnection.MODE_STREAM);
    if (completed && socket != null)
      {
        try
          {
            socket.close();
          }
        catch (IOException e)
          {
          }
        try
          {
            server.close();
          }
        catch (IOException e)
          {
          }
      }
  }
  
}

