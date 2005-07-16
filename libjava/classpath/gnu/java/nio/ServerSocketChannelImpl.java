/* ServerSocketChannelImpl.java -- 
   Copyright (C) 2002, 2003, 2004 Free Software Foundation, Inc.

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


package gnu.java.nio;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.SocketTimeoutException;
import java.nio.channels.ClosedChannelException;
import java.nio.channels.NotYetBoundException;
import java.nio.channels.ServerSocketChannel;
import java.nio.channels.SocketChannel;
import java.nio.channels.spi.SelectorProvider;

public final class ServerSocketChannelImpl extends ServerSocketChannel
{
  private NIOServerSocket serverSocket;
  private boolean connected;

  protected ServerSocketChannelImpl (SelectorProvider provider)
    throws IOException
  {
    super (provider);
    serverSocket = new NIOServerSocket (this);
    configureBlocking(true);
  }

  public void finalizer()
  {
    if (connected)
      {
        try
          {
            close ();
          }
        catch (Exception e)
          {
          }
      }
  }

  protected void implCloseSelectableChannel () throws IOException
  {
    connected = false;
    serverSocket.close();
  }

  protected void implConfigureBlocking (boolean blocking) throws IOException
  {
    serverSocket.setSoTimeout (blocking ? 0 : NIOConstants.DEFAULT_TIMEOUT);
  }

  public SocketChannel accept () throws IOException
  {
    if (!isOpen())
      throw new ClosedChannelException();

    if (!serverSocket.isBound())
      throw new NotYetBoundException();

    boolean completed = false;
    
    try
      {
        begin();
        serverSocket.getPlainSocketImpl().setInChannelOperation(true);
          // indicate that a channel is initiating the accept operation
          // so that the socket ignores the fact that we might be in
          // non-blocking mode.
        NIOSocket socket = (NIOSocket) serverSocket.accept();
        completed = true;
        return socket.getChannel();
      }
    catch (SocketTimeoutException e)
      {
        return null;
      }
    finally
      {
        serverSocket.getPlainSocketImpl().setInChannelOperation(false);
        end (completed);
      }
  }

  public ServerSocket socket ()
  {
    return serverSocket;
  }
}
