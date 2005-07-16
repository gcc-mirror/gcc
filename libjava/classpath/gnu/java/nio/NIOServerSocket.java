/* NIOServerSocket.java -- 
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


package gnu.java.nio;

import gnu.java.net.PlainSocketImpl;

import java.io.IOException;
import java.lang.reflect.Method;
import java.net.ServerSocket;
import java.net.Socket;
import java.nio.channels.ServerSocketChannel;
import java.nio.channels.SocketChannel;
import java.security.AccessController;
import java.security.PrivilegedExceptionAction;

/**
 * @author Michael Koch (konqueror@gmx.de)
 */
public final class NIOServerSocket extends ServerSocket
{
  private ServerSocketChannelImpl channel;
    
  protected NIOServerSocket (ServerSocketChannelImpl channel)
    throws IOException
  {
    super();
    this.channel = channel;
  }

  public PlainSocketImpl getPlainSocketImpl()
  {
    try
      {
	final Object t = this;
	final Method method = ServerSocket.class.getDeclaredMethod("getImpl", new Class[0]);
	method.setAccessible(true);
	PrivilegedExceptionAction action = new PrivilegedExceptionAction()
	  {
	    public Object run() throws Exception
	    {
	      return method.invoke(t, new Object[0]);
	    }
	  };
	return (PlainSocketImpl) AccessController.doPrivileged(action);
      }
    catch (Exception e)
      {
	// This should never happen.
	Error error = new InternalError("unable to invoke method ServerSocket.getImpl()");
	error.initCause(e);
	throw error;
      }
  }

  public ServerSocketChannel getChannel()
  {
    return channel;
  }

  public Socket accept() throws IOException
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkListen (getLocalPort());

    SocketChannel socketChannel = channel.provider().openSocketChannel();
    implAccept (socketChannel.socket());
    return socketChannel.socket();
  }
}
