/* KqueueSelectionKeyImpl.java -- selection key for kqueue/kevent.
   Copyright (C) 2006 Free Software Foundation, Inc.

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
import java.nio.ByteBuffer;
import java.nio.channels.SelectableChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.spi.AbstractSelectionKey;

/**
 * @author Casey Marshall (csm@gnu.org)
 */
public class KqueueSelectionKeyImpl extends AbstractSelectionKey
{
  int interestOps;
  int readyOps;
  int activeOps = 0;
  int key;
  int fd;

  /** The selector we were created for. */
  private final KqueueSelectorImpl selector;
  
  /** The channel we are attached to. */
  private final SelectableChannel channel;
  
  private final VMChannelOwner natChannel;
  
  public KqueueSelectionKeyImpl(KqueueSelectorImpl selector,
                                SelectableChannel channel)
  {
    this.selector = selector;
    this.channel = channel;
    natChannel = (VMChannelOwner) channel;
    interestOps = 0;
    readyOps = 0;
  }

  /* (non-Javadoc)
   * @see java.nio.channels.SelectionKey#channel()
   */
  //@Override
  public SelectableChannel channel()
  {
    return channel;
  }

  /* (non-Javadoc)
   * @see java.nio.channels.SelectionKey#interestOps()
   */
  //@Override
  public int interestOps()
  {
    return interestOps;
  }

  /* (non-Javadoc)
   * @see java.nio.channels.SelectionKey#interestOps(int)
   */
  //@Override
  public SelectionKey interestOps(int ops)
  {
    if (!isValid())
      throw new IllegalStateException("key is invalid");
    if ((ops & ~channel.validOps()) != 0)
      throw new IllegalArgumentException("channel does not support all operations");
    
    selector.setInterestOps(this, ops);
    return this;
  }

  /* (non-Javadoc)
   * @see java.nio.channels.SelectionKey#readyOps()
   */
  //@Override
  public int readyOps()
  {
    return readyOps;
  }

  /* (non-Javadoc)
   * @see java.nio.channels.SelectionKey#selector()
   */
  //@Override
  public Selector selector()
  {
    return selector;
  }
  
  public String toString()
  {
    if (!isValid())
      return super.toString() + " [ fd: " + fd + " <<invalid>> ]";
    return super.toString() + " [ fd: " + fd + " interest ops: {"
      + ((interestOps & OP_ACCEPT) != 0 ? " OP_ACCEPT" : "")
      + ((interestOps & OP_CONNECT) != 0 ? " OP_CONNECT" : "")
      + ((interestOps & OP_READ) != 0 ? " OP_READ" : "")
      + ((interestOps & OP_WRITE) != 0 ? " OP_WRITE" : "")
      + " }; ready ops: {"
      + ((readyOps & OP_ACCEPT) != 0 ? " OP_ACCEPT" : "")
      + ((readyOps & OP_CONNECT) != 0 ? " OP_CONNECT" : "")
      + ((readyOps & OP_READ) != 0 ? " OP_READ" : "")
      + ((readyOps & OP_WRITE) != 0 ? " OP_WRITE" : "")
      + " } ]";
  }
  
  public int hashCode()
  {
    return fd;
  }
  
  public boolean equals(Object o)
  {
    if (!(o instanceof KqueueSelectionKeyImpl))
      return false;
    KqueueSelectionKeyImpl that = (KqueueSelectionKeyImpl) o;
    return that.fd == this.fd && that.channel.equals(this.channel);
  }
  
  
  boolean isReadActive()
  {
    return (activeOps & (OP_READ | OP_ACCEPT)) != 0;
  }

  boolean isReadInterested()
  {
    return (interestOps & (OP_READ | OP_ACCEPT)) != 0;
  }
  
  boolean isWriteActive()
  {
    return (activeOps & (OP_WRITE | OP_CONNECT)) != 0;
  }
  
  boolean isWriteInterested()
  {
    return (interestOps & (OP_WRITE | OP_CONNECT)) != 0;
  }
  
  boolean needCommitRead()
  {
    return isReadActive() == (!isReadInterested());
  }

  boolean needCommitWrite()
  {
    return isWriteActive() == (!isWriteInterested());
  }
}
