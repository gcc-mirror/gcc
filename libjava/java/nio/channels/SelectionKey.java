/* SelectionKey.java -- 
   Copyright (C) 2002 Free Software Foundation, Inc.

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

package java.nio.channels;

public abstract class SelectionKey
{
  public static final int OP_ACCEPT  = 1<<0;
  public static final int OP_CONNECT = 1<<1;
  public static final int OP_READ    = 1<<2;
  public static final int OP_WRITE   = 1<<3;
    
  Object attached;
    
  protected SelectionKey()
  {
  }

  public final Object attach(Object obj)
  {
    Object old = attached;
    attached = obj;
    return old;
  }
    
  public final Object attachment()
  {
    return attached;
  }    

  /**
   * @exception CancelledKeyException FIXME
   */
  public final boolean isAcceptable()
  { 
    return (readyOps() & OP_ACCEPT) != 0;
  }

  /**
   * @exception CancelledKeyException FIXME
   */
  public final boolean isConnectable()
  {
    return (readyOps() & OP_CONNECT) != 0;  
  }        
  
  /**
   * @exception CancelledKeyException FIXME
   */
  public final boolean isReadable()
  {
    return (readyOps() & OP_READ) != 0; 
  }
  
  /**
   * @exception CancelledKeyException FIXME
   */
  public final boolean isWritable()
  {
    return (readyOps() & OP_WRITE) != 0;
  }

  public abstract void cancel(); 
  
  public abstract SelectableChannel channel();
  
  /**
   * @exception CancelledKeyException FIXME
   */
  public abstract int interestOps();
  
  /**
   * @exception CancelledKeyException FIXME
   * @exception IllegalArgumentException FIXME
   */
  public abstract SelectionKey interestOps(int ops);
  
  public abstract boolean isValid();
 
  /**
   * @exception CancelledKeyException FIXME
   */
  public abstract int readyOps();
  
  public abstract Selector selector();
}
