/* gnu.java.rmi.server.ConnectionRunnerPool
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


package gnu.java.rmi.server;

import java.util.ArrayList;
import java.util.Arrays;

//Should I generalize this class?

class ConnectionRunnerPool
{
  
  public static 
    class ConnectionRunner extends Thread{
      private UnicastConnection conn;
      private volatile boolean exiting = false;
      
      public ConnectionRunner(ThreadGroup group, String id){
        super(group, id);
      }
      
      public synchronized void run(){
        while(!exiting){
	  if(conn == null)
	    try{
	      wait();
	    }catch(InterruptedException e){
	      continue;
	    }
	  else{
	    conn.run();
	    conn = null;
	    synchronized(ConnectionRunnerPool.class){
	      freelist.add(this);
	      if(freelist.size() == 1)
		ConnectionRunnerPool.class.notifyAll();
	    }
	  }    
        }
      }
      
      public synchronized void dispatch(UnicastConnection conn){
        this.conn = conn;
        notify();
      }
      
      void exit(){
        exiting = true;
        if(conn != null)
	  try{
	    join(500);
	  }catch(InterruptedException e){}
        interrupt();
      }
      
    }
  
  // Should this value equal to number of CPU?
  private static int size = 5;
  private static int max_size = 10;
  
  private static ArrayList freelist;
  
  private static ThreadGroup group = new ThreadGroup("pool");
  
  static {
    ConnectionRunner[] pools = new ConnectionRunner[size];
    for(int i = 0; i < pools.length; i++){
      pools[i] = new ConnectionRunner(group, new Integer(i).toString());
      pools[i].setContextClassLoader(Thread.currentThread().getContextClassLoader());
      pools[i].start();
    }
    freelist = new ArrayList(Arrays.asList(pools));
  }
  
  public static void setSize(int size_){
    size = size_;
  }
  
  public static void setMaxSize(int size){
    max_size = size;
  }
  
  private synchronized static ConnectionRunner getConnectionRunner()
  {
    if(freelist.size() == 0){
      if(size < max_size){
	++size;
	ConnectionRunner a = new ConnectionRunner(group, new Integer(size).toString());
	a.start();
	freelist.add(a);
      }else
	while(freelist.size() == 0)
	  try{
	    ConnectionRunnerPool.class.wait();
	  }catch(InterruptedException e){}
    }
    
    // always let the first in pool most busy or other scheduling plan??
    ConnectionRunner a = (ConnectionRunner)freelist.get(0);
    freelist.remove(a);
    return a;
  }
  
  public static void dispatchConnection(UnicastConnection conn)
  {
    ConnectionRunner r = getConnectionRunner();
    r.dispatch(conn);
  }
  
  public static void exit()
  {
    Thread[] list = new Thread[group.activeCount()];
    group.enumerate(list);
    for(int i = 0; i < list.length; i++)
      ((ConnectionRunner)list[i]).exit();
  }
  
}
