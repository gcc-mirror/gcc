/* UnicastRemoteCall.java
  Copyright (c) 1996, 1997, 1998, 1999, 2002 Free Software Foundation, Inc.

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

import java.lang.Exception;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.ObjectOutput;
import java.io.ObjectInput;
import java.io.StreamCorruptedException;
import java.rmi.server.RemoteCall;
import java.rmi.RemoteException;
import java.rmi.MarshalException;
import java.rmi.UnmarshalException;
import java.rmi.server.UID;
import java.rmi.server.ObjID;
import java.rmi.server.RemoteObject;

import java.util.Vector;

public class UnicastRemoteCall
	implements RemoteCall, ProtocolConstants
{

  private UnicastConnection conn;
  private Object result;
  private Object object;
  private int opnum;
  private long hash;
  private Vector vec;
  private int ptr;

  private ObjectOutput oout;
  private ObjectInput oin;

  /**
   * Incoming call.
   */
  UnicastRemoteCall(UnicastConnection conn)
  {
    this.conn = conn;
  }

  /**
   * Outgoing call.
   */
  UnicastRemoteCall(UnicastConnection conn, ObjID objid, int opnum, long hash)
    throws RemoteException
  {
    this.conn = conn;
    this.opnum = opnum;
    this.hash = hash;
    
    // signal the call when constructing
    try
      {
	DataOutputStream dout = conn.getDataOutputStream();
	dout.write(MESSAGE_CALL);
	
	oout = conn.getObjectOutputStream();
	objid.write(oout);
	oout.writeInt(opnum);
	oout.writeLong(hash);
      }
    catch(IOException ex)
      {
	throw new MarshalException("Try to write header but failed.", ex);
      }
  }
  
  UnicastConnection getConnection()
  {
    return conn;
  }
  
  public ObjectOutput getOutputStream() throws IOException
  {
    if (conn != null)
      {
	if(oout == null)
	  return (oout = conn.getObjectOutputStream());
	else
	  return oout;
      }
    else
      {
	vec = new Vector();
	return (new DummyObjectOutputStream());
      }
  }

  public void releaseOutputStream() throws IOException
  {
    if(oout != null)
      oout.flush();
  }

  public ObjectInput getInputStream() throws IOException
  {
    if (conn != null)
      {
	if(oin == null)
	  return (oin = conn.getObjectInputStream());
	else
	  return oin;
      }
    else
      {
	ptr = 0;
	return (new DummyObjectInputStream());
      }
  }

  public void releaseInputStream() throws IOException
  {
    // Does nothing.
  }

  public ObjectOutput getResultStream(boolean success)
    throws IOException, StreamCorruptedException
  {
    vec = new Vector();
    return new DummyObjectOutputStream();
  }
  
  public void executeCall() throws Exception
  {
    byte returncode;
    ObjectInput oin;
    try
      {
	releaseOutputStream();
	DataInputStream din = conn.getDataInputStream();
        if (din.readByte() != MESSAGE_CALL_ACK)
	    throw new RemoteException("Call not acked");

        oin = getInputStream();
        returncode = oin.readByte();
        UID.read(oin);
      }
    catch(IOException ex)
      {
        throw new UnmarshalException("Try to read header but failed:", ex);
      }
    
    //check return code
    switch(returncode)
      {
      case RETURN_ACK: //it's ok
	return;
      case RETURN_NACK:
	Object returnobj;
	try
	  {
	    returnobj = oin.readObject();
	  }
	catch(Exception ex2)
	  {
	    throw new UnmarshalException
	      ("Try to read exception object but failed", ex2);
	  }
	
	if(!(returnobj instanceof Exception))
	  throw new UnmarshalException("Should be Exception type here: "
				       + returnobj);
	throw (Exception)returnobj;
	
      default:
	throw new UnmarshalException("Invalid return code");
      }
  }

  public void done() throws IOException
  {
    // conn.disconnect();
  }

  Object returnValue()
  {
    return (vec.size() > 0 ? vec.elementAt(0) : null);
  }

  Object[] getArguments()
  {
    return vec.toArray();
  }

  Object getObject()
  {
    return object;
  }

  int getOpnum()
  {
    return opnum;
  }

  long getHash()
  {
    return hash;
  }

  void setReturnValue(Object obj)
  {
    vec.removeAllElements();
    vec.addElement(obj);
  }

  /**
   * Dummy object output class.
   */
  private class DummyObjectOutputStream implements ObjectOutput
  {
    /**
     * Non-private constructor to reduce bytecode emitted.
     */
    DummyObjectOutputStream()
    {
    }

    public void writeBoolean(boolean v) throws IOException
    {
      vec.addElement(new Boolean(v));
    }

    public void writeByte(int v) throws IOException
    {
      vec.addElement(new Byte((byte) v));
    }

    public void writeChar(int v) throws IOException
    {
      vec.addElement(new Character((char) v));
    }

    public void writeDouble(double v) throws IOException
    {
      vec.addElement(new Double(v));
    }

    public void writeFloat(float v) throws IOException
    {
      vec.addElement(new Float(v));
    }

    public void writeInt(int v) throws IOException
    {
      vec.addElement(new Integer(v));
    }

    public void writeLong(long v) throws IOException
    {
      vec.addElement(new Long(v));
    }

    public void writeShort(int v) throws IOException
    {
      vec.addElement(new Short((short) v));
    }

    public void writeObject(Object obj) throws IOException
    {
      vec.addElement(obj);
    }

    public void write(byte b[]) throws IOException
    {
      throw new IOException("not required");
    }

    public void write(byte b[], int off, int len) throws IOException
    {
      throw new IOException("not required");
    }

    public void write(int b) throws IOException
    {
      throw new IOException("not required");
    }

    public void writeBytes(String s) throws IOException
    {
      throw new IOException("not required");
    }

    public void writeChars(String s) throws IOException
    {
      throw new IOException("not required");
    }

    public void writeUTF(String str) throws IOException
    {
      throw new IOException("not required");
    }

    public void flush() throws IOException
    {
    }

    public void close() throws IOException
    {
    }
  } // class DummyObjectOutputStream

  /**
   * Dummy object input class.
   */
  private class DummyObjectInputStream implements ObjectInput
  {
    /**
     * Non-private constructor to reduce bytecode emitted.
     */
    DummyObjectInputStream()
    {
    }

    public boolean readBoolean() throws IOException
    {
      Object obj = vec.elementAt(ptr++);
      return ((Boolean) obj).booleanValue();
    }

    public byte readByte() throws IOException
    {
      Object obj = vec.elementAt(ptr++);
      return ((Byte) obj).byteValue();
    }

    public char readChar() throws IOException
    {
      Object obj = vec.elementAt(ptr++);
      return ((Character) obj).charValue();
    }

    public double readDouble() throws IOException
    {
      Object obj = vec.elementAt(ptr++);
      return ((Double) obj).doubleValue();
    }

    public float readFloat() throws IOException
    {
      Object obj = vec.elementAt(ptr++);
      return ((Float) obj).floatValue();
    }

    public int readInt() throws IOException
    {
      Object obj = vec.elementAt(ptr++);
      return ((Integer) obj).intValue();
    }

    public long readLong() throws IOException
    {
      Object obj = vec.elementAt(ptr++);
      return ((Long) obj).longValue();
    }

    public short readShort() throws IOException
    {
      Object obj = vec.elementAt(ptr++);
      return ((Short) obj).shortValue();
    }

    public Object readObject() throws IOException
    {
      return vec.elementAt(ptr++);
    }

    public int read(byte b[]) throws IOException
    {
      throw new IOException("not required");
    }

    public int read(byte b[], int off, int len) throws IOException
    {
      throw new IOException("not required");
    }

    public int read() throws IOException
    {
      throw new IOException("not required");
    }

    public long skip(long n) throws IOException
    {
      throw new IOException("not required");
    }

    public int available() throws IOException
    {
      throw new IOException("not required");
    }

    public void readFully(byte b[]) throws IOException
    {
      throw new IOException("not required");
    }

    public void readFully(byte b[], int off, int len) throws IOException
    {
      throw new IOException("not required");
    }

    public String readLine() throws IOException
    {
      throw new IOException("not required");
    }

    public String readUTF() throws IOException
    {
      throw new IOException("not required");
    }

    public int readUnsignedByte() throws IOException
    {
      throw new IOException("not required");
    }

    public int readUnsignedShort() throws IOException
    {
      throw new IOException("not required");
    }

    public int skipBytes(int n) throws IOException
    {
      throw new IOException("not required");
    }

    public void close() throws IOException
    {
    }
  } // class DummyObjectInputStream

}
