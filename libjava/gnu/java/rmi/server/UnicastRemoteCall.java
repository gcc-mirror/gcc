/*
  Copyright (c) 1996, 1997, 1998, 1999 Free Software Foundation, Inc.

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

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License.
 */

package gnu.java.rmi.server;

import java.lang.Exception;
import java.io.IOException;
import java.io.ObjectOutput;
import java.io.ObjectInput;
import java.io.StreamCorruptedException;
import java.rmi.server.RemoteCall;
import java.util.Vector;

public class UnicastRemoteCall
	implements RemoteCall {

private UnicastConnection conn;
private Object result;
private Object object;
private int opnum;
private long hash;
private Vector vec;
private int ptr;

/**
 * Incoming call.
 */
UnicastRemoteCall(UnicastConnection conn) {
	this.conn = conn;
}

/**
 * Outgoing call.
 */
UnicastRemoteCall(Object obj, int opnum, long hash) {
	this.object = obj;
	this.opnum = opnum;
	this.hash = hash;
}

public ObjectOutput getOutputStream() throws IOException {
	vec = new Vector();
	return (new DummyObjectOutputStream());
}

public void releaseOutputStream() throws IOException {
	// Does nothing.
}

public ObjectInput getInputStream() throws IOException {
	if (conn != null) {
		return (conn.getObjectInputStream());
	}
	else {
		ptr = 0;
		return (new DummyObjectInputStream());
	}
}

public void releaseInputStream() throws IOException {
	// Does nothing.
}

public ObjectOutput getResultStream(boolean success) throws IOException, StreamCorruptedException {
	vec = new Vector();
	return (new DummyObjectOutputStream());
}

public void executeCall() throws Exception {
	throw new Error("Not implemented");
}

public void done() throws IOException {
	/* Does nothing */
}

Object returnValue() {
	return (vec.elementAt(0));
}

Object[] getArguments() {
	return (vec.toArray());
}

Object getObject() {
	return (object);
}

int getOpnum() {
	return (opnum);
}

long getHash() {
	return (hash);
}

void setReturnValue(Object obj) {
	vec.removeAllElements();
	vec.addElement(obj);
}

/**
 * Dummy object output class.
 */
private class DummyObjectOutputStream implements ObjectOutput {

public void writeBoolean(boolean v) throws IOException {
	vec.addElement(new Boolean(v));
}

public void writeByte(int v) throws IOException {
	vec.addElement(new Byte((byte)v));
}

public void writeChar(int v) throws IOException {
	vec.addElement(new Character((char)v));
}

public void writeDouble(double v) throws IOException {
	vec.addElement(new Double(v));
}

public void writeFloat(float v) throws IOException {
	vec.addElement(new Float(v));
}

public void writeInt(int v) throws IOException {
	vec.addElement(new Integer(v));
}

public void writeLong(long v) throws IOException {
	vec.addElement(new Long(v));
}

public void writeShort(int v) throws IOException {
	vec.addElement(new Short((short)v));
}

public void writeObject(Object obj) throws IOException {
	vec.addElement(obj);
}

public void write(byte b[]) throws IOException {
	throw new IOException("not required");
}

public void write(byte b[], int off, int len) throws IOException {
	throw new IOException("not required");
}

public void write(int b) throws IOException {
	throw new IOException("not required");
}

public void writeBytes(String s) throws IOException {
	throw new IOException("not required");
}

public void writeChars(String s) throws IOException {
	throw new IOException("not required");
}

public void writeUTF(String str) throws IOException {
	throw new IOException("not required");
}

public void flush() throws IOException {
}

public void close() throws IOException {
}

}

/**
 * Dummy object input class.
 */
private class DummyObjectInputStream implements ObjectInput {

public boolean readBoolean() throws IOException {
	Object obj = vec.elementAt(ptr++);
	return (((Boolean)obj).booleanValue());
}

public byte readByte() throws IOException {
	Object obj = vec.elementAt(ptr++);
	return (((Byte)obj).byteValue());
}

public char readChar() throws IOException {
	Object obj = vec.elementAt(ptr++);
	return (((Character)obj).charValue());
}

public double readDouble() throws IOException {
	Object obj = vec.elementAt(ptr++);
	return (((Double)obj).doubleValue());
}

public float readFloat() throws IOException {
	Object obj = vec.elementAt(ptr++);
	return (((Float)obj).floatValue());
}

public int readInt() throws IOException {
	Object obj = vec.elementAt(ptr++);
	return (((Integer)obj).intValue());
}

public long readLong() throws IOException {
	Object obj = vec.elementAt(ptr++);
	return (((Long)obj).longValue());
}

public short readShort() throws IOException {
	Object obj = vec.elementAt(ptr++);
	return (((Short)obj).shortValue());
}

public Object readObject() throws IOException {
	return (vec.elementAt(ptr++));
}

public int read(byte b[]) throws IOException {
	throw new IOException("not required");
}

public int read(byte b[], int off, int len) throws IOException {
	throw new IOException("not required");
}

public int read() throws IOException {
	throw new IOException("not required");
}

public long skip(long n) throws IOException {
	throw new IOException("not required");
}

public int available() throws IOException {
	throw new IOException("not required");
}

public void readFully(byte b[]) throws IOException {
	throw new IOException("not required");
}

public void readFully(byte b[], int off, int len) throws IOException {
	throw new IOException("not required");
}

public String readLine() throws IOException {
	throw new IOException("not required");
}

public String readUTF() throws IOException {
	throw new IOException("not required");
}

public int readUnsignedByte() throws IOException {
	throw new IOException("not required");
}

public int readUnsignedShort() throws IOException {
	throw new IOException("not required");
}

public int skipBytes(int n) throws IOException {
	throw new IOException("not required");
}

public void close() throws IOException {
}

}

}
