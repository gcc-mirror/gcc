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

package java.rmi.server;

import java.io.Serializable;
import java.io.ObjectOutput;
import java.io.ObjectInput;
import java.io.IOException;
import java.lang.Math;
import java.io.DataInput;
import java.io.DataOutput;
import java.util.Random;

public final class ObjID
	implements Serializable {

static final long serialVersionUID = -6386392263968365220L;

private static long next = 0x8000000000000000L;
private static final Object lock = ObjID.class;

public static final int REGISTRY_ID = 0;
public static final int ACTIVATOR_ID = 1;
public static final int DGC_ID = 2;

private long objNum;
private UID space;

public ObjID() {
	synchronized (lock) {
		objNum = next++;
	}
	space = new UID();
}

public ObjID(int num) {
	objNum = (long)num;
	space = new UID((short)0);
}

public void write(ObjectOutput out) throws IOException {
	DataOutput dout = (DataOutput)out;
	dout.writeLong(objNum);
	space.write(dout);
}

public static ObjID read(ObjectInput in) throws IOException {
	DataInput din = (DataInput)in;
	ObjID id = new ObjID();
	id.objNum = din.readLong();
	id.space = UID.read(din);
	return (id);
}

public int hashCode() {
	return ((int)objNum);
}

public boolean equals(Object obj) {
	if (obj instanceof ObjID && this.objNum == ((ObjID)obj).objNum) {
		return (true);
	}
	return (false);
}

public String toString() {
	return ("[objNum: " + objNum + ", " + space + "]");
}

}
