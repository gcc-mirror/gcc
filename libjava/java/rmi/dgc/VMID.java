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

package java.rmi.dgc;

import java.io.Serializable;
import java.rmi.server.UID;
import java.net.InetAddress;
import java.net.UnknownHostException;

public final class VMID
	implements Serializable {

static final long serialVersionUID = -538642295484486218L;
static final boolean areWeUnique;
static byte[] localAddr;

private byte[] addr;
private UID uid;

static {
	byte[] addr;
	boolean awu = true;
	try {
		addr = InetAddress.getLocalHost().getAddress();
		if (addr[0] == 127 && addr[1] == 0 && addr[2] == 0 && addr[3] == 1) {
			awu = false;
		}
	}
	catch (UnknownHostException _) {
		addr = new byte[]{ 127, 0, 0, 1 };
		awu = false;
	}
	localAddr = addr;
	areWeUnique = awu;
}

public VMID() {
	addr = localAddr;
	uid = new UID();
}

public static boolean isUnique() {
	return (areWeUnique);
}

public int hashCode() {
	return (super.hashCode());
}

public boolean equals(Object obj) {
	if (!(obj instanceof VMID)) {
		return (false);
	}
	VMID other = (VMID)obj;
	if (addr.length != other.addr.length) {
		return (false);
	}
	for (int i = addr.length - 1; i >= 0; i--) {
		if (addr[i] != other.addr[i]) {
			return (false);
		}
	}
	return (uid.equals(other.uid));
}

public String toString() {
	StringBuffer buf = new StringBuffer("[VMID: ");
	for (int i = 0; i < addr.length; i++) {
		if (i > 0) {
			buf.append(".");
		}
		buf.append(Integer.toString(addr[i]));
	}
	buf.append(" ");
	buf.append(uid.toString());
	buf.append("]");

	return (buf.toString());
}

}
