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
import java.io.DataOutput;
import java.io.DataInput;
import java.io.IOException;
import java.util.Random;
import java.lang.Thread;
import java.lang.InterruptedException;

public final class UID
	implements Serializable {

public static final long serialVersionUID = 1086053664494604050L;

private static final Object lock = UID.class;
private static long baseTime = System.currentTimeMillis();
private static short nextCount = Short.MIN_VALUE;
// This is sun's algorithm - don't ask me why ...
private static final int uniqueNr = (new Object()).hashCode();

private int unique;
private long time;
private short count;

/**
 * This is sun's algorithm - don't ask me why ...
 */
public UID() {
	synchronized (lock) {
		if (count == Short.MAX_VALUE) {
			long newtime;
			for (;;) {
				newtime = System.currentTimeMillis();
				if (newtime - baseTime > 1000) {
					break;
				}
				try {
					Thread.sleep(1000);
				}
				catch (InterruptedException _) {
				}
			}
			baseTime = newtime;
			nextCount = Short.MIN_VALUE;
		}
		count = nextCount++;
		unique = uniqueNr;
		time = baseTime;
	}
}

public UID(short num) {
	unique = (int)num;
	time = 0;
	count = 0;
}

public int hashCode() {
	return (unique);
}

public boolean equals(Object obj) {
	if (obj instanceof UID) {
		UID uid = (UID)obj;
		if (this.unique == uid.unique &&
		    this.time == uid.time &&
		    this.count == uid.count) {
			return (true);
		}
	}
	return (false);
}

public String toString() {
	return ("[UID: " + unique + "," + time + "," + count + "]");
}

public void write(DataOutput out) throws IOException {
	out.writeInt(unique);
	out.writeLong(time);
	out.writeShort(count);
}

public static UID read(DataInput in) throws IOException {
	UID id = new UID();
	id.unique = in.readInt();
	id.time = in.readLong();
	id.count = in.readShort();
	return (id);
}

}
