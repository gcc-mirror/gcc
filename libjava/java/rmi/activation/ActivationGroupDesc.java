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

package java.rmi.activation;

import java.io.Serializable;
import java.util.Properties;
import java.rmi.MarshalledObject;

public final class ActivationGroupDesc
	implements Serializable {

public static class CommandEnvironment
	implements Serializable {

private String cmdpath;
private String[] argv;

public CommandEnvironment(String cmdpath, String[] argv) {
	this.cmdpath = cmdpath;
	this.argv = argv;
}

public String getCommandPath() {
	return (cmdpath);
}

public String[] getCommandOptions() {
	return (argv);
}

public boolean equals(Object obj) {
	if (!(obj instanceof CommandEnvironment)) {
		return (false);
	}
	CommandEnvironment that = (CommandEnvironment)obj;
	
	if (!this.cmdpath.equals(that.cmdpath)) {
		return (false);
	}

	int len = this.argv.length;
	if (len != that.argv.length) {
		return (false);
	}
	for (int i = 0; i < len; i++) {
		if (!this.argv[i].equals(that.argv[i])) {
			return (false);
		}
	}
	return (true);
}

public int hashCode() {
	return (cmdpath.hashCode()); // Not a very good hash code.
}

}

public ActivationGroupDesc(Properties overrides, ActivationGroupDesc.CommandEnvironment cmd) {
	throw new Error("Not implemented");
}

public ActivationGroupDesc(String className, String location, MarshalledObject data, Properties overrides, ActivationGroupDesc.CommandEnvironment cmd) {
	throw new Error("Not implemented");
}

public String getClassName() {
	throw new Error("Not implemented");
}

public String getLocation() {
	throw new Error("Not implemented");
}

public MarshalledObject getData() {
	throw new Error("Not implemented");
}

public Properties getPropertyOverrides() {
	throw new Error("Not implemented");
}

public ActivationGroupDesc.CommandEnvironment getCommandEnvironment() {
	throw new Error("Not implemented");
}

public boolean equals(Object obj) {
	throw new Error("Not implemented");
}

public int hashCode() {
	throw new Error("Not implemented");
}

}
