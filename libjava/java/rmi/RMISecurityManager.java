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

package java.rmi;

import java.io.FileDescriptor;
import java.lang.Thread;
import java.lang.Class;
import java.lang.SecurityManager;
import java.net.InetAddress;
import java.security.Permission;

public class RMISecurityManager extends SecurityManager {

public RMISecurityManager() {
}

public void checkAccept(String host, int port) {
}

public void checkAccess(Thread g) {
}

public void checkAccess(ThreadGroup g) {
}

public void checkAwtEventQueueAccess() {
}

public void checkConnect(String host, int port) {
}

public void checkConnect(String host, int port, Object context) {
}

public void checkCreateClassLoader() {
}

public void checkDelete(String file) {
}

public void checkExec(String cmd) {
}

public void checkExit(int status) {
}

public void checkLink(String lib) {
}

public void checkListen(int port) {
}

public void checkMemberAccess ( Class clazz, int which ) {
}

public void checkMulticast(InetAddress maddr) {
}

public void checkMulticast(InetAddress maddr, byte ttl) {
}

public void checkPackageAccess(String pkg) {
}

public void checkPackageDefinition(String pkg) {
}

public void checkPermission(Permission perm) {
}

public void checkPermission(Permission perm, Object context) {
}

public void checkPrintJobAccess() {
}

public void checkPropertiesAccess() {
}

public void checkPropertyAccess(String key) {
}

/* public void checkPropertyAccess(String key, String def) {
}*/

public void checkRead(FileDescriptor fd) {
}

public void checkRead(String file) {
}

public void checkRead(String file, Object context) {
}

public void checkSecurityAccess(String action) {
}

public void checkSetFactory() {
}

public void checkSystemClipboardAccess() {
}

public boolean checkTopLevelWindow(Object window) {
	return (true);
}

public void checkWrite(FileDescriptor fd) {
}

public void checkWrite(String file) {
}

}
