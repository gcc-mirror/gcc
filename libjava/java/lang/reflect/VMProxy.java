/* VMProxy.java -- VM interface for proxy class
   Copyright (C) 2005  Free Software Foundation, Inc.

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


package java.lang.reflect;

final class VMProxy
{
  /**
   * Set to true if the VM provides a native method to implement
   * Proxy.getProxyClass completely, including argument verification.
   * If this is true, HAVE_NATIVE_GET_PROXY_DATA and
   * HAVE_NATIVE_GENERATE_PROXY_CLASS should be false.
   * @see java.lang.reflect.Proxy
   */
  static boolean HAVE_NATIVE_GET_PROXY_CLASS = false;

  /**
   * Set to true if the VM provides a native method to implement
   * the first part of Proxy.getProxyClass: generation of the array
   * of methods to convert, and verification of the arguments.
   * If this is true, HAVE_NATIVE_GET_PROXY_CLASS should be false.
   * @see java.lang.reflect.Proxy
   */
  static boolean HAVE_NATIVE_GET_PROXY_DATA = false;

  /**
   * Set to true if the VM provides a native method to implement
   * the second part of Proxy.getProxyClass: conversion of an array of
   * methods into an actual proxy class.
   * If this is true, HAVE_NATIVE_GET_PROXY_CLASS should be false.
   * @see java.lang.reflect.Proxy
   */
  static boolean HAVE_NATIVE_GENERATE_PROXY_CLASS = true;

  /**
   * Optional native method to replace (and speed up) the pure Java
   * implementation of getProxyClass.  Only needed if
   * VMProxy.HAVE_NATIVE_GET_PROXY_CLASS is true, this does the
   * work of both getProxyData and generateProxyClass with no
   * intermediate form in Java. The native code may safely assume that
   * this class must be created, and does not already exist.
   *
   * @param loader the class loader to define the proxy class in; null
   *        implies the bootstrap class loader
   * @param interfaces the interfaces the class will extend
   * @return the generated proxy class
   * @throws IllegalArgumentException if the constraints for getProxyClass
   *         were violated, except for problems with null
   * @throws NullPointerException if `interfaces' is null or contains
   *         a null entry, or if handler is null
   * @see Configuration#HAVE_NATIVE_GET_PROXY_CLASS
   * @see #getProxyClass(ClassLoader, Class[])
   * @see #getProxyData(ClassLoader, Class[])
   * @see #generateProxyClass(ProxyData)
   */
  static Class getProxyClass(ClassLoader loader, Class[] interfaces)
  {
    return null;
  }

  /**
   * Optional native method to replace (and speed up) the pure Java
   * implementation of getProxyData.  Only needed if
   * Configuration.HAVE_NATIVE_GET_PROXY_DATA is true. The native code
   * may safely assume that a new ProxyData object must be created which
   * does not duplicate any existing ones.
   *
   * @param loader the class loader to define the proxy class in; null
   *        implies the bootstrap class loader
   * @param interfaces the interfaces the class will extend
   * @return all data that is required to make this proxy class
   * @throws IllegalArgumentException if the constraints for getProxyClass
   *         were violated, except for problems with null
   * @throws NullPointerException if `interfaces' is null or contains
   *         a null entry, or if handler is null
   * @see Configuration.HAVE_NATIVE_GET_PROXY_DATA
   * @see #getProxyClass(ClassLoader, Class[])
   * @see #getProxyClass(ClassLoader, Class[])
   * @see ProxyType#getProxyData()
   */
  static Proxy.ProxyData getProxyData(ClassLoader loader, Class[] interfaces)
  {
    return null;
  }

  /**
   * Optional native method to replace (and speed up) the pure Java
   * implementation of generateProxyClass.  Only needed if
   * Configuration.HAVE_NATIVE_GENERATE_PROXY_CLASS is true. The native
   * code may safely assume that a new Class must be created, and that
   * the ProxyData object does not describe any existing class.
   *
   * @param loader the class loader to define the proxy class in; null
   *        implies the bootstrap class loader
   * @param data the struct of information to convert to a Class. This
   *        has already been verified for all problems except exceeding
   *        VM limitations
   * @return the newly generated class
   * @throws IllegalArgumentException if VM limitations are exceeded
   * @see #getProxyClass(ClassLoader, Class[])
   * @see #getProxyClass(ClassLoader, Class[])
   * @see ProxyData#generateProxyClass(ClassLoader)
   */
  static native Class generateProxyClass(ClassLoader loader, Proxy.ProxyData data);
}
