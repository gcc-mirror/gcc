/* java.lang.Package - Everything you ever wanted to know about a package.
   Copyright (C) 2000, 2001 Free Software Foundation, Inc.

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

package java.lang;

import java.net.URL;
import java.util.NoSuchElementException;
import java.util.StringTokenizer;

/**
 * Everything you ever wanted to know about a package. This class makes it
 * possible to attach specification and implementation information to a
 * package as explained in the
 * <a href="http://java.sun.com/products/jdk/1.3/docs/guide/versioning/spec/VersioningSpecification.html#PackageVersionSpecification">Package Versioning Specification</a>
 * section of the
 * <a href="http://java.sun.com/products/jdk/1.3/docs/guide/versioning/spec/VersioningSpecification.html">Product Versioning Specification</a>.
 * It also allows packages to be sealed with respect to the originating URL.
 * <p>
 * The most useful method is the <code>isCompatibleWith()</code> method that
 * compares a desired version of a specification with the version of the
 * specification as implemented by a package. A package is considered
 * compatible with another version if the version of the specification is
 * equal or higher then the requested version. Version numbers are represented
 * as strings of positive numbers separated by dots (e.g. "1.2.0").
 * The first number is called the major number, the second the minor,
 * the third the micro, etc. A version is considered higher then another
 * version if it has a bigger major number then the another version or when
 * the major numbers of the versions are equal if it has a bigger minor number
 * then the other version, etc. (If a version has no minor, micro, etc numbers
 * then they are considered the be 0.)
 *
 * @since 1.2
 * @author Mark Wielaard (mark@klomp.org)
 */
public class Package
{
  /** The name of the Package */
  final private String name;

  /** The name if the implementation */
  final private String implTitle;
  /** The vendor that wrote this implementation */
  final private String implVendor;
  /** The version of this implementation */
  final private String implVersion;

  /** The name of the specification */
  final private String specTitle;
  /** The name of the specification designer */
  final private String specVendor;
  /** The version of this specification */
  final private String specVersion;

  /** If sealed the origin of the package classes, otherwise null */
  final private URL sealed;

  /** 
   * A package local constructor for the Package class.
   * All parameters except the <code>name</code> of the package may be
   * <code>null</code>.
   * There are no public constructors defined for Package this is a package
   * local constructor that is used by java.lang.Classloader.definePackage().
   * 
   * @param name The name of the Package
   * @param specTitle The name of the specification
   * @param specVendor The name of the specification designer
   * @param specVersion The version of this specification
   * @param implTitle The name of the implementation
   * @param implVendor The vendor that wrote this implementation
   * @param implVersion The version of this implementation
   * @param sealed If sealed the origin of the package classes
   */
  Package(String name,
	  String specTitle, String specVendor, String specVersion,
	  String implTitle, String implVendor, String implVersion, URL sealed)
  {
    if (name == null)
      throw new IllegalArgumentException("null Package name");

    this.name = name;

    this.implTitle = implTitle;
    this.implVendor = implVendor;
    this.implVersion = implVersion;

    this.specTitle = specTitle;
    this.specVendor = specVendor;
    this.specVersion = specVersion;

    this.sealed = sealed;
  }

  /** 
   * Returns the Package name.
   */
  public String getName()
  {
    return name;
  }

  /** 
   * Returns the name of the implementation or null if unknown.
   */
  public String getImplementationTitle()
  {
    return implTitle;
  }

  /** 
   * Returns the vendor that wrote this implementation or null if unknown.
   */
  public String getImplementationVendor()
  {
    return implVendor;
  }

  /** 
   * Returns the version of this implementation or null if unknown.
   */
  public String getImplementationVersion()
  {
    return implVersion;
  }

  /** 
   * Returns the name of the specification or null if unknown.
   */
  public String getSpecificationTitle()
  {
    return specTitle;
  }

  /** 
   * Returns the name of the specification designer or null if unknown.
   */
  public String getSpecificationVendor()
  {
    return specVendor;
  }

  /** 
   * Returns the version of the specification or null if unknown.
   */
  public String getSpecificationVersion()
  {
    return specVersion;
  }

  /** 
   * Returns true if this Package is sealed.
   */
  public boolean isSealed()
  {
    return (sealed != null);
  }

  /** 
   * Returns true if this Package is sealed and the origin of the classes is
   * the given URL.
   * 
   * @param url 
   */
  public boolean isSealed(URL url)
  {
    return url.equals(sealed);
  }

  /**
   * Checks if the version of the specification is higher or at least as high
   * as the desired version.
   * @param version the (minimal) desired version of the specification
   * @exception NumberFormatException when either version or the
   * specification version is not a correctly formatted version number
   * @exception NullPointerException if the supplied version or the
   * Package specification version is null.
   */
  public boolean isCompatibleWith(String version) throws NumberFormatException
  {
    StringTokenizer versionTokens = new StringTokenizer(version, ".");
    StringTokenizer specTokens = new StringTokenizer(specVersion, ".");
    try
      {
	while (versionTokens.hasMoreElements())
	  {
	    int vers = Integer.parseInt(versionTokens.nextToken());
	    int spec = Integer.parseInt(specTokens.nextToken());
	    if (spec < vers)
	      return false;
	    else if (spec > vers)
	      return true;
	    // They must be equal, next Token please!
	  }
      }
    catch (NoSuchElementException e)
      {
	// this must have been thrown by spec.netToken() so return false
	return false;
      }

    // They must have been exactly the same version.
    // Or the specVersion has more subversions. That is also good.
    return true;
  }

  /**
   * Returns the named package if it is known by the callers class loader.
   * It may return null if the package is unknown, when there is no
   * information on that particular package available or when the callers
   * classloader is null.
   * @param name the name of the desired package
   */
  public static Package getPackage(String name)
  {
    // Get the caller's classloader
    SecurityManager sm = System.getSecurityManager();
    Class c = sm.getClassContext()[1];
    ClassLoader cl = c.getClassLoader();

    if (cl != null)
      return cl.getPackage(name);
    else
      return null;
  }

  /**
   * Returns all the packages that are known to the callers class loader.
   * It may return an empty array if the classloader of the caller is null.
   */
  public static Package[] getPackages()
  {
    // Get the caller's classloader
    SecurityManager sm = System.getSecurityManager();
    Class c = sm.getClassContext()[1];
    ClassLoader cl = c.getClassLoader();

    if (cl != null)
      return cl.getPackages();
    else
      return new Package[0];
  }

  /** 
   * Returns the hashCode of the name of this package.
   */
  public int hashCode()
  {
    return name.hashCode();
  }

  /** 
   * Returns a string representation of this package name, specification,
   * implementation and class origin if sealed.
   */
  public String toString()
  {
    return "package: " + name +
	   " spec: " + specTitle +
	   " version: " + specVersion +
	   " vendor: " + specVendor +
	   " implementation: " + implTitle +
	   " version: " + implVersion +
	   " vendor: " + implVendor + " sealed: " + sealed;
  }
}
