/* Package.java -- information about a package
   Copyright (C) 2000, 2001, 2002, 2003, 2005, 2006
   Free Software Foundation, Inc.

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

package java.lang;

import gnu.classpath.VMStackWalker;

import java.lang.annotation.Annotation;
import java.lang.reflect.AnnotatedElement;
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
 *
 * <p>The most useful method is the <code>isCompatibleWith()</code> method that
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
 * @author Mark Wielaard (mark@klomp.org)
 * @see ClassLoader#definePackage(String, String, String, String, String,
 *      String, String, URL)
 * @since 1.2
 * @status updated to 1.5
 */
public class Package
  implements AnnotatedElement
{
  /** The name of the Package */
  private final String name;

  /** The name if the implementation */
  private final String implTitle;

  /** The vendor that wrote this implementation */
  private final String implVendor;

  /** The version of this implementation */
  private final String implVersion;

  /** The name of the specification */
  private final String specTitle;

  /** The name of the specification designer */
  private final String specVendor;

  /** The version of this specification */
  private final String specVersion;

  /** If sealed the origin of the package classes, otherwise null */
  private final URL sealed;

  /** The class loader that defined this package */
  private ClassLoader loader;

  /** @deprecated Please use the other constructor that takes the class loader
   *              that defines the Package.
   */
  Package(String name,
          String specTitle, String specVendor, String specVersion,
          String implTitle, String implVendor, String implVersion, URL sealed)
  {
    this(name, specTitle, specVendor, specVersion, implTitle, implVendor,
         implVersion, sealed, null);
  }

  /**
   * A package local constructor for the Package class. All parameters except
   * the <code>name</code> of the package may be <code>null</code>.
   * There are no public constructors defined for Package; this is a package
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
          String implTitle, String implVendor, String implVersion, URL sealed,
          ClassLoader loader)
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
    this.loader = loader;
  }

  /**
   * Returns the Package name in dot-notation.
   *
   * @return the non-null package name
   */
  public String getName()
  {
    return name;
  }

  /**
   * Returns the name of the specification, or null if unknown.
   *
   * @return the specification title
   */
  public String getSpecificationTitle()
  {
    return specTitle;
  }

  /**
   * Returns the version of the specification, or null if unknown.
   *
   * @return the specification version
   */
  public String getSpecificationVersion()
  {
    return specVersion;
  }

  /**
   * Returns the name of the specification designer, or null if unknown.
   *
   * @return the specification vendor
   */
  public String getSpecificationVendor()
  {
    return specVendor;
  }

  /**
   * Returns the name of the implementation, or null if unknown.
   *
   * @return the implementation title
   */
  public String getImplementationTitle()
  {
    return implTitle;
  }

  /**
   * Returns the version of this implementation, or null if unknown.
   *
   * @return the implementation version
   */
  public String getImplementationVersion()
  {
    return implVersion;
  }

  /**
   * Returns the vendor that wrote this implementation, or null if unknown.
   *
   * @return the implementation vendor
   */
  public String getImplementationVendor()
  {
    return implVendor;
  }

  /**
   * Returns true if this Package is sealed.
   *
   * @return true if the package is sealed
   */
  public boolean isSealed()
  {
    return sealed != null;
  }

  /**
   * Returns true if this Package is sealed and the origin of the classes is
   * the given URL.
   *
   * @param url the URL to test
   * @return true if the package is sealed by this URL
   * @throws NullPointerException if url is null
   */
  public boolean isSealed(URL url)
  {
    return url.equals(sealed);
  }

  /**
   * Checks if the version of the specification is higher or at least as high
   * as the desired version. Comparison is done by sequentially comparing
   * dotted decimal numbers from the parameter and from
   * <code>getSpecificationVersion</code>.
   *
   * @param version the (minimal) desired version of the specification
   *
   * @return true if the version is compatible, false otherwise
   *
   * @throws NumberFormatException if either version string is invalid
   * @throws NullPointerException if either version string is null
   */
  public boolean isCompatibleWith(String version)
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
        // This must have been thrown by spec.nextToken() so return false.
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
   *
   * @param name the name of the desired package
   * @return the package by that name in the current ClassLoader
   */
  public static Package getPackage(String name)
  {
    // Get the caller's classloader
    ClassLoader cl = VMStackWalker.getCallingClassLoader();
    return cl != null ? cl.getPackage(name) : VMClassLoader.getPackage(name);
  }

  /**
   * Returns all the packages that are known to the callers class loader.
   * It may return an empty array if the classloader of the caller is null.
   *
   * @return an array of all known packages
   */
  public static Package[] getPackages()
  {
    // Get the caller's classloader
    ClassLoader cl = VMStackWalker.getCallingClassLoader();
    return cl != null ? cl.getPackages() : VMClassLoader.getPackages();
  }

  /**
   * Returns the hashCode of the name of this package.
   *
   * @return the hash code
   */
  public int hashCode()
  {
    return name.hashCode();
  }

  /**
   * Returns a string representation of this package. It is specified to
   * be <code>"package " + getName() + (getSpecificationTitle() == null
   * ? "" : ", " + getSpecificationTitle()) + (getSpecificationVersion()
   * == null ? "" : ", version " + getSpecificationVersion())</code>.
   *
   * @return the string representation of the package
   */
  public String toString()
  {
    return ("package " + name + (specTitle == null ? "" : ", " + specTitle)
            + (specVersion == null ? "" : ", version " + specVersion));
  }

  /**
   * Returns this package's annotation for the specified annotation type,
   * or <code>null</code> if no such annotation exists.
   *
   * @param annotationClass the type of annotation to look for.
   * @return this package's annotation for the specified type, or
   *         <code>null</code> if no such annotation exists.
   * @since 1.5
   */
  public <A extends Annotation> A getAnnotation(Class<A> annotationClass)
  {
    A foundAnnotation = null;
    Annotation[] annotations = getAnnotations();
    for (Annotation annotation : annotations)
      if (annotation.annotationType() == annotationClass)
        foundAnnotation = (A) annotation;
    return foundAnnotation;
  }

  /**
   * Returns all annotations associated with this package.  If there are
   * no annotations associated with this package, then a zero-length array
   * will be returned.  The returned array may be modified by the client
   * code, but this will have no effect on the annotation content of this
   * package, and hence no effect on the return value of this method for
   * future callers.
   *
   * @return this package' annotations.
   * @since 1.5
   */
  public Annotation[] getAnnotations()
  {
    /** All a package's annotations are declared within it. */
    return getDeclaredAnnotations();
  }

  /**
   * Returns all annotations directly defined by this package.  If there are
   * no annotations associated with this package, then a zero-length array
   * will be returned.  The returned array may be modified by the client
   * code, but this will have no effect on the annotation content of this
   * package, and hence no effect on the return value of this method for
   * future callers.
   *
   * @return the annotations directly defined by this package.
   * @since 1.5
   */
  public Annotation[] getDeclaredAnnotations()
  {
    try
      {
        Class pkgInfo = Class.forName(name + ".package-info", false, loader);
        return pkgInfo.getDeclaredAnnotations();
      }
    catch (ClassNotFoundException _)
      {
        return new Annotation[0];
      }
  }

  /**
   * Returns true if an annotation for the specified type is associated
   * with this package.  This is primarily a short-hand for using marker
   * annotations.
   *
   * @param annotationClass the type of annotation to look for.
   * @return true if an annotation exists for the specified type.
   * @since 1.5
   */
  public boolean isAnnotationPresent(Class<? extends Annotation>
                                     annotationClass)
  {
    return getAnnotation(annotationClass) != null;
  }

} // class Package
