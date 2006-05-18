/* java.lang.reflect.Modifier
   Copyright (C) 1998, 1999, 2001, 2002, 2005  Free Software Foundation, Inc.

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

/**
 * Modifier is a helper class with static methods to determine whether an
 * int returned from getModifiers() represents static, public, protected,
 * native, final, etc... and provides an additional method to print
 * out all of the modifiers in an int in order.
 * <p>
 * The methods in this class use the bitmask values in the VM spec to
 * determine the modifiers of an int. This means that a VM must return a
 * standard mask, conformant with the VM spec.  I don't know if this is how
 * Sun does it, but I'm willing to bet money that it is.
 *
 * @author John Keiser
 * @author Tom Tromey (tromey@cygnus.com)
 * @author Eric Blake (ebb9@email.byu.edu)
 * @see Member#getModifiers()
 * @see Method#getModifiers()
 * @see Field#getModifiers()
 * @see Constructor#getModifiers()
 * @see Class#getModifiers()
 * @since 1.1
 */
public class Modifier
{
  /** <STRONG>This constructor really shouldn't be here ... there are no
   * instance methods or variables of this class, so instantiation is
   * worthless.  However, this function is in the 1.1 spec, so it is added
   * for completeness.</STRONG>
   */
  public Modifier()
  {
  }

  /**
   * Public: accessible from any other class.
   */
  public static final int PUBLIC = 0x0001;

  /**
   * Private: accessible only from the same enclosing class.
   */
  public static final int PRIVATE = 0x0002;

  /**
   * Protected: accessible only to subclasses, or within the package.
   */
  public static final int PROTECTED = 0x0004;

  /**
   * Static:<br><ul>
   * <li>Class: no enclosing instance for nested class.</li>
   * <li>Field or Method: can be accessed or invoked without an
   *     instance of the declaring class.</li>
   * </ul>
   */
  public static final int STATIC = 0x0008;

  /**
   * Final:<br><ul>
   * <li>Class: no subclasses allowed.</li>
   * <li>Field: cannot be changed.</li>
   * <li>Method: cannot be overriden.</li>
   * </ul>
   */
  public static final int FINAL = 0x0010;

  /**
   * Synchronized: Method: lock the class while calling this method.
   */
  public static final int SYNCHRONIZED = 0x0020;

  /**
   * Volatile: Field: cannot be cached.
   */
  public static final int VOLATILE = 0x0040;

  /**
   * Transient: Field: not serialized or deserialized.
   */
  public static final int TRANSIENT = 0x0080;

  /**
   * Native: Method: use JNI to call this method.
   */
  public static final int NATIVE = 0x0100;

  /**
   * Interface: Class: is an interface.
   */
  public static final int INTERFACE = 0x0200;

  /**
   * Abstract:<br><ul>
   * <li>Class: may not be instantiated.</li>
   * <li>Method: may not be called.</li>
   * </ul>
   */
  public static final int ABSTRACT = 0x0400;

  /**
   * Strictfp: Method: expressions are FP-strict.<p>
   * Also used as a modifier for classes, to mean that all initializers
   * and constructors are FP-strict, but does not show up in
   * Class.getModifiers.
   */
  public static final int STRICT = 0x0800;


  /**
   * Super - treat invokespecial as polymorphic so that super.foo() works
   * according to the JLS. This is a reuse of the synchronized constant
   * to patch a hole in JDK 1.0. *shudder*.
   */
  static final int SUPER = 0x0020;

  /**
   * All the flags, only used by code in this package.
   */
  static final int ALL_FLAGS = 0xfff;

  /**
   * Flag indicating a bridge method.
   */
  static final int BRIDGE = 0x40;

  /**
   * Flag indicating a varargs method.
   */
  static final int VARARGS = 0x80;

  /**
   * Flag indicating a synthetic member.
   */
  static final int SYNTHETIC = 0x1000;

  /**
   * Flag indicating an enum constant or an enum class.
   */
  static final int ENUM = 0x4000;

  /**
   * Check whether the given modifier is abstract.
   * @param mod the modifier.
   * @return <code>true</code> if abstract, <code>false</code> otherwise.
   */
  public static boolean isAbstract(int mod)
  {
    return (mod & ABSTRACT) != 0;
  }

  /**
   * Check whether the given modifier is final.
   * @param mod the modifier.
   * @return <code>true</code> if final, <code>false</code> otherwise.
   */
  public static boolean isFinal(int mod)
  {
    return (mod & FINAL) != 0;
  }

  /**
   * Check whether the given modifier is an interface.
   * @param mod the modifier.
   * @return <code>true</code> if an interface, <code>false</code> otherwise.
   */
  public static boolean isInterface(int mod)
  {
    return (mod & INTERFACE) != 0;
  }

  /**
   * Check whether the given modifier is native.
   * @param mod the modifier.
   * @return <code>true</code> if native, <code>false</code> otherwise.
   */
  public static boolean isNative(int mod)
  {
    return (mod & NATIVE) != 0;
  }

  /**
   * Check whether the given modifier is private.
   * @param mod the modifier.
   * @return <code>true</code> if private, <code>false</code> otherwise.
   */
  public static boolean isPrivate(int mod)
  {
    return (mod & PRIVATE) != 0;
  }

  /**
   * Check whether the given modifier is protected.
   * @param mod the modifier.
   * @return <code>true</code> if protected, <code>false</code> otherwise.
   */
  public static boolean isProtected(int mod)
  {
    return (mod & PROTECTED) != 0;
  }

  /**
   * Check whether the given modifier is public.
   * @param mod the modifier.
   * @return <code>true</code> if public, <code>false</code> otherwise.
   */
  public static boolean isPublic(int mod)
  {
    return (mod & PUBLIC) != 0;
  }

  /**
   * Check whether the given modifier is static.
   * @param mod the modifier.
   * @return <code>true</code> if static, <code>false</code> otherwise.
   */
  public static boolean isStatic(int mod)
  {
    return (mod & STATIC) != 0;
  }

  /**
   * Check whether the given modifier is strictfp.
   * @param mod the modifier.
   * @return <code>true</code> if strictfp, <code>false</code> otherwise.
   */
  public static boolean isStrict(int mod)
  {
    return (mod & STRICT) != 0;
  }

  /**
   * Check whether the given modifier is synchronized.
   * @param mod the modifier.
   * @return <code>true</code> if synchronized, <code>false</code> otherwise.
   */
  public static boolean isSynchronized(int mod)
  {
    return (mod & SYNCHRONIZED) != 0;
  }

  /**
   * Check whether the given modifier is transient.
   * @param mod the modifier.
   * @return <code>true</code> if transient, <code>false</code> otherwise.
   */
  public static boolean isTransient(int mod)
  {
    return (mod & TRANSIENT) != 0;
  }

  /**
   * Check whether the given modifier is volatile.
   * @param mod the modifier.
   * @return <code>true</code> if volatile, <code>false</code> otherwise.
   */
  public static boolean isVolatile(int mod)
  {
    return (mod & VOLATILE) != 0;
  }

  /**
   * Get a string representation of all the modifiers represented by the
   * given int. The keywords are printed in this order:
   * <code>&lt;public|protected|private&gt; abstract static final transient
   * volatile synchronized native strictfp interface</code>.
   *
   * @param mod the modifier.
   * @return the String representing the modifiers.
   */
  public static String toString(int mod)
  {
    return toString(mod, new StringBuilder()).toString();
  }

  /**
   * Package helper method that can take a StringBuilder.
   * @param mod the modifier
   * @param r the StringBuilder to which the String representation is appended
   * @return r, with information appended
   */
  static StringBuilder toString(int mod, StringBuilder r)
  {
    r.append(toString(mod, new StringBuffer()));
    return r;
  }

  /**
   * Package helper method that can take a StringBuffer.
   * @param mod the modifier
   * @param r the StringBuffer to which the String representation is appended
   * @return r, with information appended
   */
  static StringBuffer toString(int mod, StringBuffer r)
  {
    if (isPublic(mod))
      r.append("public ");
    if (isProtected(mod))
      r.append("protected ");
    if (isPrivate(mod))
      r.append("private ");
    if (isAbstract(mod))
      r.append("abstract ");
    if (isStatic(mod))
      r.append("static ");
    if (isFinal(mod))
      r.append("final ");
    if (isTransient(mod))
      r.append("transient ");
    if (isVolatile(mod))
      r.append("volatile ");
    if (isSynchronized(mod))
      r.append("synchronized ");
    if (isNative(mod))
      r.append("native ");
    if (isStrict(mod))
      r.append("strictfp ");
    if (isInterface(mod))
      r.append("interface ");
    
    // Trim trailing space.
    if ((mod & ALL_FLAGS) != 0)
      r.setLength(r.length() - 1);
    return r;
  }
}
