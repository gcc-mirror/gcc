/* java.lang.reflect.Modifier
   Copyright (C) 1998, 1999, 2001 Free Software Foundation, Inc.

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
executable file might be covered by the GNU General Public License. */


package java.lang.reflect;

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * plus online API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status: Believed complete and correct to version 1.2.
 */

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
 * @author Tom Tromey <tromey@cygnus.com>
 *
 * @see Member#getModifiers()
 * @see Method#getModifiers()
 * @see Field#getModifiers()
 * @see Constructor#getModifiers()
 * @see Class#getModifiers()
 */
public class Modifier
{
  /** <STRONG>This constructor really shouldn't be here ... there are no
   * instance methods or variables of this class, so instantiation is
   * worthless.  However, this function is in the 1.1 spec, so it is added
   * for completeness.</STRONG>
   */
  public Modifier() {}

  /** Public: accessible from any other class. **/
  public static final int PUBLIC = 0x0001;

  /** Private: accessible only from the declaring class. **/
  public static final int PRIVATE = 0x0002;

  /** Protected: accessible only to subclasses. **/
  public static final int PROTECTED = 0x0004;

  /** Static: field or method - can be accessed or invoked without an
      instance of the declaring class. **/
  public static final int STATIC = 0x0008;

  /** Final:<BR>
   * <UL>
   * <LI> Class: no subclasses allowed. </LI>
   * <LI> Field: cannot be changed. </LI>
   * <LI> Method: cannot be overriden. </LI>
   * </UL>
   */
  public static final int FINAL = 0x0010;

  /** Synchronized: lock the class while calling this method. **/
  public static final int SYNCHRONIZED = 0x0020;

  /** Volatile: cannot be cached.<P> **/
  public static final int VOLATILE = 0x0040;

  /** Transient: not serialized or deserialized. **/
  public static final int TRANSIENT = 0x0080;

  /** Native: use JNI to call this method. **/
  public static final int NATIVE = 0x0100;

  /** Interface: is an interface. **/
  public static final int INTERFACE = 0x0200;

  /** Abstract: class - may not be instantiated;
      method - may not be called. **/
  public static final int ABSTRACT = 0x0400;

  /** Class or method - expressions are FP-strict. **/
  public static final int STRICT = 0x0800;


  /* NOTE: THIS IS HERE BECAUSE IT IS IN THE VM SPEC.
     I INCLUDE IT FOR COMPLETENESS.  IT ATTACHES TO A CLASS AND MEANS
     "Treat superclasses specially in invokespecial". Note that it is the
     same as synchronized.  Reuse of the constant.  *shudder* */
  private static final int SUPER = 0x0020;

  // This can only used by other code in this package, so it is not public.
  static final int ALL_FLAGS = 0xfff;

  /** Check whether the given modifier is abstract.
   * @param mod the modifier.
   * @return <code>true</code> if abstract, <code>false</code> otherwise.
   */
  public static boolean isAbstract (int mod)
  {
    return (mod & ABSTRACT) != 0;
  }

  /** Check whether the given modifier is final.
   * @param mod the modifier.
   * @return <code>true</code> if final, <code>false</code> otherwise.
   */
  public static boolean isFinal (int mod)
  {
    return (mod & FINAL) != 0;
  }

  /** Check whether the given modifier is an interface.
   * @param mod the modifier.
   * @return <code>true</code> if an interface, <code>false</code> otherwise.
   */
  public static boolean isInterface (int mod)
  {
    return (mod & INTERFACE) != 0;
  }

  /** Check whether the given modifier is native.
   * @param mod the modifier.
   * @return <code>true</code> if native, <code>false</code> otherwise.
   */
  public static boolean isNative (int mod)
  {
    return (mod & NATIVE) != 0;
  }

  /** Check whether the given modifier is private.
   * @param mod the modifier.
   * @return <code>true</code> if private, <code>false</code> otherwise.
   */
  public static boolean isPrivate (int mod)
  {
    return (mod & PRIVATE) != 0;
  }

  /** Check whether the given modifier is protected.
   * @param mod the modifier.
   * @return <code>true</code> if protected, <code>false</code> otherwise.
   */
  public static boolean isProtected (int mod)
  {
    return (mod & PROTECTED) != 0;
  }

  /** Check whether the given modifier is public.
   * @param mod the modifier.
   * @return <code>true</code> if public, <code>false</code> otherwise.
   */
  public static boolean isPublic (int mod)
  {
    return (mod & PUBLIC) != 0;
  }

  /** Check whether the given modifier is static.
   * @param mod the modifier.
   * @return <code>true</code> if static, <code>false</code> otherwise.
   */
  public static boolean isStatic (int mod)
  {
    return (mod & STATIC) != 0;
  }

  /** Check whether the given modifier is strictfp.
   * @param mod the modifier.
   * @return <code>true</code> if strictfp, <code>false</code> otherwise.
   */
  public static boolean isStrict (int mod)
  {
    return (mod & STRICT) != 0;
  }

  /** Check whether the given modifier is synchronized.
   * @param mod the modifier.
   * @return <code>true</code> if synchronized, <code>false</code> otherwise.
   */
  public static boolean isSynchronized (int mod)
  {
    return (mod & SYNCHRONIZED) != 0;
  }

  /** Check whether the given modifier is transient.
   * @param mod the modifier.
   * @return <code>true</code> if transient, <code>false</code> otherwise.
   */
  public static boolean isTransient (int mod)
  {
    return (mod & TRANSIENT) != 0;
  }

  /** Check whether the given modifier is volatile.
   * @param mod the modifier.
   * @return <code>true</code> if volatile, <code>false</code> otherwise.
   */
  public static boolean isVolatile (int mod)
  {
    return (mod & VOLATILE) != 0;
  }

  /** Get a string representation of all the modifiers represented by the
   * given int.
   * The keywords are printed in this order:
   * <code>&lt;public|private|protected&gt; abstract static final transient
   * volatile native synchronized interface strictfp</code><P>
   *
   * <STRONG>This is, near as I can tell, the "canonical order" of modifiers
   * mentioned by Sun in the reference implementation.  I have inferred this
   * from the order of printing in the Field, Method and Constructor
   * classes.</STRONG>
   *
   * @param mod the modifier.
   * @return the String representing the modifiers.
   */
  public static String toString (int mod)
  {
    StringBuffer r = new StringBuffer ();
    toString(mod, r);
    return r.toString();
  }

  /**
   * Package private helper toString() method that can take a StringBuffer.
   * @param mod the modifier
   * @param r the StringBuffer to which the String representation is appended
   */
  static void toString (int mod, StringBuffer r)
  {
    if (isPublic (mod))
      r.append("public ");
    if (isProtected (mod))
      r.append("protected ");
    if (isPrivate (mod))
      r.append("private ");
    if (isAbstract (mod))
      r.append("abstract ");
    if (isStatic (mod))
      r.append("static ");
    if (isFinal (mod))
      r.append("final ");
    if (isTransient (mod))
      r.append("transient ");
    if (isVolatile (mod))
      r.append("volatile ");
    if (isNative (mod))
      r.append("native ");
    if (isSynchronized (mod))
      r.append("synchronized ");
    if (isInterface (mod))
      r.append("interface ");
    if (isStrict (mod))
      r.append("strictfp ");
    
    // Trim trailing space.
    int l = r.length();
    if (l > 0)
      r.setLength(l - 1);
  }
}
