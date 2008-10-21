/* gnu.classpath.tools.gjdoc.TemporaryStore
   Copyright (C) 2001 Free Software Foundation, Inc.

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
02111-1307 USA. */

package gnu.classpath.tools.gjdoc;

/**
 *  Useful for passing big objects that are no longer needed by the
 *  calling method, reducing memory usage.  <p/>
 *
 *  Consider the following problem:
 *  <pre>
 *   public class A {
 *     public static void foo() {
 *       long[] hugeArray = new long[1000000]; // takes around 8 MB
 *       // ... fill hugeArray with some information ...
 *       bar(hugeArray);
 *       // ... hugeArray is no more required at this point
 *     }
 *     public static void bar(long[] arr) {
 *       // ... process contents of arr ...
 *       arr = null;
 *       System.gc();      // NOTE: will not collect arr!
 *       // ... do something memory-intensive where arr is not needed
 *     }
 *  }
 *  </pre>
 *
 *  In method <code>bar()</code>, the array cannot be garbage
 *  collected because the local variable <code>hugeArray</code> in
 *  method <code>foo()</code> still holds a reference to the array.
 *  <p/>
 *
 *  When calling <code>bar(new long[1000000]);</code> in
 *  <code>arr</code> the array <i>can</i> be collected in
 *  <code>bar()</code>, but that way it can't be initialized in
 *  <code>foo()</code>. A local variable is needed for
 *  initialization, but the variable can't be cleared before it is
 *  passed to <code>bar()</code>!  <p/>
 *
 *  <code>TemporaryStore</code> is the solution for this
 *  dilemma. The modified method <code>foo()</code> which uses a
 *  <code>TemporaryStore</code> object would look like this:
 *
 *  <pre>
 *     public static void foo() {
 *       long[] hugeArray = new long[1000000]; // takes around 7 MB
 *       // ... fill hugeArray with some very important information ...
 *       TemporaryStore tstore = new TemporaryStore(hugeArray);
 *       hugeArray = null;
 *       bar((long[])tstore.getAndClear());
 *     }
 *  </pre>
 *
 *  When control flow is transferred to <code>bar()</code>,
 *  <code>foo()</code> will hold no more references to the array
 *  and so it can be garbage collected in <code>bar()</code>.
 * 
 */
public class TemporaryStore {

   private Object storedObject;

   /**
    *  Temporarily store the given object for passing it to a
    *  different method.  <p/>
    *
    *  The method constructing a new TemporaryStore object should
    *  clear all other references to the stored object, so that
    *  this TemporaryStore is the only object referencing it.
    *
    *  @param storedObject  the object to store temporarily
    *
    */
   public TemporaryStore(Object storedObject) {
      this.storedObject = storedObject;
   }

   /**
    *  Return the stored object after clearing the reference to it.
    *  <p/>
    *
    *  When the user of this class followed the recommendations in
    *  the documentation of @link{TemporaryStore(Object)}, the
    *  returned reference will be the only reference to the stored
    *  object after this method returns. If the returned reference
    *  is passed in a method call, the called method will hold the
    *  only reference to the stored object and can release it by
    *  nulling the corresponding parameter.
    *
    *  @return the object which was passed to the constructor.
    *
    */
   public Object getAndClear() {
      Object rc = this.storedObject;
      this.storedObject = null;
      return rc;
   }
}
