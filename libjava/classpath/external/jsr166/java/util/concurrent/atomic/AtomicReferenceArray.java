/*
 * Written by Doug Lea with assistance from members of JCP JSR-166
 * Expert Group and released to the public domain, as explained at
 * http://creativecommons.org/licenses/publicdomain
 */

package java.util.concurrent.atomic;
import sun.misc.Unsafe;
import java.util.*;

/**
 * An array of object references in which elements may be updated
 * atomically.  See the {@link java.util.concurrent.atomic} package
 * specification for description of the properties of atomic
 * variables.
 * @since 1.5
 * @author Doug Lea
 * @param <E> The base class of elements held in this array
 */
public class AtomicReferenceArray<E> implements java.io.Serializable {
    private static final long serialVersionUID = -6209656149925076980L;

    private static final Unsafe unsafe = Unsafe.getUnsafe();
    private static final int base = unsafe.arrayBaseOffset(Object[].class);
    private static final int scale = unsafe.arrayIndexScale(Object[].class);
    private final Object[] array;

    private long rawIndex(int i) {
        if (i < 0 || i >= array.length)
            throw new IndexOutOfBoundsException("index " + i);
        return base + i * scale;
    }

    /**
     * Creates a new AtomicReferenceArray of given length.
     * @param length the length of the array
     */
    public AtomicReferenceArray(int length) {
        array = new Object[length];
        // must perform at least one volatile write to conform to JMM
        if (length > 0)
            unsafe.putObjectVolatile(array, rawIndex(0), null);
    }

    /**
     * Creates a new AtomicReferenceArray with the same length as, and
     * all elements copied from, the given array.
     *
     * @param array the array to copy elements from
     * @throws NullPointerException if array is null
     */
    public AtomicReferenceArray(E[] array) {
        if (array == null)
            throw new NullPointerException();
        int length = array.length;
        this.array = new Object[length];
        if (length > 0) {
            int last = length-1;
            for (int i = 0; i < last; ++i)
                this.array[i] = array[i];
            // Do the last write as volatile
            E e = array[last];
            unsafe.putObjectVolatile(this.array, rawIndex(last), e);
        }
    }

    /**
     * Returns the length of the array.
     *
     * @return the length of the array
     */
    public final int length() {
        return array.length;
    }

    /**
     * Gets the current value at position <tt>i</tt>.
     *
     * @param i the index
     * @return the current value
     */
    public final E get(int i) {
        return (E) unsafe.getObjectVolatile(array, rawIndex(i));
    }

    /**
     * Sets the element at position <tt>i</tt> to the given value.
     *
     * @param i the index
     * @param newValue the new value
     */
    public final void set(int i, E newValue) {
        unsafe.putObjectVolatile(array, rawIndex(i), newValue);
    }

    /**
     * Eventually sets the element at position <tt>i</tt> to the given value.
     *
     * @param i the index
     * @param newValue the new value
     * @since 1.6
     */
    public final void lazySet(int i, E newValue) {
        unsafe.putOrderedObject(array, rawIndex(i), newValue);
    }


    /**
     * Atomically sets the element at position <tt>i</tt> to the given
     * value and returns the old value.
     *
     * @param i the index
     * @param newValue the new value
     * @return the previous value
     */
    public final E getAndSet(int i, E newValue) {
        while (true) {
            E current = get(i);
            if (compareAndSet(i, current, newValue))
                return current;
        }
    }

    /**
     * Atomically sets the element at position <tt>i</tt> to the given
     * updated value if the current value <tt>==</tt> the expected value.
     * @param i the index
     * @param expect the expected value
     * @param update the new value
     * @return true if successful. False return indicates that
     * the actual value was not equal to the expected value.
     */
    public final boolean compareAndSet(int i, E expect, E update) {
        return unsafe.compareAndSwapObject(array, rawIndex(i),
                                         expect, update);
    }

    /**
     * Atomically sets the element at position <tt>i</tt> to the given
     * updated value if the current value <tt>==</tt> the expected value.
     * May fail spuriously and does not provide ordering guarantees,
     * so is only rarely an appropriate alternative to <tt>compareAndSet</tt>.
     *
     * @param i the index
     * @param expect the expected value
     * @param update the new value
     * @return true if successful.
     */
    public final boolean weakCompareAndSet(int i, E expect, E update) {
        return compareAndSet(i, expect, update);
    }

    /**
     * Returns the String representation of the current values of array.
     * @return the String representation of the current values of array.
     */
    public String toString() {
        if (array.length > 0) // force volatile read
            get(0);
        return Arrays.toString(array);
    }

}
