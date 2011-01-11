package gnu.javax.net.ssl.provider;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.ConcurrentModificationException;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.NoSuchElementException;

/**
 * A list of extensions, that may appear in either the {@link ClientHello} or
 * {@link ServerHello}. The form of the extensions list is:
 *
 * <tt>   Extension extensions_list&lt;1..2^16-1&gt;</tt>
 *
 * @author csm
 */
public class ExtensionList implements Builder, Iterable<Extension>
{
  private final ByteBuffer buffer;
  private int modCount;

  public ExtensionList (ByteBuffer buffer)
  {
    this.buffer = buffer.duplicate().order(ByteOrder.BIG_ENDIAN);
    modCount = 0;
  }

  public ExtensionList(List<Extension> extensions)
  {
    int length = 2;
    for (Extension extension : extensions)
      length += extension.length();
    buffer = ByteBuffer.allocate(length);
    buffer.putShort((short) (length - 2));
    for (Extension extension : extensions)
      buffer.put(extension.buffer());
    buffer.rewind();
  }

  public ByteBuffer buffer()
  {
    return (ByteBuffer) buffer.duplicate().limit(length());
  }

  public Extension get (final int index)
  {
    int length = length ();
    int i;
    int n = 0;
    for (i = 2; i < length && n < index; )
      {
        int l = buffer.getShort (i+2) & 0xFFFF;
        i += l + 4;
        n++;
      }
    if (n < index)
      throw new IndexOutOfBoundsException ("no elemenet at " + index);
    int el = buffer.getShort (i+2) & 0xFFFF;
    ByteBuffer b = (ByteBuffer) buffer.duplicate().position(i).limit(i+el+4);
    return new Extension(b.slice());
  }

  /**
   * Returns the number of extensions this list contains.
   *
   * @return The number of extensions.
   */
  public int size ()
  {
    int length = length ();
    if (length == 0)
      return 0;
    int n = 0;
    for (int i = 2; i < length; )
      {
        int len = buffer.getShort (i+2) & 0xFFFF;
        i += len + 4;
        n++;
      }
    return n;
  }

  /**
   * Returns the length of this extension list, in bytes.
   *
   * @return The length of this extension list, in bytes.
   */
  public int length ()
  {
    return (buffer.getShort (0) & 0xFFFF) + 2;
  }

  /**
   * Sets the extension at index <i>i</i> to <i>e</i>. Note that setting an
   * element at an index <b>may</b> invalidate any other elements that come
   * after element at index <i>i</i>. In other words, no attempt is made to
   * move existing elements in this list, and since extensions are variable
   * length, you can <em>not</em> guarantee that extensions later in the list
   * will still be valid.
   *
   * <p>Thus, elements of this list <b>must</b> be set in order of increasing
   * index.
   *
   * @param index The index to set the extension at.
   * @param e The extension.
   * @throws java.nio.BufferOverflowException If setting the extension overflows
   *  the buffer.
   * @throws IllegalArgumentException If it isn't possible to find the given index
   *  in the current list (say, if no element index - 1 is set), or if setting
   *  the extension will overflow the current list length (given by {@link
   *  #length()}).
   */
  public void set (final int index, Extension e)
  {
    int length = length();
    int n = 0;
    int i;
    for (i = 2; i < length && n < index; )
      {
        int len = buffer.getShort(i+2) & 0xFFFF;
        i += len + 4;
        n++;
      }
    if (n < index)
      throw new IllegalArgumentException("nothing set at index " + (index-1)
                                         + " or insufficient space");
    if (i + e.length() + 2 > length)
      throw new IllegalArgumentException("adding this element will exceed the "
                                         + "list length");
    buffer.putShort(i, (short) e.type().getValue());
    buffer.putShort(i+2, (short) e.length());
    ((ByteBuffer) buffer.duplicate().position(i+4)).put (e.valueBuffer());
    modCount++;
  }

  /**
   * Reserve space for an extension at index <i>i</i> in the list. In other
   * words, this does the job of {@link #set(int, Extension)}, but does not
   * copy the extension value to the underlying buffer.
   *
   * @param index The index of the extension to reserve space for.
   * @param t The type of the extension.
   * @param eLength The number of bytes to reserve for this extension. The total
   *  number of bytes used by this method is this length, plus four.
   */
  public void set (final int index, Extension.Type t, final int eLength)
  {
    int length = length ();
    int n = 0;
    int i;
    for (i = 2; i < length && n < index; )
      {
        int len = buffer.getShort (i+2) & 0xFFFF;
        i += len + 4;
        n++;
      }
    if (n < index)
      throw new IllegalArgumentException ("nothing set at index " + (index-1)
                                          + " or insufficient space");
    if (i + eLength + 2 > length)
      throw new IllegalArgumentException ("adding this element will exceed the "
                                          + "list length");
    buffer.putShort(i, (short) t.getValue());
    buffer.putShort(i+2, (short) eLength);
    modCount++;
  }

  /**
   * Set the total length of this list, in bytes.
   *
   * @param newLength The new list length.
   */
  public void setLength (final int newLength)
  {
    if (newLength < 0 || newLength > 65535)
      throw new IllegalArgumentException ("invalid length");
    buffer.putShort (0, (short) newLength);
    modCount++;
  }

  public Iterator<Extension> iterator()
  {
    return new ExtensionsIterator();
  }

  public String toString()
  {
    return toString (null);
  }

  public String toString(final String prefix)
  {
    StringWriter str = new StringWriter();
    PrintWriter out = new PrintWriter(str);
    if (prefix != null) out.print(prefix);
    out.println("ExtensionList {");
    if (prefix != null) out.print(prefix);
    out.print("  length = ");
    out.print(length());
    out.println(";");
    String subprefix = "  ";
    if (prefix != null)
      subprefix = prefix + subprefix;
    for (Extension e : this)
      out.println(e.toString(subprefix));
    if (prefix != null) out.print(prefix);
    out.print("};");
    return str.toString();
  }

  /**
   * List iterator interface to an extensions list.
   *
   * @author csm@gnu.org
   */
  public final class ExtensionsIterator implements ListIterator<Extension>
  {
    private final int modCount;
    private int index;
    private final int size;

    public ExtensionsIterator ()
    {
      this.modCount = ExtensionList.this.modCount;
      index = 0;
      size = size ();
    }

    public boolean hasNext()
    {
      return index < size;
    }

    public boolean hasPrevious()
    {
      return index > 0;
    }

    public Extension next() throws NoSuchElementException
    {
      if (modCount != ExtensionList.this.modCount)
        throw new ConcurrentModificationException ();
      if (!hasNext ())
        throw new NoSuchElementException ();
      return get (index++);
    }

    public Extension previous() throws NoSuchElementException
    {
      if (modCount != ExtensionList.this.modCount)
        throw new ConcurrentModificationException ();
      if (!hasPrevious ())
        throw new NoSuchElementException ();
      return get (--index);
    }

    public int nextIndex()
    {
      if (hasNext ())
        return index + 1;
      return index;
    }

    public int previousIndex()
    {
      if (hasPrevious ())
        return index - 1;
      return -1;
    }

    public void add(Extension e)
    {
      throw new UnsupportedOperationException ("cannot add items to this iterator");
    }

    public void remove()
    {
      throw new UnsupportedOperationException ("cannot remove items from this iterator");
    }

    public void set(Extension e)
    {
      ExtensionList.this.set (index, e);
    }
  }
}
