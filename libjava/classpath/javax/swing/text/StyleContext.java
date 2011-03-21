/* StyleContext.java --
   Copyright (C) 2004 Free Software Foundation, Inc.

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


package javax.swing.text;

import java.awt.Color;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Toolkit;
import java.io.IOException;
import java.io.NotSerializableException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.lang.ref.WeakReference;
import java.util.Collections;
import java.util.Enumeration;
import java.util.EventListener;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Map;
import java.util.WeakHashMap;

import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.EventListenerList;

public class StyleContext
  implements Serializable, AbstractDocument.AttributeContext
{
  /** The serialization UID (compatible with JDK1.5). */
  private static final long serialVersionUID = 8042858831190784241L;

  public class NamedStyle
    implements Serializable, Style
  {
    /** The serialization UID (compatible with JDK1.5). */
    private static final long serialVersionUID = -6690628971806226374L;

    protected transient ChangeEvent changeEvent;
    protected EventListenerList listenerList;

    private transient AttributeSet attributes;

    public NamedStyle()
    {
      this(null, null);
    }

    public NamedStyle(Style parent)
    {
      this(null, parent);
    }

    public NamedStyle(String name, Style parent)
    {
      attributes = getEmptySet();
      listenerList = new EventListenerList();
      if (name != null)
        setName(name);
      if (parent != null)
        setResolveParent(parent);
    }

    public String getName()
    {
      String name = null;
      if (isDefined(StyleConstants.NameAttribute))
        name = getAttribute(StyleConstants.NameAttribute).toString();
      return name;
    }

    public void setName(String n)
    {
      if (n != null)
        addAttribute(StyleConstants.NameAttribute, n);
    }

    public void addChangeListener(ChangeListener l)
    {
      listenerList.add(ChangeListener.class, l);
    }

    public void removeChangeListener(ChangeListener l)
    {
      listenerList.remove(ChangeListener.class, l);
    }

    public <T extends EventListener> T[] getListeners(Class<T> listenerType)
    {
      return listenerList.getListeners(listenerType);
    }

    public ChangeListener[] getChangeListeners()
    {
      return (ChangeListener[]) getListeners(ChangeListener.class);
    }

    protected  void fireStateChanged()
    {
      ChangeListener[] listeners = getChangeListeners();
      for (int i = 0; i < listeners.length; ++i)
        {
          // Lazily create event.
          if (changeEvent == null)
            changeEvent = new ChangeEvent(this);
          listeners[i].stateChanged(changeEvent);
        }
    }

    public void addAttribute(Object name, Object value)
    {
      attributes = StyleContext.this.addAttribute(attributes, name, value);
      fireStateChanged();
    }

    public void addAttributes(AttributeSet attr)
    {
      attributes = StyleContext.this.addAttributes(attributes, attr);
      fireStateChanged();
    }

    public boolean containsAttribute(Object name, Object value)
    {
      return attributes.containsAttribute(name, value);
    }

    public boolean containsAttributes(AttributeSet attrs)
    {
      return attributes.containsAttributes(attrs);
    }

    public AttributeSet copyAttributes()
    {
      // The RI returns a NamedStyle as copy, so do we.
      NamedStyle copy = new NamedStyle();
      copy.attributes = attributes.copyAttributes();
      return copy;
    }

    public Object getAttribute(Object attrName)
    {
      return attributes.getAttribute(attrName);
    }

    public int getAttributeCount()
    {
      return attributes.getAttributeCount();
    }

    public Enumeration<?> getAttributeNames()
    {
      return attributes.getAttributeNames();
    }

    public boolean isDefined(Object attrName)
    {
      return attributes.isDefined(attrName);
    }

    public boolean isEqual(AttributeSet attr)
    {
      return attributes.isEqual(attr);
    }

    public void removeAttribute(Object name)
    {
      attributes = StyleContext.this.removeAttribute(attributes, name);
      fireStateChanged();
    }

    public void removeAttributes(AttributeSet attrs)
    {
      attributes = StyleContext.this.removeAttributes(attributes, attrs);
      fireStateChanged();
    }

    public void removeAttributes(Enumeration<?> names)
    {
      attributes = StyleContext.this.removeAttributes(attributes, names);
      fireStateChanged();
    }


    public AttributeSet getResolveParent()
    {
      return attributes.getResolveParent();
    }

    public void setResolveParent(AttributeSet parent)
    {
      if (parent != null)
        addAttribute(StyleConstants.ResolveAttribute, parent);
      else
        removeAttribute(StyleConstants.ResolveAttribute);
    }

    public String toString()
    {
      return "NamedStyle:" + getName() + " " + attributes;
    }

    private void writeObject(ObjectOutputStream s)
      throws IOException
    {
      s.defaultWriteObject();
      writeAttributeSet(s, attributes);
    }

    private void readObject(ObjectInputStream s)
      throws ClassNotFoundException, IOException
    {
      s.defaultReadObject();
      attributes = SimpleAttributeSet.EMPTY;
      readAttributeSet(s, this);
    }
  }

  public class SmallAttributeSet
    implements AttributeSet
  {
    final Object [] attrs;
    private AttributeSet resolveParent;
    public SmallAttributeSet(AttributeSet a)
    {
      int n = a.getAttributeCount();
      int i = 0;
      attrs = new Object[n * 2];
      Enumeration e = a.getAttributeNames();
      while (e.hasMoreElements())
        {
          Object name = e.nextElement();
          Object value = a.getAttribute(name);
          if (name == ResolveAttribute)
            resolveParent = (AttributeSet) value;
          attrs[i++] = name;
          attrs[i++] = value;
        }
    }

    public SmallAttributeSet(Object [] a)
    {
      attrs = a;
      for (int i = 0; i < attrs.length; i += 2)
        {
          if (attrs[i] == ResolveAttribute)
            resolveParent = (AttributeSet) attrs[i + 1];
        }
    }

    public Object clone()
    {
      return this;
    }

    public boolean containsAttribute(Object name, Object value)
    {
      return value.equals(getAttribute(name));
    }

    public boolean containsAttributes(AttributeSet a)
    {
      boolean res = true;
      Enumeration e = a.getAttributeNames();
      while (e.hasMoreElements() && res)
        {
          Object name = e.nextElement();
          res = a.getAttribute(name).equals(getAttribute(name));
        }
      return res;
    }

    public AttributeSet copyAttributes()
    {
      return this;
    }

    public boolean equals(Object obj)
    {
      boolean eq = false;
      if (obj instanceof AttributeSet)
        {
          AttributeSet atts = (AttributeSet) obj;
          eq = getAttributeCount() == atts.getAttributeCount()
               && containsAttributes(atts);
        }
      return eq;
    }

    public Object getAttribute(Object key)
    {
      Object att = null;
      if (key == StyleConstants.ResolveAttribute)
        att = resolveParent;

      for (int i = 0; i < attrs.length && att == null; i += 2)
        {
          if (attrs[i].equals(key))
            att = attrs[i + 1];
        }

      // Check the resolve parent, unless we're looking for the
      // ResolveAttribute, which must not be looked up
      if (att == null)
          {
            AttributeSet parent = getResolveParent();
            if (parent != null)
              att = parent.getAttribute(key);
          }

      return att;
    }

    public int getAttributeCount()
    {
      return attrs.length / 2;
    }

    public Enumeration<?> getAttributeNames()
    {
      return new Enumeration()
        {
          int i = 0;
          public boolean hasMoreElements()
          {
            return i < attrs.length;
          }
          public Object nextElement()
          {
            i += 2;
            return attrs[i-2];
          }
        };
    }

    public AttributeSet getResolveParent()
    {
      return resolveParent;
    }

    public int hashCode()
    {
      return java.util.Arrays.asList(attrs).hashCode();
    }

    public boolean isDefined(Object key)
    {
      for (int i = 0; i < attrs.length; i += 2)
        {
          if (attrs[i].equals(key))
            return true;
        }
      return false;
    }

    public boolean isEqual(AttributeSet attr)
    {
      boolean eq;
      // If the other one is also a SmallAttributeSet, it is only considered
      // equal if it's the same instance.
      if (attr instanceof SmallAttributeSet)
        eq = attr == this;
      else
        eq = getAttributeCount() == attr.getAttributeCount()
             && this.containsAttributes(attr);
      return eq;
    }

    public String toString()
    {
      StringBuilder sb = new StringBuilder();
      sb.append('{');
      for (int i = 0; i < attrs.length; i += 2)
        {
          if (attrs[i + 1] instanceof AttributeSet)
            {
              sb.append(attrs[i]);
              sb.append("=AttributeSet,");
            }
          else
            {
              sb.append(attrs[i]);
              sb.append('=');
              sb.append(attrs[i + 1]);
              sb.append(',');
            }
        }
      sb.append("}");
      return sb.toString();
    }
  }

  /**
   * Register StyleConstant keys as static attribute keys for serialization.
   */
  static
  {
    // Don't let problems while doing this prevent class loading.
    try
      {
        for (Iterator i = StyleConstants.keys.iterator(); i.hasNext();)
          registerStaticAttributeKey(i.next());
      }
    catch (Throwable t)
      {
        t.printStackTrace();
      }
  }

  /**
   * The name of the default style.
   */
  public static final String DEFAULT_STYLE = "default";

  static Hashtable sharedAttributeSets = new Hashtable();
  static Hashtable sharedFonts = new Hashtable();

  static StyleContext defaultStyleContext;
  static final int compressionThreshold = 9;

  /**
   * These attribute keys are handled specially in serialization.
   */
  private static Hashtable writeAttributeKeys;
  private static Hashtable readAttributeKeys;

  private NamedStyle styles;

  /**
   * Used for searching attributes in the pool.
   */
  private transient MutableAttributeSet search = new SimpleAttributeSet();

  /**
   * A pool of immutable AttributeSets.
   */
  private transient Map attributeSetPool =
    Collections.synchronizedMap(new WeakHashMap());

  /**
   * Creates a new instance of the style context. Add the default style
   * to the style table.
   */
  public StyleContext()
  {
    styles = new NamedStyle(null);
    addStyle(DEFAULT_STYLE, null);
  }

  protected SmallAttributeSet createSmallAttributeSet(AttributeSet a)
  {
    return new SmallAttributeSet(a);
  }

  protected MutableAttributeSet createLargeAttributeSet(AttributeSet a)
  {
    return new SimpleAttributeSet(a);
  }

  public void addChangeListener(ChangeListener listener)
  {
    styles.addChangeListener(listener);
  }

  public void removeChangeListener(ChangeListener listener)
  {
    styles.removeChangeListener(listener);
  }

  public ChangeListener[] getChangeListeners()
  {
    return styles.getChangeListeners();
  }

  public Style addStyle(String name, Style parent)
  {
    Style newStyle = new NamedStyle(name, parent);
    if (name != null)
      styles.addAttribute(name, newStyle);
    return newStyle;
  }

  public void removeStyle(String name)
  {
    styles.removeAttribute(name);
  }

  /**
   * Get the style from the style table. If the passed name
   * matches {@link #DEFAULT_STYLE}, returns the default style.
   * Otherwise returns the previously defined style of
   * <code>null</code> if the style with the given name is not defined.
   *
   * @param name the name of the style.
   *
   * @return the style with the given name or null if no such defined.
   */
  public Style getStyle(String name)
  {
    return (Style) styles.getAttribute(name);
  }

  /**
   * Get the names of the style. The returned enumeration always
   * contains at least one member, the default style.
   */
  public Enumeration<?> getStyleNames()
  {
    return styles.getAttributeNames();
  }

  private void readObject(ObjectInputStream in)
    throws ClassNotFoundException, IOException
  {
    search = new SimpleAttributeSet();
    attributeSetPool = Collections.synchronizedMap(new WeakHashMap());
    in.defaultReadObject();
  }

  private void writeObject(ObjectOutputStream out)
    throws IOException
  {
    cleanupPool();
    out.defaultWriteObject();
  }

  //
  // StyleContexts only understand the "simple" model of fonts present in
  // pre-java2d systems: fonts are a family name, a size (integral number
  // of points), and a mask of style parameters (plain, bold, italic, or
  // bold|italic). We have an inner class here called SimpleFontSpec which
  // holds such triples.
  //
  // A SimpleFontSpec can be built for *any* AttributeSet because the size,
  // family, and style keys in an AttributeSet have default values (defined
  // over in StyleConstants).
  //
  // We keep a static cache mapping SimpleFontSpecs to java.awt.Fonts, so
  // that we reuse Fonts between styles and style contexts.
  //

  private static class SimpleFontSpec
  {
    String family;
    int style;
    int size;
    public SimpleFontSpec(String family,
                          int style,
                          int size)
    {
      this.family = family;
      this.style = style;
      this.size = size;
    }
    public boolean equals(Object obj)
    {
      return (obj != null)
        && (obj instanceof SimpleFontSpec)
        && (((SimpleFontSpec)obj).family.equals(this.family))
        && (((SimpleFontSpec)obj).style == this.style)
        && (((SimpleFontSpec)obj).size == this.size);
    }
    public int hashCode()
    {
      return family.hashCode() + style + size;
    }
  }

  public Font getFont(AttributeSet attr)
  {
    String family = StyleConstants.getFontFamily(attr);
    int style = Font.PLAIN;
    if (StyleConstants.isBold(attr))
      style += Font.BOLD;
    if (StyleConstants.isItalic(attr))
      style += Font.ITALIC;
    int size = StyleConstants.getFontSize(attr);
    return getFont(family, style, size);
  }

  public Font getFont(String family, int style, int size)
  {
    SimpleFontSpec spec = new SimpleFontSpec(family, style, size);
    if (sharedFonts.containsKey(spec))
      return (Font) sharedFonts.get(spec);
    else
      {
        Font tmp = new Font(family, style, size);
        sharedFonts.put(spec, tmp);
        return tmp;
      }
  }

  public FontMetrics getFontMetrics(Font f)
  {
    return Toolkit.getDefaultToolkit().getFontMetrics(f);
  }

  public Color getForeground(AttributeSet a)
  {
    return StyleConstants.getForeground(a);
  }

  public Color getBackground(AttributeSet a)
  {
    return StyleConstants.getBackground(a);
  }

  protected int getCompressionThreshold()
  {
    return compressionThreshold;
  }

  public static StyleContext getDefaultStyleContext()
  {
    if (defaultStyleContext == null)
      defaultStyleContext = new StyleContext();
    return defaultStyleContext;
  }

  public synchronized AttributeSet addAttribute(AttributeSet old, Object name,
                                                Object value)
  {
    AttributeSet ret;
    if (old.getAttributeCount() + 1 < getCompressionThreshold())
      {
        search.removeAttributes(search);
        search.addAttributes(old);
        search.addAttribute(name, value);
        reclaim(old);
        ret = searchImmutableSet();
      }
    else
      {
        MutableAttributeSet mas = getMutableAttributeSet(old);
        mas.addAttribute(name, value);
        ret = mas;
      }
    return ret;
  }

  public synchronized AttributeSet addAttributes(AttributeSet old,
                                                 AttributeSet attributes)
  {
    AttributeSet ret;
    if (old.getAttributeCount() + attributes.getAttributeCount()
        < getCompressionThreshold())
      {
        search.removeAttributes(search);
        search.addAttributes(old);
        search.addAttributes(attributes);
        reclaim(old);
        ret = searchImmutableSet();
      }
    else
      {
        MutableAttributeSet mas = getMutableAttributeSet(old);
        mas.addAttributes(attributes);
        ret = mas;
      }
    return ret;
  }

  public AttributeSet getEmptySet()
  {
    return SimpleAttributeSet.EMPTY;
  }

  public void reclaim(AttributeSet attributes)
  {
    cleanupPool();
  }

  public synchronized AttributeSet removeAttribute(AttributeSet old,
                                                   Object name)
  {
    AttributeSet ret;
    if (old.getAttributeCount() - 1 <= getCompressionThreshold())
      {
        search.removeAttributes(search);
        search.addAttributes(old);
        search.removeAttribute(name);
        reclaim(old);
        ret = searchImmutableSet();
      }
    else
      {
        MutableAttributeSet mas = getMutableAttributeSet(old);
        mas.removeAttribute(name);
        ret = mas;
      }
    return ret;
  }

  public synchronized AttributeSet removeAttributes(AttributeSet old,
                                                    AttributeSet attributes)
  {
    AttributeSet ret;
    if (old.getAttributeCount() <= getCompressionThreshold())
      {
        search.removeAttributes(search);
        search.addAttributes(old);
        search.removeAttributes(attributes);
        reclaim(old);
        ret = searchImmutableSet();
      }
    else
      {
        MutableAttributeSet mas = getMutableAttributeSet(old);
        mas.removeAttributes(attributes);
        ret = mas;
      }
    return ret;
  }

  public synchronized AttributeSet removeAttributes(AttributeSet old,
                                                    Enumeration<?> names)
  {
    AttributeSet ret;
    if (old.getAttributeCount() <= getCompressionThreshold())
      {
        search.removeAttributes(search);
        search.addAttributes(old);
        search.removeAttributes(names);
        reclaim(old);
        ret = searchImmutableSet();
      }
    else
      {
        MutableAttributeSet mas = getMutableAttributeSet(old);
        mas.removeAttributes(names);
        ret = mas;
      }
    return ret;
  }

  /**
   * Gets the object previously registered with registerStaticAttributeKey.
   *
   * @param key - the key that was registered.
   * @return the object previously registered with registerStaticAttributeKey.
   */
  public static Object getStaticAttribute(Object key)
  {
    if (key == null)
      return null;
    return readAttributeKeys.get(key);
  }

  /**
   * Returns the String that key will be registered with
   * registerStaticAttributeKey.
   *
   * @param key - the key that will be registered.
   * @return the string the key will be registered with.
   */
  public static Object getStaticAttributeKey(Object key)
  {
    return key.getClass().getName() + "." + key.toString();
  }

  /**
   * Reads a set of attributes from the given object input stream. This will
   * attempt to restore keys that were static objects by considering only the
   * keys that have were registered with registerStaticAttributeKey. The
   * attributes retrieved will be placed into the given set.
   *
   * @param in - the stream to read from
   * @param a - the set of attributes
   * @throws ClassNotFoundException - may be encountered when reading from
   *           stream
   * @throws IOException - any I/O error
   */
  public static void readAttributeSet(ObjectInputStream in,
                                      MutableAttributeSet a)
    throws ClassNotFoundException, IOException
  {
    int count = in.readInt();
    for (int i = 0; i < count; i++)
      {
        Object key = in.readObject();
        Object val = in.readObject();
        if (readAttributeKeys != null)
          {
            Object staticKey = readAttributeKeys.get(key);
            if (staticKey != null)
              key = staticKey;
            Object staticVal = readAttributeKeys.get(val);
            if (staticVal != null)
              val = staticVal;
          }
        a.addAttribute(key, val);
      }
  }

  /**
   * Serialize an attribute set in a way that is compatible with it
   * being read in again by {@link #readAttributeSet(ObjectInputStream, MutableAttributeSet)}.
   * In particular registered static keys are transformed properly.
   *
   * @param out - stream to write to
   * @param a - the attribute set
   * @throws IOException - any I/O error
   */
  public static void writeAttributeSet(ObjectOutputStream out, AttributeSet a)
    throws IOException
  {
    int count = a.getAttributeCount();
    out.writeInt(count);
    Enumeration e = a.getAttributeNames();
    while (e.hasMoreElements())
      {
        Object key = e.nextElement();
        // Write key.
        if (key instanceof Serializable)
          out.writeObject(key);
        else
          {
            Object io = writeAttributeKeys.get(key);
            if (io == null)
              throw new NotSerializableException(key.getClass().getName()
                                                 + ", key: " + key);
            out.writeObject(io);
          }
        // Write value.
        Object val = a.getAttribute(key);
        Object io = writeAttributeKeys.get(val);
        if (val instanceof Serializable)
          out.writeObject(io != null ? io : val);
        else
          {
            if (io == null)
              throw new NotSerializableException(val.getClass().getName());
            out.writeObject(io);
          }
      }
  }

  /**
   * Handles reading in the attributes.
   * @see #readAttributeSet(ObjectInputStream, MutableAttributeSet)
   *
   * @param in - the stream to read from
   * @param a - the set of attributes
   * @throws ClassNotFoundException - may be encountered when reading from stream
   * @throws IOException - any I/O error
   */
  public void readAttributes(ObjectInputStream in, MutableAttributeSet a)
    throws ClassNotFoundException, IOException
  {
    readAttributeSet(in, a);
  }

  /**
   * Handles writing of the given attributes.
   * @see #writeAttributeSet(ObjectOutputStream, AttributeSet)
   *
   * @param out - stream to write to
   * @param a - the attribute set
   * @throws IOException - any I/O error
   */
  public void writeAttributes(ObjectOutputStream out, AttributeSet a)
    throws IOException
  {
    writeAttributeSet(out, a);
  }

  /**
   * Registers an attribute key as a well-known keys. When an attribute with
   * such a key is written to a stream, a special syntax is used so that it
   * can be recognized when it is read back in. All attribute keys defined
   * in <code>StyleContext</code> are registered as static keys. If you define
   * additional attribute keys that you want to exist as nonreplicated objects,
   * then you should register them using this method.
   *
   * @param key the key to register as static attribute key
   */
  public static void registerStaticAttributeKey(Object key)
  {
    String io = key.getClass().getName() + "." + key.toString();
    if (writeAttributeKeys == null)
      writeAttributeKeys = new Hashtable();
    if (readAttributeKeys == null)
      readAttributeKeys = new Hashtable();
    writeAttributeKeys.put(key, io);
    readAttributeKeys.put(io, key);
  }

  /**
   * Returns a string representation of this StyleContext.
   *
   * @return a string representation of this StyleContext
   */
  public String toString()
  {
    cleanupPool();
    StringBuilder b = new StringBuilder();
    Iterator i = attributeSetPool.keySet().iterator();
    while (i.hasNext())
      {
        Object att = i.next();
        b.append(att);
        b.append('\n');
      }
    return b.toString();
  }

  /**
   * Searches the AttributeSet pool and returns a pooled instance if available,
   * or pool a new one.
   *
   * @return an immutable attribute set that equals the current search key
   */
  private AttributeSet searchImmutableSet()
  {
    SmallAttributeSet k = createSmallAttributeSet(search);
    WeakReference ref = (WeakReference) attributeSetPool.get(k);
    SmallAttributeSet a;
    if (ref == null || (a = (SmallAttributeSet) ref.get()) == null)
      {
        a = k;
        attributeSetPool.put(a, new WeakReference(a));
      }
    return a;
  }

  /**
   * Cleans up the attribute set pool from entries that are no longer
   * referenced.
   */
  private void cleanupPool()
  {
    // TODO: How else can we force cleaning up the WeakHashMap?
    attributeSetPool.size();
  }

  /**
   * Returns a MutableAttributeSet that holds a. If a itself is mutable,
   * this returns a itself, otherwise it creates a new SimpleAtttributeSet
   * via {@link #createLargeAttributeSet(AttributeSet)}.
   *
   * @param a the AttributeSet to create a mutable set for
   *
   * @return a mutable attribute set that corresponds to a
   */
  private MutableAttributeSet getMutableAttributeSet(AttributeSet a)
  {
    MutableAttributeSet mas;
    if (a instanceof MutableAttributeSet)
      mas = (MutableAttributeSet) a;
    else
      mas = createLargeAttributeSet(a);
    return mas;
  }
}
