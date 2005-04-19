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


package javax.swing.text;

import java.awt.Color;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Toolkit;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.Enumeration;
import java.util.EventListener;
import java.util.Hashtable;

import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.EventListenerList;

public class StyleContext 
    implements Serializable, AbstractDocument.AttributeContext
{
  public class NamedStyle
    implements Serializable, Style
  {
    protected ChangeEvent changeEvent;
    protected EventListenerList listenerList;
      
    AttributeSet attributes;
    String name;

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
      this.name = name;
      this.attributes = getEmptySet();
      this.changeEvent = new ChangeEvent(this);
      this.listenerList = new EventListenerList();
      setResolveParent(parent);
    }

    public String getName()
    {
      return name;
    }

    public void setName(String n)
    {
      name = n;
      fireStateChanged();
    }

    public void addChangeListener(ChangeListener l)
    {
      listenerList.add(ChangeListener.class, l);
    }
      
    public void removeChangeListener(ChangeListener l)
    {
      listenerList.remove(ChangeListener.class, l);
    }
      
    public EventListener[] getListeners(Class listenerType)
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
      return attributes.copyAttributes();
    }
            
    public Object getAttribute(Object attrName)
    {
      return attributes.getAttribute(attrName);
    }

    public int getAttributeCount()
    {
      return attributes.getAttributeCount();
    }

    public Enumeration getAttributeNames()
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

    public void removeAttributes(Enumeration names)
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
        {
          attributes = StyleContext.this.addAttribute
            (attributes, ResolveAttribute, parent);
        }
      fireStateChanged();
    }
      
    public String toString()
    {
      return ("[NamedStyle: name=" + name + ", attrs=" + attributes.toString() + "]");
    }      
  }
  
  public class SmallAttributeSet
    implements AttributeSet
  {
    final Object [] attrs;
    public SmallAttributeSet(AttributeSet a)
    {
      if (a == null)
        attrs = new Object[0];
      else
        {
          int n = a.getAttributeCount();
          int i = 0;
          attrs = new Object[n * 2];
          Enumeration e = a.getAttributeNames();
          while (e.hasMoreElements())
            {
              Object name = e.nextElement();
              attrs[i++] = name;
              attrs[i++] = a.getAttribute(name);
            }
        }
    }

    public SmallAttributeSet(Object [] a)
    {
      if (a == null)
        attrs = new Object[0];
      else
        {
          attrs = new Object[a.length];
          System.arraycopy(a, 0, attrs, 0, a.length);
        }
    }

    public Object clone()
    {
      return new SmallAttributeSet(this.attrs);
    }

    public boolean containsAttribute(Object name, Object value)
    {
      for (int i = 0; i < attrs.length; i += 2)
        {
          if (attrs[i].equals(name) &&
              attrs[i+1].equals(value))
            return true;
        }
      return false;
    }

    public boolean containsAttributes(AttributeSet a)
    {
      Enumeration e = a.getAttributeNames();
      while (e.hasMoreElements())
        {
          Object name = e.nextElement();
          Object val = a.getAttribute(name);
          if (!containsAttribute(name, val))
            return false;
        }
      return true;			
    }

    public AttributeSet copyAttributes()
    {
      return (AttributeSet) clone();
    }

    public boolean equals(Object obj)
    {
      return 
        (obj instanceof SmallAttributeSet)
        && this.isEqual((AttributeSet)obj);
    }
 
    public Object getAttribute(Object key)
    {
      for (int i = 0; i < attrs.length; i += 2)
        {
          if (attrs[i].equals(key))
            return attrs[i+1];
        }
            
      Object p = getResolveParent();
      if (p != null && p instanceof AttributeSet)
        return (((AttributeSet)p).getAttribute(key));
      
      return null;
    }

    public int getAttributeCount()
    {
      return attrs.length / 2;
    }

    public Enumeration getAttributeNames()
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
      return (AttributeSet) getAttribute(ResolveAttribute);
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
      return attr != null 
        && attr.containsAttributes(this)
        && this.containsAttributes(attr);
    }
	
    public String toString()
    {
      StringBuffer sb = new StringBuffer();
      sb.append("[StyleContext.SmallattributeSet:");
      for (int i = 0; i < attrs.length; ++i)
        {
          sb.append(" (");
          sb.append(attrs[i].toString());
          sb.append("=");
          sb.append(attrs[i+1].toString());
          sb.append(")");
        }
      sb.append("]");
      return sb.toString();
    }
  }

  // FIXME: official javadocs suggest that these might be more usefully
  // implemented using a WeakHashMap, but not sure if that works most
  // places or whether it really matters anyways.
  //
  // FIXME: also not sure if these tables ought to be static (singletons),
  // shared across all StyleContexts. I think so, but it's not clear in
  // docs. revert to non-shared if you think it matters.

  public static final String DEFAULT_STYLE = "default";
  
  static Hashtable sharedAttributeSets = new Hashtable();
  static Hashtable sharedFonts = new Hashtable();

  static StyleContext defaultStyleContext = new StyleContext();
  static final int compressionThreshold = 9;
  
  EventListenerList listenerList;
  Hashtable styleTable;
  
  public StyleContext()
  {
    listenerList = new EventListenerList();
    styleTable = new Hashtable();
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
    listenerList.add(ChangeListener.class, listener);
  }

  public void removeChangeListener(ChangeListener listener)
  {
    listenerList.remove(ChangeListener.class, listener);
  }

  public ChangeListener[] getChangeListeners()
  {
    return (ChangeListener[]) listenerList.getListeners(ChangeListener.class);
  }
    
  public Style addStyle(String name, Style parent)
  {
    Style newStyle = new NamedStyle(name, parent);
    if (name != null)
      styleTable.put(name, newStyle);
    return newStyle;
  }

  public void removeStyle(String name)
  {
    styleTable.remove(name);
  }

  public Style getStyle(String name)
  {
    return (Style) styleTable.get(name);
  }

  public Enumeration getStyleNames()
  {
    return styleTable.keys();
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
    return defaultStyleContext;
  }

  public AttributeSet addAttribute(AttributeSet old, Object name, Object value)
  {
    if (old instanceof MutableAttributeSet)
      {
        ((MutableAttributeSet)old).addAttribute(name, value);
        return old;
      }
    else 
      {
        MutableAttributeSet mutable = createLargeAttributeSet(old);
        mutable.addAttribute(name, value);
        if (mutable.getAttributeCount() >= getCompressionThreshold())
          return mutable;
        else
          {
            SmallAttributeSet small = createSmallAttributeSet(mutable);
            if (sharedAttributeSets.containsKey(small))
              small = (SmallAttributeSet) sharedAttributeSets.get(small);
            else
              sharedAttributeSets.put(small,small);
            return small;
          }
      }
  }

  public AttributeSet addAttributes(AttributeSet old, AttributeSet attributes)
  {
    if (old instanceof MutableAttributeSet)
      {
        ((MutableAttributeSet)old).addAttributes(attributes);
        return old;
      }
    else 
      {
        MutableAttributeSet mutable = createLargeAttributeSet(old);
        mutable.addAttributes(attributes);
        if (mutable.getAttributeCount() >= getCompressionThreshold())
          return mutable;
        else
          {
            SmallAttributeSet small = createSmallAttributeSet(mutable);
            if (sharedAttributeSets.containsKey(small))
              small = (SmallAttributeSet) sharedAttributeSets.get(small);
            else
              sharedAttributeSets.put(small,small);
            return small;
          }
      }
  }

  public AttributeSet getEmptySet()
  {
    AttributeSet e = createSmallAttributeSet(null);
    if (sharedAttributeSets.containsKey(e))
      e = (AttributeSet) sharedAttributeSets.get(e);
    else
      sharedAttributeSets.put(e, e);
    return e;
  }

  public void reclaim(AttributeSet attributes)
  {
    if (sharedAttributeSets.containsKey(attributes))
      sharedAttributeSets.remove(attributes);
  }

  public AttributeSet removeAttribute(AttributeSet old, Object name)
  {
    if (old instanceof MutableAttributeSet)
      {
        ((MutableAttributeSet)old).removeAttribute(name);
        if (old.getAttributeCount() < getCompressionThreshold())
          {
            SmallAttributeSet small = createSmallAttributeSet(old);
            if (!sharedAttributeSets.containsKey(small))
              sharedAttributeSets.put(small,small);
            old = (AttributeSet) sharedAttributeSets.get(small);
          }
        return old;
      }
    else 
      {          
        MutableAttributeSet mutable = createLargeAttributeSet(old);
        mutable.removeAttribute(name);
        SmallAttributeSet small = createSmallAttributeSet(mutable);
        if (sharedAttributeSets.containsKey(small))
          small = (SmallAttributeSet) sharedAttributeSets.get(small);
        else
          sharedAttributeSets.put(small,small);
        return small;
      }
  }

  public AttributeSet removeAttributes(AttributeSet old, AttributeSet attributes)
  {
    return removeAttributes(old, attributes.getAttributeNames());
  }

  public AttributeSet removeAttributes(AttributeSet old, Enumeration names)
  {
    if (old instanceof MutableAttributeSet)
      {
        ((MutableAttributeSet)old).removeAttributes(names);
        if (old.getAttributeCount() < getCompressionThreshold())
          {
            SmallAttributeSet small = createSmallAttributeSet(old);
            if (!sharedAttributeSets.containsKey(small))
              sharedAttributeSets.put(small,small);
            old = (AttributeSet) sharedAttributeSets.get(small);
          }
        return old;
      }
    else 
      {          
        MutableAttributeSet mutable = createLargeAttributeSet(old);
        mutable.removeAttributes(names);
        SmallAttributeSet small = createSmallAttributeSet(mutable);
        if (sharedAttributeSets.containsKey(small))
          small = (SmallAttributeSet) sharedAttributeSets.get(small);
        else
          sharedAttributeSets.put(small,small);
        return small;
      }	
  }


  // FIXME: there's some sort of quasi-serialization stuff in here which I
  // have left incomplete; I'm not sure I understand the intent properly.

  public static Object getStaticAttribute(Object key)
  {
    throw new InternalError("not implemented");
  }
  
  public static Object getStaticAttributeKey(Object key)
  {
    throw new InternalError("not implemented");
  }

  public static void readAttributeSet(ObjectInputStream in, MutableAttributeSet a)
    throws ClassNotFoundException, IOException
  {
    throw new InternalError("not implemented");
  }
  
  public static void writeAttributeSet(ObjectOutputStream out, AttributeSet a)
    throws IOException
  {
    throw new InternalError("not implemented");
  }

  public void readAttributes(ObjectInputStream in, MutableAttributeSet a)
    throws ClassNotFoundException, IOException 
  {
    throw new InternalError("not implemented");
  }

  public void writeAttributes(ObjectOutputStream out, AttributeSet a)
    throws IOException
  {
    throw new InternalError("not implemented");
  }
}
