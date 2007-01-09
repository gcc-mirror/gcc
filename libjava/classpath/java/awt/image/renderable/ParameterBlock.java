/* ParameterBlock.java -- 
   Copyright (C) 2002 Free Software Foundation, Inc.

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


package java.awt.image.renderable;

import java.awt.image.RenderedImage;
import java.io.Serializable;
import java.util.Vector;

public class ParameterBlock implements Cloneable, Serializable
{
  private static final long serialVersionUID = -7577115551785240750L;
  protected Vector<Object> sources;
  protected Vector<Object> parameters;

  public ParameterBlock()
  {
    this(new Vector<Object>(), new Vector<Object>());
  }

  public ParameterBlock(Vector<Object> sources)
  {
    this(sources, new Vector<Object>());
  }

  public ParameterBlock(Vector<Object> sources, Vector<Object> parameters)
  {
    this.sources = sources;
    this.parameters = parameters;
  }

  public Object shallowClone()
  {
    try
      {
        return super.clone();
      }
    catch (CloneNotSupportedException e)
      {
        throw (Error) new InternalError().initCause(e); // impossible
      }
  }

  public Object clone()
  {
    ParameterBlock pb = (ParameterBlock) shallowClone();
    if (sources != null)
      pb.sources = (Vector<Object>) sources.clone();
    if (parameters != null)
      pb.parameters = (Vector<Object>) parameters.clone();
    return pb;
  }

  public ParameterBlock addSource(Object source)
  {
    sources.add(source);
    return this;
  }

  public Object getSource(int index)
  {
    return sources.get(index);
  }

  public ParameterBlock setSource(Object source, int index)
  {
    sources.ensureCapacity(index);
    sources.set(index, source);
    return this;
  }

  public RenderedImage getRenderedSource(int index)
  {
    return (RenderedImage) sources.get(index);
  }

  public RenderableImage getRenderableSource(int index)
  {
    return (RenderableImage) sources.get(index);
  }

  public int getNumSources()
  {
    return sources.size();
  }

  public Vector<Object> getSources()
  {
    return sources;
  }

  public void setSources(Vector<Object> sources)
  {
    this.sources = sources;
  }

  public void removeSources()
  {
    if (sources != null)
      sources.clear();
  }

  public int getNumParameters()
  {
    return parameters.size();
  }

  public Vector<Object> getParameters()
  {
    return parameters;
  }

  public void setParameters(Vector<Object> parameters)
  {
    this.parameters = parameters;
  }

  public void removeParameters()
  {
    if (parameters != null)
      parameters.clear();
  }

  public ParameterBlock add(Object o)
  {
    parameters.add(o);
    return this;
  }

  public ParameterBlock add(byte b)
  {
    return add(new Byte(b));
  }

  public ParameterBlock add(char c)
  {
    return add(new Character(c));
  }

  public ParameterBlock add(short s)
  {
    return add(new Short(s));
  }

  public ParameterBlock add(int i)
  {
    return add(new Integer(i));
  }

  public ParameterBlock add(long l)
  {
    return add(new Long(l));
  }

  public ParameterBlock add(float f)
  {
    return add(new Float(f));
  }

  public ParameterBlock add(double d)
  {
    return add(new Double(d));
  }

  public ParameterBlock set(Object o, int index)
  {
    parameters.ensureCapacity(index);
    parameters.set(index, o);
    return this;
  }

  public ParameterBlock set(byte b, int index)
  {
    return set(new Byte(b), index);
  }

  public ParameterBlock set(char c, int index)
  {
    return set(new Character(c), index);
  }

  public ParameterBlock set(short s, int index)
  {
    return set(new Short(s), index);
  }

  public ParameterBlock set(int i, int index)
  {
    return set(new Integer(i), index);
  }

  public ParameterBlock set(long l, int index)
  {
    return set(new Long(l), index);
  }

  public ParameterBlock set(float f, int index)
  {
    return set(new Float(f), index);
  }

  public ParameterBlock set(double d, int index)
  {
    return set(new Double(d), index);
  }

  public Object getObjectParameter(int index)
  {
    return parameters.get(index);
  }

  public byte getByteParameter(int index)
  {
    return ((Byte) parameters.get(index)).byteValue();
  }

  public char getCharParameter(int index)
  {
    return ((Character) parameters.get(index)).charValue();
  }

  public short getShortParameter(int index)
  {
    return ((Short) parameters.get(index)).shortValue();
  }

  public int getIntParameter(int index)
  {
    return ((Integer) parameters.get(index)).intValue();
  }

  public long getLongParameter(int index)
  {
    return ((Long) parameters.get(index)).longValue();
  }

  public float getFloatParameter(int index)
  {
    return ((Float) parameters.get(index)).floatValue();
  }

  public double getDoubleParameter(int index)
  {
    return ((Double) parameters.get(index)).doubleValue();
  }

  public Class[] getParamClasses()
  {
    int i = parameters.size();
    Class[] result = new Class[i];
    while (--i >= 0)
      {
        Class c = parameters.get(i).getClass();
        if (c == Byte.class)
          result[i] = byte.class;
        else if (c == Character.class)
          result[i] = char.class;
        else if (c == Short.class)
          result[i] = short.class;
        else if (c == Integer.class)
          result[i] = int.class;
        else if (c == Long.class)
          result[i] = long.class;
        else if (c == Float.class)
          result[i] = float.class;
        else if (c == Double.class)
          result[i] = double.class;
        else
          result[i] = c;
      }
    return result;
  }
} // class ParameterBlock
