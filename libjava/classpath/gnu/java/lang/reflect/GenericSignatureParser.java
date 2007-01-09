/* GenericSignatureParser.java
   Copyright (C) 2005
   Free Software Foundation

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

package gnu.java.lang.reflect;

import java.lang.reflect.*;
import java.util.ArrayList;
import java.util.Arrays;

final class TypeVariableImpl extends TypeImpl implements TypeVariable
{
    private GenericDeclaration decl;
    private Type[] bounds;
    private String name;

    TypeVariableImpl(GenericDeclaration decl, Type[] bounds, String name)
    {
        this.decl = decl;
        this.bounds = bounds;
        this.name = name;
    }

    Type resolve()
    {
        return this;
    }

    public Type[] getBounds()
    {
        resolve(bounds);
        return (Type[]) bounds.clone();
    }

    public GenericDeclaration getGenericDeclaration()
    {
        return decl;
    }

    public String getName()
    {
        return name;
    }

    public boolean equals(Object obj)
    {
        if (obj instanceof TypeVariableImpl)
        {
            TypeVariableImpl other = (TypeVariableImpl)obj;
            return decl.equals(other.decl) && name.equals(other.name);
        }
        return false;
    }

    public int hashCode()
    {
        return 0x5f4d5156 ^ decl.hashCode() ^ name.hashCode();
    }

    public String toString()
    {
        return name;
    }
}

final class ParameterizedTypeImpl extends TypeImpl implements ParameterizedType
{
    private String rawTypeName;
    private ClassLoader loader;
    private Class rawType;
    private Type owner;
    private Type[] typeArgs;

    ParameterizedTypeImpl(String rawTypeName, ClassLoader loader, Type owner,
        Type[] typeArgs)
    {
        this.rawTypeName = rawTypeName;
        this.loader = loader;
        this.owner = owner;
        this.typeArgs = typeArgs;
    }

    Type resolve()
    {
        if (rawType == null)
        {
            try
            {
                rawType = Class.forName(rawTypeName, false, loader);
            }
            catch (ClassNotFoundException x)
            {
                throw new TypeNotPresentException(rawTypeName, x);
            }
        }
        if (typeArgs == null)
        {
            if (owner == null)
            {
                return rawType;
            }
            typeArgs = new Type[0];
        }
        resolve(typeArgs);
        owner = resolve(owner);
        return this;
    }

    public Type[] getActualTypeArguments()
    {
      return (Type[]) typeArgs.clone();
    }

    public Type getRawType()
    {
        return rawType;
    }

    public Type getOwnerType()
    {
        return owner;
    }

    public boolean equals(Object obj)
    {
        if (obj instanceof ParameterizedTypeImpl)
        {
            ParameterizedTypeImpl other = (ParameterizedTypeImpl)obj;
            return rawType.equals(other.rawType)
                && ((owner == null && other.owner == null)
                    || owner.equals(other.owner))
                && Arrays.deepEquals(typeArgs, other.typeArgs);
        }
        return false;
    }

    public int hashCode()
    {
        int h = 0x58158970 ^ rawType.hashCode();
        if (owner != null)
        {
            h ^= Integer.reverse(owner.hashCode());
        }
        for (int i = 0; i < typeArgs.length; i++)
        {
            h ^= Integer.rotateLeft(typeArgs[i].hashCode(), i);
        }
        return h;
    }

    public String toString()
    {
        StringBuilder sb = new StringBuilder();
        if (owner != null)
        {
            sb.append(owner);
            sb.append('.');
            sb.append(rawType.getSimpleName());
        }
        else
        {
            sb.append(rawTypeName);
        }
        if (typeArgs.length > 0)
        {
            sb.append('<');
            for (int i = 0; i < typeArgs.length; i++)
            {
                if (i > 0)
                    sb.append(", ");
                if (typeArgs[i] instanceof Class)
                {
                    sb.append(((Class)typeArgs[i]).getName());
                }
                else
                {
                    sb.append(typeArgs[i]);
                }
            }
            sb.append('>');
        }
        return sb.toString();
    }
}

final class GenericArrayTypeImpl extends TypeImpl implements GenericArrayType
{
    private Type componentType;

    GenericArrayTypeImpl(Type componentType)
    {
        this.componentType = componentType;
    }

    Type resolve()
    {
        componentType = resolve(componentType);
        return this;
    }

    public Type getGenericComponentType()
    {
        return componentType;
    }

    public boolean equals(Object obj)
    {
        if (obj instanceof GenericArrayTypeImpl)
        {
            GenericArrayTypeImpl other = (GenericArrayTypeImpl)obj;
            return componentType.equals(other.componentType);
        }
        return false;
    }

    public int hashCode()
    {
        return 0x4be37a7f ^ componentType.hashCode();
    }

    public String toString()
    {
        return componentType + "[]";
    }
}

final class UnresolvedTypeVariable extends TypeImpl implements Type
{
    private GenericDeclaration decl;
    private String name;

    UnresolvedTypeVariable(GenericDeclaration decl, String name)
    {
        this.decl = decl;
        this.name = name;
    }

    Type resolve()
    {
        GenericDeclaration d = decl;
        while (d != null)
        {
            for (TypeVariable t : d.getTypeParameters())
            {
                if (t.getName().equals(name))
                {
                    return t;
                }
            }
            d = getParent(d);
        }
        throw new MalformedParameterizedTypeException();
    }

    private static GenericDeclaration getParent(GenericDeclaration d)
    {
        if (d instanceof Class)
        {
            Method m = ((Class)d).getEnclosingMethod();
            if (m != null)
            {
                return m;
            }
            Constructor c = ((Class)d).getEnclosingConstructor();
            if (c != null)
            {
                return c;
            }
            return ((Class)d).getEnclosingClass();
        }
        else if (d instanceof Method)
        {
            return ((Method)d).getDeclaringClass();
        }
        else if (d instanceof Constructor)
        {
            return ((Constructor)d).getDeclaringClass();
        }
        else
        {
            // TODO figure out what this represents
            throw new Error();
        }
    }
}

final class WildcardTypeImpl extends TypeImpl implements WildcardType
{
    private Type lower;
    private Type upper;

    WildcardTypeImpl(Type lower, Type upper)
    {
        this.lower = lower;
        this.upper = upper;
    }

    Type resolve()
    {
        upper = resolve(upper);
        lower = resolve(lower);
        return this;
    }

    public Type[] getUpperBounds()
    {
        if (upper == null)
        {
            return new Type[0];
        }
        return new Type[] { upper };
    }

    public Type[] getLowerBounds()
    {
        if (lower == null)
        {
            return new Type[0];
        }
        return new Type[] { lower };
    }

    public boolean equals(Object obj)
    {
        if (obj instanceof WildcardTypeImpl)
        {
            WildcardTypeImpl other = (WildcardTypeImpl)obj;
            return Arrays.deepEquals(getUpperBounds(), other.getUpperBounds())
                && Arrays.deepEquals(getLowerBounds(), other.getLowerBounds());
        }
        return false;
    }

    public int hashCode()
    {
        int h = 0x75d074fd;
        if (upper != null)
        {
            h ^= upper.hashCode();
        }
        if (lower != null)
        {
            h ^= lower.hashCode();
        }
        return h;
    }

    public String toString()
    {
        if (lower != null)
        {
            return "? super " + lower;
        }
        if (upper == java.lang.Object.class)
        {
            return "?";
        }
        return "? extends " + upper;
    }
}

class GenericSignatureParser
{
    private ClassLoader loader;
    private GenericDeclaration container;
    private String signature;
    private int pos;

    GenericSignatureParser(GenericDeclaration container, ClassLoader loader,
        String signature)
    {
        this.container = container;
        this.loader = loader;
        this.signature = signature;
    }

    TypeVariable[] readFormalTypeParameters()
    {
        consume('<');
        ArrayList<TypeVariable> params = new ArrayList<TypeVariable>();
        do
        {
            // TODO should we handle name clashes?
            params.add(readFormalTypeParameter());
        } while (peekChar() != '>');
        consume('>');
        TypeVariable[] list = new TypeVariable[params.size()];
        params.toArray(list);
        return list;
    }

    private TypeVariable readFormalTypeParameter()
    {
        String identifier = readIdentifier();
        consume(':');
        ArrayList<Type> bounds = new ArrayList<Type>();
        if (peekChar() != ':')
        {
            bounds.add(readFieldTypeSignature());
        }
        while (peekChar() == ':')
        {
            consume(':');
            bounds.add(readFieldTypeSignature());
        }
        Type[] b = new Type[bounds.size()];
        bounds.toArray(b);
        return new TypeVariableImpl(container, b, identifier);
    }

    Type readFieldTypeSignature()
    {
        switch (peekChar())
        {
            case 'L':
                return readClassTypeSignature();
            case '[':
                return readArrayTypeSignature();
            case 'T':
                return readTypeVariableSignature();
            default:
                throw new GenericSignatureFormatError();
        }
    }

    Type readClassTypeSignature()
    {
        consume('L');
        String className = "";
        for (;;)
        {
            String part = readIdentifier();
            if (peekChar() != '/')
            {
                className += part;
                break;
            }
            consume('/');
            className += part + ".";
        }
        Type[] typeArguments = null;
        if (peekChar() == '<')
        {
            typeArguments = readTypeArguments();
        }
        Type type = new ParameterizedTypeImpl(className, loader, null,
                                              typeArguments);
        while (peekChar() == '.')
        {
            consume('.');
            className += "$" + readIdentifier();
            typeArguments = null;
            if (peekChar() == '<')
            {
                typeArguments = readTypeArguments();
            }
            type = new ParameterizedTypeImpl(className, loader, type,
                                             typeArguments);
        }
        consume(';');
        return type;
    }

    private Type[] readTypeArguments()
    {
        consume('<');
        ArrayList<Type> list = new ArrayList<Type>();
        do
        {
            list.add(readTypeArgument());
        } while ((peekChar() != '>'));
        consume('>');
        Type[] arr = new Type[list.size()];
        list.toArray(arr);
        return arr;
    }

    private Type readTypeArgument()
    {
        char c = peekChar();
        if (c == '+')
        {
            consume('+');
            return new WildcardTypeImpl(null, readFieldTypeSignature());
        }
        else if (c == '-')
        {
            consume('-');
            return new WildcardTypeImpl(readFieldTypeSignature(),
                java.lang.Object.class);
        }
        else if (c == '*')
        {
            consume('*');
            return new WildcardTypeImpl(null, java.lang.Object.class);
        }
        else
        {
            return readFieldTypeSignature();
        }
    }

    Type readArrayTypeSignature()
    {
        consume('[');
        switch (peekChar())
        {
            case 'L':
            case '[':
            case 'T':
                return new GenericArrayTypeImpl(readFieldTypeSignature());
            case 'Z':
                consume('Z');
                return boolean[].class;
            case 'B':
                consume('B');
                return byte[].class;
            case 'S':
                consume('S');
                return short[].class;
            case 'C':
                consume('C');
                return char[].class;
            case 'I':
                consume('I');
                return int[].class;
            case 'F':
                consume('F');
                return float[].class;
            case 'J':
                consume('J');
                return long[].class;
            case 'D':
                consume('D');
                return double[].class;
            default:
                throw new GenericSignatureFormatError();
        }
    }

    Type readTypeVariableSignature()
    {
        consume('T');
        String identifier = readIdentifier();
        consume(';');
        return new UnresolvedTypeVariable(container, identifier);
    }

    private String readIdentifier()
    {
        int start = pos;
        char c;
        do
        {
            readChar();
            c = peekChar();
        } while (";:./<>-+*".indexOf(c) == -1);
        return signature.substring(start, pos);
    }

    final char peekChar()
    {
        if (pos == signature.length())
            return '\u0000';
        else
            return signature.charAt(pos);
    }

    final char readChar()
    {
        return signature.charAt(pos++);
    }

    final void consume(char c)
    {
        if (readChar() != c)
            throw new GenericSignatureFormatError();
    }

    final void end()
    {
        if (pos != signature.length())
            throw new GenericSignatureFormatError();
    }
}
