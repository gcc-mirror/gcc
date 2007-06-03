/* NameConstraints.java -- the NameConstraints X.509 extension.
   Copyright (C) 2006  Free Software Foundation, Inc.

This file is a part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301
USA

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


package gnu.java.security.x509.ext;

import gnu.java.security.OID;
import gnu.java.security.der.DERReader;
import gnu.java.security.der.DERValue;
import gnu.java.security.x509.ext.Extension.Value;

import java.io.IOException;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

/**
 * The NameConstraints extension. From RFC 3280, section 4.2.1.11, this
 * extension is defined as:
 * 
 * <pre>
  id-ce-nameConstraints OBJECT IDENTIFIER ::=  { id-ce 30 }

  NameConstraints ::= SEQUENCE {
    permittedSubtrees       [0]     GeneralSubtrees OPTIONAL,
    excludedSubtrees        [1]     GeneralSubtrees OPTIONAL }

  GeneralSubtrees ::= SEQUENCE SIZE (1..MAX) OF GeneralSubtree

  GeneralSubtree ::= SEQUENCE {
    base                    GeneralName,
    minimum         [0]     BaseDistance DEFAULT 0,
    maximum         [1]     BaseDistance OPTIONAL }

  BaseDistance ::= INTEGER (0..MAX)
  </pre>
 * 
 * See also the classes {@link GeneralNames} and {@link GeneralSubtree}.
 * 
 * @author csm
 */
public class NameConstraints extends Value
{
  public static final OID ID = new OID("2.5.29.30");
  
  private List<GeneralSubtree> permittedSubtrees;
  private List<GeneralSubtree> excludedSubtrees;
  
  public NameConstraints(byte[] encoded) throws IOException
  {
    super(encoded);
    
    DERReader der = new DERReader(encoded);
    DERValue value = der.read();
    if (!value.isConstructed())
      {
        throw new IOException("malformed NameConstraints");
      }
    
    permittedSubtrees = new LinkedList<GeneralSubtree>();
    excludedSubtrees = new LinkedList<GeneralSubtree>();
    int len = 0;
    if (len < value.getLength())
      {
        DERValue subtrees = der.read();
        if (subtrees.getTag() == 0)
          {
            int len2 = 0;
            while (len2 < subtrees.getLength())
              {
                DERValue subtree = der.read();
                permittedSubtrees.add(new GeneralSubtree(subtree.getEncoded()));
                der.skip(subtree.getLength());
                len2 += subtree.getEncodedLength();
              }
            len += subtrees.getEncodedLength();
            
            if (len < value.getLength())
              {
                subtrees = der.read();
                if (subtrees.getTag() != 1)
                  throw new IOException("unexpected tag " + subtrees.getTag()
                                        + " (expecting 1 for excludedSubtrees)");
                len2 = 0;
                while (len2 < subtrees.getLength())
                  {
                    DERValue subtree = der.read();
                    excludedSubtrees.add(new GeneralSubtree(subtree.getEncoded()));
                    der.skip(subtree.getLength());
                    len2 += subtree.getEncodedLength();
                  }
              }
          }
        else if (subtrees.getTag() == 1)
          {
            int len2 = 0;
            while (len2 < subtrees.getLength())
              {
                DERValue subtree = der.read();
                excludedSubtrees.add(new GeneralSubtree(subtree.getEncoded()));
                der.skip(subtree.getLength());
                len2 += subtree.getEncodedLength();
              }            
          }
        else
          throw new IOException("unexpected tag " + subtrees.getTag()
                                + " (expecting 0 or 1)");
      }
  }
  
  public List<GeneralSubtree> permittedSubtrees()
  {
    return Collections.unmodifiableList(permittedSubtrees);
  }
  
  public List<GeneralSubtree> excludedSubtrees()
  {
    return Collections.unmodifiableList(excludedSubtrees);
  }
  
  public String toString()
  {
    return NameConstraints.class.getName() + " [ permittedSubtrees="
      + permittedSubtrees + "; excludedSubtrees=" + excludedSubtrees
      + " ]";
  }
}
