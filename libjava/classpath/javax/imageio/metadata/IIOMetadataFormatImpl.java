/* IIOMetadataFormatImpl.java --
   Copyright (C) 2004  Free Software Foundation, Inc.

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


package javax.imageio.metadata;

import org.w3c.dom.Attr;
import org.w3c.dom.DOMException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.TypeInfo;
import org.w3c.dom.UserDataHandler;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.ResourceBundle;
import java.util.MissingResourceException;
import javax.imageio.ImageTypeSpecifier;

public abstract class IIOMetadataFormatImpl implements IIOMetadataFormat
{
  /**
   * The standard metadata format name constant set to
   * "javax_imageio_1.0".
   */
  public static final String standardMetadataFormatName = "javax_imageio_1.0";

  private String rootName;

  // These maps assume that each element name is unique.

  private Map nodes = new HashMap();

  // A mapping from element name to child policy.
  private Map childPolicies = new HashMap();

  // A mapping from element name to the permissible number of
  // children.  Values in this map are length-two integer arrays; the
  // first index is the minimum bound, the second index is the maximum
  // bound.
  private Map childRanges = new HashMap();

  private String resourceBaseName;

  // Package-private so that it may be used in IIOMetadataNode.
  static class IIOMetadataNodeAttr extends IIOMetadataNode
    implements Attr
  {
    protected Element owner;
    protected String name;
    protected int dataType;
    protected boolean required;
    protected String defaultValue;

    public IIOMetadataNodeAttr (Element owner,
				String name,
				String defaultValue)
    {
      this (owner, name, IIOMetadataFormat.DATATYPE_STRING,
            true, defaultValue);
    }

    public IIOMetadataNodeAttr (Element owner,
				String name,
				int dataType,
				boolean required,
				String defaultValue)
    {
      this.owner = owner;
      this.name = name;
      this.dataType = dataType;
      this.required = required;
      this.defaultValue = defaultValue;
    }

    public String getName ()
    {
      return name;
    }

    public Element getOwnerElement ()
    {
      return owner;
    }

    public int getDataType ()
    {
      return dataType;
    }

    public TypeInfo getSchemaTypeInfo ()
    {
      return null;
    }

    public boolean getSpecified ()
    {
      return false;
    }

    public String getValue ()
    {
      return defaultValue;
    }

    public boolean isId()
    {
      return false;
    }

    public void setValue (String value)
    {
    }

    // new methods

    public boolean isRequired ()
    {
      return required;
    }
  }

  private class IIOMetadataNodeAttrEnumerated extends IIOMetadataNodeAttr
  {
    protected List enumeratedValues;

    public IIOMetadataNodeAttrEnumerated (Element owner,
					  String name,
					  int dataType,
					  boolean required,
					  String defaultValue,
					  List enumeratedValues)
    {
      super (owner, name, dataType, required, defaultValue);
      this.enumeratedValues = new ArrayList (enumeratedValues);
    }

    public Object[] getEnumerations ()
    {
      return enumeratedValues.toArray ();
    }
  }

  private class IIOMetadataNodeAttrBounded extends IIOMetadataNodeAttr
  {
    protected String minValue;
    protected String maxValue;
    protected boolean minInclusive;
    protected boolean maxInclusive;

    public IIOMetadataNodeAttrBounded (Element owner,
				       String name,
				       int dataType,
				       boolean required,
				       String defaultValue,
				       String minValue,
				       String maxValue,
				       boolean minInclusive,
				       boolean maxInclusive)
    {
      super (owner, name, dataType, required, defaultValue);
      this.minValue = minValue;
      this.maxValue = maxValue;
      this.minInclusive = minInclusive;
      this.maxInclusive = maxInclusive;
    }

    public String getMinValue ()
    {
      return minValue;
    }

    public String getMaxValue ()
    {
      return maxValue;
    }
  }

  private class IIOMetadataNodeAttrList extends IIOMetadataNodeAttr
  {
    protected int listMinLength;
    protected int listMaxLength;

    public IIOMetadataNodeAttrList (Element owner,
				    String name,
				    int dataType,
				    boolean required,
				    int listMinLength,
				    int listMaxLength)
    {
      super (owner, name, dataType, required, null);
      this.listMinLength = listMinLength;
      this.listMaxLength = listMaxLength;
    }

    public int getListMinLength ()
    {
      return listMinLength;
    }

    public int getListMaxLength ()
    {
      return listMaxLength;
    }
  }

  private class NodeObject
  {
    protected Element owner;
    protected Class classType;
    protected boolean required;
    protected Object defaultValue;
    protected int valueType;

    public NodeObject (Element owner,
                       Class classType,
                       boolean required,
                       Object defaultValue)
    {
      this.owner = owner;
      this.classType = classType;
      this.required = required;
      this.defaultValue = defaultValue;
      valueType = IIOMetadataFormat.VALUE_ARBITRARY;
    }

    public int getValueType ()
    {
      return valueType;
    }

    public Class getClassType ()
    {
      return classType;
    }

    public Element getOwnerElement ()
    {
      return owner;
    }

    public Object getDefaultValue ()
    {
      return defaultValue;
    }

    public boolean isRequired ()
    {
      return required;
    }
  }

  private class NodeObjectEnumerated extends NodeObject
  {
    protected List enumeratedValues;

    public NodeObjectEnumerated (Element owner,
                                 Class classType,
                                 boolean required,
                                 Object defaultValue,
                                 List enumeratedValues)
    {
      super (owner, classType, false, defaultValue);
      this.enumeratedValues = enumeratedValues;
      valueType = IIOMetadataFormat.VALUE_ENUMERATION;
    }

    public Object[] getEnumerations ()
    {
      return enumeratedValues.toArray();
    }
  }

  private class NodeObjectBounded extends NodeObject
  {
    protected Comparable minValue;
    protected Comparable maxValue;
    protected boolean minInclusive;
    protected boolean maxInclusive;

    public NodeObjectBounded (Element owner,
                              Class classType,
                              Object defaultValue,
                              Comparable minValue,
                              Comparable maxValue,
                              boolean minInclusive,
                              boolean maxInclusive)
    {
      super (owner, classType, false, defaultValue);
      this.minValue = minValue;
      this.maxValue = maxValue;
      this.minInclusive = minInclusive;
      this.maxInclusive = maxInclusive;
      if (minInclusive)
        {
          if (maxInclusive)
            valueType = IIOMetadataFormat.VALUE_RANGE_MIN_MAX_INCLUSIVE;
          else
            valueType = IIOMetadataFormat.VALUE_RANGE_MIN_INCLUSIVE;
        }
      else
        {
          if (maxInclusive)
            valueType = IIOMetadataFormat.VALUE_RANGE_MAX_INCLUSIVE;
          else
            valueType = IIOMetadataFormat.VALUE_RANGE;
        }
    }

    public Comparable getMinValue ()
    {
      return minValue;
    }

    public Comparable getMaxValue ()
    {
      return maxValue;
    }
  }

  private class NodeObjectArray extends NodeObject
  {
    protected Integer arrayMinLength;
    protected Integer arrayMaxLength;

    public NodeObjectArray (Element owner,
                            Class classType,
                            int arrayMinLength,
                            int arrayMaxLength)
    {
      super (owner, classType, false, null);
      this.arrayMinLength = new Integer (arrayMinLength);
      this.arrayMaxLength = new Integer (arrayMaxLength);
      valueType = IIOMetadataFormat.VALUE_LIST;
    }

    public Comparable getArrayMinLength ()
    {
      return arrayMinLength;
    }

    public Comparable getArrayMaxLength ()
    {
      return arrayMaxLength;
    }
  }

  /**
   * Construct a blank IIOMetadataFormatImpl with the given root name
   * and child policy.
   *
   * @param rootName the root element name
   * @param childPolicy the child policy of the root element
   *
   * @exception IllegalArgumentException if rootName is null
   * @exception IllegalArgumentException if childPolicy is
   * CHILD_POLICY_REPEAT or if childPolicy is not a CHILD_POLICY
   * constant
   */
  public IIOMetadataFormatImpl (String rootName, int childPolicy)
  {
    if (rootName == null)
      throw new IllegalArgumentException ("null argument");

    if (childPolicy < IIOMetadataFormat.CHILD_POLICY_ALL
	|| childPolicy > IIOMetadataFormat.CHILD_POLICY_SOME
	|| childPolicy == IIOMetadataFormat.CHILD_POLICY_REPEAT)
      throw new IllegalArgumentException ("wrong child policy");

    nodes.put (rootName, new IIOMetadataNode (rootName));
    childPolicies.put (rootName, new Integer (childPolicy));
    this.rootName = rootName;
  }

  /**
   * Construct a blank IIOMetadataFormatImpl with the given root name,
   * a child policy of CHILD_POLICY_REPEAT and the given minimum and
   * maximum limits on the number of root element children.
   *
   * @param rootName the root element name
   * @param minChildren the minimum number of children that this node
   * can have
   * @param maxChildren the maximum number of children that this node
   * can have
   *
   * @exception IllegalArgumentException if rootName is null
   * @exception IllegalArgumentException if minChildren is less than
   * zero or greater than maxChildren
   */
  public IIOMetadataFormatImpl (String rootName,
				int minChildren,
				int maxChildren)
  {
    if (rootName == null)
      throw new IllegalArgumentException ("null argument");

    if (minChildren < 0 || maxChildren < minChildren)
      throw new IllegalArgumentException ("invalid min or max children argument");

    nodes.put (rootName, new IIOMetadataNode (rootName));
    childPolicies.put (rootName, new Integer (IIOMetadataFormat.CHILD_POLICY_REPEAT));
    childRanges.put (rootName, new int [] { minChildren, maxChildren });
    this.rootName = rootName;
  }

  protected void addAttribute (String elementName,
                               String attrName,
                               int dataType,
                               boolean required,
                               String defaultValue)
  {
    IIOMetadataNode node = (IIOMetadataNode) nodes.get (elementName);
    node.setAttributeNode (new IIOMetadataNodeAttr (node,
						    attrName,
						    dataType,
						    required,
						    defaultValue));
  }

  protected void addAttribute (String elementName,
                               String attrName,
                               int dataType,
                               boolean required,
                               String defaultValue,
                               List enumeratedValues)
  {
    IIOMetadataNode node = (IIOMetadataNode) nodes.get (elementName);
    node.setAttributeNode (new IIOMetadataNodeAttrEnumerated (node,
							      attrName,
							      dataType,
							      required,
							      defaultValue,
							      enumeratedValues));
  }

  protected void addAttribute (String elementName,
                               String attrName,
                               int dataType,
                               boolean required,
                               String defaultValue,
                               String minValue,
                               String maxValue,
                               boolean minInclusive,
                               boolean maxInclusive)
  {
    IIOMetadataNode node = (IIOMetadataNode) nodes.get (elementName);
    node.setAttributeNode (new IIOMetadataNodeAttrBounded (node,
							   attrName,
							   dataType,
							   required,
							   defaultValue,
							   minValue,
							   maxValue,
							   minInclusive,
							   maxInclusive));
  }

  protected void addAttribute (String elementName,
                               String attrName,
                               int dataType,
                               boolean required,
                               int listMinLength,
                               int listMaxLength)
  {
    IIOMetadataNode node = (IIOMetadataNode) nodes.get (elementName);
    node.setAttributeNode (new IIOMetadataNodeAttrList (node,
							attrName,
							dataType,
							required,
							listMinLength,
							listMaxLength));
  }

  protected void addBooleanAttribute (String elementName,
                                      String attrName,
                                      boolean hasDefaultValue,
                                      boolean defaultValue)
  {
    IIOMetadataNode node = (IIOMetadataNode) nodes.get (elementName);

    List enumeratedValues = new ArrayList();
    enumeratedValues.add ("TRUE");
    enumeratedValues.add ("FALSE");

    node.setAttributeNode (new IIOMetadataNodeAttrEnumerated (node,
							      attrName,
							      IIOMetadataFormat.DATATYPE_BOOLEAN,
							      hasDefaultValue,
							      defaultValue ? "TRUE" : "FALSE",
							      enumeratedValues));
  }

  protected void addChildElement (String elementName, String parentName)
  {
    IIOMetadataNode node = (IIOMetadataNode) nodes.get (parentName);

    node.appendChild (new IIOMetadataNode (elementName));
    childPolicies.put (elementName, new Integer (IIOMetadataFormat.CHILD_POLICY_REPEAT));
  }

  protected void addElement (String elementName, String parentName, int childPolicy)
  {
    IIOMetadataNode node = (IIOMetadataNode) nodes.get (parentName);

    node.appendChild (new IIOMetadataNode (elementName));
    childPolicies.put (elementName, new Integer (childPolicy));
  }

  protected void addElement (String elementName, String parentName,
                             int minChildren, int maxChildren)
  {
    addChildElement (elementName, parentName);
    childRanges.put (elementName, new int [] { minChildren, maxChildren });
  }

  private void addNodeObject (IIOMetadataNode node, NodeObject o)
  {
    node.setUserObject (o);
  }

  private NodeObject getNodeObject (IIOMetadataNode node)
  {
    return (NodeObject) node.getUserObject ();
  }

  private void removeNodeObject (IIOMetadataNode node)
  {
    node.setUserObject (null);
  }

  protected void addObjectValue (String elementName, Class classType,
                                 boolean required, Object defaultValue)
  {
    IIOMetadataNode node = (IIOMetadataNode) nodes.get (elementName);
    addNodeObject (node, new NodeObject (node,
                                         classType,
                                         required,
                                         defaultValue));
  }

  protected void addObjectValue (String elementName, Class classType,
                                 boolean required, Object defaultValue,
                                 List enumeratedValues)
  {
    IIOMetadataNode node = (IIOMetadataNode) nodes.get (elementName);
    addNodeObject (node, new NodeObjectEnumerated (node,
                                                   classType,
                                                   required,
                                                   defaultValue,
                                                   enumeratedValues));
  }

  protected void addObjectValue (String elementName, Class classType,
                                 Object defaultValue,
                                 Comparable minValue,
                                 Comparable maxValue,
                                 boolean minInclusive,
                                 boolean maxInclusive)
  {
    IIOMetadataNode node = (IIOMetadataNode) nodes.get (elementName);
    addNodeObject (node, new NodeObjectBounded (node,
                                                classType,
                                                defaultValue,
                                                minValue,
                                                maxValue,
                                                minInclusive,
                                                maxInclusive));
  }

  protected void addObjectValue (String elementName, Class classType,
                                 int arrayMinLength, int arrayMaxLength)
  {
    IIOMetadataNode node = (IIOMetadataNode) nodes.get (elementName);
    addNodeObject (node, new NodeObjectArray (node,
                                              classType,
                                              arrayMinLength,
                                              arrayMaxLength));
  }

  public String getRootName ()
  {
    return rootName;
  }

  protected String getResourceBaseName ()
  {
    return resourceBaseName;
  }

  public static IIOMetadataFormat getStandardFormatInstance ()
  {
    // FIXME: populate this with the standard metadata format
    return new IIOMetadataFormatImpl (standardMetadataFormatName,
                                      IIOMetadataFormat.CHILD_POLICY_ALL)
      {
        public boolean canNodeAppear (String elementName,
                                      ImageTypeSpecifier specifier)
        {
          return true;
        }
      };
  }

  public abstract boolean canNodeAppear (String elementName,
                                         ImageTypeSpecifier specifier);

  protected void removeAttribute (String elementName,
                                  String attrName)
  {
    IIOMetadataNode node = (IIOMetadataNode) nodes.get (elementName);
    node.removeAttribute (attrName);
  }

  protected void removeElement (String elementName)
  {
    nodes.remove (elementName);
  }

  protected void removeObjectValue (String elementName)
  {
    IIOMetadataNode node = (IIOMetadataNode) nodes.get (elementName);
    removeNodeObject (node);
  }

  protected void setResourceBaseName (String resourceBaseName)
  {
    this.resourceBaseName = resourceBaseName;
  }

  public int getAttributeDataType (String elementName, String attrName)
  {
    IIOMetadataNode node = (IIOMetadataNode) nodes.get (elementName);
    IIOMetadataNodeAttr attr = (IIOMetadataNodeAttr) node.getAttributeNode (attrName);
    return attr.getDataType ();
  }

  public String getAttributeDefaultValue (String elementName, String attrName)
  {
    IIOMetadataNode node = (IIOMetadataNode) nodes.get (elementName);
    IIOMetadataNodeAttr attr = (IIOMetadataNodeAttr) node.getAttributeNode (attrName);
    return attr.getValue();
  }

  public String getAttributeDescription (String elementName, String attrName, Locale locale)
  {
    return getDescription (elementName + "/" + attrName, locale);
  }

  public String[] getAttributeEnumerations (String elementName, String attrName)
  {
    IIOMetadataNode node = (IIOMetadataNode) nodes.get (elementName);
    IIOMetadataNodeAttrEnumerated attr =
      (IIOMetadataNodeAttrEnumerated) node.getAttributeNode (attrName);

    Object[] attrEnums = attr.getEnumerations();

    String[] attrNames = new String[attrEnums.length];

    for (int i = 0; i < attrEnums.length; i++)
      {
        attrNames[i] = (String) attrEnums[i];
      }

    return attrNames;
  }

  public int getAttributeListMaxLength (String elementName, String attrName)
  {
    IIOMetadataNode node = (IIOMetadataNode) nodes.get (elementName);
    IIOMetadataNodeAttrList attr =
      (IIOMetadataNodeAttrList) node.getAttributeNode (attrName);
    return attr.getListMaxLength();
  }

  public int getAttributeListMinLength (String elementName, String attrName)
  {
    IIOMetadataNode node = (IIOMetadataNode) nodes.get (elementName);
    IIOMetadataNodeAttrList attr =
      (IIOMetadataNodeAttrList) node.getAttributeNode (attrName);
    return attr.getListMinLength();
  }

  public String getAttributeMaxValue (String elementName, String attrName)
  {
    IIOMetadataNode node = (IIOMetadataNode) nodes.get (elementName);
    IIOMetadataNodeAttrBounded attr =
      (IIOMetadataNodeAttrBounded) node.getAttributeNode (attrName);
    return attr.getMaxValue();
  }

  public String getAttributeMinValue (String elementName, String attrName)
  {
    IIOMetadataNode node = (IIOMetadataNode) nodes.get (elementName);
    IIOMetadataNodeAttrBounded attr =
      (IIOMetadataNodeAttrBounded) node.getAttributeNode (attrName);
    return attr.getMinValue();
  }

  public String[] getAttributeNames (String elementName)
  {
    IIOMetadataNode node = (IIOMetadataNode) nodes.get (elementName);

    NamedNodeMap attrNodes = node.getAttributes();

    String[] attrNames = new String[attrNodes.getLength()];

    for (int i = 0; i < attrNodes.getLength(); i++)
      {
        attrNames[i] = attrNodes.item (i).getLocalName();
      }

    return attrNames;
  }

  public int getAttributeValueType (String elementName, String attrName)
  {
    IIOMetadataNode node = (IIOMetadataNode) nodes.get (elementName);
    IIOMetadataNodeAttr attr = (IIOMetadataNodeAttr) node.getAttributeNode (attrName);
    return attr.getDataType();
  }

  public String[] getChildNames (String elementName)
  {
    IIOMetadataNode node = (IIOMetadataNode) nodes.get (elementName);

    NodeList childNodes = node.getChildNodes();

    String[] childNames = new String[childNodes.getLength()];

    for (int i = 0; i < childNodes.getLength(); i++)
      {
        childNames[i] = childNodes.item (i).getLocalName();
      }

    return childNames;
  }

  public int getChildPolicy (String elementName)
  {
    return ((Integer) childPolicies.get (elementName)).intValue();
  }

  private String getDescription (String resourceName, Locale locale)
  {
    if (resourceBaseName == null)
      return null;

    Locale l = locale;

    if (l == null)
      l = Locale.getDefault();

    ResourceBundle bundle = ResourceBundle.getBundle (resourceBaseName, locale);

    String desc = null;

    if (bundle == null)
      {
        try
          {
            desc = bundle.getString (resourceName);
          }
        catch (MissingResourceException e)
          {
            desc = null;
          }
      }

    return desc;
  }

  public String getElementDescription (String elementName, Locale locale)
  {
    return getDescription (elementName, locale);
  }

  public int getElementMaxChildren (String elementName)
  {
    return ((int[]) childRanges.get (elementName))[1];
  }

  public int getElementMinChildren (String elementName)
  {
    return ((int[]) childRanges.get (elementName))[0];
  }

  public int getObjectArrayMaxLength (String elementName)
  {
    IIOMetadataNode node = (IIOMetadataNode) nodes.get (elementName);
    return ((Integer) ((NodeObjectArray) getNodeObject (node)).getArrayMaxLength ()).intValue();
  }

  public int getObjectArrayMinLength (String elementName)
  {
    IIOMetadataNode node = (IIOMetadataNode) nodes.get (elementName);
    return ((Integer) ((NodeObjectArray) getNodeObject (node)).getArrayMinLength ()).intValue();
  }

  public Class getObjectClass (String elementName)
  {
    IIOMetadataNode node = (IIOMetadataNode) nodes.get (elementName);
    return getNodeObject (node).getClassType ();
  }

  public Object getObjectDefaultValue (String elementName)
  {
    IIOMetadataNode node = (IIOMetadataNode) nodes.get (elementName);
    return getNodeObject (node).getDefaultValue ();
  }

  public Object[] getObjectEnumerations (String elementName)
  {
    IIOMetadataNode node = (IIOMetadataNode) nodes.get (elementName);
    return ((NodeObjectEnumerated) getNodeObject (node)).getEnumerations ();
  }

  public Comparable getObjectMaxValue (String elementName)
  {
    IIOMetadataNode node = (IIOMetadataNode) nodes.get (elementName);
    return ((NodeObjectBounded) getNodeObject (node)).getMaxValue ();
  }

  public Comparable getObjectMinValue (String elementName)
  {
    IIOMetadataNode node = (IIOMetadataNode) nodes.get (elementName);
    return ((NodeObjectBounded) getNodeObject (node)).getMinValue ();
  }

  public int getObjectValueType (String elementName)
  {
    IIOMetadataNode node = (IIOMetadataNode) nodes.get (elementName);
    NodeObject n = getNodeObject (node);

    if (n == null)
      return IIOMetadataFormat.VALUE_NONE;
    else
      return n.getValueType ();
  }

  public boolean isAttributeRequired (String elementName, String attrName)
  {
    IIOMetadataNode node = (IIOMetadataNode) nodes.get (elementName);
    return ((IIOMetadataNodeAttr) node.getAttributeNode (attrName)).isRequired();
  }
}
