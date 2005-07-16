/* xmlj_node.c -
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

#include "xmlj_error.h"
#include "xmlj_node.h"
#include "xmlj_util.h"
#include <libxml/xmlstring.h>

/*
 * Returns the node ID for the given GnomeNode object.
 */
xmlNodePtr
xmljGetNodeID (JNIEnv * env, jobject self)
{
  jclass cls;
  jfieldID field;
  jobject id;
  xmlNodePtr node;

  if (self == NULL)
    {
      xmljThrowDOMException (env, 8, NULL);     /* NOT_FOUND_ERR */
      return NULL;
    }
  cls = (*env)->GetObjectClass (env, self);
  if (cls == NULL)
    {
      return NULL;
    }
  field = (*env)->GetFieldID (env, cls, "id", "Ljava/lang/Object;");
  if (field == NULL)
    {
      return NULL;
    }
  id = (*env)->GetObjectField (env, self, field);
  node = (xmlNodePtr) xmljAsPointer (env, id);
  if (node == NULL)
    {
      xmljThrowDOMException (env, 8, NULL);	/* NOT_FOUND_ERR */
    }
  return node;
}

/*
 * Returns the Java node instanced corresponding to the specified node ID.
 */
jobject
xmljGetNodeInstance (JNIEnv * env, xmlNodePtr node)
{
  jclass cls;
  jmethodID method;
  xmlElementType type;

  if (node == NULL)
    return NULL;

  /* Invoke the GnomeNode.newInstance class method */
  cls = (*env)->FindClass (env, "gnu/xml/libxmlj/dom/GnomeNode");
  if (cls == NULL)
    {
      return NULL;
    }
  method = (*env)->GetStaticMethodID (env, cls, "newInstance",
                                      "(Ljava/lang/Object;Ljava/lang/Object;I)Lgnu/xml/libxmlj/dom/GnomeNode;");
  
  if (method == NULL)
    {
      return NULL;
    }
  type = node->type;
  switch (type)
    {
    case XML_DTD_NODE:
      type = XML_DOCUMENT_TYPE_NODE;
      break;
    case XML_ATTRIBUTE_DECL:
      type = XML_ATTRIBUTE_NODE;
      break;
    case XML_ENTITY_DECL:
      type = XML_ENTITY_NODE;
      break;
    default:
      break;
    }
  return (*env)->CallStaticObjectMethod (env, cls, method,
                                         xmljAsField (env, node->doc),
                                         xmljAsField (env, node),
                                         type);
}

void
xmljFreeDoc (JNIEnv * env, xmlDocPtr doc)
{
  jclass cls;
  jmethodID method;

  /* Invoke the GnomeNode.freeDocument class method */
  cls = (*env)->FindClass (env, "gnu/xml/libxmlj/dom/GnomeNode");
  if (cls == NULL)
    {
      return;
    }
  method = (*env)->GetStaticMethodID (env, cls, "freeDocument",
                                      "(Ljava/lang/Object;)V");
  if (method == NULL)
    {
      return;
    }
  (*env)->CallStaticVoidMethod (env, cls, method, xmljAsField (env, doc));
}

int
xmljMatch (const xmlChar * name, xmlNodePtr node)
{
  switch (node->type)
    {
    case XML_ELEMENT_NODE:
    case XML_ATTRIBUTE_NODE:
      return xmlStrcmp (node->name, name);
    default:
      return 1;
    }
}

int
xmljMatchNS (const xmlChar * uri, const xmlChar * localName, xmlNodePtr node)
{
  xmlNsPtr ns;
  const xmlChar *nodeLocalName;
  int *len;
  int ret;

  switch (node->type)
    {
    case XML_ELEMENT_NODE:
    case XML_ATTRIBUTE_NODE:
      len = (int *) malloc (sizeof (int));
      if (xmlSplitQName3 (node->name, len) != NULL)
        {
          nodeLocalName = node->name + (*len);
        }
      else
        {
          nodeLocalName = node->name;
        }
      free (len);
      ns = node->ns;
      if (ns == NULL || ns->href == NULL)
        {
          if (uri != NULL)
            {
              return 0;
            }
          ret = xmlStrcmp (localName, nodeLocalName);
        }
      else
        {
          if (uri == NULL)
            {
              return 0;
            }
          ret = (xmlStrcmp (localName, nodeLocalName) &&
                 xmlStrcmp (uri, ns->href));
        }
      return ret;
    default:
      return 1;
    }
}
