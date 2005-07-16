/* xmlj_dom.c - 
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

#include "xmlj_dom.h"
#include "xmlj_error.h"
#include "xmlj_io.h"
#include "xmlj_node.h"
#include "xmlj_sax.h"
#include "xmlj_util.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

JNIEnv *dom_cb_env;
jobject dom_cb_obj;

typedef struct
{
  int index;
  int count;
  xmlNodePtr node;
}
xmljHashScanData;

/* Prototypes for local functions */

void
xmljAddAttribute (xmlNodePtr node, xmlAttrPtr attr);

void
xmljHashScanner (void *payload, void *vdata, xmlChar *name);

xmlChar *
xmljGetNodeValue (xmlNodePtr node);

/*
 * Determines whether a child node is suitable for insertion in the list of
 * children for a given parent node.
 * Returns 0 on success, a DOMException code otherwise.
 */
void
xmljValidateChildNode (JNIEnv *env, xmlNodePtr parent, xmlNodePtr child)
{
  xmlNodePtr cur;
  
  if (child == NULL || parent == NULL)
    {
      xmljThrowDOMException (env, 8, NULL); /* NOT_FOUND_ERR */
      return;
    }
  if (child->doc != parent->doc)
    {
      xmljThrowDOMException (env, 4, NULL); /* WRONG_DOCUMENT_ERR */
      return;
    }
  /* Check that new parent is of an allowed type */
  switch (parent->type)
    {
    case XML_CDATA_SECTION_NODE:
    case XML_COMMENT_NODE:
    case XML_TEXT_NODE:
    case XML_ENTITY_NODE:
    case XML_ENTITY_REF_NODE:
    case XML_NOTATION_NODE:
    case XML_PI_NODE:
      /* these can't have any children */
      /* HIERARCHY_REQUEST_ERR */
      xmljThrowDOMException (env, 3, "parent type does not allow children");
      return;
    case XML_ATTRIBUTE_NODE:
      if (child->type != XML_TEXT_NODE &&
          child->type != XML_ENTITY_REF_NODE)
        {
          /* HIERARCHY_REQUEST_ERR */
          xmljThrowDOMException (env, 3, "attributes may only contain text or entity reference nodes");
          return;
        }
      break;
    case XML_DOCUMENT_FRAG_NODE:
    case XML_ELEMENT_NODE:
      if (child->type == XML_DTD_NODE ||
          child->type == XML_DOCUMENT_TYPE_NODE ||
          child->type == XML_ENTITY_NODE ||
          child->type == XML_NOTATION_NODE ||
          child->type == XML_PI_NODE)
        {
          /* HIERARCHY_REQUEST_ERR */
          xmljThrowDOMException (env, 3, "parent type does not allow child of this type");
          return;
        }
      /* fall through */
    default:
      if (child->type == XML_ATTRIBUTE_NODE ||
          child->type == XML_DOCUMENT_NODE ||
          child->type == XML_DOCUMENT_FRAG_NODE)
        {
          /* HIERARCHY_REQUEST_ERR */
          xmljThrowDOMException (env, 3, "node type may not be a child");
          return;
        }
      /* TODO others? */
    }
  /* Check that new parent is not self or an ancestor */
  for (cur = parent; cur != NULL; cur = cur->parent)
    {
      if (cur == child)
        {
          /* HIERARCHY_REQUEST_ERR */
          xmljThrowDOMException (env, 3, "child cannot be an ancestor of itself");
          return;
        }
    }
  /* Check that new parent does not add a second doctype or root element
   * to a document parent */
  if (parent->type == XML_DOCUMENT_NODE)
    {
      cur = parent->children;
      while (cur != NULL)
        {
          if (cur->type == XML_DTD_NODE ||
              cur->type == XML_DOCUMENT_TYPE_NODE ||
              (cur->type == XML_ELEMENT_NODE &&
               parent->type == XML_DOCUMENT_NODE))
            {
              if (child->type == cur->type && child != cur)
                {
                  /* HIERARCHY_REQUEST_ERR */
                  xmljThrowDOMException (env, 3, "cannot add a second doctype or root element");
                  return;
                }
            }
          cur = cur->next;
        }
    }
}

/*
 * Adds the specified attribute node to the list of attributes for the given
 * element.
 */
void
xmljAddAttribute (xmlNodePtr node, xmlAttrPtr attr)
{
  xmlAttrPtr cur = node->properties;
  
  if (cur == NULL)
    {
      node->properties = attr;
      attr->prev = NULL;
      attr->next = NULL;
      attr->parent = node;
      attr->doc = node->doc;
    }
  else
    {
      while (cur->next != NULL)
        {
          cur = cur->next;
        }
      cur->next = attr;
      attr->prev = cur;
      attr->next = NULL;
      attr->parent = node;
      attr->doc = node->doc;
    }
}

/* -- GnomeAttr -- */

JNIEXPORT jboolean JNICALL
Java_gnu_xml_libxmlj_dom_GnomeAttr_getSpecified (JNIEnv * env, jobject self)
{
  xmlAttrPtr attr;

  attr = (xmlAttrPtr) xmljGetNodeID (env, self);
  return (attr->atype != 0);
}

JNIEXPORT jstring JNICALL
Java_gnu_xml_libxmlj_dom_GnomeAttr_getValue (JNIEnv * env, jobject self)
{
  xmlNodePtr node;
  xmlChar *text;
  jstring ret;

  node = xmljGetNodeID (env, self);
  text = xmlNodeGetContent (node);
  ret = xmljNewString (env, (const xmlChar *) text);
  if (text != NULL)
    {
      xmlFree (text);
    }
  return ret;
}

JNIEXPORT void JNICALL
Java_gnu_xml_libxmlj_dom_GnomeAttr_setValue (JNIEnv * env,
                                             jobject self, jstring value)
{
  xmlNodePtr node;
  const xmlChar *s_value;

  node = xmljGetNodeID (env, self);
  s_value = xmljGetStringChars (env, value);
  xmlNodeSetContent (node, s_value);
}

JNIEXPORT jboolean JNICALL
Java_gnu_xml_libxmlj_dom_GnomeAttr_xmljIsId (JNIEnv * env, jobject self)
{
  xmlAttrPtr attr;

  attr = (xmlAttrPtr) xmljGetNodeID (env, self);
  return (attr->atype == XML_ATTRIBUTE_ID);
}

/* -- GnomeDocument -- */

JNIEXPORT void JNICALL
Java_gnu_xml_libxmlj_dom_GnomeDocument_free (JNIEnv * env,
                                             jobject self
					     __attribute__ ((__unused__)),
					     jobject id)
{
  xmlDocPtr doc;

  doc = (xmlDocPtr) xmljAsPointer (env, id);
  xmljFreeDoc (env, doc);
  xmlFree (doc);
}

JNIEXPORT jobject JNICALL
Java_gnu_xml_libxmlj_dom_GnomeDocument_getDoctype (JNIEnv * env, jobject self)
{
  xmlDocPtr doc;
  xmlDtdPtr dtd;

  doc = (xmlDocPtr) xmljGetNodeID (env, self);
  dtd = doc->extSubset;
  if (dtd == NULL)
    {
      dtd = doc->intSubset;
    }
  return xmljGetNodeInstance (env, (xmlNodePtr) dtd);
}

JNIEXPORT jobject JNICALL
Java_gnu_xml_libxmlj_dom_GnomeDocument_getDocumentElement (JNIEnv * env,
                                                           jobject self)
{
  xmlDocPtr doc;

  doc = (xmlDocPtr) xmljGetNodeID (env, self);
  return xmljGetNodeInstance (env, xmlDocGetRootElement (doc));
}

JNIEXPORT jobject JNICALL
Java_gnu_xml_libxmlj_dom_GnomeDocument_createDocumentType (JNIEnv * env,
                                                           jobject self,
                                                           jstring name,
                                                           jstring publicId,
                                                           jstring systemId)
{
  xmlDocPtr doc;
  xmlDtdPtr dtd;

  doc = (xmlDocPtr) xmljGetNodeID (env, self);
  dtd = xmlNewDtd (doc,
                   xmljGetStringChars (env, name),
                   xmljGetStringChars (env, publicId),
                   xmljGetStringChars (env, systemId));
  return xmljGetNodeInstance (env, (xmlNodePtr) dtd);
}

JNIEXPORT jobject JNICALL
Java_gnu_xml_libxmlj_dom_GnomeDocument_createDocumentFragment (JNIEnv * env,
                                                               jobject self)
{
  xmlDocPtr doc;

  doc = (xmlDocPtr) xmljGetNodeID (env, self);
  return xmljGetNodeInstance (env, xmlNewDocFragment (doc));
}

JNIEXPORT jobject JNICALL
Java_gnu_xml_libxmlj_dom_GnomeDocument_createTextNode (JNIEnv * env,
                                                       jobject self,
                                                       jstring data)
{
  xmlDocPtr doc;
  xmlNodePtr text;
  const xmlChar *s_data;

  doc = (xmlDocPtr) xmljGetNodeID (env, self);
  s_data = xmljGetStringChars (env, data);
  text = xmlNewDocText (doc, s_data);
  return xmljGetNodeInstance (env, text);
}

JNIEXPORT jobject JNICALL
Java_gnu_xml_libxmlj_dom_GnomeDocument_createComment (JNIEnv * env,
                                                      jobject self,
                                                      jstring data)
{
  xmlDocPtr doc;
  xmlNodePtr comment;
  const xmlChar *s_data;

  doc = (xmlDocPtr) xmljGetNodeID (env, self);
  s_data = xmljGetStringChars (env, data);
  comment = xmlNewDocComment (doc, s_data);
  return xmljGetNodeInstance (env, comment);
}

JNIEXPORT jobject JNICALL
Java_gnu_xml_libxmlj_dom_GnomeDocument_createCDATASection (JNIEnv * env,
                                                           jobject self,
                                                           jstring data)
{
  xmlDocPtr doc;
  xmlNodePtr cdata;
  const xmlChar *s_data;
  int len;

  doc = (xmlDocPtr) xmljGetNodeID (env, self);
  s_data = xmljGetStringChars (env, data);
  len = xmlStrlen (s_data);
  cdata = xmlNewCDataBlock (doc, s_data, len);
  return xmljGetNodeInstance (env, cdata);
}

JNIEXPORT jobject JNICALL
Java_gnu_xml_libxmlj_dom_GnomeDocument_createProcessingInstruction (JNIEnv *
                                                                    env,
                                                                    jobject
                                                                    self,
                                                                    jstring
                                                                    target,
                                                                    jstring
                                                                    data)
{
  xmlDocPtr doc;
  xmlNodePtr pi;
  const xmlChar *s_target;
  const xmlChar *s_data;

  doc = (xmlDocPtr) xmljGetNodeID (env, self);
  s_target = xmljGetStringChars (env, target);
  s_data = xmljGetStringChars (env, data);
  pi = xmlNewPI (s_target, s_data);
  pi->doc = doc;
  return xmljGetNodeInstance (env, pi);
}

JNIEXPORT jobject JNICALL
Java_gnu_xml_libxmlj_dom_GnomeDocument_createEntityReference (JNIEnv * env,
                                                              jobject self,
                                                              jstring name)
{
  xmlDocPtr doc;
  xmlNodePtr ref;
  const xmlChar *s_name;

  doc = (xmlDocPtr) xmljGetNodeID (env, self);
  s_name = xmljGetStringChars (env, name);
  ref = xmlNewReference (doc, s_name);
  return xmljGetNodeInstance (env, ref);
}

JNIEXPORT jobject JNICALL
Java_gnu_xml_libxmlj_dom_GnomeDocument_xmljImportNode (JNIEnv * env,
                                                       jobject self,
                                                       jobject importedNode,
                                                       jboolean deep)
{
  xmlDocPtr doc;
  xmlNodePtr node;

  doc = (xmlDocPtr) xmljGetNodeID (env, self);
  node = xmljGetNodeID (env, importedNode);
  if (node == NULL)
    {
      xmljThrowDOMException (env, 8, NULL); /* NOT_FOUND_ERR */
      return NULL;
    }
  if (node->type == XML_DOCUMENT_NODE ||
      node->type == XML_DOCUMENT_TYPE_NODE)
    {
      xmljThrowDOMException (env, 9, NULL); /* NOT_SUPPORTED_ERR */
      return NULL;
    }
  node = xmlDocCopyNode (node, doc, deep);
  return xmljGetNodeInstance (env, node);
}

JNIEXPORT jobject JNICALL
Java_gnu_xml_libxmlj_dom_GnomeDocument_createElementNS (JNIEnv * env,
                                                        jobject self,
                                                        jstring uri,
                                                        jstring qName)
{
  xmlDocPtr doc;
  xmlNodePtr element;
  xmlNsPtr ns = NULL;
  const xmlChar *s_uri;
  const xmlChar *s_qName;
  const xmlChar *s_prefix;
  const xmlChar *s_localName;

  doc = (xmlDocPtr) xmljGetNodeID (env, self);
  s_qName = xmljGetStringChars (env, qName);
  if (xmlValidateQName (s_qName, 0))
    {
      xmljThrowDOMException (env, 5, NULL); /* INVALID_CHARACTER_ERR */
      return NULL;
    }
  if (uri != NULL)
    {
      s_uri = xmljGetStringChars (env, uri);
      s_prefix = xmljGetPrefix (s_qName);
      s_localName = xmljGetLocalName (s_qName);
      ns = xmlNewNs ((xmlNodePtr) doc, s_uri, s_prefix);
    }
  element = xmlNewDocNode (doc, ns, s_qName, NULL);
  return xmljGetNodeInstance (env, element);
}

JNIEXPORT jobject JNICALL
Java_gnu_xml_libxmlj_dom_GnomeDocument_createAttributeNS (JNIEnv * env,
                                                          jobject self,
                                                          jstring uri,
                                                          jstring qName)
{
  xmlDocPtr doc;
  xmlNodePtr attr;
  xmlNsPtr ns = NULL;
  const xmlChar *s_uri;
  const xmlChar *s_qName;
  const xmlChar *s_prefix;
  const xmlChar *s_localName;

  doc = (xmlDocPtr) xmljGetNodeID (env, self);
  s_qName = xmljGetStringChars (env, qName);
  if (xmlValidateQName (s_qName, 0))
    {
      xmljThrowDOMException (env, 5, NULL); /* INVALID_CHARACTER_ERR */
      return NULL;
    }
  if (uri != NULL)
    {
      s_uri = xmljGetStringChars (env, uri);
      s_prefix = xmljGetPrefix (s_qName);
      s_localName = xmljGetLocalName (s_qName);
      ns = xmlNewNs ((xmlNodePtr) doc, s_uri, s_prefix);
    }
  attr = (xmlNodePtr) xmlNewNsProp ((xmlNodePtr) doc, ns, s_qName, NULL);
  attr->parent = NULL;
  return xmljGetNodeInstance (env, attr);
}

JNIEXPORT jobject JNICALL
Java_gnu_xml_libxmlj_dom_GnomeDocument_xmljGetElementById (JNIEnv * env,
                                                           jobject self,
                                                           jstring elementId)
{
  xmlDocPtr doc;
  xmlNodePtr ctx, tmp;
  xmlAttrPtr attr;
  const xmlChar *id;
  const xmlChar *val;

  doc = (xmlDocPtr) xmljGetNodeID (env, self);
  id = xmljGetStringChars (env, elementId);

  ctx = xmlDocGetRootElement (doc);
  while (ctx && ctx != (xmlNodePtr) doc)
    {
      if (ctx->type == XML_ELEMENT_NODE)
        {
          for (attr = ctx->properties; attr;
               attr = (xmlAttrPtr) attr->next)
            {
              if (xmlIsID (doc, ctx, attr))
                {
                  val = xmlGetProp (ctx, attr->name);
                  if (val && xmlStrEqual (id, val))
                    {
                      return xmljGetNodeInstance (env, ctx);
                    }
                }
            }
        }
      if (ctx->children)
        {
          ctx = ctx->children;
        }
      else
        {
          tmp = ctx->next;
          if (tmp)
            {
              ctx = tmp;
            }
          else
            {
              do
                {
                  tmp = ctx->parent;
                  if (!tmp)
                    {
                      return NULL;
                    }
                  ctx = tmp;
                  tmp = ctx->next;
                }
              while (!tmp);
              ctx = tmp;
            }
        }
    }
  return NULL;
}

JNIEXPORT jstring JNICALL
Java_gnu_xml_libxmlj_dom_GnomeDocument_getInputEncoding (JNIEnv * env,
                                                         jobject self)
{
  xmlDocPtr doc;

  doc = (xmlDocPtr) xmljGetNodeID (env, self);
  if (doc->encoding)
    {
      return xmljNewString (env, doc->encoding);
    }
  switch (doc->charset)
    {
    case XML_CHAR_ENCODING_ASCII:
      return xmljNewString (env, BAD_CAST "US-ASCII");
    case XML_CHAR_ENCODING_UTF16LE:
      return xmljNewString (env, BAD_CAST "UTF-16LE");
    case XML_CHAR_ENCODING_UTF16BE:
      return xmljNewString (env, BAD_CAST "UTF-16BE");
    case XML_CHAR_ENCODING_8859_1:
      return xmljNewString (env, BAD_CAST "ISO-8859-1");
      /* TODO others */
    default:
      return xmljNewString (env, BAD_CAST "UTF-8");
    }
}

JNIEXPORT jstring JNICALL
Java_gnu_xml_libxmlj_dom_GnomeDocument_getXmlEncoding (JNIEnv * env,
                                                       jobject self)
{
  xmlDocPtr doc;

  doc = (xmlDocPtr) xmljGetNodeID (env, self);
  return (doc->encoding == NULL) ? 
    xmljNewString (env, BAD_CAST "UTF-8") :
    xmljNewString (env, doc->encoding);
}

JNIEXPORT jboolean JNICALL
Java_gnu_xml_libxmlj_dom_GnomeDocument_getXmlStandalone (JNIEnv * env,
                                                         jobject self)
{
  xmlDocPtr doc;

  doc = (xmlDocPtr) xmljGetNodeID (env, self);
  return doc->standalone;
}

JNIEXPORT void JNICALL
Java_gnu_xml_libxmlj_dom_GnomeDocument_setXmlStandalone (JNIEnv * env,
                                                         jobject self,
                                                         jboolean xmlStandalone)
{
  xmlDocPtr doc;

  doc = (xmlDocPtr) xmljGetNodeID (env, self);
  doc->standalone = xmlStandalone;
}

JNIEXPORT jstring JNICALL
Java_gnu_xml_libxmlj_dom_GnomeDocument_getXmlVersion (JNIEnv * env,
                                                      jobject self)
{
  xmlDocPtr doc;

  doc = (xmlDocPtr) xmljGetNodeID (env, self);
  return (doc->version == NULL) ?
    xmljNewString (env, BAD_CAST "1.0") :
    xmljNewString (env, doc->version);
}

JNIEXPORT void JNICALL
Java_gnu_xml_libxmlj_dom_GnomeDocument_setXmlVersion (JNIEnv * env,
                                                      jobject self,
                                                      jstring xmlVersion)
{
  xmlDocPtr doc;

  doc = (xmlDocPtr) xmljGetNodeID (env, self);
  if (xmlVersion == NULL)
    {
      doc->version = NULL;
    }
  else
    {
      const xmlChar *version = xmljGetStringChars (env, xmlVersion);
      if (!xmlStrEqual (version, BAD_CAST "1.0") &&
          !xmlStrEqual (version, BAD_CAST "1.1"))
        {
          xmljThrowDOMException (env, 9, NULL); /* NOT_SUPPORTED_ERR */
          return;
        }
      doc->version = version;
    }
}

JNIEXPORT jstring JNICALL
Java_gnu_xml_libxmlj_dom_GnomeDocument_getDocumentURI (JNIEnv * env,
                                                       jobject self)
{
  xmlDocPtr doc;

  doc = (xmlDocPtr) xmljGetNodeID (env, self);
  return (doc->name == NULL) ? NULL :
    xmljNewString (env, (const xmlChar *) doc->URL);
}

JNIEXPORT void JNICALL
Java_gnu_xml_libxmlj_dom_GnomeDocument_setDocumentURI (JNIEnv * env,
                                                       jobject self,
                                                       jstring documentURI)
{
  xmlDocPtr doc;

  doc = (xmlDocPtr) xmljGetNodeID (env, self);
  if (documentURI == NULL)
    {
      doc->URL = NULL;
    }
  else
    {
      doc->URL = xmljGetStringChars (env, documentURI);
    }
}

JNIEXPORT jobject JNICALL
Java_gnu_xml_libxmlj_dom_GnomeDocument_xmljAdoptNode (JNIEnv *env,
                                                      jobject self,
                                                      jobject jnode)
{
  xmlDocPtr doc;
  xmlNodePtr node;

  doc = (xmlDocPtr) xmljGetNodeID (env, self);
  node = xmljGetNodeID (env, jnode);

  if (node == NULL)
    {
      xmljThrowDOMException (env, 8, NULL); /* NOT_FOUND_ERR */
      return NULL;
    }
  if (node->type == XML_DOCUMENT_NODE ||
      node->type == XML_DOCUMENT_TYPE_NODE ||
      node->type == XML_ENTITY_NODE ||
      node->type == XML_NOTATION_NODE)
    {
      xmljThrowDOMException (env, 9, NULL); /* NOT_SUPPORTED_ERR */
      return NULL;
    }
  xmlUnlinkNode (node);
  node = xmlDocCopyNode (node, doc, 1);
  return xmljGetNodeInstance (env, node);
}

JNIEXPORT jobject JNICALL
Java_gnu_xml_libxmlj_dom_GnomeDocument_renameNode (JNIEnv * env,
                                                   jobject self
						   __attribute__ ((__unused__)),
                                                   jobject n
 						   __attribute__ ((__unused__)),
                                                   jstring namespaceURI
						   __attribute__ ((__unused__)),
                                                   jstring qName
						   __attribute__ ((__unused__)))
{
  xmlNodePtr node;
  xmlNsPtr ns;
  const xmlChar *s_qName;
  const xmlChar *href;
  const xmlChar *prefix;
  int *len;

  node = xmljGetNodeID (env, n);
  if (node == NULL)
    {
      xmljThrowDOMException (env, 8, NULL); /* NOT_FOUND_ERR */
      return NULL;
    }
  s_qName = xmljGetStringChars (env, qName);
  if (xmlValidateQName (s_qName, 0))
    {
      xmljThrowDOMException (env, 5, NULL); /* INVALID_CHARACTER_ERR */
      return NULL;
    }
  xmlNodeSetName (node, s_qName);
  
  href = xmljGetStringChars (env, namespaceURI);
  len = (int *) malloc (sizeof (int));
  prefix = xmlSplitQName3 (s_qName, len);
  ns = node->ns;
  if (ns == NULL)
    {
      if (href != NULL)
        {
          ns = xmlNewNs (node, href, prefix);
          xmlSetNs (node, ns);
        }
    }
  else
    {
      node->ns = NULL;
      /*xmlFreeNs (ns); FIXME this can segfault (?) */
      if (href != NULL)
        {
          ns = xmlNewNs (node, href, prefix);
          xmlSetNs (node, ns);
        }
    }
  free (len);
  return n;
}

/* -- GnomeDocumentBuilder -- */

JNIEXPORT jobject JNICALL
Java_gnu_xml_libxmlj_dom_GnomeDocumentBuilder_parseStream (JNIEnv * env,
                                                           jobject self,
                                                           jobject in,
                                                           jbyteArray
                                                           detectBuffer,
                                                           jstring publicId,
                                                           jstring systemId,
                                                           jstring base,
                                                           jboolean validate,
                                                           jboolean coalesce,
                                                           jboolean
                                                           expandEntities,
                                                           jboolean
                                                           entityResolver,
                                                           jboolean
                                                           errorHandler)
{
  xmlDocPtr doc;

  doc = xmljParseDocument(env,
                          self,
                          in,
                          detectBuffer,
                          publicId,
                          systemId,
                          base,
                          validate,
                          coalesce,
                          expandEntities,
                          0,
                          0,
                          entityResolver,
                          errorHandler,
                          0,
                          0,
                          1);
  return xmljCreateDocument (env, self, doc);
}

JNIEXPORT jobject JNICALL
Java_gnu_xml_libxmlj_dom_GnomeDocumentBuilder_createDocument
(JNIEnv * env,
 jobject self,
 jstring namespaceURI,
 jstring qualifiedName,
 jobject doctype)
{
  xmlDocPtr doc;
  xmlNodePtr root;
  xmlNsPtr ns;
  const xmlChar *href;
  const xmlChar *prefix;
  const xmlChar *qName;

  qName = xmljGetStringChars (env, qualifiedName);
  href = xmljGetStringChars (env, namespaceURI);
  if (qName == NULL)
    {
      prefix = NULL;
    }
  else
    {
      int *len;
      
      len = (int *) malloc (sizeof (int));
      prefix = xmlSplitQName3 (qName, len);
      free (len);
    }
  
  /* Create the document node */
  doc = xmlNewDoc (BAD_CAST "1.0");

  /* doctype */
  if (doctype != NULL)
    {
      jclass cls;
      jmethodID method;
      jstring ret;
      const xmlChar *name;
      const xmlChar *publicId;
      const xmlChar *systemId;
      const xmlChar *internalSubset;
      xmlDtdPtr dtd;

      cls = (*env)->FindClass (env, "org/w3c/dom/DocumentType");
      if (cls == NULL)
        {
          return NULL;
        }
      /* name */
      method = (*env)->GetMethodID (env, cls, "getName",
                                    "()Ljava/lang/String;");
      if (method == NULL)
        {
          return NULL;
        }
      ret = (jstring) (*env)->CallObjectMethod (env, doctype, method);
      name = xmljGetStringChars (env, ret);
      
      /* publicId */
      method = (*env)->GetMethodID (env, cls, "getPublicId",
                                    "()Ljava/lang/String;");
      if (method == NULL)
        {
          return NULL;
        }
      ret = (jstring) (*env)->CallObjectMethod (env, doctype, method);
      publicId = xmljGetStringChars (env, ret);

      /* systemId */
      method = (*env)->GetMethodID (env, cls, "getSystemId",
                                    "()Ljava/lang/String;");
      if (method == NULL)
        {
          return NULL;
        }
      ret = (jstring) (*env)->CallObjectMethod (env, doctype, method);
      systemId = xmljGetStringChars (env, ret);

      /* internalSubset */
      method = (*env)->GetMethodID (env, cls, "getInternalSubset",
                                    "()Ljava/lang/String;");
      if (method == NULL)
        {
          return NULL;
        }
      ret = (jstring) (*env)->CallObjectMethod (env, doctype, method);
      internalSubset = xmljGetStringChars (env, ret);

      /* TODO notations */
      /* TODO entities */
      if (internalSubset == NULL)
        {
          dtd = xmlNewDtd (doc, name, publicId, systemId);
        }
      else
        {
          dtd = xmlCreateIntSubset (doc, name, publicId, systemId);
          /* TODO parse internal subset? */
          xmljThrowDOMException (env, 9, NULL); /* NOT_SUPPORTED_ERR */
          return NULL;
        }
    }
  
  /* Create the root element */
  root = xmlNewNode (NULL, qName);
  xmlDocSetRootElement (doc, root);
  ns = xmlNewNs (root, href, prefix);
  xmlSetNs (root, ns);
  
  return xmljCreateDocument (env, self, doc);
}

/* -- GnomeDocumentType -- */

JNIEXPORT jstring JNICALL
Java_gnu_xml_libxmlj_dom_GnomeDocumentType_getPublicId (JNIEnv * env,
                                                        jobject self)
{
  xmlDtdPtr dtd;

  dtd = (xmlDtdPtr) xmljGetNodeID (env, self);
  return xmljNewString (env, dtd->ExternalID);
}

JNIEXPORT jstring JNICALL
Java_gnu_xml_libxmlj_dom_GnomeDocumentType_getSystemId (JNIEnv * env,
                                                        jobject self)
{
  xmlDtdPtr dtd;

  dtd = (xmlDtdPtr) xmljGetNodeID (env, self);
  return xmljNewString (env, dtd->SystemID);
}

JNIEXPORT jstring JNICALL
Java_gnu_xml_libxmlj_dom_GnomeDocumentType_getInternalSubset (JNIEnv * env,
                                                             jobject self
							     __attribute__ ((__unused__)))
{
  /* TODO */
  xmljThrowDOMException (env, 9, NULL); /* NOT_SUPPORTED_ERR */
  return NULL;
}

/* -- GnomeElement -- */

JNIEXPORT jstring JNICALL
Java_gnu_xml_libxmlj_dom_GnomeElement_getAttribute (JNIEnv * env,
                                                    jobject self,
                                                    jstring name)
{
  xmlNodePtr node;
  const xmlChar *s_name;
  const xmlChar *s_value;
  
  node = xmljGetNodeID (env, self);
  s_name = xmljGetStringChars (env, name);
  s_value = xmlGetProp (node, s_name);
  xmlFree ((xmlChar *) s_name);
  return (s_value == NULL) ?
    xmljNewString (env, BAD_CAST "") :
    xmljNewString (env, s_value);
}

JNIEXPORT void JNICALL
Java_gnu_xml_libxmlj_dom_GnomeElement_setAttribute (JNIEnv * env,
                                                    jobject self,
                                                    jstring name,
                                                    jstring value)
{
  xmlNodePtr node;
  const xmlChar *s_name;
  const xmlChar *s_value;
  
  node = xmljGetNodeID (env, self);
  s_name = xmljGetStringChars (env, name);
  if (xmlValidateName (s_name, 0))
    {
      xmljThrowDOMException (env, 5, NULL); /* INVALID_CHARACTER_ERR */
      return;
    }
  s_value = xmljGetStringChars (env, value);
  xmlSetProp (node, s_name, s_value);
}

JNIEXPORT jobject JNICALL
Java_gnu_xml_libxmlj_dom_GnomeElement_getAttributeNode (JNIEnv * env,
                                                        jobject self,
                                                        jstring name)
{
  xmlNodePtr node;
  const xmlChar *s_name;
  xmlAttrPtr attr;

  node = xmljGetNodeID (env, self);
  s_name = xmljGetStringChars (env, name);
  attr = xmlHasProp (node, s_name);
  if (attr == NULL)
    {
      return NULL;
    }
  xmlFree ((xmlChar *) s_name);
  return xmljGetNodeInstance (env, (xmlNodePtr) attr);
}

JNIEXPORT jobject JNICALL
Java_gnu_xml_libxmlj_dom_GnomeElement_setAttributeNode (JNIEnv * env,
                                                        jobject self,
                                                        jobject newAttr)
{
  xmlNodePtr node;
  xmlAttrPtr new_attr;
  xmlAttrPtr old_attr;

  node = xmljGetNodeID (env, self);
  new_attr = (xmlAttrPtr) xmljGetNodeID (env, newAttr);
  if (new_attr->parent != NULL)
    {
      xmljThrowDOMException (env, 10, NULL); /* INUSE_ATTRIBUTE_ERR */
      return NULL;
    }
  if (new_attr->doc != node->doc)
    {
      xmljThrowDOMException (env, 4, NULL); /* WRONG_DOCUMENT_ERR */
      return NULL;
    }
  old_attr = xmlHasProp (node, new_attr->name);
  if (old_attr)
    {
      xmlUnlinkNode ((xmlNodePtr) old_attr);
    }
  xmljAddAttribute (node, new_attr);
  return xmljGetNodeInstance (env, (xmlNodePtr) old_attr);
}

JNIEXPORT jobject JNICALL
Java_gnu_xml_libxmlj_dom_GnomeElement_removeAttributeNode (JNIEnv * env,
                                                           jobject self
							   __attribute__ ((__unused__)),
                                                           jobject oldAttr)
{
  xmlNodePtr attr;

  attr = xmljGetNodeID (env, oldAttr);
  xmlUnlinkNode (attr);
  return oldAttr;
}

JNIEXPORT jstring JNICALL
Java_gnu_xml_libxmlj_dom_GnomeElement_getAttributeNS (JNIEnv * env,
                                                      jobject self,
                                                      jstring uri,
                                                      jstring localName)
{
  xmlNodePtr node;
  const xmlChar *s_uri;
  const xmlChar *s_localName;
  const xmlChar *s_value;

  node = xmljGetNodeID (env, self);
  s_localName = xmljGetStringChars (env, localName);
  if (uri == NULL)
    {
      s_value = xmlGetNoNsProp (node, s_localName);
    }
  else
    {
      s_uri = xmljGetStringChars (env, uri);
      s_value = xmlGetNsProp (node, s_localName, s_uri);
      xmlFree ((xmlChar *) s_uri);
    }
  xmlFree ((xmlChar *) s_localName);
  return (s_value == NULL) ?
    xmljNewString (env, BAD_CAST "") :
    xmljNewString (env, s_value);
}

JNIEXPORT void JNICALL
Java_gnu_xml_libxmlj_dom_GnomeElement_setAttributeNS (JNIEnv * env,
                                                      jobject self,
                                                      jstring uri,
                                                      jstring qName,
                                                      jstring value)
{
  xmlNodePtr node;
  xmlNsPtr ns;
  const xmlChar *s_uri;
  const xmlChar *s_qName;
  const xmlChar *s_prefix;
  const xmlChar *s_localName;
  const xmlChar *s_value;

  node = xmljGetNodeID (env, self);
  s_qName = xmljGetStringChars (env, qName);
  if (xmlValidateQName (s_qName, 0))
    {
      xmljThrowDOMException (env, 5, NULL); /* INVALID_CHARACTER_ERR */
      return;
    }
  s_value = xmljGetStringChars (env, value);
  if (uri == NULL)
    {
      xmlSetProp (node, s_qName, s_value);
    }
  else
    {
      s_prefix = xmljGetPrefix (s_qName);
      s_localName = xmljGetLocalName (s_qName);
      s_uri = xmljGetStringChars (env, uri);
      ns = xmlNewNs (node, s_uri, s_prefix);
      xmlSetNsProp (node, ns, s_localName, s_value);
    }
}

JNIEXPORT jobject JNICALL
Java_gnu_xml_libxmlj_dom_GnomeElement_getAttributeNodeNS (JNIEnv * env,
                                                          jobject self,
                                                          jstring uri,
                                                          jstring localName)
{
  xmlNodePtr node;
  xmlAttrPtr attr;
  const xmlChar *s_uri;
  const xmlChar *s_localName;

  node = xmljGetNodeID (env, self);
  attr = node->properties;
  s_uri = xmljGetStringChars (env, uri);
  s_localName = xmljGetStringChars (env, localName);
  while (attr != NULL)
    {
      if (uri == NULL)
        {
          if (xmljMatch (s_localName, (xmlNodePtr) attr))
            break;
        }
      else
        {
          if (xmljMatchNS (s_uri, s_localName, (xmlNodePtr) attr))
            break;
        }
      attr = attr->next;
    }
  xmlFree ((xmlChar *) s_uri);
  xmlFree ((xmlChar *) s_localName);
  return xmljGetNodeInstance (env, (xmlNodePtr) attr);
}

JNIEXPORT jobject JNICALL
Java_gnu_xml_libxmlj_dom_GnomeElement_setAttributeNodeNS (JNIEnv * env,
                                                          jobject self,
                                                          jobject newAttr)
{
  xmlNodePtr node;
  xmlAttrPtr new_attr;
  xmlAttrPtr old_attr;
  const xmlChar *uri;

  node = xmljGetNodeID (env, self);
  new_attr = (xmlAttrPtr) xmljGetNodeID (env, newAttr);
  if (new_attr->parent != NULL)
    {
      xmljThrowDOMException (env, 10, NULL); /* INUSE_ATTRIBUTE_ERR */
      return NULL;
    }
  if (new_attr->doc != node->doc)
    {
      xmljThrowDOMException (env, 4, NULL); /* WRONG_DOCUMENT_ERR */
      return NULL;
    }
  uri = (new_attr->ns != NULL) ? new_attr->ns->href : NULL;
  old_attr = xmlHasNsProp (node, new_attr->name, uri);
  if (old_attr)
    {
      xmlUnlinkNode ((xmlNodePtr) old_attr);
    }
  xmljAddAttribute (node, new_attr);
  return xmljGetNodeInstance (env, (xmlNodePtr) old_attr);
}

JNIEXPORT jboolean JNICALL
Java_gnu_xml_libxmlj_dom_GnomeElement_hasAttribute (JNIEnv * env,
                                                    jobject self,
                                                    jstring name)
{
  xmlNodePtr node;
  const xmlChar *s_name;
  const xmlChar *s_value;
  
  node = xmljGetNodeID (env, self);
  s_name = xmljGetStringChars (env, name);
  s_value = xmlGetProp (node, s_name);
  xmlFree ((xmlChar *) s_name);
  return (s_value != NULL);
}

JNIEXPORT jboolean JNICALL
Java_gnu_xml_libxmlj_dom_GnomeElement_hasAttributeNS (JNIEnv * env,
                                                      jobject self,
                                                      jstring uri,
                                                      jstring localName)
{
  xmlNodePtr node;
  const xmlChar *s_uri;
  const xmlChar *s_localName;
  const xmlChar *s_value;

  node = xmljGetNodeID (env, self);
  s_localName = xmljGetStringChars (env, localName);
  if (uri == NULL)
    {
      s_value = xmlGetNoNsProp (node, s_localName);
    }
  else
    {
      s_uri = xmljGetStringChars (env, uri);
      s_value = xmlGetNsProp (node, s_localName, s_uri);
      xmlFree ((xmlChar *) s_uri);
    }
  xmlFree ((xmlChar *) s_localName);
  return (s_value != NULL);
}

/* -- GnomeEntity -- */

JNIEXPORT jstring JNICALL
Java_gnu_xml_libxmlj_dom_GnomeEntity_getPublicId (JNIEnv * env, jobject self)
{
  xmlEntityPtr entity;

  entity = (xmlEntityPtr) xmljGetNodeID (env, self);
  return xmljNewString (env, entity->ExternalID);
}

JNIEXPORT jstring JNICALL
Java_gnu_xml_libxmlj_dom_GnomeEntity_getSystemId (JNIEnv * env, jobject self)
{
  xmlEntityPtr entity;

  entity = (xmlEntityPtr) xmljGetNodeID (env, self);
  return xmljNewString (env, entity->SystemID);
}

JNIEXPORT jstring JNICALL
Java_gnu_xml_libxmlj_dom_GnomeEntity_getNotationName (JNIEnv * env,
                                                      jobject self
						      __attribute__ ((__unused__)))
{
  /* TODO */
  xmljThrowDOMException (env, 9, NULL); /* NOT_SUPPORTED_ERR */
  return NULL;
}

/* -- GnomeNamedNodeMap -- */

JNIEXPORT jobject JNICALL
Java_gnu_xml_libxmlj_dom_GnomeNamedNodeMap_getNamedItem (JNIEnv * env,
                                                         jobject self,
                                                         jstring name)
{
  jclass cls;
  jfieldID field;
  jint type;

  cls = (*env)->GetObjectClass (env, self);
  field = (*env)->GetFieldID (env, cls, "type", "I");
  type = (*env)->GetIntField (env, self, field);

  if (type == 0)
    {
      xmlAttrPtr attr;
      
      attr = xmljGetNamedItem (env, self, name);
      return xmljGetNodeInstance (env, (xmlNodePtr) attr);
    }
  else
    {
      xmlDtdPtr dtd;
      xmlHashTablePtr hash;
      const xmlChar *s_name;
      xmlNodePtr ret;

      dtd = (xmlDtdPtr) xmljGetNodeID (env, self);
      hash = (xmlHashTablePtr) ((type == 1) ? dtd->entities : dtd->notations);
      if (hash == NULL)
        {
          return NULL;
        }
      s_name = xmljGetStringChars (env, name);
      ret = (xmlNodePtr) xmlHashLookup (hash, s_name);
      xmlFree ((xmlChar *) s_name);
      return xmljGetNodeInstance (env, ret);
    }
}

JNIEXPORT jobject JNICALL
Java_gnu_xml_libxmlj_dom_GnomeNamedNodeMap_setNamedItem (JNIEnv * env,
                                                         jobject self,
                                                         jobject arg)
{
  jclass cls;
  jfieldID field;
  jint type;
  xmlNodePtr node;
  xmlNodePtr argNode;

  cls = (*env)->GetObjectClass (env, self);
  field = (*env)->GetFieldID (env, cls, "type", "I");
  type = (*env)->GetIntField (env, self, field);

  node = xmljGetNodeID (env, self);
  argNode = xmljGetNodeID (env, arg);

  if (argNode->doc != node->doc)
    {
      xmljThrowDOMException (env, 4, NULL);	/* WRONG_DOCUMENT_ERR */
    }
  xmljValidateChildNode (env, node, argNode);
  if ((*env)->ExceptionOccurred (env))
    {
      return NULL;
    }
  if (type == 0)
    {
      if (argNode->parent != NULL)
        {
          xmljThrowDOMException (env, 10, NULL);  /* INUSE_ATTRIBUTE_ERR */
          return NULL;
        }
      xmlAddChild (node, argNode);
    }
  else
    {
      xmlDtdPtr dtd;
      xmlHashTablePtr hash;

      dtd = (xmlDtdPtr) xmljGetNodeID (env, self);
      hash = (xmlHashTablePtr) ((type == 1) ? dtd->entities : dtd->notations);
      if (hash == NULL)
        {
          hash = xmlHashCreate (10);
          if (type == 1)
            {
              dtd->entities = hash;
            }
          else
            {
              dtd->notations = hash;
            }
        }
      xmlHashAddEntry (hash, argNode->name, argNode);
    }
  return arg;
}

JNIEXPORT jobject JNICALL
Java_gnu_xml_libxmlj_dom_GnomeNamedNodeMap_removeNamedItem (JNIEnv * env,
                                                            jobject self,
                                                            jstring name)
{
  jclass cls;
  jfieldID field;
  jint type;

  cls = (*env)->GetObjectClass (env, self);
  field = (*env)->GetFieldID (env, cls, "type", "I");
  type = (*env)->GetIntField (env, self, field);

  if (type == 0)
    {
      xmlAttrPtr attr;

      attr = xmljGetNamedItem (env, self, name);
      if (attr == NULL)
        {
          xmljThrowDOMException (env, 8, NULL);	/* NOT_FOUND_ERR */
          return NULL;
        }
      xmlUnlinkNode ((xmlNodePtr) attr);
      return xmljGetNodeInstance (env, (xmlNodePtr) attr);
    }
  else
    {
      xmlDtdPtr dtd;
      xmlHashTablePtr hash;
      const xmlChar *s_name;
      xmlNodePtr ret;

      dtd = (xmlDtdPtr) xmljGetNodeID (env, self);
      hash = (xmlHashTablePtr) ((type == 1) ? dtd->entities : dtd->notations);
      if (hash == NULL)
        {
          return NULL;
        }
      s_name = xmljGetStringChars (env, name);
      ret = (xmlNodePtr) xmlHashLookup (hash, s_name);
      if (ret != NULL)
        {
          xmlHashRemoveEntry (hash, s_name, NULL);
        }
      xmlFree ((xmlChar *) s_name);
      return xmljGetNodeInstance (env, ret);
    }
}

void
xmljHashScanner (void *payload, void *vdata, xmlChar *name)
{
  xmljHashScanData *data;

  data = (xmljHashScanData *) vdata;
  if (data->count <= data->index)
    {
      data->node = (xmlNodePtr) payload;
    }
  data->count++;
}

JNIEXPORT jobject JNICALL
Java_gnu_xml_libxmlj_dom_GnomeNamedNodeMap_item (JNIEnv * env,
                                                 jobject self, jint index)
{
  jclass cls;
  jfieldID field;
  jint type;

  cls = (*env)->GetObjectClass (env, self);
  field = (*env)->GetFieldID (env, cls, "type", "I");
  type = (*env)->GetIntField (env, self, field);

  if (type == 0)
    {
      xmlNodePtr node;
      xmlAttrPtr attr;
      jint count;
      
      node = xmljGetNodeID (env, self);
      switch (node->type)
        {
        case XML_ELEMENT_NODE:
          attr = node->properties;
          for (count = 0; attr != NULL && count < index; count++)
            {
              attr = attr->next;
            }
          if (attr == NULL)
            {
              char msg[1024];
              sprintf (msg, "No attribute at index %d\n", (int) index);
              xmljThrowException (env, "java/lang/NullPointerException", msg);
              return NULL;
            }
          return xmljGetNodeInstance (env, (xmlNodePtr) attr);
        default:
          return NULL;
        }
    }
  else
    {
      xmlDtdPtr dtd;
      xmlHashTablePtr hash;
      xmljHashScanData *data;
      xmlNodePtr ret;

      dtd = (xmlDtdPtr) xmljGetNodeID (env, self);
      hash = (xmlHashTablePtr) ((type == 1) ? dtd->entities : dtd->notations);
      if (hash == NULL)
        {
          return NULL;
        }
      data = (xmljHashScanData *) malloc (sizeof (xmljHashScanData));
      if (data == NULL)
        {
          return NULL;
        }
      data->index = index;
      data->count = 0;
      data->node = NULL;
      xmlHashScan (hash, xmljHashScanner, data);
      ret = data->node;
      free (data);
      return xmljGetNodeInstance (env, ret);
    }
}

JNIEXPORT jint JNICALL
Java_gnu_xml_libxmlj_dom_GnomeNamedNodeMap_getLength (JNIEnv * env,
                                                      jobject self)
{
  jclass cls;
  jfieldID field;
  jint type;

  cls = (*env)->GetObjectClass (env, self);
  field = (*env)->GetFieldID (env, cls, "type", "I");
  type = (*env)->GetIntField (env, self, field);

  if (type == 0)
    {
      xmlNodePtr node;
      xmlAttrPtr attr;
      jint count;
      
      node = xmljGetNodeID (env, self);
      switch (node->type)
        {
        case XML_ELEMENT_NODE:
          count = 0;
          attr = node->properties;
          while (attr != NULL)
            {
              count++;
              attr = attr->next;
            }
          return count;
        default:
          return -1;
        }
    }
  else
    {
      xmlDtdPtr dtd;
      xmlHashTablePtr hash;
      xmljHashScanData *data;
      jint ret;

      dtd = (xmlDtdPtr) xmljGetNodeID (env, self);
      hash = (xmlHashTablePtr) ((type == 1) ? dtd->entities : dtd->notations);
      if (hash == NULL)
        {
          return 0;
        }
      data = (xmljHashScanData *) malloc (sizeof (xmljHashScanData));
      if (data == NULL)
        {
          return 0;
        }
      data->index = -1;
      data->count = 0;
      data->node = NULL;
      xmlHashScan (hash, xmljHashScanner, data);
      ret = data->count;
      free (data);
      return ret;
    }
}

JNIEXPORT jobject JNICALL
Java_gnu_xml_libxmlj_dom_GnomeNamedNodeMap_getNamedItemNS (JNIEnv * env,
                                                           jobject self,
                                                           jstring uri,
                                                           jstring localName)
{
  jclass cls;
  jfieldID field;
  jint type;

  cls = (*env)->GetObjectClass (env, self);
  field = (*env)->GetFieldID (env, cls, "type", "I");
  type = (*env)->GetIntField (env, self, field);

  if (type == 0)
    {
      xmlAttrPtr attr;

      attr = xmljGetNamedItemNS (env, self, uri, localName);
      return xmljGetNodeInstance (env, (xmlNodePtr) attr);
    }
  else
    {
      return NULL;
    }
}

JNIEXPORT jobject JNICALL
Java_gnu_xml_libxmlj_dom_GnomeNamedNodeMap_setNamedItemNS (JNIEnv * env,
                                                           jobject self,
                                                           jobject arg)
{
  return Java_gnu_xml_libxmlj_dom_GnomeNamedNodeMap_setNamedItem (env, self,
                                                                  arg);
}

JNIEXPORT jobject JNICALL
Java_gnu_xml_libxmlj_dom_GnomeNamedNodeMap_removeNamedItemNS (JNIEnv * env,
                                                              jobject self,
                                                              jstring uri,
                                                              jstring
                                                              localName)
{
  jclass cls;
  jfieldID field;
  jint type;

  cls = (*env)->GetObjectClass (env, self);
  field = (*env)->GetFieldID (env, cls, "type", "I");
  type = (*env)->GetIntField (env, self, field);

  if (type == 0)
    {
      xmlAttrPtr attr;
      
      attr = xmljGetNamedItemNS (env, self, uri, localName);
      if (attr == NULL)
        {
          xmljThrowDOMException (env, 8, NULL);	/* NOT_FOUND_ERR */
          return NULL;
        }
      else
        {
          xmlUnlinkNode ((xmlNodePtr) attr);
          return xmljGetNodeInstance (env, (xmlNodePtr) attr);
        }
    }
  else
    {
      return NULL;
    }
}

/* -- GnomeNode -- */

JNIEXPORT jstring JNICALL
Java_gnu_xml_libxmlj_dom_GnomeNode_getNodeName (JNIEnv * env, jobject self)
{
  xmlNodePtr node;

  node = xmljGetNodeID (env, self);
  if (node == NULL)
    {
      return NULL;
    }
  return xmljNewString (env, node->name);
}

xmlChar *
xmljGetNodeValue (xmlNodePtr node)
{
  /* If not character data, return null */
  switch (node->type)
    {
    case XML_TEXT_NODE:
    case XML_CDATA_SECTION_NODE:
    case XML_COMMENT_NODE:
    case XML_ATTRIBUTE_NODE:
      return xmlNodeGetContent (node);
    default:
      return NULL;
    }
}

JNIEXPORT jstring JNICALL
Java_gnu_xml_libxmlj_dom_GnomeNode_getNodeValue (JNIEnv * env, jobject self)
{
  xmlNodePtr node;
  xmlChar *text;
  jstring ret;

  node = xmljGetNodeID (env, self);
  text = xmljGetNodeValue (node);
  ret = xmljNewString (env, (const xmlChar *) text);
  if (text != NULL)
    {
      xmlFree (text);
    }
  return ret;
}

JNIEXPORT void JNICALL
Java_gnu_xml_libxmlj_dom_GnomeNode_setNodeValue (JNIEnv * env,
                                                 jobject self,
                                                 jstring nodeValue)
{
  xmlNodePtr node;
  const xmlChar *s_nodeValue;

  node = xmljGetNodeID (env, self);

  /* If not character data, return */
  if (node->type != XML_TEXT_NODE &&
      node->type != XML_CDATA_SECTION_NODE && node->type != XML_COMMENT_NODE)
    return;

  s_nodeValue = xmljGetStringChars (env, nodeValue);
  xmlNodeSetContent (node, s_nodeValue);
}

JNIEXPORT jshort JNICALL
Java_gnu_xml_libxmlj_dom_GnomeNode_getNodeType (JNIEnv * env, jobject self)
{
  xmlNodePtr node;

  node = xmljGetNodeID (env, self);
  switch (node->type)
    {
    case XML_DTD_NODE:
      return XML_DOCUMENT_TYPE_NODE;
    case XML_ATTRIBUTE_DECL:
      return XML_ATTRIBUTE_NODE;
    case XML_ENTITY_DECL:
      return XML_ENTITY_NODE;
    default:
      return node->type;
    }
}

JNIEXPORT jobject JNICALL
Java_gnu_xml_libxmlj_dom_GnomeNode_getParentNode (JNIEnv * env, jobject self)
{
  xmlNodePtr node;

  node = xmljGetNodeID (env, self);
  return xmljGetNodeInstance (env, node->parent);
}

JNIEXPORT jobject JNICALL
Java_gnu_xml_libxmlj_dom_GnomeNode_getFirstChild (JNIEnv * env, jobject self)
{
  xmlNodePtr node;

  node = xmljGetNodeID (env, self);
  return xmljGetNodeInstance (env, node->children);
}

JNIEXPORT jobject JNICALL
Java_gnu_xml_libxmlj_dom_GnomeNode_getLastChild (JNIEnv * env, jobject self)
{
  xmlNodePtr node;

  node = xmljGetNodeID (env, self);
  return xmljGetNodeInstance (env, node->last);
}

JNIEXPORT jobject JNICALL
Java_gnu_xml_libxmlj_dom_GnomeNode_getPreviousSibling (JNIEnv * env,
                                                       jobject self)
{
  xmlNodePtr node;

  node = xmljGetNodeID (env, self);
  return xmljGetNodeInstance (env, node->prev);
}

JNIEXPORT jobject JNICALL
Java_gnu_xml_libxmlj_dom_GnomeNode_getNextSibling (JNIEnv * env, jobject self)
{
  xmlNodePtr node;

  node = xmljGetNodeID (env, self);
  return xmljGetNodeInstance (env, node->next);
}

JNIEXPORT jobject JNICALL
Java_gnu_xml_libxmlj_dom_GnomeNode_getOwnerDocument (JNIEnv * env,
                                                     jobject self)
{
  xmlNodePtr node;

  node = xmljGetNodeID (env, self);
  return xmljGetNodeInstance (env, (xmlNodePtr) node->doc);
}

JNIEXPORT jobject JNICALL
Java_gnu_xml_libxmlj_dom_GnomeNode_xmljInsertBefore (JNIEnv * env,
                                                     jobject self,
                                                     jobject newChild,
                                                     jobject refChild)
{
  xmlNodePtr node;
  xmlNodePtr newChildNode;
  xmlNodePtr refChildNode;

  node = xmljGetNodeID (env, self);
  newChildNode = xmljGetNodeID (env, newChild);
  refChildNode = xmljGetNodeID (env, refChild);

  /* Is refChildNode a child of this node? */
  if (refChildNode == NULL ||
      refChildNode->parent == NULL ||
      refChildNode->parent != node)
    {
      xmljThrowDOMException (env, 8, NULL);	/* NOT_FOUND_ERR */
      return NULL;
    }
  /* Check new child */
  xmljValidateChildNode (env, node, newChildNode);
  if ((*env)->ExceptionOccurred (env))
    {
      return NULL;
    }

  newChildNode = xmlAddPrevSibling (refChildNode, newChildNode);
  return xmljGetNodeInstance (env, newChildNode);
}

JNIEXPORT jobject JNICALL
Java_gnu_xml_libxmlj_dom_GnomeNode_xmljReplaceChild (JNIEnv * env,
                                                     jobject self,
                                                     jobject newChild,
                                                     jobject oldChild)
{
  xmlNodePtr node;
  xmlNodePtr newChildNode;
  xmlNodePtr oldChildNode;

  node = xmljGetNodeID (env, self);
  newChildNode = xmljGetNodeID (env, newChild);
  oldChildNode = xmljGetNodeID (env, oldChild);

  /* Is oldChildNode a child of this node? */
  if (oldChildNode == NULL ||
      oldChildNode->parent == NULL ||
      oldChildNode->parent != node)
    {
      xmljThrowDOMException (env, 8, NULL);	/* NOT_FOUND_ERR */
      return NULL;
    }
  /* Check new child */
  xmljValidateChildNode (env, node, newChildNode);
  if ((*env)->ExceptionOccurred (env))
    {
      return NULL;
    }
  
  newChildNode = xmlReplaceNode (oldChildNode, newChildNode);
  return xmljGetNodeInstance (env, newChildNode);
}

JNIEXPORT jobject JNICALL
Java_gnu_xml_libxmlj_dom_GnomeNode_xmljRemoveChild (JNIEnv * env,
                                                    jobject self,
                                                    jobject oldChild)
{
  xmlNodePtr node;
  xmlNodePtr oldChildNode;

  node = xmljGetNodeID (env, self);
  oldChildNode = xmljGetNodeID (env, oldChild);

  if (oldChildNode == NULL ||
      oldChildNode->parent == NULL ||
      oldChildNode->parent != node)
    {
      xmljThrowDOMException (env, 8, NULL);	/* NOT_FOUND_ERR */
      return NULL;
    }
  xmlUnlinkNode (oldChildNode);
  return oldChild;
}

JNIEXPORT jobject JNICALL
Java_gnu_xml_libxmlj_dom_GnomeNode_xmljAppendChild (JNIEnv * env,
                                                    jobject self,
                                                    jobject newChild)
{
  xmlNodePtr node;
  xmlNodePtr newChildNode;

  node = xmljGetNodeID (env, self);
  newChildNode = xmljGetNodeID (env, newChild);
  
  /* Check new child */
  xmljValidateChildNode (env, node, newChildNode);
  if ((*env)->ExceptionOccurred (env))
    {
      return NULL;
    }

  newChildNode = xmlAddChild (node, newChildNode);
  return xmljGetNodeInstance (env, newChildNode);
}

JNIEXPORT jboolean JNICALL
Java_gnu_xml_libxmlj_dom_GnomeNode_hasChildNodes (JNIEnv * env, jobject self)
{
  xmlNodePtr node;

  node = xmljGetNodeID (env, self);
  return (node->children != NULL);
}

JNIEXPORT jobject JNICALL
Java_gnu_xml_libxmlj_dom_GnomeNode_xmljCloneNode (JNIEnv * env,
                                                  jobject self, jboolean deep)
{
  xmlNodePtr node;
  xmlNodePtr clone;

  node = xmljGetNodeID (env, self);
  clone = xmlCopyNode (node, deep);
  clone->parent = NULL;
  clone->doc = node->doc;
  return xmljGetNodeInstance (env, clone);
}

JNIEXPORT void JNICALL
Java_gnu_xml_libxmlj_dom_GnomeNode_normalize (JNIEnv * env, jobject self)
{
  xmlNodePtr node;

  node = xmljGetNodeID (env, self);
  xmljNormalizeNode (node);
}

void
xmljNormalizeNode (xmlNodePtr node)
{
  xmlNodePtr cur;
  xmlNodePtr last = NULL;

  cur = node->children;
  while (cur != NULL)
    {
      switch (cur->type)
        {
        case XML_CDATA_SECTION_NODE:
        case XML_TEXT_NODE:
          if (xmlIsBlankNode (cur))
            {
              xmlNodePtr next = cur->next;
              xmlUnlinkNode (cur);
              xmlFreeNode (cur);
              cur = next;
              continue;
            }
          if (last != NULL)
            {
              last = xmlTextMerge (last, cur);
              xmlUnlinkNode (cur);
              xmlFreeNode (cur);
              cur = last;
            }
          else
            {
              last = cur;
            }
          break;
        default:
          last = NULL;
          xmljNormalizeNode (cur);
        }
      cur = cur->next;
    }
}

JNIEXPORT jstring JNICALL
Java_gnu_xml_libxmlj_dom_GnomeNode_getNamespaceURI (JNIEnv * env,
                                                    jobject self)
{
  xmlNodePtr node;

  node = xmljGetNodeID (env, self);
  if (node->type != XML_ELEMENT_NODE &&
      node->type != XML_ATTRIBUTE_NODE)
    {
      return NULL;
    }
  if (node->ns == NULL)
    {
      return NULL;
    }
  return xmljNewString (env, node->ns->href);
}

JNIEXPORT jstring JNICALL
Java_gnu_xml_libxmlj_dom_GnomeNode_getPrefix (JNIEnv * env, jobject self)
{
  xmlNodePtr node;

  node = xmljGetNodeID (env, self);
  if (node->type != XML_ELEMENT_NODE &&
      node->type != XML_ATTRIBUTE_NODE)
    {
      return NULL;
    }
  if (node->ns == NULL)
    {
      return NULL;
    }
  return xmljNewString (env, node->ns->prefix);
}

JNIEXPORT void JNICALL
Java_gnu_xml_libxmlj_dom_GnomeNode_setPrefix (JNIEnv * env,
                                              jobject self, jstring prefix)
{
  xmlNodePtr node;
  const xmlChar *s_prefix;

  s_prefix = xmljGetStringChars (env, prefix);
  if (xmlValidateName (s_prefix, 0))
    {
      xmljThrowDOMException (env, 5, NULL);	/* INVALID_CHARACTER_ERR */
    }
  node = xmljGetNodeID (env, self);
  if (node->type != XML_ELEMENT_NODE &&
      node->type != XML_ATTRIBUTE_NODE)
    {
      xmljThrowDOMException (env, 3, NULL);	/* HIERARCHY_REQUEST_ERR */
      return;
    }
  if (node->ns == NULL)
    {
      xmljThrowDOMException (env, 14, NULL);	/* NAMESPACE_ERR */
      return;
    }
  node->ns->prefix = s_prefix;
}

JNIEXPORT jstring JNICALL
Java_gnu_xml_libxmlj_dom_GnomeNode_getLocalName (JNIEnv * env, jobject self)
{
  xmlNodePtr node;
  int *len;
  jstring ret;

  node = xmljGetNodeID (env, self);
  if (node->name == NULL)
    {
      return NULL;
    }
  len = (int *) malloc (sizeof (int));
  if (xmlSplitQName3 (node->name, len) != NULL)
    {
      ret = xmljNewString (env, node->name + (*len));
    }
  else
    {
      ret = xmljNewString (env, node->name);
    }
  free (len);
  return ret;
}

JNIEXPORT jboolean JNICALL
Java_gnu_xml_libxmlj_dom_GnomeNode_hasAttributes (JNIEnv * env, jobject self)
{
  xmlNodePtr node;

  node = xmljGetNodeID (env, self);
  return (node->properties != NULL);
}

JNIEXPORT jstring JNICALL
Java_gnu_xml_libxmlj_dom_GnomeNode_getBaseURI (JNIEnv * env, jobject self)
{
  xmlNodePtr node;
  xmlChar *baseURI;
  jstring ret;
  
  node = xmljGetNodeID (env, self);
  baseURI = xmlNodeGetBase (node->doc, node);
  ret = xmljNewString (env, (const xmlChar *) baseURI);
  if (baseURI != NULL)
    {
      xmlFree (baseURI);
    }
  return ret;
}

JNIEXPORT jstring JNICALL
Java_gnu_xml_libxmlj_dom_GnomeNode_lookupPrefix (JNIEnv * env, jobject self,
                                                 jstring namespaceURI)
{
  xmlNodePtr node;
  xmlNsPtr ns;
  xmlDocPtr doc;
  const xmlChar *s_uri;
  
  node = xmljGetNodeID (env, self);
  doc = node->doc;
  /* If this is a document node, search from the root element */
  if (node->type == XML_DOCUMENT_NODE)
    {
      doc = (xmlDocPtr) node;
      node = xmlDocGetRootElement (doc);
    }
  s_uri = xmljGetStringChars (env, namespaceURI);
  ns = xmlSearchNsByHref (doc, node, s_uri);
  xmlFree ((xmlChar *) s_uri);
  if (ns == NULL)
    {
      return NULL;
    }
  return xmljNewString (env, ns->prefix);
}

JNIEXPORT jboolean JNICALL
Java_gnu_xml_libxmlj_dom_GnomeNode_isDefaultNamespace (JNIEnv * env,
                                                       jobject self,
                                                       jstring namespaceURI)
{
  xmlNodePtr node;
  xmlNsPtr ns;
  const xmlChar *s_uri;
  
  node = xmljGetNodeID (env, self);
  s_uri = xmljGetStringChars (env, namespaceURI);
  ns = xmlSearchNsByHref (node->doc, node, s_uri);
  xmlFree ((xmlChar *) s_uri);
  if (ns == NULL)
    {
      return 0;
    }
  return (ns->prefix == NULL || xmlStrlen (ns->prefix) == 0);
}

JNIEXPORT jstring JNICALL
Java_gnu_xml_libxmlj_dom_GnomeNode_lookupNamespaceURI (JNIEnv * env,
                                                       jobject self,
                                                       jstring prefix)
{
  xmlNodePtr node;
  xmlDocPtr doc;
  xmlNsPtr ns;
  const xmlChar *s_prefix;
  
  node = xmljGetNodeID (env, self);
  doc = node->doc;
  /* If this is a document node, search from the root element */
  if (node->type == XML_DOCUMENT_NODE)
    {
      doc = (xmlDocPtr) node;
      node = xmlDocGetRootElement (doc);
    }
  s_prefix = xmljGetStringChars (env, prefix);
  ns = xmlSearchNs (doc, node, s_prefix);
  xmlFree ((xmlChar *) s_prefix);
  if (ns == NULL)
    {
      return NULL;
    }
  return xmljNewString (env, ns->href);
}

JNIEXPORT jint JNICALL
Java_gnu_xml_libxmlj_dom_GnomeNode_xmljCompareTo (JNIEnv * env,
                                                  jobject self,
                                                  jobject other)
{
  xmlNodePtr n1, n2, x;
  int d1, d2, delta, c;

  n1 = xmljGetNodeID (env, self);
  n2 = xmljGetNodeID (env, other);
  if (n1->doc != n2->doc)
    {
      return 0;
    }
  if (n1->type == XML_ATTRIBUTE_NODE || n2->type == XML_ATTRIBUTE_NODE)
    {
      return 0;
    }
  d1 = 0;
  for (x = n1->parent; x && x->type != XML_DOCUMENT_NODE; x = x->parent)
    {
      d1++;
    }
  d2 = 0;
  for (x = n2->parent; x && x->type != XML_DOCUMENT_NODE; x = x->parent)
    {
      d2++;
    }
  delta = d1 - d2;
  while (d1 > d2)
    {
      n1 = n1->parent;
      d1--;
    }
  while (d2 > d1)
    {
      n2 = n2->parent;
      d2--;
    }
  c = xmljCompare (n1, n2);
  return (c != 0) ? c : delta;
}

/* Compare at same level */
int
xmljCompare (xmlNodePtr n1, xmlNodePtr n2)
{
  int c, i1, i2;
  
  if (n1->parent == NULL || n1->type == XML_DOCUMENT_NODE ||
      n2->parent == NULL || n2->type == XML_DOCUMENT_NODE ||
      n1 == n2)
    {
      return 0;
    }
  c = xmljCompare (n1->parent, n2->parent);
  if (c != 0)
    {
      return c;
    }
  i1 = 0;
  for (n1 = n1->prev; n1; n1 = n1->prev)
    {
      i1++;
    }
  i2 = 0;
  for (n2 = n2->prev; n2; n2 = n2->prev)
    {
      i2++;
    }
  return i1 - i2;
}

int
xmljIsEqualNodeList (xmlNodePtr node1, xmlNodePtr node2)
{
  while (node1 != NULL)
    {
      if (!xmljIsEqualNode (node1, node2))
        {
          return 0;
        }
      node1 = node1->next;
      node2 = node2->next;
    }
  return 1;
}

int
xmljIsEqualNode (xmlNodePtr node1, xmlNodePtr node2)
{
  const xmlChar *val1;
  const xmlChar *val2;
  
  if (node1 == node2)
    {
      return 1;
    }
  if (node1 == NULL || node2 == NULL)
    {
      return 0;
    }
  /* Check node type */
  if (node1->type != node2->type)
    {
      return 0;
    }
  /* Check node name */
  if (!xmlStrEqual (node1->name, node2->name))
    {
      return 0;
    }
  /* Check node namespace */
  if (node1->type == XML_ELEMENT_NODE ||
      node1->type == XML_ATTRIBUTE_NODE)
    {
      xmlNsPtr ns1, ns2;
      
      ns1 = node1->ns;
      if (ns1 != NULL)
        {
          ns2 = node2->ns;
          if (ns2 == NULL)
            {
              return 0;
            }
          val1 = ns1->href;
          val2 = ns2->href;
          if (!xmlStrEqual (val1, val2))
            {
              return 0;
            }
        }
    }
  /* Check node value */
  val1 = xmljGetNodeValue (node1);
  val2 = xmljGetNodeValue (node2);
  if (!xmlStrEqual (val1, val2))
    {
      return 0;
    }
  /* Check attributes */
  if (node1->type == XML_ELEMENT_NODE &&
      !xmljIsEqualNodeList ((xmlNodePtr) node1->properties,
                            (xmlNodePtr) node2->properties))
    {
      return 0;
    }
  /* Check doctype */
  if (node1->type == XML_DOCUMENT_NODE)
    {
      xmlDocPtr doc1 = (xmlDocPtr) node1;
      xmlDocPtr doc2 = (xmlDocPtr) node2;

      if (!xmljIsEqualNode ((xmlNodePtr) doc1->intSubset,
                            (xmlNodePtr) doc2->intSubset) ||
          !xmljIsEqualNode ((xmlNodePtr) doc1->extSubset,
                            (xmlNodePtr) doc2->extSubset))
        {
          return 0;
        }
    }
  /* Check child nodes */
  if (!xmljIsEqualNodeList (node1->children, node2->children))
    {
      return 0;
    }
  return 1;
}

JNIEXPORT jboolean JNICALL
Java_gnu_xml_libxmlj_dom_GnomeNode_isEqualNode (JNIEnv * env,
                                                jobject self,
                                                jobject arg)
{
  xmlNodePtr node1;
  xmlNodePtr node2;

  node1 = xmljGetNodeID (env, self);
  node2 = xmljGetNodeID (env, arg);
  return xmljIsEqualNode (node1, node2);
}

/* -- GnomeNodeList -- */

JNIEXPORT jobject JNICALL
Java_gnu_xml_libxmlj_dom_GnomeNodeList_item (JNIEnv * env,
                                             jobject self, jint index)
{
  xmlNodePtr node;
  jint count;

  node = xmljGetNodeID (env, self);
  node = node->children;
  count = 0;
  for (count = 0; node != NULL && count < index; count++)
    {
      node = node->next;
    }
  return xmljGetNodeInstance (env, node);
}

JNIEXPORT jint JNICALL
Java_gnu_xml_libxmlj_dom_GnomeNodeList_getLength (JNIEnv * env, jobject self)
{
  xmlNodePtr node;
  jint count;

  node = xmljGetNodeID (env, self);
  count = 0;
  node = node->children;
  while (node != NULL)
    {
      count++;
      node = node->next;
    }
  return count;
}

/* -- GnomeNotation -- */

JNIEXPORT jstring JNICALL
Java_gnu_xml_libxmlj_dom_GnomeNotation_getPublicId (JNIEnv * env,
                                                    jobject self)
{
  xmlNotationPtr notation;

  notation = (xmlNotationPtr) xmljGetNodeID (env, self);
  if (notation->PublicID == NULL)
    {
      return NULL;
    }
  return xmljNewString (env, notation->PublicID);
}

JNIEXPORT jstring JNICALL
Java_gnu_xml_libxmlj_dom_GnomeNotation_getSystemId (JNIEnv * env,
                                                    jobject self)
{
  xmlNotationPtr notation;

  notation = (xmlNotationPtr) xmljGetNodeID (env, self);
  if (notation->SystemID == NULL)
    {
      return NULL;
    }
  return xmljNewString (env, notation->SystemID);
}

/* -- GnomeProcessingInstruction -- */

JNIEXPORT jstring JNICALL
Java_gnu_xml_libxmlj_dom_GnomeProcessingInstruction_getData (JNIEnv * env,
                                                             jobject self)
{
  xmlNodePtr node;
  xmlChar *text;
  jstring ret;

  node = xmljGetNodeID (env, self);
  text = xmlNodeGetContent (node);
  ret = xmljNewString (env, (const xmlChar *) text);
  if (text != NULL)
    {
      xmlFree (text);
    }
  return ret;
}

JNIEXPORT void JNICALL
Java_gnu_xml_libxmlj_dom_GnomeProcessingInstruction_setData (JNIEnv * env,
                                                             jobject self,
                                                             jstring data)
{
  xmlNodePtr node;
  const xmlChar *s_data;

  node = xmljGetNodeID (env, self);
  s_data = xmljGetStringChars (env, data);
  xmlNodeSetContent (node, s_data);
}

/* -- GnomeTypeInfo -- */

xmlDtdPtr xmljGetDtd (xmlDocPtr doc)
{
  xmlNodePtr ctx;

  for (ctx = doc->children; ctx; ctx = ctx->next)
    {
      if (ctx->type == XML_DOCUMENT_TYPE_NODE)
        {
          return (xmlDtdPtr) ctx;
        }
    }
  return NULL;
}

JNIEXPORT jstring JNICALL 
Java_gnu_xml_libxmlj_dom_GnomeTypeInfo_getTypeName (JNIEnv *env, jobject self)
{
  xmlNodePtr node;
  xmlDtdPtr dtd;
  xmlAttributePtr attribute;
  
  node = xmljGetNodeID (env, self);
  dtd = xmljGetDtd (node->doc);
  if (dtd)
    {
      switch (node->type)
        {
        case XML_ATTRIBUTE_NODE:
          attribute = xmlGetDtdAttrDesc (dtd, node->parent->name, node->name);
          if (attribute)
            {
              switch (attribute->type)
                {
                case XML_ATTRIBUTE_CDATA:
                  return xmljNewString (env, BAD_CAST "CDATA");
                case XML_ATTRIBUTE_ID:
                  return xmljNewString (env, BAD_CAST "ID");
                case XML_ATTRIBUTE_IDREF:
                  return xmljNewString (env, BAD_CAST "IDREF");
                case XML_ATTRIBUTE_IDREFS:
                  return xmljNewString (env, BAD_CAST "IDREFS");
                case XML_ATTRIBUTE_ENTITY:
                  return xmljNewString (env, BAD_CAST "ENTITY");
                case XML_ATTRIBUTE_ENTITIES:
                  return xmljNewString (env, BAD_CAST "ENTITIES");
                case XML_ATTRIBUTE_NMTOKEN:
                  return xmljNewString (env, BAD_CAST "NMTOKEN");
                case XML_ATTRIBUTE_NMTOKENS:
                  return xmljNewString (env, BAD_CAST "NMTOKENS");
                default:
                  return NULL;
                }
            }
          return NULL;
        default:
          return NULL;
        }
    }
  /* TODO when XML Schema support is available */
  return NULL;
}

JNIEXPORT jstring JNICALL 
Java_gnu_xml_libxmlj_dom_GnomeTypeInfo_getTypeNamespace (JNIEnv *env,
                                                         jobject self)
{
  xmlNodePtr node;
  xmlDtdPtr dtd;
  xmlAttributePtr attribute;
  
  node = xmljGetNodeID (env, self);
  dtd = xmljGetDtd (node->doc);
  if (dtd)
    {
      switch (node->type)
        {
        case XML_ATTRIBUTE_NODE:
          attribute = xmlGetDtdAttrDesc (dtd, node->parent->name, node->name);
          if (attribute)
            {
              return xmljNewString (env,
                                    BAD_CAST "http://www.w3.org/TR/REC-xml");
            }
          return NULL;
        default:
          return NULL;
        }
    }
  /* TODO when XML Schema support is available */
  return NULL;
}

JNIEXPORT jboolean JNICALL 
Java_gnu_xml_libxmlj_dom_GnomeTypeInfo_isDerivedFrom (JNIEnv *env
						      __attribute__ ((__unused__)),
                                                      jobject self
						      __attribute__ ((__unused__)),
                                                      jstring typeNS
						      __attribute__ ((__unused__)),
                                                      jstring typeName
						      __attribute__ ((__unused__)),
                                                      jint method
						      __attribute__ ((__unused__)))
{
  /* TODO when XML Schema support is available */
  return 0;
}

/* -- Utility -- */

/*
 * Create GnomeDocument object from the given xmlDocPtr
 */
jobject
xmljCreateDocument (JNIEnv * env, jobject self, xmlDocPtr doc)
{
  jclass cls;
  jfieldID field;
  jobject ret;

  if (!doc)
    {
      return NULL;
    }

  /* Get document object */
  ret = xmljGetNodeInstance (env, (xmlNodePtr) doc);

  /* Set DOM implementation field */
  cls = (*env)->FindClass (env, "gnu/xml/libxmlj/dom/GnomeDocument");
  field = (*env)->GetFieldID (env, cls, "dom",
                              "Lorg/w3c/dom/DOMImplementation;");
  (*env)->SetObjectField (env, ret, field, self);
  return ret;
}

xmlAttrPtr
xmljGetNamedItem (JNIEnv * env, jobject self, jstring name)
{
  xmlNodePtr node;
  xmlAttrPtr attr;
  const xmlChar *s_name;

  s_name = xmljGetStringChars (env, name);

  node = xmljGetNodeID (env, self);
  attr = node->properties;
  while (attr != NULL)
    {
      if (xmljMatch (s_name, (xmlNodePtr) attr))
        break;
      attr = attr->next;
    }
  xmlFree ((xmlChar *) s_name);

  return attr;
}

xmlAttrPtr
xmljGetNamedItemNS (JNIEnv * env, jobject self, jstring uri, jstring localName)
{
  xmlNodePtr node;
  xmlAttrPtr attr;
  const xmlChar *s_uri;
  const xmlChar *s_localName;

  s_uri = xmljGetStringChars (env, uri);
  s_localName = xmljGetStringChars (env, localName);

  node = xmljGetNodeID (env, self);
  attr = node->properties;
  while (attr != NULL)
    {
      if (xmljMatchNS (s_uri, s_localName, (xmlNodePtr) attr))
        break;
      attr = attr->next;
    }
  xmlFree ((xmlChar *) s_uri);
  xmlFree ((xmlChar *) s_localName);

  return attr;
}
