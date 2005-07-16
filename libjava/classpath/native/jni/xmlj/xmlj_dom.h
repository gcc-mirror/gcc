/* xmlj_dom.h -
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

#ifndef XMLJ_DOM_H
#define XMLJ_DOM_H

#include "gnu_xml_libxmlj_dom_GnomeAttr.h"
#include "gnu_xml_libxmlj_dom_GnomeDocument.h"
#include "gnu_xml_libxmlj_dom_GnomeDocumentBuilder.h"
#include "gnu_xml_libxmlj_dom_GnomeDocumentType.h"
#include "gnu_xml_libxmlj_dom_GnomeElement.h"
#include "gnu_xml_libxmlj_dom_GnomeEntity.h"
#include "gnu_xml_libxmlj_dom_GnomeNamedNodeMap.h"
#include "gnu_xml_libxmlj_dom_GnomeNode.h"
#include "gnu_xml_libxmlj_dom_GnomeNodeList.h"
#include "gnu_xml_libxmlj_dom_GnomeNotation.h"
#include "gnu_xml_libxmlj_dom_GnomeProcessingInstruction.h"
#include "gnu_xml_libxmlj_dom_GnomeTypeInfo.h"

#include <libxml/parser.h>
#include <libxml/valid.h>

void xmljValidateChildNode (JNIEnv *env, xmlNodePtr parent, xmlNodePtr child);
int xmljIsEqualNode (xmlNodePtr node1, xmlNodePtr node2);
int xmljIsEqualNodeList (xmlNodePtr node1, xmlNodePtr node2);
void xmljNormalizeNode (xmlNodePtr node);
xmlDtdPtr xmljGetDtd (xmlDocPtr doc);
int xmljCompare (xmlNodePtr n1, xmlNodePtr n2);

/* Utility */
jobject xmljCreateDocument (JNIEnv * env, jobject self, xmlDocPtr doc);
xmlAttrPtr xmljGetNamedItem (JNIEnv * env, jobject self, jstring name);
xmlAttrPtr xmljGetNamedItemNS (JNIEnv * env, jobject self, jstring uri,
			   jstring localName);

#endif /* !defined XMLJ_DOM_H */
