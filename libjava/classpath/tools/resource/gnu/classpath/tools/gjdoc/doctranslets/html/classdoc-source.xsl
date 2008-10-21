<?xml version="1.0" encoding="utf-8"?>

<!-- classdoc-source.xsl
     Copyright (C) 2003 Free Software Foundation, Inc.
     
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
     -->

<!-- Outputs an HTML file showing the source code for the class
     designated by $gjdoc.outputfile.info.
     -->

<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:gjdoc="http://www.gnu.org/software/cp-tools/gjdocxml"
  xmlns:html="http://www.w3.org/TR/REC-html40"
  xmlns="http://www.w3.org/TR/REC-html40">

  <xsl:include href="html_common.xsl"/>

  <xsl:output method="html"
    encoding="utf-8"
    doctype-public="-//W3C//DTD HTML 4.01 Transitional//EN"
    doctype-system="http://www.w3.org/TR/html4/loose.dtd"
    indent="no"/>

  <xsl:strip-space elements="*"/>

  <xsl:template match="/">
    <xsl:for-each select="document(concat($gjdoc.outputfile.info, '.xml'),/gjdoc:rootdoc)/gjdoc:classdoc/gjdoc:source">
      <html>
      <head>
        <xsl:call-template name="include_common"/>
        <xsl:call-template name="output_title">
          <xsl:with-param name="p_pagetitle" select="concat(ancestor::gjdoc:classdoc/@name, '.java')"/>
        </xsl:call-template>
      </head>
        <body>
          <xsl:copy-of select="."/>
        </body>
      </html>
    </xsl:for-each>
    
  </xsl:template>

</xsl:stylesheet>
