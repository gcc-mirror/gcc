<?xml version="1.0" encoding="utf-8"?>

<!-- index.xsl
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

<!-- Creates the index.html file for HTML documentation. This is only
     a simple frameset.
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
    <html>
      <head>
        <xsl:call-template name="include_common"/>
      </head>
      <frameset cols="20%,80%">
        <xsl:choose>
          <xsl:when test="count(/gjdoc:rootdoc/gjdoc:packagedoc) > 1">
            <frameset rows="25%,75%">
              <frame src="allpackages.html" name="packages"/>
              <frame src="allclasses.html" name="classes"/>
            </frameset>
            <frame src="index_noframes.html" name="content"/>
          </xsl:when>
          <xsl:otherwise>
            <frame src="allclasses.html" name="classes"/>
            <frame src="{concat(translate(/gjdoc:rootdoc/gjdoc:packagedoc[position()=1]/@name, '.', '/'), '/package-summary.html')}" name="content"/>
          </xsl:otherwise>
        </xsl:choose>
      </frameset>
    </html>
  </xsl:template>
</xsl:stylesheet>
