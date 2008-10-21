<?xml version="1.0" encoding="utf-8"?>

<!-- classdoc.xsl
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

<!-- Creates the class descriptor files for HTML documentation. 
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

  <xsl:template match="gjdoc:tag[attribute::kind='@see'][attribute::name='@link']">
    <xsl:call-template name="output_link_tag"/>
  </xsl:template>

  <xsl:template mode="pre" match="*">
    <xsl:choose>
      <xsl:when test="'pre'=local-name(.)">
        <xsl:apply-templates mode="pre"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:copy-of select="."/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template mode="pre" match="text()[position()=last()]">
    <xsl:call-template name="strip_trailing_ws">
      <xsl:with-param name="p_content" select="."/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template name="strip_trailing_ws">
    <xsl:param name="p_content"/>

    <xsl:if test="normalize-space($p_content)!=''">
      <xsl:choose>
        <xsl:when test="normalize-space(substring($p_content, string-length($p_content), 1))=''">
          <xsl:call-template name="strip_trailing_ws">
            <xsl:with-param name="p_content">
              <xsl:value-of select="substring($p_content, 1, string-length($p_content)-1)"/>
            </xsl:with-param>
          </xsl:call-template>
        </xsl:when>
        <xsl:otherwise>
          <xsl:copy-of select="$p_content"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:if>
  </xsl:template>

  <xsl:template match="*">
    <xsl:element name="{name()}">
      <xsl:for-each select="attribute::*">
        <xsl:attribute name="{name()}">
          <xsl:value-of select="."/>              
        </xsl:attribute>
      </xsl:for-each>
      <xsl:choose>
        <xsl:when test="name()='pre'">
          <xsl:apply-templates select="." mode="pre"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:apply-templates/>
        </xsl:otherwise>
      </xsl:choose>        
    </xsl:element>
  </xsl:template>

  <xsl:template match="/">

    <xsl:for-each select="document(concat($gjdoc.outputfile.info, '.xml'),/gjdoc:rootdoc)/gjdoc:classdoc">
      <xsl:call-template name="create_classdoc"/>
    </xsl:for-each>

  </xsl:template>

  <!-- For every classdoc node found in the source XML, create a corresponding -->
  <!-- class rootdoc HTML file. -->
  
  <xsl:template name="create_classdoc">

    <xsl:variable name="v_sub_xml_filename">
      <xsl:value-of select="concat(@qualifiedtypename,'.xml')"/>
    </xsl:variable>  
    
    <html>
      <head>
        <xsl:call-template name="output_title">
          <xsl:with-param name="p_pagetitle" select="@name"/>
        </xsl:call-template>
        <xsl:call-template name="include_common"/>
      </head>
      <body class="classdoc" onload="if(parent.contentPageLoaded)parent.contentPageLoaded(document.title)">

        <div class="pagebody">

        <xsl:call-template name="classdoc_header"/>
        <xsl:call-template name="classdoc_all_field_summary">
          <xsl:with-param name="v_sub_xml_filename">
            <xsl:value-of select="$v_sub_xml_filename"/>
          </xsl:with-param>
        </xsl:call-template>
        <xsl:call-template name="classdoc_all_constructor_summary">
          <xsl:with-param name="v_sub_xml_filename">
            <xsl:value-of select="$v_sub_xml_filename"/>
          </xsl:with-param>
        </xsl:call-template>
        <xsl:call-template name="classdoc_all_method_summary">
          <xsl:with-param name="v_sub_xml_filename">
            <xsl:value-of select="$v_sub_xml_filename"/>
          </xsl:with-param>
        </xsl:call-template>
        
        <xsl:if test="gjdoc:fielddoc">
          <h1 class="classdoc-header">Field Details</h1>
          
          <xsl:for-each select="gjdoc:fielddoc">
            <xsl:sort select="@name" order="ascending"/>
            <xsl:call-template name="classdoc_field_details"/>
          </xsl:for-each>
        </xsl:if>
        
        <xsl:if test="gjdoc:constructordoc">
          <h1 class="classdoc-header">Constructor Details</h1>
          
          <xsl:for-each select="gjdoc:constructordoc">
            <xsl:sort select="gjdoc:signature/@full" order="ascending"/>
            <xsl:call-template name="classdoc_method_details"/>
          </xsl:for-each>
        </xsl:if>
        
        <xsl:if test="gjdoc:methoddoc">
          <h1 class="classdoc-header">Method Details</h1>
          
          <xsl:for-each select="gjdoc:methoddoc">
            <xsl:sort select="@name" order="ascending"/>
            <xsl:sort select="gjdoc:signature/@full" order="ascending"/>
            <xsl:call-template name="classdoc_method_details"/>
          </xsl:for-each>
        </xsl:if>

        <!-- Bottom Navigation Bar -->
        <xsl:call-template name="output_navbar">
          <xsl:with-param name="p_show_frames" select="1"/>
          <xsl:with-param name="p_show_noframes" select="1"/>
          <xsl:with-param name="p_show_package" select="1"/>
          <xsl:with-param name="p_show_package_tree" select="1"/>
          <xsl:with-param name="p_show_full_tree" select="1"/>
          <xsl:with-param name="p_show_index" select="1"/>
          <xsl:with-param name="p_show_help" select="1"/> 
          <xsl:with-param name="p_top" select="0"/> 
          <xsl:with-param name="p_show_source" select="concat($gjdoc.pathtoroot, 'src-html/', translate(gjdoc:containingPackage/@name, '.', '/'), '/', @name, '.html')"/>
          <xsl:with-param name="p_show_use" select="concat($gjdoc.pathtoroot, 'class-use/', translate(gjdoc:containingPackage/@name, '.', '/'), '/', @name, '.html')"/>
          <xsl:with-param name="p_curr_class" select="1"/>
        </xsl:call-template>
        
        </div>
      </body>
    </html>
  </xsl:template>

  <!-- Output header for Class documentation  -->
  
  <xsl:template name="classdoc_header">
    
    <!-- Top Navigation Bar -->
    <xsl:call-template name="output_navbar">
      <xsl:with-param name="p_show_frames" select="1"/>
      <xsl:with-param name="p_show_noframes" select="1"/>
      <xsl:with-param name="p_show_package" select="1"/>
      <xsl:with-param name="p_show_package_tree" select="1"/>
      <xsl:with-param name="p_show_full_tree" select="1"/>
      <xsl:with-param name="p_show_index" select="1"/>
      <xsl:with-param name="p_show_help" select="1"/> 
      <xsl:with-param name="p_top" select="1"/> 
      <xsl:with-param name="p_show_source" select="concat($gjdoc.pathtoroot, 'src-html/', translate(gjdoc:containingPackage/@name, '.', '/'), '/', @name, '.html')"/>
      <xsl:with-param name="p_show_use" select="concat($gjdoc.pathtoroot, 'class-use/', translate(gjdoc:containingPackage/@name, '.', '/'), '/', @name, '.html')"/>
      <xsl:with-param name="p_curr_class" select="1"/>
    </xsl:call-template>

    <div class="classdoc-head">

        <div class="classdoc-head-packagename classdoc-subtitle">
          <xsl:value-of select="gjdoc:containingPackage/@name"/>
        </div>
    
        <h1 class="classdoc-head-classname classdoc-title">
          <xsl:choose>
            <xsl:when test="gjdoc:isInterface">
              <xsl:text>Interface </xsl:text>
            </xsl:when>
            <xsl:otherwise>
              <xsl:text>Class </xsl:text>
            </xsl:otherwise>
          </xsl:choose>
          <xsl:value-of select="@name"/>
        </h1>

    </div>

    <xsl:if test="gjdoc:isClass">
      <xsl:call-template name="classdoc_heritage_tree"/>
    </xsl:if>

    <xsl:if test="gjdoc:containingClass">
      <xsl:variable name="v_qualifiedtypename" select="attribute::qualifiedtypename"/>
      <b>Enclosing Class:</b><br/>
      <xsl:call-template name="link_to_class">
        <xsl:with-param name="p_name" select="gjdoc:containingClass/@typename"/>
        <xsl:with-param name="p_qualifiedname" select="gjdoc:containingClass/@qualifiedtypename"/>
      </xsl:call-template>
    </xsl:if>
    
    <xsl:if test="gjdoc:implements">
      <p/>
      <b>
        <xsl:choose>
          <xsl:when test="gjdoc:isInterface">
            <xsl:text>All Superinterfaces:</xsl:text>
          </xsl:when>
          <xsl:otherwise>
            <xsl:text>All Implemented Interfaces:</xsl:text>
          </xsl:otherwise>
        </xsl:choose>
      </b>
      <br/>
      <xsl:for-each select="gjdoc:implements|gjdoc:superimplements">
        <xsl:sort select="@qualifiedtypename" order="ascending"/>
        <xsl:call-template name="link_to_class">
          <xsl:with-param name="p_name" select="@typename"/>
          <xsl:with-param name="p_qualifiedname" select="@qualifiedtypename"/>
        </xsl:call-template>
        <xsl:if test="position() != last()">
          <xsl:text>, </xsl:text>
        </xsl:if>
      </xsl:for-each>
    </xsl:if>
    <p/>

    <xsl:if test="gjdoc:isInterface">
      <xsl:variable name="v_qualifiedtypename" select="attribute::qualifiedtypename"/>
      <xsl:variable name="v_implementors" select="document('index.xml', /)/gjdoc:rootdoc/gjdoc:classdoc[child::gjdoc:isInterface]/gjdoc:implements[attribute::qualifiedtypename=$v_qualifiedtypename]"/>
      <xsl:if test="$v_implementors">
        <b>All Known Direct Subinterfaces:</b><br/>
        <xsl:for-each select="$v_implementors">
          <xsl:sort select="../@qualifiedtypename" order="ascending"/>
          <xsl:call-template name="link_to_class">
            <xsl:with-param name="p_name" select="../@name"/>
            <xsl:with-param name="p_qualifiedname" select="../@qualifiedtypename"/>
          </xsl:call-template>
          <xsl:if test="position() != last()">
            <xsl:text>, </xsl:text>
          </xsl:if>
        </xsl:for-each>
      </xsl:if>
    </xsl:if>

    <xsl:if test="gjdoc:isInterface">
      <xsl:variable name="v_qualifiedtypename" select="attribute::qualifiedtypename"/>
      <xsl:variable name="v_implementors" select="document('index.xml', /)/gjdoc:rootdoc/gjdoc:classdoc[child::gjdoc:isClass]/gjdoc:implements[attribute::qualifiedtypename=$v_qualifiedtypename]"/>
      <xsl:if test="$v_implementors">
        <b>Known Implementing Classes:</b><br/>
        <xsl:for-each select="$v_implementors">
          <xsl:sort select="../@qualifiedtypename" order="ascending"/>
          <xsl:call-template name="link_to_class">
            <xsl:with-param name="p_name" select="../@name"/>
            <xsl:with-param name="p_qualifiedname" select="../@qualifiedtypename"/>
          </xsl:call-template>
          <xsl:if test="position() != last()">
            <xsl:text>, </xsl:text>
          </xsl:if>
        </xsl:for-each>
      </xsl:if>
    </xsl:if>

    <xsl:if test="gjdoc:isClass">
      <xsl:variable name="v_qualifiedtypename" select="attribute::qualifiedtypename"/>
      <xsl:variable name="v_implementors" select="document('index.xml', /)/gjdoc:rootdoc/gjdoc:classdoc[not(child::gjdoc:isInterface)]/gjdoc:superclass[attribute::qualifiedtypename=$v_qualifiedtypename]"/>
      <xsl:if test="$v_implementors">
        <b>Known Direct Subclasses:</b><br/>
        <xsl:for-each select="$v_implementors">
          <xsl:sort select="../@qualifiedtypename" order="ascending"/>
          <xsl:call-template name="link_to_class">
            <xsl:with-param name="p_name" select="../@name"/>
            <xsl:with-param name="p_qualifiedname" select="../@qualifiedtypename"/>
          </xsl:call-template>
          <xsl:if test="position() != last()">
            <xsl:text>, </xsl:text>
          </xsl:if>
        </xsl:for-each>
      </xsl:if>
    </xsl:if>

    <hr/>
    
    <div class="classdoc-prototype">

      <!-- 'public final class Byte' -->
      
      <xsl:value-of select="gjdoc:access/@scope"/><xsl:text> </xsl:text>
      <xsl:if test="gjdoc:isStatic"><xsl:text>static </xsl:text></xsl:if>
      <xsl:if test="gjdoc:isFinal"><xsl:text>final </xsl:text></xsl:if>
      <xsl:choose>
        <xsl:when test="gjdoc:isInterface">
          <xsl:text>interface </xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <xsl:if test="gjdoc:isAbstract"><xsl:text>abstract </xsl:text></xsl:if>
          <xsl:text>class </xsl:text>
        </xsl:otherwise>
      </xsl:choose>

      <xsl:call-template name="link_to_class_source"/><br/>
    
      <!-- 'extends Number' -->
      
      <xsl:if test="gjdoc:isClass and @qualifiedtypename!='java.lang.Object'">
        <xsl:text>extends </xsl:text>
        <xsl:call-template name="link_to_class">
          <xsl:with-param name="p_qualifiedname">
            <xsl:value-of select="gjdoc:superclass/@qualifiedtypename"/>
          </xsl:with-param>
          <xsl:with-param name="p_name">
            <xsl:value-of select="gjdoc:superclass/@typename"/>
          </xsl:with-param>
        </xsl:call-template>
        <br/>
      </xsl:if>
      
      <!-- 'implements Comparable' -->
      
      <xsl:if test="gjdoc:implements">
        <xsl:text>implements </xsl:text>
      </xsl:if>
      <xsl:for-each select="gjdoc:implements">
        <xsl:call-template name="link_to_class">
          <xsl:with-param name="p_qualifiedname">
            <xsl:value-of select="@qualifiedtypename"/>
          </xsl:with-param>
          <xsl:with-param name="p_name">
            <xsl:value-of select="@typename"/>
          </xsl:with-param>
        </xsl:call-template>
        <xsl:if test="position() != last()">
          <xsl:text>, </xsl:text>
        </xsl:if>
      </xsl:for-each>
      
    </div>
    
    <!-- Documentation -->
    
    <p/>
    
    <xsl:if test="gjdoc:inlineTags">
      <div class="classdoc-class-comment-body">
        <xsl:for-each select="gjdoc:inlineTags/node()">
          <xsl:apply-templates select="."/>
        </xsl:for-each>
      </div>
    </xsl:if>

    <xsl:call-template name="output_see_tags"/>
    <xsl:call-template name="output_taglet_tags"/>

  </xsl:template>

  <xsl:template name="output_taglet_tags">

    <xsl:for-each select="gjdoc:tags/gjdoc:tag">
      <xsl:choose>
        <xsl:when test="@name='@deprecated'">
          <div class="classdoc-tag-section-header">
            <xsl:value-of select="'Deprecated:'"/>
          </div>
          <dl class="classdoc-list">
            <xsl:for-each select="gjdoc:inlineTags">
              <xsl:apply-templates/>
            </xsl:for-each>
          </dl>
        </xsl:when>
        <xsl:when test="@taglet-generated">
          <xsl:copy-of select="./*"/>
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
    
  </xsl:template>

  <!-- Output summary of all fields in this class -->

  <xsl:template name="classdoc_all_field_summary">

    <xsl:if test=".//gjdoc:fielddoc">    
    
      <table border="1" cellspacing="0" class="classdoc-table">
        <tr><td class="section-header" colspan="2"><div class="section-header">Field Summary</div></td></tr>
        <xsl:for-each select="gjdoc:fielddoc">
          <xsl:sort select="@name" order="ascending"/>
          <xsl:call-template name="classdoc_field_summary_tr"/>  
        </xsl:for-each>
      </table>

      <xsl:for-each select="gjdoc:superclass">
        <xsl:call-template name="output_superclass_fields"/>
      </xsl:for-each>
    </xsl:if>

  </xsl:template>
 
  <!-- Output summary of all methods in this class -->
  
  <xsl:template name="classdoc_all_method_summary">
    
    <xsl:if test=".//gjdoc:methoddoc">
      <table border="1" cellspacing="0" class="classdoc-table">
        <tr><td class="section-header" colspan="2"><div class="section-header">Method Summary</div></td></tr>
        <xsl:for-each select="gjdoc:methoddoc">
          <xsl:sort select="@name" order="ascending"/>
          <xsl:call-template name="classdoc_method_summary_tr"/>  
        </xsl:for-each>
      </table>

      <xsl:for-each select="gjdoc:superclass">
        <xsl:call-template name="output_superclass_methods"/>
      </xsl:for-each>
    </xsl:if>
    
  </xsl:template>
  
  <!-- Output summary of all constructors in this class -->
  
  <xsl:template name="classdoc_all_constructor_summary">
    
    <xsl:if test=".//gjdoc:constructordoc">
      <!--
      <h1 class="classdoc-header">Constructor Summary</h1>
-->
    
      <table class="classdoc-table">
        <tr><td class="section-header" colspan="2"><div class="section-header">Constructor Summary</div></td></tr>
        <xsl:for-each select="gjdoc:constructordoc">
          <xsl:sort select="@name" order="ascending"/>
          <xsl:call-template name="classdoc_method_summary_tr"/>  
        </xsl:for-each>
      </table>
    </xsl:if>

  </xsl:template>
  
  <!-- Output summary of a single field -->
  
  <xsl:template name="classdoc_field_summary_tr">
    
    <tr valign="top">

      <!-- Left table cell: Modifiers and Return Type  -->
      
      <td valign="top" class="member-summary field modifiers">
        <code class="member-summary field modifiers">
          <xsl:call-template name="output_modifiers_summary"/>
          <xsl:call-template name="link_to_class">
            <xsl:with-param name="p_name">
              <xsl:value-of select="gjdoc:type/@typename"/>
            </xsl:with-param>
            <xsl:with-param name="p_qualifiedname">
              <xsl:value-of select="gjdoc:type/@qualifiedtypename"/>
            </xsl:with-param>
          </xsl:call-template>
          <xsl:value-of select="gjdoc:type/@dimension"/>
        </code>
      </td>

      <!-- Right table cell: name and short description  -->
      
      <td class="member-summary field name">
        <!-- Method signature -->
        
        <!-- Link to field definition -->
          
        <code class="member-summary field name">
          <a href="{concat('#',@name)}" class="field name"><xsl:value-of select="@name"/></a>
        </code>

        <!-- Brief description of field -->
        
        <blockquote class="member-summary field comment">
          <xsl:for-each select="gjdoc:firstSentenceTags/node()">
            <xsl:value-of select="." disable-output-escaping="yes"/>
          </xsl:for-each>
        </blockquote>
      </td>

    </tr>

  </xsl:template>
  
  <xsl:template name="output_modifiers">
    <xsl:if test="gjdoc:isNative">
      <xsl:text>native </xsl:text>
    </xsl:if>
    <xsl:if test="gjdoc:isStatic">
      <xsl:text>static </xsl:text>
    </xsl:if>
    <xsl:if test="gjdoc:isFinal">
      <xsl:text>final </xsl:text>
    </xsl:if>
    <xsl:if test="gjdoc:isAbstract">
      <xsl:text>abstract </xsl:text>
    </xsl:if>
    <xsl:if test="gjdoc:isTransient">
      <xsl:text>transient </xsl:text>
    </xsl:if>
    <xsl:if test="gjdoc:isVolatile">
      <xsl:text>volatile </xsl:text>
    </xsl:if>
    <xsl:if test="gjdoc:isSynchronized">
      <xsl:text>synchronized </xsl:text>
    </xsl:if>
  </xsl:template>
  
  <xsl:template name="output_modifiers_summary">
    <xsl:if test="gjdoc:isStatic">
      <xsl:text>static </xsl:text>
    </xsl:if>
    <xsl:if test="gjdoc:isAbstract">
      <xsl:text>abstract </xsl:text>
    </xsl:if>
    <xsl:if test="gjdoc:isTransient">
      <xsl:text>transient </xsl:text>
    </xsl:if>
    <xsl:if test="gjdoc:isVolatile">
      <xsl:text>volatile </xsl:text>
    </xsl:if>
    <xsl:if test="gjdoc:isSynchronized">
      <xsl:text>synchronized </xsl:text>
    </xsl:if>
  </xsl:template>
  
  <!-- Output summary of a single method or constructor -->
  
  <xsl:template name="classdoc_method_summary_tr">
    
    <tr valign="top">
      
      <!-- Left table cell: Modifiers and Return Type  -->
      
      <xsl:if test="gjdoc:isMethod">
        <td valign="top" class="member-summary method modifiers">
          <code class="member-summary method modifiers">
            <xsl:call-template name="output_modifiers_summary"/>
            <xsl:text> </xsl:text>
            <xsl:call-template name="link_to_class">
              <xsl:with-param name="p_qualifiedname" select="gjdoc:returns/@qualifiedtypename"/>
              <xsl:with-param name="p_name" select="gjdoc:returns/@typename"/>
            </xsl:call-template>
            <xsl:value-of select="gjdoc:returns/@dimension"/>
          </code>
        </td>
      </xsl:if>
      
      <!-- Right table cell: signature and short description  -->
      
      <td align="left" valign="top" class="member-summary method name">
        
        <!-- Method signature -->
        
        <code class="member-summary method signature">
          
          <!-- Link to method definition -->
          
          <a href="{concat('#',@name,gjdoc:signature/@full)}" class="member-summary method name"><xsl:value-of select="@name"/></a>
          
          <!-- Parameter List -->
          
          <xsl:text>(</xsl:text>
          <xsl:call-template name="list_parameters"/>
          <xsl:text>)</xsl:text>
        </code>
        
        <!-- Brief description of Method -->
        
        <blockquote class="member-summary method comment">
          <xsl:choose>
            <xsl:when test="gjdoc:tags/gjdoc:tag[@kind='@deprecated']">
              <i>
                <b>Deprecated. </b>
                <xsl:for-each select="gjdoc:tags/gjdoc:tag[@kind='@deprecated']/gjdoc:firstSentenceTags">
                  <xsl:apply-templates/>
                </xsl:for-each>
              </i>
            </xsl:when>
            <xsl:otherwise>
              <xsl:for-each select="gjdoc:firstSentenceTags/node()">
                <xsl:value-of disable-output-escaping="yes" select="."/>
              </xsl:for-each>
            </xsl:otherwise>
          </xsl:choose>
        </blockquote>
      </td>
    </tr>
    
  </xsl:template>
  
  <!-- Output a list of all parameters of the current methoddoc  -->
  <xsl:template name="list_parameters">
    <xsl:for-each select="gjdoc:parameter">
      <span class="parameter">
        <xsl:call-template name="link_to_class">
          <xsl:with-param name="p_qualifiedname">
            <xsl:value-of select="@qualifiedtypename"/>
          </xsl:with-param>
          <xsl:with-param name="p_name">
            <xsl:value-of select="@typename"/>
          </xsl:with-param>
        </xsl:call-template>
        <xsl:value-of select="@dimension"/>
        <xsl:text disable-output-escaping="no">&#160;</xsl:text>
        <xsl:value-of select="@name"/>
        <xsl:if test="position() != last()">
          <xsl:text>,</xsl:text>
        </xsl:if>
      </span>
      <xsl:if test="position() != last()">
        <xsl:text> </xsl:text>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>
  
  <xsl:template name="list_parameter_details">
    <dl class="classdoc-list">
      <xsl:for-each select="gjdoc:parameter">
        <dt>
          <span class="prototype-name">
            <xsl:value-of select="@name"/>
          </span>
          <xsl:variable name="param_position" select="position()"/>

          <xsl:if test="../gjdoc:tags/gjdoc:tag[attribute::kind='@param' and position()=$param_position]">
            <xsl:text> - </xsl:text>
            <xsl:for-each select="../gjdoc:tags/gjdoc:tag[attribute::kind='@param' and position()=$param_position]/gjdoc:inlineTags/node()">
              <xsl:copy-of select="."/>
            </xsl:for-each>
          </xsl:if>
        </dt>
      </xsl:for-each>
    </dl>
  </xsl:template>

  <xsl:template name="output_see_tags">
    <xsl:if test="gjdoc:tags/gjdoc:tag[attribute::kind='@see']">
      <div class="classdoc-tag-section-header"><b>See Also:</b></div>

      <dl class="classdoc-list">
        <xsl:for-each select="gjdoc:tags/gjdoc:tag[attribute::kind='@see']">
          <!--
          <xsl:variable name="v_see" select="normalize-space(.)"/>
          -->
          <dt>
            <code>
              <xsl:call-template name="output_link_tag"/>
            </code>
          </dt>
        </xsl:for-each>
      </dl>
    </xsl:if>
  </xsl:template>

  <!-- Output details of all fields in this class -->
  
  <xsl:template name="classdoc_field_details">

    <a name="{@name}" class="classdoc"> </a>
    <div class="before-details"> </div>

    <h3><xsl:value-of select="@name"/></h3>
    
    <p class="prototype"><code>
      <xsl:value-of select="gjdoc:access/@scope"/>
      <xsl:text> </xsl:text>
      <xsl:call-template name="output_modifiers"/>
      <xsl:value-of select="gjdoc:type/@typename"/>
      <xsl:value-of select="gjdoc:type/@dimension"/>
      <xsl:text> </xsl:text>
      <xsl:call-template name="link_to_member_source"/>
    </code></p>

    <p/>
    
    <!-- Full comment text -->
    
    <xsl:if test="gjdoc:inlineTags">
      <div class="classdoc-comment-body">
        <xsl:for-each select="gjdoc:inlineTags/node()">
          <xsl:apply-templates select="."/>
        </xsl:for-each>
      </div>
    </xsl:if>
      
    <p/>

    <xsl:call-template name="output_taglet_tags"/>

    <!-- See Also -->

    <xsl:call-template name="output_see_tags"/>

    <xsl:if test="position()!=last()">
      <hr/>
    </xsl:if>

  </xsl:template>

  
  <!-- Output details of all methods in this class -->
  
  <xsl:template name="classdoc_method_details">

    <a name="{concat(@name,gjdoc:signature/@full)}" class="classdoc"> </a>
    <div class="before-details"> </div>
    
    <h3><xsl:value-of select="@name"/></h3>
    
    <p class="prototype"><code>
      <xsl:value-of select="gjdoc:access/@scope"/>
      <xsl:text> </xsl:text>
      <xsl:call-template name="output_modifiers"/>
      <xsl:value-of select="gjdoc:returns/@typename"/><xsl:value-of select="gjdoc:returns/@dimension"/>
      <xsl:text> </xsl:text>
      <xsl:call-template name="link_to_member_source"/>
      <xsl:text>(</xsl:text>
      <xsl:call-template name="list_parameters"/>
      <xsl:text>)</xsl:text>
    </code></p>    
    
    <!-- Full comment text -->
    
    <xsl:if test="gjdoc:inlineTags">
      <div class="classdoc-comment-body">
        <xsl:for-each select="gjdoc:inlineTags/node()">
          <xsl:apply-templates select="."/>
        </xsl:for-each>
      </div>
    </xsl:if>

    <p/>

    <xsl:call-template name="output_taglet_tags"/>
    
    <xsl:if test="gjdoc:parameter">
      <div class="classdoc-tag-section-header"><b>Parameters:</b></div>
      <xsl:call-template name="list_parameter_details"/>
    </xsl:if>
    
    <xsl:if test="gjdoc:tags/gjdoc:tag[attribute::kind='@return']">
      <div class="classdoc-tag-section-header"><b>Returns:</b></div>
      <dl class="classdoc-list">
        <dt>
          <xsl:for-each select="gjdoc:tags/gjdoc:tag[attribute::kind='@return']">
            <xsl:value-of select="." disable-output-escaping="yes"/>
          </xsl:for-each>
        </dt>
      </dl>
    </xsl:if>
    
    <xsl:if test="gjdoc:tags/gjdoc:tag[attribute::kind='@throws']">
      <div class="classdoc-tag-section-header"><b>Throws:</b></div>
      <dl class="classdoc-list">
        <xsl:for-each select="gjdoc:tags/gjdoc:tag[attribute::kind='@throws']">
          <dt>
            <code>
              <xsl:call-template name="link_to_class">
                <xsl:with-param name="p_qualifiedname">
                  <xsl:value-of select="gjdoc:exception/@qualifiedtypename"/>
                </xsl:with-param>
                <xsl:with-param name="p_name">
                  <xsl:value-of select="gjdoc:exception/@typename"/>
                </xsl:with-param>
              </xsl:call-template>
            </code>
            <xsl:text> - </xsl:text>
            <xsl:for-each select="gjdoc:inlineTags/node()">
              <xsl:copy-of select="."/>
            </xsl:for-each>
          </dt>
        </xsl:for-each>
      </dl>
    </xsl:if>
    
    <xsl:call-template name="output_see_tags"/>

    <xsl:if test="position()!=last()">
      <hr/>
    </xsl:if>
    
  </xsl:template>

  <!-- Recursively create the tree showing the heritage of the -->
  <!-- given class -->
  
  <xsl:template name="create_tree">
    <xsl:param name="p_superclass"/>
    <xsl:param name="p_indent"/>	
    <xsl:param name="p_final" select="0"/>

    <xsl:for-each select="/gjdoc:rootdoc/gjdoc:classdoc[attribute::qualifiedtypename=$p_superclass]">
      
      <xsl:if test="gjdoc:superclass">
        <xsl:call-template name="create_tree">
          <xsl:with-param name="p_indent" select="$p_indent+-1"/>
          <xsl:with-param name="p_superclass" select="gjdoc:superclass/@qualifiedtypename"/>
        </xsl:call-template>

        <xsl:call-template name="tree_indentation_text_empty">
          <xsl:with-param name="p_indent" select="$p_indent"/>
        </xsl:call-template>
        <xsl:text>|
</xsl:text>
      </xsl:if>
    </xsl:for-each>	
    
    <xsl:if test="$p_indent>0">	
      <xsl:call-template name="tree_indentation">
        <xsl:with-param name="p_indent" select="$p_indent"/>
      </xsl:call-template>
    </xsl:if>	
        
    <xsl:choose>
      <xsl:when test="$p_final=1">
        <xsl:value-of select="$p_superclass"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="link_to_class_full">
          <xsl:with-param name="p_qualifiedname">
            <xsl:value-of select="$p_superclass"/>
          </xsl:with-param>
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:text>
</xsl:text>
  </xsl:template>

  <!-- Recursively create the tree showing the heritage of the -->
  <!-- given class, use bitmaps -->
  
  <xsl:template name="create_tree_gfx">
    <xsl:param name="p_superclass"/>
    <xsl:param name="p_indent"/>	
    <xsl:param name="p_final" select="0"/>
    <xsl:if test="/gjdoc:rootdoc/gjdoc:classdoc[attribute::qualifiedtypename=$p_superclass]/gjdoc:superclass">
      <xsl:call-template name="create_tree">
        <xsl:with-param name="p_indent" select="$p_indent+-1"/>
        <xsl:with-param name="p_superclass" select="/gjdoc:rootdoc/gjdoc:classdoc[attribute::qualifiedtypename=$p_superclass]/gjdoc:superclass/@qualifiedtypename"/>
      </xsl:call-template>
    </xsl:if>
    <tr><td>
    <table cellspacing="0" cellpadding="0" border="0">
      <tr>
        <xsl:variable name="imagename">
          <xsl:choose>
            <xsl:when test="$p_final=1">
              <xsl:text>images/tree-final-node.png</xsl:text>
            </xsl:when>
            <xsl:otherwise>
              <xsl:text>images/tree-node.png</xsl:text>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:variable>

        <td>
          <xsl:if test="$p_indent>0">	
            <xsl:call-template name="tree_indentation">
              <xsl:with-param name="p_indent"><xsl:value-of select="$p_indent"/></xsl:with-param>
            </xsl:call-template>
          </xsl:if>	
          <img width="10" height="21" class="classdoc-tree-image" src="{$imagename}"/>
        </td>

        <xsl:choose>
          <xsl:when test="$p_final=1">
            <td class="classdoc-tree-label"><xsl:value-of select="$p_superclass"/></td>
          </xsl:when>
          <xsl:otherwise>
            <td class="classdoc-tree-label">
              <xsl:call-template name="link_to_class_full">
                <xsl:with-param name="p_qualifiedname">
                  <xsl:value-of select="$p_superclass"/>
                </xsl:with-param>
              </xsl:call-template>
            </td>
          </xsl:otherwise>
        </xsl:choose>
      </tr></table>
    </td></tr>
  </xsl:template>

  <!-- Recursively output one character for each superclass found for the given -->
  <!-- class. The length of the resulting string tells how many levels of -->
  <!-- indentation are required for creating the heritage tree.  -->
  
  <xsl:template name="output_base_markers">
    <xsl:param name="p_superclass"/>

    <xsl:for-each select="/gjdoc:rootdoc/gjdoc:classdoc[attribute::qualifiedtypename=$p_superclass]">      
      <xsl:if test="gjdoc:superclass">
        <xsl:call-template name="output_base_markers">
          <xsl:with-param name="p_superclass" select="gjdoc:superclass/@qualifiedtypename"/>
        </xsl:call-template>
      </xsl:if>
    </xsl:for-each>

    <xsl:text>*</xsl:text>
  </xsl:template>

  <!-- Heritage Tree -->
  
  <xsl:template name="classdoc_heritage_tree">
    
    <xsl:variable name="p_qualifiedtypename" select="@qualifiedtypename"/>

    <xsl:for-each select="document('index.xml', /)/gjdoc:rootdoc/gjdoc:classdoc[@qualifiedtypename=$p_qualifiedtypename]">

      <xsl:variable name="superclass_markers">
        <xsl:call-template name="output_base_markers">
          <xsl:with-param name="p_superclass" select="gjdoc:superclass/@qualifiedtypename"/>
        </xsl:call-template>
      </xsl:variable>

      <xsl:choose>
        <xsl:when test="$gjdoc.allowimages=1">
          <table border="0" cellpadding="0" cellspacing="0" style="margin:0px; padding:0px; line-height:1px">
            <xsl:call-template name="create_tree_gfx">
              <xsl:with-param name="p_indent" select="string-length($superclass_markers)"/>
              <xsl:with-param name="p_superclass" select="$p_qualifiedtypename"/>
              <xsl:with-param name="p_final" select="1"/>
            </xsl:call-template>
          </table>
        </xsl:when>
        <xsl:otherwise>
          <pre class="inheritance-tree">
            <xsl:call-template name="create_tree">
              <xsl:with-param name="p_indent" select="string-length($superclass_markers)"/>
              <xsl:with-param name="p_superclass" select="$p_qualifiedtypename"/>
              <xsl:with-param name="p_final" select="1"/>
            </xsl:call-template>
          </pre>
        </xsl:otherwise>
      </xsl:choose>      
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="tree_indentation_text_empty">
    <xsl:param name="p_indent"/>
    <xsl:choose>
      <xsl:when test="$p_indent&gt;1">
        <xsl:text>   </xsl:text>
        <xsl:call-template name="tree_indentation_text_empty">
          <xsl:with-param name="p_indent"><xsl:value-of select="$p_indent+-1"/></xsl:with-param>
        </xsl:call-template>
      </xsl:when>
    </xsl:choose>
  </xsl:template>

  <!-- Recursively insert indentation for heritage tree. -->
  <!-- This one uses images for nice, visual indentation. -->
  <!-- p_indent selects level of indentation. -->
  
  <xsl:template name="tree_indentation">
    <xsl:param name="p_indent"/>
    <xsl:choose>
      <xsl:when test="$p_indent&gt;1">
        <xsl:choose>
          <xsl:when test="$gjdoc.allowimages=1">
            <img width="26" height="21" border="0" src="images/tree-empty.png" class="classdoc-tree-image"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:text>   </xsl:text>
          </xsl:otherwise>
        </xsl:choose>

        <xsl:call-template name="tree_indentation">
          <xsl:with-param name="p_indent"><xsl:value-of select="$p_indent+-1"/></xsl:with-param>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:choose>
          <xsl:when test="$gjdoc.allowimages=1">
            <img width="26" height="21" border="0" src="images/tree-branch.png" class="classdoc-tree-image"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:text>+--</xsl:text>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="output_superclass_methods">
    <xsl:if test="gjdoc:methoddoc">
      <table class="classdoc-table">
        <tr>
          <td class="sub-section-header">
            <div class="sub-section-header">Methods inherited from class <xsl:value-of select="@qualifiedtypename"/></div>
          </td>
        </tr>
        <tr>
          <td class="member-summary super method">
            <xsl:for-each select="gjdoc:methoddoc">
              <code class="member-summary super method"><a href="{concat($gjdoc.pathtoroot, translate(../gjdoc:containingPackage/@name, '.', '/'), '/', ../@typename, '.html#', @name, gjdoc:signature/@full)}" class="member-summary super method"><xsl:value-of select="@name"/></a><xsl:if test="position() != last()">, </xsl:if></code>
            </xsl:for-each>
          </td>
        </tr>
      </table>
    </xsl:if>
  </xsl:template>


  <xsl:template name="output_superclass_fields">
    <xsl:if test="gjdoc:fielddoc">
      <table class="classdoc-table">
        <tr>
          <td class="sub-section-header">
            <div class="sub-section-header">Fields inherited from class <xsl:value-of select="@qualifiedtypename"/></div>
          </td>
        </tr>
        <tr>
          <td class="member-summary super field">
            <xsl:for-each select="gjdoc:fielddoc">
              <code class="member-summary super field"><a href="{concat($gjdoc.pathtoroot, translate(../gjdoc:containingPackage/@name, '.', '/'), '/', ../@typename, '.html#', @name)}" class="member-summary super field"><xsl:value-of select="@name"/></a><xsl:if test="position() != last()">, </xsl:if></code>
            </xsl:for-each>
          </td>
        </tr>
      </table>
    </xsl:if>
  </xsl:template>

</xsl:stylesheet>
