<?xml version="1.0" encoding="utf-8"?>

<!-- html_common.xsl
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

<!-- Common templates for HTML generation.
     -->

<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:gjdoc="http://www.gnu.org/software/cp-tools/gjdocxml"
  xmlns:html="http://www.w3.org/TR/REC-html40"
  xmlns="http://www.w3.org/TR/REC-html40">

  <xsl:include href="../gjdoc_common.xsl"/>

  <!-- HTML head directives to be included in all generated HTML files. -->

  <xsl:template name="include_common">
    <script src="{concat($gjdoc.pathtoroot, 'gjdoc.js')}" type="text/javascript"><xsl:comment>this comment required for konqueror 3.2.2</xsl:comment>
    </script>
    <xsl:choose>
      <xsl:when test="$gjdoc.option.stylesheetfile">
        <link rel="stylesheet" type="text/css" 
          href="{concat($gjdoc.pathtoroot, 'user.css')}"/>
      </xsl:when>
      <xsl:otherwise>
        <link rel="stylesheet" type="text/css" 
          href="{concat($gjdoc.pathtoroot, 'gjdochtml.css')}" title="GNU Clean"/>
        <link rel="stylesheet" type="text/css" 
          href="{concat($gjdoc.pathtoroot, 'gjdochtml-clean.css')}" title="GNU Clean"/>
        <link rel="alternate stylesheet" type="text/css" 
          href="{concat($gjdoc.pathtoroot, 'gjdochtml.css')}" title="Santa Clara"/>
        <link rel="alternate stylesheet" type="text/css" 
          href="{concat($gjdoc.pathtoroot, 'gjdochtml-sclara.css')}" title="Santa Clara"/>
        <link rel="alternate stylesheet" type="text/css" 
          href="{concat($gjdoc.pathtoroot, 'gjdochtml.css')}" title="Fixed Fruit"/>
        <link rel="alternate stylesheet" type="text/css" 
          href="{concat($gjdoc.pathtoroot, 'gjdochtml-fixed.css')}" title="Fixed Fruit"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="output_copyright_footer">
    <xsl:copy-of select="document('index.xml', /)/gjdoc:rootdoc/gjdoc:bottomnote"/>
    <hr class="footer"/>
    <div class="footer">
      <xsl:text>Generated on</xsl:text><xsl:value-of select="document('index.xml', /)/gjdoc:rootdoc/gjdoc:created"/><xsl:text> by </xsl:text><a href="http://www.gnu.org/software/cp-tools" target="cptoolsinfo"><xsl:text>GNU Classpath Tools</xsl:text></a><xsl:text> (Gjdoc XmlDoclet </xsl:text><xsl:value-of select="$gjdoc.xmldoclet.version"/><xsl:text>).</xsl:text>
    </div>
  </xsl:template>

  <!-- If the given class is also included, create a link to it. -->
  <!-- Otherwise output the qualified name in plain text. -->
  
  <xsl:template name="link_to_class_full">
    <xsl:param name="p_qualifiedname" select="@qualifiedtypename"/>
    <xsl:variable name="p_classdoc" select="document('index.xml', /)/gjdoc:rootdoc/gjdoc:classdoc[attribute::qualifiedtypename=$p_qualifiedname]"/>
    <xsl:choose>
      <xsl:when test="$p_classdoc">
        <a href="{concat($gjdoc.pathtoroot, translate($p_classdoc/gjdoc:containingPackage/@name, '.', '/'), '/', $p_classdoc/@name, '.html')}">
          <xsl:value-of select="$p_qualifiedname"/>
        </a>
      </xsl:when>
      <xsl:when test="$gjdoc.refdocs1 and document(concat($gjdoc.refdocs1, '/descriptor.xml'), /)//gjdoc:class[attribute::qualifiedtypename=$p_qualifiedname]">
        <a href="{concat($gjdoc.refdocs1, '/', $p_qualifiedname, '.html')}">
          <xsl:value-of select="$p_qualifiedname"/>
        </a>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$p_qualifiedname"/>
      </xsl:otherwise>
    </xsl:choose> 
  </xsl:template>
  
  <!-- If the given class is also included, create a link to it. -->
  <!-- Otherwise output the qualified name in plain text. -->

  <xsl:template name="link_to_class">
    <xsl:param name="p_qualifiedname" select="@qualifiedtypename"/>
    <xsl:param name="p_name" select="@name"/>
    <xsl:variable name="p_classdoc" select="document('index.xml', /)/gjdoc:rootdoc/gjdoc:classdoc[attribute::qualifiedtypename=$p_qualifiedname]"/>
    <xsl:choose>
      <xsl:when test="$p_classdoc">
        <a href="{concat($gjdoc.pathtoroot, translate($p_classdoc/gjdoc:containingPackage/@name, '.', '/'), '/', $p_classdoc/@name, '.html')}"><xsl:value-of select="$p_name"/></a>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$p_qualifiedname"/>
      </xsl:otherwise>
    </xsl:choose> 
  </xsl:template>

  <xsl:template name="output_alphaindex_category">
    <dl>
      <xsl:for-each select="gjdoc:entry">
        <dt>
          <xsl:element name="a">
            <xsl:attribute name="href">
              <xsl:choose>
                <xsl:when test="gjdoc:isPackage">
                  <xsl:value-of select="concat(translate(@name, '.', '/'), '/package-summary.html')"/>
                </xsl:when>
                <xsl:when test="gjdoc:isClass and gjdoc:containingClass">
                  <xsl:value-of select="concat(translate(gjdoc:containingPackage/@name, '.', '/'), '/', gjdoc:containingClass/@name, '.', @name, '.html')"/>
                </xsl:when>
                <xsl:when test="gjdoc:isClass">
                  <xsl:value-of select="concat(translate(gjdoc:containingPackage/@name, '.', '/'), '/', @name, '.html')"/>
                </xsl:when>
                <xsl:when test="gjdoc:isMethod">
                  <xsl:value-of select="concat(translate(gjdoc:containingPackage/@name, '.', '/'), '/', gjdoc:containingClass/@name, '.html#', gjdoc:method/@name, gjdoc:signature/@full)"/>
                </xsl:when>
                <xsl:when test="gjdoc:isField">
                  <xsl:value-of select="concat(translate(gjdoc:containingPackage/@name, '.', '/'), '/', gjdoc:containingClass/@name, '.html#', @name)"/>
                </xsl:when>
              </xsl:choose>
            </xsl:attribute>
            <xsl:value-of select="@name"/>
          </xsl:element>
          -
          <xsl:choose>
            <xsl:when test="gjdoc:isPackage">
              Package
            </xsl:when>
            <xsl:when test="gjdoc:isOrdinaryClass and gjdoc:containingClass">
              Nested class in class
              <xsl:value-of select="gjdoc:containingClass/@name"/>
            </xsl:when>
            <xsl:when test="gjdoc:isException and gjdoc:containingClass">
              Nested exception in class
              <xsl:value-of select="gjdoc:containingClass/@name"/>
            </xsl:when>
            <xsl:when test="gjdoc:isError and gjdoc:containingClass">
              Nested error in class
              <xsl:value-of select="gjdoc:containingClass/@name"/>
            </xsl:when>
            <xsl:when test="gjdoc:isInterface and gjdoc:containingClass">
              Nested interface in class
              <xsl:value-of select="gjdoc:containingClass/@name"/>
            </xsl:when>
            <xsl:when test="gjdoc:isOrdinaryClass">
              Class in package
              <xsl:value-of select="gjdoc:containingPackage/@name"/>
            </xsl:when>
            <xsl:when test="gjdoc:isException">
              Exception in package
              <xsl:value-of select="gjdoc:containingPackage/@name"/>
            </xsl:when>
            <xsl:when test="gjdoc:isError">
              Error in package
              <xsl:value-of select="gjdoc:containingPackage/@name"/>
            </xsl:when>
            <xsl:when test="gjdoc:isInterface">
              Interface in package
              <xsl:value-of select="gjdoc:containingPackage/@name"/>
            </xsl:when>
            <xsl:when test="gjdoc:isMethod">
              Method in class
              <xsl:value-of select="gjdoc:containingClass/@name"/>
            </xsl:when>
            <xsl:when test="gjdoc:isField">
              Field in class
              <xsl:value-of select="gjdoc:containingClass/@name"/>
            </xsl:when>
          </xsl:choose>
        </dt>
        <dd>
          <xsl:for-each select="gjdoc:firstSentenceTags/node()">
            <xsl:copy-of select="."/>
          </xsl:for-each>
        </dd>
      </xsl:for-each>
    </dl>
    
  </xsl:template>

  <xsl:template name="output_navlink">
    <xsl:param name="p_href"/>
    <xsl:param name="p_label"/>
    <xsl:param name="p_target" select="'_self'"/>
    <xsl:param name="p_enabled"/>
    <xsl:param name="p_current"/>
    <xsl:param name="p_onclick"/>
    <xsl:param name="p_last" select="0"/>
    <xsl:param name="p_css_class"/>
    <xsl:param name="p_title"/>

    <xsl:choose>
      <xsl:when test="$p_enabled">
        <span class="navi-button link"><a href="{$p_href}" target="{$p_target}" class="navi-button" title="{$p_title}" onClick="{$p_onclick}"><span class="{concat('navi-link ', $p_css_class)}"><xsl:value-of select="$p_label" disable-output-escaping="yes"/></span></a></span>
      </xsl:when>
      <xsl:when test="$p_current">
        <span class="navi-button current"><xsl:value-of select="$p_label" disable-output-escaping="yes"/></span>
      </xsl:when>
      <xsl:otherwise>
        <span class="navi-button unavailable"><xsl:value-of select="$p_label" disable-output-escaping="yes"/></span>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:if test="not($p_last)"><span class="navsep"><xsl:text disable-output-escaping="yes">&amp;nbsp;</xsl:text></span></xsl:if>
  </xsl:template>

  <xsl:template name="output_navbar">
    <xsl:param name="p_show_frames" select="1"/>
    <xsl:param name="p_curr_frames"/>

    <xsl:param name="p_show_noframes" select="1"/>
    <xsl:param name="p_curr_noframes"/>

    <xsl:param name="p_show_package" select="1"/>
    <xsl:param name="p_curr_package"/>

    <xsl:param name="p_show_package_tree" select="1"/>
    <xsl:param name="p_curr_package_tree"/>

    <xsl:param name="p_show_full_tree" select="1"/>
    <xsl:param name="p_curr_full_tree"/>

    <xsl:param name="p_show_index" select="1"/>
    <xsl:param name="p_curr_index"/>

    <xsl:param name="p_show_help" select="1"/>
    <xsl:param name="p_curr_help"/>

    <xsl:param name="p_show_about" select="1"/>
    <xsl:param name="p_curr_about"/>

    <xsl:param name="p_show_source" select="1"/>
    <xsl:param name="p_curr_source"/>

    <xsl:param name="p_show_class"/>
    <xsl:param name="p_curr_class"/>

    <xsl:param name="p_show_use"/>
    <xsl:param name="p_curr_use"/>

    <xsl:param name="p_show_deprecated" select="1"/>
    <xsl:param name="p_curr_deprecated"/>

    <xsl:param name="p_top"/>

    <xsl:variable name="v_navbar_class">
      <xsl:choose>
        <xsl:when test="$p_top">
          <xsl:value-of select="'navbar top'"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="'navbar bottom'"/>          
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>

    <xsl:if test="not($gjdoc.option.nonavbar)">
      <div class="{$v_navbar_class}">
      <table border="1" cellspacing="0" class="{$v_navbar_class}">
        <tr class="{$v_navbar_class}">
          <td class="{$v_navbar_class}">
            <div class="navbar-first-row">
              <xsl:call-template name="output_navlink">
                <xsl:with-param name="p_href" select="concat($gjdoc.pathtoroot, 'index_noframes.html')"/>
                <xsl:with-param name="p_label" select="'Overview'"/>
                <xsl:with-param name="p_enabled" select="$p_show_noframes"/>
                <xsl:with-param name="p_current" select="$p_curr_noframes"/>
              </xsl:call-template>
              <xsl:call-template name="output_navlink">
                <xsl:with-param name="p_href" select="'package-summary.html'"/>
                <xsl:with-param name="p_label" select="'Package'"/>
                <xsl:with-param name="p_enabled" select="$p_show_package"/>
                <xsl:with-param name="p_current" select="$p_curr_package"/>
              </xsl:call-template>
              <xsl:call-template name="output_navlink">
                <xsl:with-param name="p_href" select="$p_show_class"/>
                <xsl:with-param name="p_label" select="'Class'"/>
                <xsl:with-param name="p_enabled" select="$p_show_class"/>
                <xsl:with-param name="p_current" select="$p_curr_class"/>
              </xsl:call-template>
              <xsl:if test="$gjdoc.option.linksource">
                <xsl:call-template name="output_navlink">
                  <xsl:with-param name="p_href" select="$p_show_source"/>
                  <xsl:with-param name="p_label" select="'Source'"/>
                  <xsl:with-param name="p_enabled" select="$p_show_source"/>
                  <xsl:with-param name="p_current" select="$p_curr_source"/>
                </xsl:call-template>
              </xsl:if>
              <xsl:if test="$gjdoc.option.uses">
                <xsl:call-template name="output_navlink">
                  <xsl:with-param name="p_href" select="$p_show_use"/>
                  <xsl:with-param name="p_label" select="'Use'"/>
                  <xsl:with-param name="p_enabled" select="$p_show_use"/>
                  <xsl:with-param name="p_current" select="$p_curr_use"/>
              </xsl:call-template>
              </xsl:if>
              <xsl:call-template name="output_navlink">
                <xsl:with-param name="p_href" select="'package-tree.html'"/>
                <xsl:with-param name="p_label" select="'Tree'"/>
                <xsl:with-param name="p_enabled" select="$p_show_package_tree"/>
                <xsl:with-param name="p_current" select="$p_curr_package_tree"/>
              </xsl:call-template>
              <xsl:if test="not($gjdoc.option.noindex)">
                <xsl:call-template name="output_navlink">
                  <xsl:with-param name="p_href" select="concat($gjdoc.pathtoroot, 'alphaindex.html')"/>
                  <xsl:with-param name="p_label" select="'Index'"/>
                  <xsl:with-param name="p_enabled" select="$p_show_index"/>
                  <xsl:with-param name="p_current" select="$p_curr_index"/>
                </xsl:call-template>
              </xsl:if>
              <xsl:if test="not($gjdoc.option.notree)">
                <xsl:call-template name="output_navlink">
                  <xsl:with-param name="p_href" select="concat($gjdoc.pathtoroot, 'fulltree.html')"/>
                  <xsl:with-param name="p_label" select="'Full&amp;nbsp;Tree'"/>
                  <xsl:with-param name="p_enabled" select="$p_show_full_tree"/>
                  <xsl:with-param name="p_current" select="$p_curr_full_tree"/>
                </xsl:call-template>
              </xsl:if>
              <xsl:if test="not($gjdoc.option.nodeprecatedlist)">
                <xsl:call-template name="output_navlink">
                  <xsl:with-param name="p_href" select="concat($gjdoc.pathtoroot, 'deprecated.html')"/>
                  <xsl:with-param name="p_label" select="'Deprecated'"/>
                  <xsl:with-param name="p_enabled" select="$p_show_deprecated"/>
                  <xsl:with-param name="p_current" select="$p_curr_deprecated"/>
                </xsl:call-template>
              </xsl:if>
              <xsl:if test="not($gjdoc.option.nohelp)">
                <xsl:call-template name="output_navlink">
                  <xsl:with-param name="p_href" select="concat($gjdoc.pathtoroot, 'help.html')"/>
                  <xsl:with-param name="p_label" select="'Help'"/>
                  <xsl:with-param name="p_enabled" select="$p_show_help"/>
                  <xsl:with-param name="p_current" select="$p_curr_help"/>
                </xsl:call-template>
              </xsl:if>
              <xsl:call-template name="output_navlink">
                <xsl:with-param name="p_href" select="concat($gjdoc.pathtoroot, 'about.html')"/>
                <xsl:with-param name="p_label" select="'About'"/>
                <xsl:with-param name="p_enabled" select="$p_show_about"/>
                <xsl:with-param name="p_current" select="$p_curr_about"/>
                <xsl:with-param name="p_last" select="1"/>
              </xsl:call-template>
            </div>
            <div class="navbar-second-row">
              <xsl:call-template name="output_navlink">
                <xsl:with-param name="p_href" select="concat($gjdoc.pathtoroot, 'index.html')"/>
                <xsl:with-param name="p_label" select="'Frames'"/>
                <xsl:with-param name="p_enabled" select="1"/>
                <xsl:with-param name="p_target" select="'_top'"/>
              </xsl:call-template>
              <xsl:call-template name="output_navlink">
                <xsl:with-param name="p_href" select="concat($gjdoc.pathtoroot, 'index_noframes.html')"/>
                <xsl:with-param name="p_label" select="'No&amp;nbsp;Frames'"/>
                <xsl:with-param name="p_enabled" select="1"/>
                <xsl:with-param name="p_target" select="'_top'"/>
                <xsl:with-param name="p_last" select="0"/>
              </xsl:call-template>
                <xsl:call-template name="output_navlink">
                  <xsl:with-param name="p_onclick" select="'return setStyleSheet(&quot;GNU Clean&quot;);'"/>
                  <xsl:with-param name="p_title" select="'GNU Clean Style'"/>
                  <xsl:with-param name="p_label" select="'GNU Clean'"/>
                  <xsl:with-param name="p_enabled" select="1"/>
                  <xsl:with-param name="p_last" select="0"/>
                </xsl:call-template>
                <xsl:call-template name="output_navlink">
                  <xsl:with-param name="p_onclick" select="'return setStyleSheet(&quot;Santa Clara&quot;);'"/>
                  <xsl:with-param name="p_title" select="'Santa Clara Style'"/>
                  <xsl:with-param name="p_label" select="'Santa Clara'"/>
                  <xsl:with-param name="p_enabled" select="1"/>
                  <xsl:with-param name="p_last" select="0"/>
                </xsl:call-template>
                <xsl:call-template name="output_navlink">
                  <xsl:with-param name="p_onclick" select="'return setStyleSheet(&quot;Fixed Fruit&quot;);'"/>
                  <xsl:with-param name="p_title" select="'Fixed Fruit Style'"/>
                  <xsl:with-param name="p_label" select="'Fixed Fruit'"/>
                  <xsl:with-param name="p_enabled" select="1"/>
                  <xsl:with-param name="p_last" select="0"/>
                </xsl:call-template>
                <xsl:call-template name="output_navlink">
                  <xsl:with-param name="p_onclick" select="'return setStyleSheet(&quot;Vanilla&quot;);'"/>
                  <xsl:with-param name="p_title" select="'Vanilla Style'"/>
                  <xsl:with-param name="p_label" select="'Vanilla'"/>
                  <xsl:with-param name="p_enabled" select="1"/>
                  <xsl:with-param name="p_last" select="0"/>
                </xsl:call-template>

            </div>
          </td>
          <xsl:choose>
            <xsl:when test="$p_top">
              <xsl:if test="$gjdoc.option.header">
                <td class="{$v_navbar_class} navbar-header">
                  <xsl:value-of select="$gjdoc.option.header"/>
                </td>
              </xsl:if>
            </xsl:when>
            <xsl:otherwise>              
              <xsl:if test="$gjdoc.option.footer">
                <td class="{$v_navbar_class} navbar-footer">
                  <xsl:value-of select="$gjdoc.option.footer"/>
                </td>
              </xsl:if>
            </xsl:otherwise>
          </xsl:choose>

        </tr>
      </table>

      </div>
      <xsl:if test="not($p_top) and $gjdoc.option.bottom">
        <hr/>
        <xsl:value-of select="$gjdoc.option.bottom"/>
      </xsl:if>     
    </xsl:if>
  </xsl:template>

  <xsl:template name="link_to_class_source">
    <xsl:choose>
      <xsl:when test="$gjdoc.option.linksource">
        <a href="{concat($gjdoc.pathtoroot, 'src-html/', translate(gjdoc:containingPackage/@name, '.', '/'), '/', @name, '.html')}"><b><xsl:value-of select="@name"/></b></a>
      </xsl:when>
      <xsl:otherwise>
        <b><xsl:value-of select="@name"/></b>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="link_to_member_source">
    <xsl:choose>
      <xsl:when test="$gjdoc.option.linksource">
        <a href="{concat($gjdoc.pathtoroot, 'src-html/', translate(gjdoc:containingPackage/@name, '.', '/'), '/', gjdoc:containingClass/@typename, '.html#line.', gjdoc:position/@line)}"><b><xsl:value-of select="@name"/></b></a>
      </xsl:when>
      <xsl:otherwise>
        <b><xsl:value-of select="@name"/></b>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="output_title">
    <xsl:param name="p_pagetitle"/>
    <xsl:variable name="v_title">
      <xsl:value-of select="$p_pagetitle"/>
      <xsl:if test="$gjdoc.option.windowtitle">
        <xsl:text> (</xsl:text>
        <xsl:value-of select="$gjdoc.option.windowtitle"/>
        <xsl:text>)</xsl:text>
      </xsl:if>      
    </xsl:variable>
    <title>
      <xsl:value-of select="$v_title"/>
    </title>
  </xsl:template>

  <xsl:template name="get_qualified_type">
    <xsl:param name="p_typename"/>
    <xsl:variable name="v_plaintype">
      <xsl:choose>
        <xsl:when test="contains($p_typename,'[')">
          <xsl:value-of select="normalize-space(substring-before($p_typename, '['))"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="$p_typename"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:variable name="v_containingpackage">
      <xsl:value-of select="ancestor-or-self::gjdoc:classdoc/gjdoc:containingPackage/attribute::name"/>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="document('index.xml', /)/gjdoc:rootdoc/gjdoc:classdoc[attribute::name=$v_plaintype]/gjdoc:containingPackage[attribute::name=$v_containingpackage]">
        <xsl:value-of select="$v_containingpackage"/>
        <xsl:text>.</xsl:text>
      </xsl:when>
      <xsl:when test="document('index.xml', /)/gjdoc:rootdoc/gjdoc:classdoc[attribute::name=$v_plaintype]/gjdoc:containingPackage[attribute::name='java.lang']">
        <xsl:text>java.lang.</xsl:text>
      </xsl:when>
    </xsl:choose>
    <xsl:value-of select="$p_typename"/>
  </xsl:template>


  <xsl:template name="output_link_tag">
    <xsl:param name="p_contexturl"/>
    <xsl:variable name="v_see">
      <xsl:choose>
        <xsl:when test="contains(., '(')">
          <xsl:value-of select="normalize-space(substring-before(., '('))"/>
          <xsl:text>(</xsl:text>
          <xsl:call-template name="resolve_parameter_list">
            <xsl:with-param name="p_anchor">
              <xsl:value-of select="normalize-space(substring-before(substring-after(., '('), ')'))"/>
            </xsl:with-param>
          </xsl:call-template>
          <xsl:text>)</xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="."/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:variable name="v_class">
      <xsl:choose>
        <xsl:when test="contains(., '#')">
          <xsl:value-of select="normalize-space(substring-before(., '#'))"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="normalize-space(.)"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:variable name="v_anchor" select="normalize-space(substring-after(., '#'))"/>
    <xsl:variable name="v_seeanchor">
      <xsl:if test="contains($v_see, '#')">
        <xsl:value-of select="concat('#', normalize-space(substring-after($v_see, '#')))"/>
      </xsl:if>
    </xsl:variable>
    <xsl:variable name="v_classname" select="document('index.xml', /)/gjdoc:rootdoc/gjdoc:classdoc[attribute::name=$v_class]/attribute::qualifiedtypename"/>
    <xsl:variable name="v_classnamefq" select="document('index.xml', /)/gjdoc:rootdoc/gjdoc:classdoc[attribute::qualifiedtypename=$v_class]/attribute::qualifiedtypename"/>
    <xsl:choose>
      <xsl:when test="starts-with(., '#')">
        <xsl:choose>
          <xsl:when test="$p_contexturl">
            <a href="{concat($p_contexturl, translate($v_see,' ',''))}"><xsl:value-of select="$v_anchor"/></a>            
          </xsl:when>
          <xsl:otherwise>
            <a href="{translate($v_see,' ','')}"><xsl:value-of select="$v_anchor"/></a>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <xsl:when test="$v_classname">
        <a href="{translate(concat($gjdoc.pathtoroot, translate($v_classname, '.', '/'), '.html', $v_seeanchor),' ','')}"><xsl:value-of select="translate($v_see,'#','.')"/></a>
      </xsl:when>
      <xsl:when test="$v_classnamefq">
        <a href="{translate(concat($gjdoc.pathtoroot, translate($v_classnamefq, '.', '/'), '.html', $v_seeanchor),' ','')}"><xsl:value-of select="translate($v_see,'#','.')"/></a>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="translate(., '#', '.')" disable-output-escaping="yes"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="resolve_parameter_list">
    <xsl:param name="p_anchor"/>
    <xsl:variable name="v_parameter">
      <xsl:value-of select="normalize-space(substring-before($p_anchor, ','))"/>
    </xsl:variable>
    <xsl:variable name="v_remainder">
      <xsl:value-of select="normalize-space(substring-after($p_anchor, ','))"/>
    </xsl:variable>
    <xsl:call-template name="get_qualified_type">
      <xsl:with-param name="p_typename">
        <xsl:choose>
          <xsl:when test="string-length($v_remainder)>0">
            <xsl:value-of select="$v_parameter"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="$p_anchor"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:with-param>
    </xsl:call-template>

    <xsl:if test="string-length($v_remainder)>0">
      <xsl:text>,</xsl:text>
      <xsl:call-template name="resolve_parameter_list">
        <xsl:with-param name="p_anchor">
          <xsl:value-of select="$v_remainder"/>
        </xsl:with-param>
      </xsl:call-template>
    </xsl:if>
  </xsl:template>

</xsl:stylesheet>

