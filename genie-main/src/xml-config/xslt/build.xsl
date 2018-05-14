<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
<xsl:output method="text"/>

<!--param values can be supplied to the XSLT processor-->
<!--e.g. '${DEFINE)' could be changed to '\DEFINE:' if req'd by Windows-->  
<xsl:param name="defsym" select="'$(DEFINE)'"/>
<xsl:param name="defrepsym" select="'='"/>

  <xsl:template match="/">
    <!--note, <job> is the top-level element after definition
        and job files have been merged-->
    <xsl:apply-templates select="job"/>
  </xsl:template>
  
  <xsl:template match="job">
   <xsl:apply-templates select="build"/>
   <xsl:apply-templates select="config"/>
  </xsl:template>

  <xsl:template match="config">
   <xsl:text>MODULE_NAMES='</xsl:text>
    <xsl:for-each select="model">
     <xsl:value-of select="normalize-space(@name)"/>
     <xsl:text> </xsl:text>
    </xsl:for-each>
    <xsl:text>'</xsl:text>
    <xsl:text>&#xa;</xsl:text>
   <xsl:text>FPPOPTS2='</xsl:text>
    <xsl:for-each select="model">
     <xsl:value-of select="$defsym"/>
     <xsl:text>USE_</xsl:text>
     <xsl:value-of select="normalize-space(@name)"/>
     <xsl:text> </xsl:text>
    </xsl:for-each>
    <xsl:text>'</xsl:text>
    <xsl:text>&#xa;</xsl:text>
  </xsl:template>
  
  <xsl:template match="build">
   <xsl:apply-templates select="make-arg"/>
   <xsl:text>FPPOPTS='</xsl:text>
   <xsl:for-each select="macro">
    <xsl:if test="@status = 'defined'">
     <xsl:value-of select="$defsym"/>
     <xsl:value-of select="normalize-space(identifier)"/>
     <xsl:if test="replacement != ''">
       <xsl:value-of select="$defrepsym"/>
       <xsl:value-of select="normalize-space(replacement)"/>
     </xsl:if>
     <xsl:text> </xsl:text>
    </xsl:if>
   </xsl:for-each>
   <xsl:text>'</xsl:text>
    <xsl:text>&#xa;</xsl:text>
  </xsl:template>

  <xsl:template match="make-arg">
    <xsl:value-of select="normalize-space(@name)"/>
    <xsl:text>=</xsl:text>
    <xsl:value-of select="normalize-space(.)"/>
    <xsl:text>&#xa;</xsl:text>
  </xsl:template>

</xsl:stylesheet>
