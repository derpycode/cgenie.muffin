<?xml version="1.0"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<!--
    CWA
    merge.xsl: merge a config doc into the definition doc and output the result.
    The approach is to iterate through the defintiion document and check to 
    see if the user has overridden an option in the config file. If so, output
    the value given in the config file rather than the definition default. If not,
    just copy the definition default into the output.

    SAM - 20100721
    made some additions to the functionality of the original
    'merge.xsl' stylesheet (history and model-state features) and
    separated it into two functional parts which are now two
    separate stylesheets ('apply_job_configuration.xsl' (this
    stylesheet) and 'build_job.xsl')

    'apply_job_configuration.xsl' updates values in a "definition" or
    "pre-configured definition" document
    ('genie-main/src/xml-config/xml/definition.xml' is the default
    "definition" document) with values specified in a "job
    configuration" document and outputs a "pre-configured definition"
    document, which can be used to replace the previous "definition"
    or "pre-configured definition" document in subsequent applications
    of 'merge_config.xsl'.

    'build_job.xsl' (this stylesheet) converts the "pre-configured
    definition" into a "job" document.

-->

<xsl:param name="DATE" select="unknown"/>
<xsl:param name="CODEDIR" select="'~/genie'"/>
<xsl:param name="OUTROOT" select="'~/genie_output'"/>
<xsl:param name="RUNTIME_ROOT" select="'../..'"/>
<xsl:param name="RUNTIME_OUTDIR" select="'.'"/>

<xsl:output method="xml" indent="yes"/>

<!-- The path of the config document must be passed in as the parameter user_xml.-->
<xsl:param name="user_xml"/>
<!-- Convert the config document into a parseable set of nodes-->
<xsl:param name="user_nodes" select="document($user_xml)"/>

<!-- 
     Head template. This matches on the root of the definition document, tests whether the config
     file has been specified (exits if not) and then processes each child of the <definition> root in turn.
 -->
<xsl:template match="/" >
  <xsl:if test="not($user_xml)">
    <xsl:message terminate="yes">
      <xsl:text>Error: The path of the config file must be passed in as the parameter 'user_xml'.</xsl:text>
    </xsl:message>
  </xsl:if>
  <definition creator="apply_job_configuration.xsl" date="{$DATE}">
    <history>
      <xsl:copy>
	<xsl:copy-of select="definition/history/@*" />
      </xsl:copy>
      <entry action="apply job configuration" date="{$DATE}" name="{$user_nodes/job/@name}" author="{$user_nodes/job/@author}"/>
      <xsl:apply-templates select="definition/history/*"/>
    </history>
    <xsl:apply-templates select="definition/*"/>
  </definition>
</xsl:template>

<!-- Match on <history> elements: Empty template deletes old history.
All of its children and attributes are used to build the updated
history element elsewhere -->
<xsl:template match="history"/>

<!--
     Match on <var> elements. <var> may appear in two places in the definition document: 
     as a child of <vars>, defining variables, or as a child of <testing>. Each occurrence 
     is handled separately. <vars><var> elements are handled in the first 'when' block
     and <testing><var> elements are handled in the second.
     <var>s here are mixed content, so call the 'outputMixed' template.
-->
<xsl:template match="var">
 <xsl:choose>
  <xsl:when test="local-name(..)='vars' and $user_nodes/job/vars/var[@name=current()/@name]">
    <var name="{@name}">
      <!-- <xsl:value-of select="$user_nodes/job/vars/var[@name=current()/@name]/text()"/>-->
      <xsl:call-template name="outputMixed">
	<xsl:with-param name="newVal" select="$user_nodes/job/vars/var[@name=current()/@name]/node()"/>
      </xsl:call-template>
    </var>
  </xsl:when>
  <xsl:when test="local-name(..)='testing' and $user_nodes/job/testing/var[@name=current()/@name]">
   <var name="{@name}">
    <xsl:call-template name="outputMixed">
     <xsl:with-param name="newVal" select="$user_nodes/job/testing/var[@name=current()/@name]/node()"/>
    </xsl:call-template>
   </var>
  </xsl:when>
  <xsl:otherwise>
    <!-- Don't copy <vars><var> elements with the names 'OUTROOT',
	 'CODEDIR', 'RUNTIME_ROOT', 'RUNTIME_ROOT' -->
    <xsl:if test="not(local-name(..)='vars' and (@name='OUTROOT' or @name='CODEDIR' or @name='RUNTIME_ROOT' or @name='RUNTIME_OUTDIR'))">
      <xsl:copy>
	<xsl:copy-of select="@*" />
	<xsl:call-template name="outputMixed">
	  <xsl:with-param name="newVal" select="node()"/>
	</xsl:call-template>
      </xsl:copy>
    </xsl:if>
<!--
    <xsl:copy-of select="normalize-space(text())" />
    <xsl:apply-templates select="*"/>
-->
  </xsl:otherwise>
 </xsl:choose>
</xsl:template>

<!-- Match on <config> elements. Always newly define <var> elements
    with the names 'OUTROOT', 'CODEDIR', 'RUNTIME_ROOT', 'RUNTIME_ROOT'
    -->
<xsl:template match="vars">
  <vars>
    <var name="OUTROOT"><xsl:value-of select="$OUTROOT"/></var>
    <var name="CODEDIR"><xsl:value-of select="$CODEDIR"/></var>
    <var name="RUNTIME_ROOT"><xsl:value-of select="$RUNTIME_ROOT"/></var>
    <var name="RUNTIME_OUTDIR"><xsl:value-of select="$RUNTIME_OUTDIR"/></var>
    <xsl:apply-templates select="var"/>
  </vars>
</xsl:template>

<!--
    Match on <config> elements.
-->
<xsl:template match="config">
  <config>
    <xsl:apply-templates select="model" mode="state"/>
    <xsl:apply-templates select="*"/>
  </config>
</xsl:template>


<!--
    Match on <build><make-arg> elements. These elements have a simple type, so this
    template just copies any text given in the config file.
-->
<xsl:template match="make-arg">
 <xsl:choose>
  <xsl:when test="$user_nodes/job/build/make-arg[@name=current()/@name]">
   <make-arg name="{@name}">
    <xsl:value-of select="$user_nodes/job/build/make-arg[@name=current()/@name]/text()"/>
   </make-arg>
  </xsl:when>
  <xsl:otherwise>
   <xsl:copy>
    <xsl:copy-of select="@*" />
    <xsl:apply-templates select="./node()"/><!-- need to match on node() here to catch text children-->
   </xsl:copy>
  </xsl:otherwise>
 </xsl:choose>
</xsl:template>


<!--
    Match on <build><macro> elements. Macros are cross-checked with those in the config 
    file that have the same identifier, rather than looking for the same handle.
-->
<xsl:template match="macro">
 <xsl:choose>
  <xsl:when test="$user_nodes/job/build/macro[normalize-space(identifier)=normalize-space(current()/identifier)]">
   <macro handle="{$user_nodes/job/build/macro[normalize-space(identifier)=normalize-space(current()/identifier)]/@handle}"
          status="{$user_nodes/job/build/macro[normalize-space(identifier)=normalize-space(current()/identifier)]/@status}">
    <xsl:apply-templates select="$user_nodes/job/build/macro[normalize-space(identifier)=normalize-space(current()/identifier)]/node()"/>
   </macro>
  </xsl:when>
  <xsl:otherwise>
   <xsl:copy>
    <xsl:copy-of select="@*" />
    <xsl:apply-templates select="*"/>
   </xsl:copy>
  </xsl:otherwise>
 </xsl:choose>
</xsl:template>

<!--
This template matches <model> elements and copies all available entries from the "definition" or "pre-configured definition" document unless it's a child of <config>
-->
<xsl:template match="model">
  <xsl:if test="not(local-name(..)='config')">
    <xsl:copy>
      <xsl:copy-of select="@*" />
      <xsl:apply-templates select="*"/>
    </xsl:copy>
  </xsl:if>
</xsl:template>

<!--
Special version of the template which matches 'model' elements and is used to update the 'state' attribute of /definition/config/model to 'on' if the model is listed in the "job configuration".
-->
<xsl:template match="model" mode="state">
  <xsl:choose>
    <xsl:when test="@name=$user_nodes/job/config/model/@name">
      <model name="{@name}" type="{@type}" flagname="{@flagname}" state="on">
	<xsl:apply-templates select="*"/>
      </model>
    </xsl:when>
    <xsl:otherwise>
      <xsl:copy>
	<xsl:copy-of select="@*" />
	<xsl:apply-templates select="*"/>
      </xsl:copy>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<!--
    Match namelists.
-->
<xsl:template match="namelist">
 <xsl:copy>
  <xsl:copy-of select="@*" />
  <xsl:apply-templates select="param"/>
  <xsl:apply-templates select="paramArray"/>
 </xsl:copy>
</xsl:template>



<!--
    Process <namelist><param> elements. If there is a config file override, pass the new value
    down the call tree to the template for <value>.
-->
<xsl:template match="param">
 <xsl:choose>
  <xsl:when test="local-name(../../..)='control'">
   <xsl:choose>
    <xsl:when test="$user_nodes//control/param[@name=current()/@name]">
     <xsl:copy>
      <xsl:copy-of select="@*" />
      <xsl:apply-templates select="*">
       <xsl:with-param name="newVal" select="$user_nodes//control/param[@name=current()/@name]/node()"/>
      </xsl:apply-templates>
     </xsl:copy>
    </xsl:when>
    <xsl:otherwise>
     <xsl:copy>
      <xsl:copy-of select="@*" />
      <xsl:apply-templates select="./*"/>
     </xsl:copy>
    </xsl:otherwise>
   </xsl:choose>
  </xsl:when>
  <xsl:when test="local-name(../../..)='model'">
   <xsl:variable name="modelName" select="../../../@name"/>
   <xsl:choose>
    <xsl:when test="$user_nodes//model[@name=$modelName]/param[@name=current()/@name]">
     <xsl:copy>
      <xsl:copy-of select="@*" />
      <xsl:apply-templates select="*">
       <xsl:with-param name="newVal" select="$user_nodes//model[@name=$modelName]/param[@name=current()/@name]/node()"/>
      </xsl:apply-templates>
     </xsl:copy>
    </xsl:when>
    <xsl:otherwise>
     <xsl:copy>
      <xsl:copy-of select="@*" />
      <xsl:apply-templates select="./*"/>
     </xsl:copy>
    </xsl:otherwise>
   </xsl:choose>
  </xsl:when>
  <xsl:otherwise>
   <xsl:message terminate="yes">
    <xsl:text>Error in processing param </xsl:text><xsl:value-of select="@name"/>
    <xsl:text>: It belongs to neither a model or control element</xsl:text>
   </xsl:message>
  </xsl:otherwise>
 </xsl:choose>
</xsl:template>

<!--
    Process <namelist><paramArray><param> elements. Again, pass the new value down the call 
    tree if there is an override.
-->
<xsl:template match="param" mode="array">
 <xsl:choose>
  <xsl:when test="local-name(../../../..)='control'">
   <xsl:choose>
    <xsl:when test="$user_nodes//control/paramArray[@name=current()/../@name]/param[@index=current()/@index]">
     <xsl:copy>
      <xsl:copy-of select="@*" />
      <xsl:apply-templates select="*">
       <xsl:with-param name="newVal" select="$user_nodes//control/paramArray[@name=current()/../@name]/param[@index=current()/@index]/node()"/>
      </xsl:apply-templates>
     </xsl:copy>
    </xsl:when>
    <xsl:otherwise>
     <xsl:copy>
      <xsl:copy-of select="@*" />
      <xsl:apply-templates select="./*"/>
     </xsl:copy>
    </xsl:otherwise>
   </xsl:choose>
  </xsl:when>
  <xsl:when test="local-name(../../../..)='model'">
   <xsl:variable name="modelName" select="../../../../@name"/>
   <xsl:choose>
    <xsl:when test="$user_nodes//model[@name=$modelName]/paramArray[@name=current()/../@name]/param[@index=current()/@index]">
     <xsl:copy>
      <xsl:copy-of select="@*" />
      <xsl:apply-templates select="*">
       <xsl:with-param name="newVal" select="$user_nodes//model[@name=$modelName]/paramArray[@name=current()/../@name]/param[@index=current()/@index]/node()"/>
      </xsl:apply-templates>
     </xsl:copy>
    </xsl:when>
    <xsl:otherwise>
     <xsl:copy>
      <xsl:copy-of select="@*" />
       <xsl:apply-templates select="./*"/>
     </xsl:copy>
    </xsl:otherwise>
   </xsl:choose>
  </xsl:when>
  <xsl:otherwise>
   <xsl:message terminate="yes">
    <xsl:text>Error in processing paramArray param </xsl:text><xsl:value-of select="current()/../@name"/>
    <xsl:text>: It belongs to neither a model or control element</xsl:text>
   </xsl:message>
  </xsl:otherwise>
 </xsl:choose>
</xsl:template>

<!--
    Handling <param> children of <paramArrays> different to standalone <param> elements.
-->
<xsl:template match="paramArray">
 <xsl:copy>
  <xsl:copy-of select="@*" />
  <xsl:copy-of select="description"/>
  <xsl:apply-templates select="./param" mode="array"/>
 </xsl:copy>
</xsl:template>




<!--
    If a new value has been passed down the call tree, output that new value,
    otherwise just output the value given in the definition file.
-->
<xsl:template match="value">
 <xsl:param name="newVal"/>
 <xsl:copy>
  <xsl:copy-of select="@*" />
  <xsl:choose>
   <xsl:when test="$newVal">
    <xsl:call-template name="outputMixed">
     <xsl:with-param name="newVal" select="$newVal"/>
    </xsl:call-template>
   </xsl:when>
   <xsl:otherwise>
    <xsl:for-each select="node()">
     <xsl:choose>
      <xsl:when test="local-name()='varref'">
       <xsl:apply-templates select="."/>
      </xsl:when>
      <xsl:when test="local-name()='sep'">
       <xsl:apply-templates select="."/>
      </xsl:when>
      <xsl:otherwise>
       <xsl:value-of select="."/>
      </xsl:otherwise>
     </xsl:choose>
    </xsl:for-each>
   </xsl:otherwise>
  </xsl:choose>
 </xsl:copy>
</xsl:template>


<!--
    This callable template outputs a path, which consist of mixed 
    content (text, <varref> and <sep> nodes). A path is passed through
    the 'newVal' parameter.
-->
<xsl:template name="outputMixed">
 <xsl:param name="newVal"/>
    <xsl:for-each select="$newVal">
     <xsl:choose>
      <xsl:when test="local-name()='varref'">
       <xsl:apply-templates select="."/>
      </xsl:when>
      <xsl:when test="local-name()='sep'">
       <xsl:apply-templates select="."/>
      </xsl:when>
      <xsl:otherwise>
       <xsl:value-of select="."/>
      </xsl:otherwise>
     </xsl:choose>
    </xsl:for-each>
</xsl:template>


<!--
    Catch-all template. Any nodes that are not specially handled are simply output
    in the form they are presented.
-->
<xsl:template match="node()">
   <xsl:copy>
    <xsl:copy-of select="@*" />
    <xsl:copy-of select="normalize-space(text())" />
    <xsl:apply-templates select="*"/>
   </xsl:copy>
</xsl:template>

</xsl:stylesheet>
