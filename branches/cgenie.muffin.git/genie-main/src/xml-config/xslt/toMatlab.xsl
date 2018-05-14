<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
	<xsl:output method="xml" version="1.0" encoding="UTF-8" indent="yes"/>

	<xsl:param name="UP" select="'ABCDEFGHIJKLMNOPQRSTUVWXYZ'"/>
	<xsl:param name="LO" select="'abcdefghijklmnopqrstuvwxyz'"/>
	<xsl:param name="DIR_SEP" select="'/'"/>

	<xsl:param name="mixedcontent" select="false()"/>

	<xsl:template match="node()">
		<xsl:apply-templates/>
	</xsl:template>

	<xsl:template match="job">
		<xsl:element name="job">
			<xsl:element name="author">
				<xsl:attribute name="type">char</xsl:attribute>
				<xsl:attribute name="idx">1</xsl:attribute>
				<xsl:attribute name="size">
					<xsl:text>1 </xsl:text>
					<xsl:value-of select="string-length(@author)"/>
				</xsl:attribute>
				<xsl:value-of select="@author"/>
			</xsl:element>
			<xsl:apply-templates select="*"/>
		</xsl:element>
	</xsl:template>

	<xsl:template match="definition">
		<xsl:element name="definition">
			<xsl:call-template name="addSingleStructAtts"/>
			<xsl:apply-templates select="*"/>
		</xsl:element>
	</xsl:template>

	<xsl:template match="vars">
		<xsl:element name="vars">
			<xsl:call-template name="addSingleStructAtts"/>
			<xsl:apply-templates select="var"/>
		</xsl:element>
	</xsl:template>

	<xsl:template match="testing">
		<xsl:element name="testing">
			<xsl:call-template name="addSingleStructAtts"/>
			<xsl:apply-templates select="var"/>
		</xsl:element>
	</xsl:template>

	<xsl:template match="var">
		<xsl:param name="varname" select="@name"/>
		<xsl:element name="{$varname}">
			<xsl:call-template name="addSingleStructAtts"/>
			<xsl:call-template name="processString"/>
		</xsl:element>
	</xsl:template>

	<xsl:template match="config">
		<xsl:element name="config">
			<xsl:call-template name="addSingleStructAtts"/>
			<xsl:apply-templates select="model"/>
		</xsl:element>
	</xsl:template>

	<xsl:template match="model">
		<xsl:param name="modelname" select="@name"/>
		<xsl:element name="{$modelname}">
			<xsl:call-template name="addSingleStructAtts"/>
			<xsl:if test="@type">
				<xsl:element name="type">
					<xsl:attribute name="type">char</xsl:attribute>
					<xsl:attribute name="idx">1</xsl:attribute>
					<xsl:attribute name="size">
						<xsl:text>1 </xsl:text>
						<xsl:value-of select="string-length(@type)"/>
					</xsl:attribute>
					<xsl:value-of select="normalize-space(@type)"/>
				</xsl:element>
			</xsl:if>
			<xsl:if test="@flagname">
				<xsl:element name="flagname">
					<xsl:attribute name="type">char</xsl:attribute>
					<xsl:attribute name="idx">1</xsl:attribute>
					<xsl:attribute name="size">
						<xsl:text>1 </xsl:text>
						<xsl:value-of select="string-length(@flagname)"/>
					</xsl:attribute>
					<xsl:value-of select="normalize-space(@flagname)"/>
				</xsl:element>
			</xsl:if>
		</xsl:element>
	</xsl:template>

	<xsl:template match="build">
		<xsl:element name="build">
			<xsl:call-template name="addSingleStructAtts"/>
			<xsl:element name="make_arg">
				<xsl:call-template name="addSingleStructAtts"/>
				<xsl:apply-templates select="make-arg"/>
			</xsl:element>
			<xsl:element name="macro">
				<xsl:call-template name="addSingleStructAtts"/>
				<xsl:apply-templates select="macro"/>
			</xsl:element>
		</xsl:element>
	</xsl:template>

	<xsl:template match="make-arg">
		<xsl:param name="name" select="@name"/>
		<xsl:element name="{$name}">
			<xsl:call-template name="addSingleStructAtts"/>
			<xsl:call-template name="processString"/>
		</xsl:element>
	</xsl:template>

	<xsl:template match="macro">
		<xsl:param name="handle" select="@handle"/>
		<xsl:param name="status" select="@status"/>
		<xsl:element name="{$handle}">
			<xsl:call-template name="addSingleStructAtts"/>
			<xsl:element name="status">
				<xsl:call-template name="addSingleStructAtts"/>
				<xsl:attribute name="type">char</xsl:attribute>
				<xsl:attribute name="idx">1</xsl:attribute>
				<xsl:attribute name="size">
					<xsl:text>1 </xsl:text>
					<xsl:value-of select="string-length(@status)"/>
				</xsl:attribute>
				<xsl:value-of select="normalize-space(@status)"/>
			</xsl:element>
			<xsl:apply-templates select="identifier"/>
			<xsl:apply-templates select="replacement"/>
		</xsl:element>
	</xsl:template>

	<xsl:template match="identifier">
		<xsl:element name="identifier">
			<xsl:call-template name="addSingleStructAtts"/>
			<xsl:call-template name="processString"/>
		</xsl:element>
	</xsl:template>

	<xsl:template match="replacement">
		<xsl:element name="replacement">
			<xsl:call-template name="addSingleStructAtts"/>
			<xsl:call-template name="processString"/>
		</xsl:element>
	</xsl:template>

	<xsl:template match="parameters">
		<xsl:element name="parameters">
			<xsl:call-template name="addSingleStructAtts"/>
			<xsl:apply-templates select="*"/>
		</xsl:element>
	</xsl:template>

	<xsl:template match="control">
		<xsl:element name="control">
			<xsl:call-template name="addSingleStructAtts"/>
			<xsl:apply-templates select="*"/>
		</xsl:element>
	</xsl:template>

	<xsl:template match="parameters/model">
		<xsl:param name="modelname" select="@name"/>
		<xsl:element name="{$modelname}">
			<xsl:call-template name="addSingleStructAtts"/>
			<xsl:apply-templates select="*"/>
		</xsl:element>
	</xsl:template>

	<xsl:template match="file">
		<xsl:param name="filename" select="@name"/>
		<xsl:element name="{$filename}">
			<xsl:call-template name="addSingleStructAtts"/>
			<xsl:apply-templates select="*"/>
		</xsl:element>
	</xsl:template>

	<xsl:template match="namelist">
		<xsl:param name="elname" select="@name"/>
		<xsl:element name="{$elname}">
			<xsl:call-template name="addSingleStructAtts"/>
			<xsl:apply-templates select="*"/>
		</xsl:element>
	</xsl:template>

<!--	<xsl:template match="param">
		<xsl:param name="paramname" select="@name"/>
		<xsl:element name="{$paramname}">
			<xsl:call-template name="addSingleStructAtts"/>
			<xsl:element name="value">
				<xsl:apply-templates select="value"/>
			</xsl:element>
			<xsl:element name="description">
				<xsl:apply-templates select="description"/>
			</xsl:element>
		</xsl:element>
	</xsl:template> -->

	<xsl:template match="param">
		<xsl:param name="paramname" select="@name"/>
		<xsl:element name="{$paramname}">
			<xsl:attribute name="idx">1</xsl:attribute>
			<xsl:attribute name="type">param</xsl:attribute>
			<xsl:attribute name="size">
				<xsl:text>1 1</xsl:text>
			</xsl:attribute>
			<xsl:element name="value">
				<xsl:apply-templates select="value"/>
			</xsl:element>
			<xsl:element name="description">
				<xsl:apply-templates select="description"/>
			</xsl:element>
		</xsl:element>
	</xsl:template>

	<xsl:template match="value">
		<xsl:param name="idx">1</xsl:param>
		<xsl:choose>
			<xsl:when test="@datatype='boolean'">
				<xsl:call-template name="addAttributes">
					<xsl:with-param name="type">boolean</xsl:with-param>
					<xsl:with-param name="size">1 1</xsl:with-param>
					<xsl:with-param name="idx">
						<xsl:value-of select="$idx"/>
					</xsl:with-param>
				</xsl:call-template>
				<xsl:choose>
					<xsl:when test="translate(.,$UP,$LO)='.false.' or translate(.,$UP,$LO)='false'">false</xsl:when>
					<xsl:when test="translate(.,$UP,$LO)='.true.' or translate(.,$UP,$LO)='true'">true</xsl:when>
				</xsl:choose>
			</xsl:when>
			<xsl:when test="@datatype='integer'">
				<xsl:call-template name="addAttributes">
					<xsl:with-param name="type">int32</xsl:with-param>
					<xsl:with-param name="size">1 1</xsl:with-param>
					<xsl:with-param name="idx">
						<xsl:value-of select="$idx"/>
					</xsl:with-param>
				</xsl:call-template>
				<xsl:value-of select="normalize-space(.)"/>
			</xsl:when>
			<xsl:when test="@datatype='real'">
				<xsl:call-template name="addAttributes">
					<xsl:with-param name="type">double</xsl:with-param>
					<xsl:with-param name="size">1 1</xsl:with-param>
					<xsl:with-param name="idx">
						<xsl:value-of select="$idx"/>
					</xsl:with-param>
				</xsl:call-template>
				<xsl:value-of select="normalize-space(.)"/>
			</xsl:when>
			<xsl:when test="@datatype='string'">
				<xsl:call-template name="processString">
					<xsl:with-param name="idx">
						<xsl:value-of select="$idx"/>
					</xsl:with-param>
				</xsl:call-template>
			</xsl:when>
		</xsl:choose>
	</xsl:template>

	<xsl:template match="value" mode="arraytype">
		<xsl:value-of select="normalize-space(.)"/>
	</xsl:template>

<!--	<xsl:template match="paramArray">
		<xsl:param name="paramArrayName" select="@name"/>
		<xsl:element name="{$paramArrayName}">
			<xsl:element name="description">
				<xsl:apply-templates select="description"/>
			</xsl:element>
			<xsl:apply-templates select="param" mode="EnumerateArray"/>
		</xsl:element>
	</xsl:template> -->

	<xsl:template match="paramArray">
		<xsl:param name="paramArrayName" select="@name"/>
		<xsl:element name="{$paramArrayName}">
			<xsl:attribute name="idx">1</xsl:attribute>
			<xsl:attribute name="type">struct</xsl:attribute>
			<xsl:attribute name="size">
				<xsl:text>1 1</xsl:text>
			</xsl:attribute>
			<xsl:element name="description">
				<xsl:apply-templates select="description"/>
			</xsl:element>
			<xsl:element name="{$paramArrayName}">
				<xsl:attribute name="idx">1</xsl:attribute>
				<xsl:attribute name="type">param</xsl:attribute>
				<xsl:attribute name="size">
					<xsl:text>1 </xsl:text>
					<xsl:value-of select="@extent"/>
				</xsl:attribute>
				<xsl:apply-templates select="param" mode="EnumerateArray"/>
			</xsl:element>
		</xsl:element>
	</xsl:template>

<!--	<xsl:template match="param" mode="EnumerateArray">
		<xsl:param name="valuename">
			<xsl:text>value__</xsl:text>
			<xsl:value-of select="@index"/>
			<xsl:text>__</xsl:text>
		</xsl:param>
		<xsl:param name="descriptionname">
			<xsl:text>description__</xsl:text>
			<xsl:value-of select="@index"/>
			<xsl:text>__</xsl:text>
		</xsl:param>
		<xsl:element name="{$valuename}">
			<xsl:apply-templates select="value"/>
		</xsl:element>
		<xsl:element name="{$descriptionname}">
			<xsl:apply-templates select="description"/>
		</xsl:element>
	</xsl:template> -->

	<xsl:template match="param" mode="EnumerateArray">
		<xsl:element name="value">
			<xsl:apply-templates select="value">
				<xsl:with-param name="idx">
					<xsl:value-of select="@index"/>
				</xsl:with-param>
			</xsl:apply-templates>
		</xsl:element>
		<xsl:element name="description">
			<xsl:apply-templates select="description">
				<xsl:with-param name="idx">
					<xsl:value-of select="@index"/>
				</xsl:with-param>
			</xsl:apply-templates>
		</xsl:element>
	</xsl:template>

	<xsl:template match="description">
		<xsl:param name="idx">1</xsl:param>
		<xsl:variable name="textstring">
			<xsl:value-of select="normalize-space(.)"/>
		</xsl:variable>
		<xsl:attribute name="type">char</xsl:attribute>
		<xsl:attribute name="idx">
			<xsl:value-of select="$idx"/>
		</xsl:attribute>
		<xsl:attribute name="size">
			<xsl:text>1 </xsl:text>
			<xsl:value-of select="string-length($textstring)"/>
		</xsl:attribute>
		<xsl:value-of select="$textstring"/>
	</xsl:template>

	<xsl:template match="param" mode="description">
		<xsl:apply-templates select="description" mode="paramArray">
			<xsl:with-param name="index">
				<xsl:value-of select="@index"/>
			</xsl:with-param>
		</xsl:apply-templates>
	</xsl:template>

	<xsl:template match="description" mode="paramArray">
		<xsl:param name="index"/>
		<xsl:variable name="textstring">
			<xsl:value-of select="normalize-space(.)"/>
		</xsl:variable>
		<xsl:element name="item">
			<xsl:attribute name="idx">
				<xsl:value-of select="$index"/>
			</xsl:attribute>
			<xsl:attribute name="type">char</xsl:attribute>
			<xsl:attribute name="size">
				<xsl:text>1 </xsl:text>
				<xsl:value-of select="string-length($textstring)"/>
			</xsl:attribute>
			<xsl:value-of select="$textstring"/>
		</xsl:element>
	</xsl:template>

	<xsl:template match="param" mode="element">
		<xsl:apply-templates select="value" mode="arraytype"/>
		<xsl:if test="position()!=last()">
			<xsl:text> </xsl:text>
		</xsl:if>
	</xsl:template>

	<xsl:template name="addAttributes">
		<xsl:param name="type">struct</xsl:param>
		<xsl:param name="idx">1</xsl:param>
		<xsl:param name="size">1 1</xsl:param>
		<xsl:if test="not(@type)">
			<xsl:attribute name="type">
				<xsl:value-of select="$type"/>
			</xsl:attribute>
		</xsl:if>
		<xsl:if test="not(@idx)">
			<xsl:attribute name="idx">
				<xsl:value-of select="$idx"/>
			</xsl:attribute>
		</xsl:if>
		<xsl:if test="not(@size) and $size != ''">
			<xsl:attribute name="size">
				<xsl:value-of select="$size"/>
			</xsl:attribute>
		</xsl:if>
	</xsl:template>

	<xsl:template name="processString">
		<xsl:param name="idx">1</xsl:param>
		<xsl:variable name="textstring">
			<xsl:for-each select="node()">
				<xsl:choose>
					<xsl:when test="local-name()='varref'">
						<xsl:choose>
							<xsl:when test="$mixedcontent=false()">
								<xsl:value-of select="/job/vars/var[@name=normalize-space(current()/text())]/text()"/>
							</xsl:when>
							<xsl:when test="$mixedcontent=true()">
								<xsl:text><![CDATA[<varref>]]></xsl:text>
								<xsl:value-of select="normalize-space(current()/text())"/>
								<xsl:text><![CDATA[</varref>]]></xsl:text>
							</xsl:when>
						</xsl:choose>
					</xsl:when>
					<xsl:when test="local-name()='sep'">
						<xsl:choose>
							<xsl:when test="$mixedcontent=false()">
								<xsl:value-of select="$DIR_SEP"/>
							</xsl:when>
							<xsl:when test="$mixedcontent=true()">
								<xsl:text><![CDATA[<sep/>]]></xsl:text>
							</xsl:when>
						</xsl:choose>
					</xsl:when>
					<xsl:otherwise>
						<xsl:value-of select="normalize-space(.)"/>
					</xsl:otherwise>
				</xsl:choose>
			</xsl:for-each>
		</xsl:variable>
		<xsl:call-template name="addAttributes">
			<xsl:with-param name="type">char</xsl:with-param>
			<xsl:with-param name="size">
				<xsl:text>1 </xsl:text>
				<xsl:value-of select="string-length($textstring)"/>
			</xsl:with-param>
			<xsl:with-param name="idx">
				<xsl:value-of select="$idx"/>
			</xsl:with-param>
		</xsl:call-template>
		<xsl:value-of select="$textstring"/>
	</xsl:template>

	<xsl:template name="addSingleStructAtts">
		<xsl:attribute name="idx">1</xsl:attribute>
		<xsl:attribute name="type">struct</xsl:attribute>
		<xsl:attribute name="size">
			<xsl:text>1 1</xsl:text>
		</xsl:attribute>
	</xsl:template>

</xsl:stylesheet>
