---
language: coldfusion
filename: learncoldfusion-es.cfm
contributors:
    - ["Wayne Boka", "http://wboka.github.io"]
    - ["Kevin Morris", "https://twitter.com/kevinmorris"]
translators:
    - ["Ivan Alburquerque", "https://github.com/AlburIvan"]
lang: es-es
---

ColdFusion es un lenguaje de scripting para desarrollo web.
[Lea más aquí](Http://www.adobe.com/products/coldfusion-family.html)

### CFML
_**C**old**F**usion **M**arkup **L**anguage_  
ColdFusion comenzó como un lenguaje basado en etiquetas. Casi toda la funcionalidad está disponible usando etiquetas.

```cfm
<em>Se han proporcionado etiquetas HTML para facilitar la lectura.</em>

<!--- Los comentarios comienzan con "<!---" y terminan con "--->" --->
<!---
    Los comentarios
    también
    pueden ocupar
    multiples líneas
--->

<!--- Las etiquetas CFML tienen un formato similar a las etiquetas HTML. --->
<h1>Variables simples</h1>
<!--- Declaración de variables: las variables son débilmente tipadas, similar a javascript --->
<p>Set <b>miVariable</b> to "miValor"</p>
<cfset miVariable = "miValor" />
<p>Set <b>miNumero</b> to 3.14</p>
<cfset miNumero = 3.14 />

<!--- Mostrando datos simples --->
<!--- Use <cfoutput> para valores simples como cadenas, números y expresiones --->
<p>Muestra <b>miVariable</b>: <cfoutput>#miVariable#</cfoutput></p><!--- miValor --->
<p>Muestra <b>miNumero</b>: <cfoutput>#miNumero#</cfoutput></p><!--- 3.14 --->

<hr />

<h1>Variables complejas</h1>
<!--- Declarar variables complejas. --->
<!--- Declarar una matriz de 1 dimensión: literal o notación de corchete --->
<p>Establecer <b>miArreglo1</b> en una matriz de 1 dimensión utilizando la notación literal o de corchete</p>
<cfset miArreglo1 = [] />
<!--- Declarar una matriz de 1 dimensión: notación de función --->
<p>Establecer <b> miArreglo2 </b> en una matriz de 1 dimensión usando la notación de funciones</p>
<cfset miArreglo2 = ArrayNew(1) />

<!--- Salida de variables complejas. --->
<p>Contenidos de <b>miArreglo1</b></p>
<cfdump var="#miArreglo1#" /> <!--- Un objeto de matriz vacío --->
<p>Contenidos de <b>miArreglo2</b></p>
<cfdump var="#miArreglo2#" /> <!--- Un objeto de matriz vacío --->

<!--- Los operadores --->
<!--- Aritméticos --->
<h1>Operadores</h1>
<h2>Aritméticos</h2>
<p>1 + 1 = <cfoutput>#1 + 1#</cfoutput></p>
<p>10 - 7 = <cfoutput>#10 - 7#<br /></cfoutput></p>
<p>15 * 10 = <cfoutput>#15 * 10#<br /></cfoutput></p>
<p>100 / 5 = <cfoutput>#100 / 5#<br /></cfoutput></p>
<p>120 % 5 = <cfoutput>#120 % 5#<br /></cfoutput></p>
<p>120 mod 5 = <cfoutput>#120 mod 5#<br /></cfoutput></p>

<hr />

<!--- Comparación --->
<h2>Comparación</h2>
<h3>Notación estándar</h3>
<p>Is 1 eq 1? <cfoutput>#1 eq 1#</cfoutput></p>
<p>Is 15 neq 1? <cfoutput>#15 neq 1#</cfoutput></p>
<p>Is 10 gt 8? <cfoutput>#10 gt 8#</cfoutput></p>
<p>Is 1 lt 2? <cfoutput>#1 lt 2#</cfoutput></p>
<p>Is 10 gte 5? <cfoutput>#10 gte 5#</cfoutput></p>
<p>Is 1 lte 5? <cfoutput>#1 lte 5#</cfoutput></p>

<h3>Notación alternativa</h3>
<p>Is 1 == 1? <cfoutput>#1 eq 1#</cfoutput></p>
<p>Is 15 != 1? <cfoutput>#15 neq 1#</cfoutput></p>
<p>Is 10 > 8? <cfoutput>#10 gt 8#</cfoutput></p>
<p>Is 1 < 2? <cfoutput>#1 lt 2#</cfoutput></p>
<p>Is 10 >= 5? <cfoutput>#10 gte 5#</cfoutput></p>
<p>Is 1 <= 5? <cfoutput>#1 lte 5#</cfoutput></p>

<hr />

<!--- Estructuras de Control --->
<h1>Estructuras de Control</h1>

<cfset miCondicion = "Prueba" />

<p>Condición a probar: "<cfoutput>#miCondicion#</cfoutput>"</p>

<cfif miCondicion eq "Prueba">
    <cfoutput>#miCondicion#. Estamos probando.</cfoutput>
<cfelseif miCondicion eq "Producción">
    <cfoutput>#miCondicion#. Procede con cuidado!!!</cfoutput>
<cfelse>
    miCondicion es desconocido
</cfif>

<hr />

<!--- Bucles --->
<h1>Bucles</h1>
<h2>Bucle For</h2>
<cfloop from="0" to="10" index="i">
	<p>Index equals <cfoutput>#i#</cfoutput></p>
</cfloop>

<h2>Bucle For Each (Variables complejas)</h2>

<p>Establecer <b>miArreglo3</b> to [5, 15, 99, 45, 100]</p>

<cfset miArreglo3 = [5, 15, 99, 45, 100] />

<cfloop array="#miArreglo3#" index="i">
	<p>Index equals <cfoutput>#i#</cfoutput></p>
</cfloop>

<p>Establecer <b>myArray4</b> to ["Alpha", "Bravo", "Charlie", "Delta", "Echo"]</p>

<cfset myArray4 = ["Alpha", "Bravo", "Charlie", "Delta", "Echo"] />

<cfloop array="#myArray4#" index="s">
	<p>Index equals <cfoutput>#s#</cfoutput></p>
</cfloop>

<h2>Declaración Switch</h2>

<p>Establecer <b>miArreglo5</b> to [5, 15, 99, 45, 100]</p>

<cfset miArreglo5 = [5, 15, 99, 45, 100] />

<cfloop array="#miArreglo5#" index="i">
	<cfswitch expression="#i#">
		<cfcase value="5,15,45" delimiters=",">
			<p><cfoutput>#i#</cfoutput> es un múltiplo de 5.</p>
		</cfcase>
		<cfcase value="99">
			<p><cfoutput>#i#</cfoutput> es noventa y nueve.</p>
		</cfcase>
		<cfdefaultcase>
			<p><cfoutput>#i#</cfoutput> no es 5, 15, 45, or 99.</p>
		</cfdefaultcase> 
	</cfswitch> 
</cfloop>

<hr />

<h1>Conversión de tipos</h1>

<style>
	table.table th, table.table td {
		border: 1px solid #000000;
		padding: 2px;
	}
	
	table.table th {
		background-color: #CCCCCC;
	}
</style>

<table class="table" cellspacing="0">
	<thead>
		<tr>
			<th>Valor</th>
			<th>Como booleano</th>
			<th>Como número</th>
			<th>Como fecha</th>
			<th>Como cadena</th>
		</tr>
	</thead>
	<tbody>
		<tr>
			<th>"Si"</th>
			<td>TRUE</td>
			<td>1</td>
			<td>Error</td>
			<td>"Si"</td>
		</tr>
		<tr>
			<th>"No"</th>
			<td>FALSE</td>
			<td>0</td>
			<td>Error</td>
			<td>"No"</td>
		</tr>
		<tr>
			<th>TRUE</th>
			<td>TRUE</td>
			<td>1</td>
			<td>Error</td>
			<td>"Yes"</td>
		</tr>
		<tr>
			<th>FALSE</th>
			<td>FALSE</td>
			<td>0</td>
			<td>Error</td>
			<td>"No"</td>
		</tr>
		<tr>
			<th>Número</th>
			<td>True si el número no es 0; False de lo contrario.</td>
			<td>Número</td>
			<td>Consulte &#34;Date-time values&#34; anteriormente en este capítulo.</td>
			<td>Representación de cadena del número (for example, &#34;8&#34;).</td>
		</tr>
		<tr>
			<th>Cadena</th>
			<td>Si representa una fecha y hora (ver la siguiente columna), se convierte al valor numérico del objeto de fecha y hora correspondiente. <br> Si es una fecha, hora o marca de tiempo ODBC (por ejemplo, "{ts '2001-06-14 11:30:13'}", o si se expresa en un formato de fecha u hora estándar de EE. UU., incluido al usar nombres de mes completos o abreviados, se convierte al valor de fecha y hora correspondiente. <br> Los días de la semana o la puntuación inusual dan como resultado un error. <br> Generalmente se permiten guiones, barras diagonales y espacios. </td>
			<td>Cadena</td>
		</tr>
		<tr>
			<th>Fecha</th>
			<td>Error</td>
			<td>El valor numérico del objeto fecha-hora.</td>
			<td>Fecha</td>
			<td>una marca de tiempo de ODBC.</td>
		</tr>
	</tbody>
</table>

<hr />

<h1>Componentes</h1>

<em>Código de referencia (las funciones deben devolver algo para admitir IE)</em>
```
```cfs
<cfcomponent>
	<cfset this.hola = "Hola" />
	<cfset this.mundo = "Mundo" />

	<cffunction name="sayHhola">
		<cfreturn this.hola & ", " & this.mundo & "!" />
	</cffunction>
	
	<cffunction name="setHhola">
		<cfargument name="newHola" type="string" required="true" />
		
		<cfset this.hola = arguments.newHola />
		 
		<cfreturn true />
	</cffunction>
	
	<cffunction name="setMundo">
		<cfargument name="newMundo" type="string" required="true" />
		
		<cfset this.mundo = arguments.newMundo />
		 
		<cfreturn true />
	</cffunction>
	
	<cffunction name="getHola">
		<cfreturn this.hola />
	</cffunction>
	
	<cffunction name="getMundo">
		<cfreturn this.mundo />
	</cffunction>
</cfcomponent>

<cfset this.hola = "Hola" />
<cfset this.mundo = "Mundo" />

<cffunction name="sayHola">
	<cfreturn this.hola & ", " & this.mundo & "!" />
</cffunction>

<cffunction name="setHola">
	<cfargument name="newHola" type="string" required="true" />
	
	<cfset this.hola = arguments.newHola />
	 
	<cfreturn true />
</cffunction>

<cffunction name="setMundo">
	<cfargument name="newMundo" type="string" required="true" />
	
	<cfset this.mundo = arguments.newMundo />
	 
	<cfreturn true />
</cffunction>

<cffunction name="getHola">
	<cfreturn this.hola />
</cffunction>

<cffunction name="getMundo">
	<cfreturn this.mundo />
</cffunction>


<b>sayHola()</b>
<cfoutput><p>#sayHola()#</p></cfoutput>
<b>getHola()</b>
<cfoutput><p>#getHola()#</p></cfoutput>
<b>getMundo()</b>
<cfoutput><p>#getMundo()#</p></cfoutput>
<b>setHola("Hola")</b>
<cfoutput><p>#setHola("Hola")#</p></cfoutput>
<b>setMundo("mundo")</b>
<cfoutput><p>#setMundo("mundo")#</p></cfoutput>
<b>sayHola()</b>
<cfoutput><p>#sayHola()#</p></cfoutput>
<b>getHola()</b>
<cfoutput><p>#getHola()#</p></cfoutput>
<b>getMundo()</b>
<cfoutput><p>#getMundo()#</p></cfoutput>
```

### CFScript
_**C**old**F**usion **S**cript_  
En los últimos años, el lenguaje ColdFusion ha agregado sintaxis de script para simular la funcionalidad de etiquetas. Cuando se utiliza un servidor CF actualizado, casi todas las funciones están disponibles mediante la sintaxis de script.

## Otras lecturas

Los enlaces que se proporcionan a continuación son solo para comprender el tema, siéntase libre de buscar en Google y encuentrar ejemplos específicos.

1. [Coldfusion Reference From Adobe](https://helpx.adobe.com/coldfusion/cfml-reference/topics.html)
2. [Open Source Documentation](http://cfdocs.org/)
