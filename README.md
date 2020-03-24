# 1. Objetivo

El Covid19 es un tema de salud pública y como tal queremos ayudar a todos los interesados en el uso, distribución y análisis de los datos que genera la Secretaría de Salud.

# 2. Información

Siguiendo las propuestas de varios actores de la sociedad civil, ponemos a disposición publica los siguientes contenidos:

<img src="imagenes/folder_icon.png" width="20" height="15"/> **data**: Datos oficiales de casos sospechosos y confirmados publicados por la Secretaría de Salud en formatos **csv** desde el 17 de Marzo de 2020.

<img src="imagenes/folder_icon.png" width="20" height="15"/>  **output**: 

   - Confirmados: Información públicada de los casos confirmados hasta el último corte, con la fecha en que cada uno de los casos fue publicado. Esta base es actualizada diariamente *de manera manual* para incluir los nuevos casos que da a conocer la Secretaría de Salud. Debido a esto podemos incurrir en errores propios o por parte de la misma Secretaría de Salud. 
     
     De hecho, como menciona [@guzmart_](https://github.com/guzmart/covid19_mex) en un ejercicio similar, y corroborado por nosotros, los datos publicados el 20 de Marzo de 2020 toman como un nuevo caso a un paciente ya considerado en la información del día anterior. Ver más información acerca de errores en la sección 4.

   - Acumulados: Datos acumulados de los casos confirmados, sospechosos, negativos, recuperados, fallecidos y portadores obtenidos de los [Comunicados Técnicos Diarios](https://www.gob.mx/salud/documentos/informacion-internacional-y-nacional-sobre-nuevo-coronavirus-2019-ncov) de la Secretaría de Salud.

<img src="imagenes/folder_icon.png" width="20" height="15"/>  **pdf_SS**: Originales de las Tablas de casos positivos (desde el 17 ed Marzo) y sospechosos (desde el 20 de Marzo) que publica la Secretaría de Salud [aquí](https://www.gob.mx/salud/documentos/coronavirus-covid-19-comunicado-tecnico-diario-238449/).


# 3. Análisis

![](imagenes/01_casos_acumulados.png)

![](imagenes/02_nuevos_casos.png)

![](imagenes/03_casos_por_sexo.png)

![](imagenes/04_casos_por_edad.png)

![](imagenes/06_casos_por_estado.png)

![](imagenes/07_casos_por_sexo_tiempo.png)

![](imagenes/08_casos_acumulados_latam.png)

# 4. Errores de fuente

## Tabla de positivos

Las tablas de la Secretaria de Salud reportan diariamente las tablas de casos positivos en formato pdf, resaltando en color *azul* los *casos nuevos*. En esta sección se documentan errores en las versiones de las tablas.

    - En la tabla del 20.03.2020 se marco el **caso 163** como caso nuevo, sin embargo este caso esta confirmado en la tabla del 19.03.2020 (**caso 149**)
    - La tabla del 21-03-2020 no reporta en azul, es decir como casos nuevos, los **casos 226, 227, 228, 229**, pero estos casos no se encuentran en la tabla del 20.03.2020. Además, en la tabla del 20.03.2020 se reporta el **caso 225**, que *desaparece* en la tabla del 21.03.2020.
