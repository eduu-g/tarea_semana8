# Regresión económica: CASEN y ENE

Proyecto de la T3 de Fundamentos de Programación para Análisis Económico. Estima y compara modelos de regresión con datos de CASEN y la Encuesta Nacional de Empleo (ENE).

## Datos

- **CASEN:** archivo reducido `data/raw/casen_reducido.csv`, utilizado para estudiar la asociación entre educación, experiencia, género e ingreso.
- **ENE:** archivo `data/raw/ene-2026-06-mjj.csv`, correspondiente al trimestre móvil mayo-julio de 2026, del Instituto Nacional de Estadísticas (INE). Se utiliza `fact_cal` como factor de expansión en los modelos ponderados.
- **Documentación ENE:** libro de códigos del INE `codigos-ene-2020.pdf`.

## Pregunta y respuesta principal

**Pregunta ENE:** Entre las personas ocupadas, ¿ser mujer se asocia con una probabilidad distinta de empleo informal y cuánto cambia esa diferencia al controlar por edad, educación y región?

En el modelo base ponderado, ser mujer se asocia con una probabilidad de informalidad aproximadamente 3,21 puntos porcentuales mayor que la de los hombres.

Al incorporar edad, edad al cuadrado, educación y región, la diferencia estimada aumenta a aproximadamente 5,94 puntos porcentuales.

Sin ponderar, la diferencia estimada es aproximadamente 6,47 puntos porcentuales; los resultados describen asociaciones y no permiten concluir causalidad.

## Cómo correrlo

1. Abrir el proyecto `tarea_semana8.Rproj` en RStudio.
2. Verificar que los archivos de datos estén en las rutas indicadas en la sección **Datos**.
3. Abrir `scripts/tarea_t3.R` y ejecutar el script desde el comienzo, en orden.
4. El script genera los resultados en `data/processed/`, incluidos `t3_tabla_ene.csv` y `t3_coeficientes_casen.csv`.

El script utiliza los paquetes `dplyr`, `broom` y `modelsummary`, que deben estar disponibles en la instalación de R.

## Estructura

```text
tarea_semana8/
├── data/
│   ├── raw/          # datos originales; no se modifican
│   └── processed/    # tablas y datos procesados
├── scripts/          # código R de la tarea
├── tarea_semana8.Rproj
└── README.md
```

## Uso de inteligencia artificial

```markdown
## Declaración de autoría y uso de IA
- Herramienta utilizada: ChatGPT (o «ninguna»)
- Para qué la usé: Redacción del README. Entender función de algunos codigos y como usarlosS, codigo enviado a CHATGPT para mejorar la redacción de palabras en la interpretacion
- Qué hice yo: Escritura de codigo e interpretación (ej.: elegí la pregunta, los controles y escribí la
interpretación)
- Verificación: confirmo que entiendo y puedo explicar todo lo que entrego.
```

## Autor

Eduardo Guerrero — octubre de 2026
