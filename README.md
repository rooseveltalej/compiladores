# Proyecto Compilador de MiniCSharp

Este proyecto es un compilador completo para un subconjunto del lenguaje de programación C#, denominado "MiniCSharp". El compilador está desarrollado en C# y utiliza ANTLR4 para el análisis sintáctico. Incluye un editor de código básico con resaltado de sintaxis (futura implementación) y numeración de líneas, desde el cual se pueden compilar y ejecutar los programas de MiniCSharp.

## Características Principales

* **Análisis Léxico y Sintáctico**: Implementado con ANTLR4 para reconocer la gramática de MiniCSharp.
* **Análisis Semántico**: Realiza comprobaciones de tipos, declaraciones de variables, y correctitud de llamadas a métodos.
* **Generación de Código**: Genera ensamblados en memoria (IL) que son ejecutados dinámicamente.
* **Manejo de Símbolos**: Utiliza una tabla de símbolos con manejo de ámbitos (scopes) para gestionar variables, métodos y clases.
* **Editor de Código Integrado**: Una aplicación de Windows Forms que sirve como un IDE simple para escribir, compilar y ver la salida de los programas MiniCSharp.
* **Manejo de Módulos**: Soporta la importación de otras clases de MiniCSharp a través de la directiva `using`.

## Estructura del Proyecto

El proyecto está organizado en los siguientes directorios y archivos principales:

* **`/` (Raíz del proyecto)**
    * `Compiler.cs`: Orquesta todo el proceso de compilación, desde el análisis léxico hasta la ejecución.
    * `MainForm.cs` y `MainForm.Designer.cs`: Implementan la interfaz gráfica de usuario del editor.
    * `Program.cs`: Punto de entrada de la aplicación Windows Forms.

* **`/Checker`**
    * `MiniCSharpChecker.cs`: Implementa el visitante (visitor) que recorre el árbol sintáctico para realizar el análisis semántico.
    * `TablaSimbolos.cs`: Define las estructuras para la tabla de símbolos, incluyendo los tipos, símbolos y ámbitos.
    * `CompilationManager.cs`: Gestiona la compilación de módulos externos importados con `using`.

* **`/CodeGen`**
    * `MiniCSharpCodeGenerator.cs`: Genera el código IL (Intermediate Language) a partir del árbol sintáctico validado.

* **`/generated`**
    * Contiene los archivos C# generados por ANTLR a partir de las gramáticas (`.g4`), como el lexer, el parser, el listener y el visitor.

* **`/obj` y `/bin`**
    * Directorios de compilación de .NET que contienen los artefactos de la compilación del propio compilador.

## El Lenguaje MiniCSharp

MiniCSharp es un lenguaje de programación procedural y orientado a objetos simplificado. Sus características incluyen:

### Tipos de Datos

* `int`: Entero de 32 bits.
* `double`: Punto flotante de doble precisión.
* `char`: Caracter Unicode.
* `bool`: Booleano (`true` o `false`).
* `string`: Cadena de caracteres.
* `void`: Representa la ausencia de un valor de retorno.
* **Arrays**: Se pueden crear arrays de `int`, `double` y `char` (ej. `int[]`).
* **Clases**: Se pueden definir clases que contienen variables miembro.

### Estructura del Programa

Un programa en MiniCSharp consiste en una clase principal que puede contener declaraciones de variables, otras clases anidadas y métodos.

```csharp
using OtroModulo; // Opcional

class MiPrograma
{
    // Variables globales a la clase
    int version;

    // Clases anidadas
    class MiClaseAnidada
    {
        // ...
    }

    // Métodos
    void Main()
    {
        // Código del programa principal
    }
}
```

### Declaraciones

* **Variables**: `tipo nombreVariable;` o `tipo var1, var2;`.
* **Métodos**: `tipoRetorno nombreMetodo(tipo param1, tipo param2) { ... }`. El tipo de retorno puede ser `void`.
* **Clases**: `class NombreClase { ... }`.

### Sentencias

* **Asignación**: `variable = expresion;`
* **Incremento/Decremento**: `variable++;` o `variable--;`
* **Llamada a método**: `objeto.metodo(params);` o `metodo(params);`
* **Condicional `if-else`**:
    ```csharp
    if (condicion)
        sentencia1;
    else
        sentencia2;
    ```
* **Bucle `while`**:
    ```csharp
    while (condicion)
        sentencia;
    ```
* **Bucle `for`**:
    ```csharp
    for (expresionInicial; condicion; sentenciaIteracion)
        sentenciaCuerpo;
    ```
* **Switch**:
    ```csharp
    switch (expresion)
    {
        case constante1:
            // sentencias
            break;
        case constante2:
            // sentencias
            break;
        default:
            // sentencias
            break;
    }
    ```
* **`break`**: Para salir de bucles y `switch`.
* **`return`**: Para devolver un valor de un método.
* **`read(variable)`**: Lee un valor desde la entrada estándar y lo asigna a la variable.
* **`write(expresion)`**: Escribe el valor de una expresión en la salida estándar.

### Expresiones

Soporta operaciones aritméticas (`+`, `-`, `*`, `/`, `%`), lógicas (`&&`, `||`) y relacionales (`==`, `!=`, `<`, `<=`, `>`, `>=`).

## Cómo Funciona el Compilador

El proceso de compilación se realiza en varias fases:

1.  **Entrada de Código**: El usuario escribe el código en el editor de la `MainForm`.
2.  **Análisis Léxico y Sintáctico**:
    * Al presionar "Guardar y Compilar", el código se guarda en un archivo `.mcs`.
    * `Compiler.cs` inicia el proceso.
    * Se crea un `MiniCSharpLexer` que convierte el código en una secuencia de tokens.
    * Un `MiniCSharpParser` utiliza estos tokens para construir un Árbol de Sintaxis Abstracta (AST).
    * Se utiliza un `MyErrorListener` para capturar errores de sintaxis.
3.  **Análisis Semántico**:
    * Se instancia `MiniCSharpChecker` y se invoca su método `Visit` sobre el AST.
    * El *checker* recorre el árbol, llenando y consultando la `TablaSimbolos` para verificar:
        * Que los tipos se usen correctamente.
        * Que las variables estén declaradas antes de usarse.
        * Que las llamadas a métodos coincidan con sus firmas.
        * Se resuelven las directivas `using` a través del `CompilationManager`.
4.  **Generación de Código**:
    * Si el análisis semántico es exitoso, se crea una instancia de `MiniCSharpCodeGenerator`.
    * Este *visitor* recorre nuevamente el AST y emite instrucciones de Lenguaje Intermedio (IL) de .NET utilizando `System.Reflection.Emit`.
    * El código IL se genera en un ensamblado dinámico en memoria.
5.  **Ejecución**:
    * Una vez generado el ensamblado, el `Compiler` busca el método `Main` en la clase principal.
    * Utilizando `System.Reflection`, se invoca el método `Main` y la salida del programa se redirige al cuadro de texto de salida del editor.

## Cómo Usar

1.  Abre la solución en un IDE compatible con .NET (como JetBrains Rider o Visual Studio).
2.  Ejecuta el proyecto. Esto abrirá el "MiniCSharp Editor Pro".
3.  Escribe tu código MiniCSharp en una de las pestañas o abre un archivo `.mcs`.
4.  Haz clic en el botón **"▶️ Guardar y Compilar"**.
5.  El editor te pedirá que guardes el archivo si no lo has hecho.
6.  El resultado de la compilación y la ejecución (si es exitosa) aparecerá en el panel de salida en la parte inferior.
