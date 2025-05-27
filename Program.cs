// See https://aka.ms/new-console-template for more information

using generated;

namespace Compiladores
{
    using Antlr4.Runtime;
    using System;
    using System.IO;
    using Compiladores.Checker; 

    public class Compiler
    {
        public static void Main(string[] args)
        {
            string filePath = "/Volumes/macOs/rooseveltalej/Documents/Compiladores/myProgram.mcs"; // Tu archivo de prueba MiniC#

            if (args.Length > 0) { // Permitir pasar el archivo como argumento
                filePath = args[0];
            }
            
            if (!File.Exists(filePath)) {
                Console.WriteLine($"Error: Input file not found at '{filePath}'");
                return;
            }
            Console.WriteLine($"Compiling main file: {filePath}");
            
            try
            {
                
                // << NUEVO: Crear el CompilationManager >>
                CompilationManager compilationManager = new CompilationManager(filePath);
                
                ICharStream stream = CharStreams.fromPath(filePath);
                MiniCSharpLexer lexer = new MiniCSharpLexer(stream);
                CommonTokenStream tokens = new CommonTokenStream(lexer);
                MiniCSharpParser parser = new MiniCSharpParser(tokens);

                // Crear una instancia de nuestro ErrorListener
                MyErrorListener errorListener = new MyErrorListener();

                // Quitar los listeners por defecto de ANTLR
                lexer.RemoveErrorListeners();
                parser.RemoveErrorListeners();

                // Añadir nuestro listener personalizado
                lexer.AddErrorListener(errorListener);
                parser.AddErrorListener(errorListener);

                // Iniciar el parsing (la regla 'program' es tu punto de entrada)
                MiniCSharpParser.ProgramContext tree = parser.program();

                // Comprobar si hubo errores
                if (errorListener.HasErrors)
                {
                    Console.WriteLine("Compilation failed with Lexer/Parser errors:");
                    Console.WriteLine(errorListener.ToString());
                    // Aquí podrías mostrar los errores en tu GUI
                }
                else
                {
                    Console.WriteLine("Lexer and Parser finished successfully!");
                    Console.WriteLine("Proceeding to Semantic Analysis...");
                    
                    
                    TablaSimbolos mainSymbolTable = new TablaSimbolos(); // Tabla para el archivo principal
                    // << MODIFICADO: Pasar CompilationManager y la tabla de símbolos >>
                    MiniCSharpChecker semanticChecker = new MiniCSharpChecker(compilationManager, mainSymbolTable, filePath);

                    
                    // Visitar el árbol de sintaxis (la raíz, usualmente 'program')
                    // El método Visit del visitor iniciará el recorrido.
                    // 'tree' es el ProgramContext devuelto por parser.program()
                    semanticChecker.Visit(tree);
                    
                    // Comprobar si hubo errores semánticos
                    if (semanticChecker.ErrorMessages.Count > 0)
                    {
                        Console.WriteLine($"Semantic Analysis FAILED with {semanticChecker.ErrorMessages.Count} error(s):");
                        foreach (string error in semanticChecker.ErrorMessages)
                        {
                            Console.WriteLine(error);
                        }
                    }
                    else
                    {
                        Console.WriteLine("Semantic Analysis for main file (and its dependencies) finished successfully! No errors found.");
                        // Aquí, si todo está bien, procederías a la generación de código.
                        semanticChecker.SymbolTable.PrintFlatTable();
                    }
                    
                }
            }
            catch (IOException ex)
            {
                Console.WriteLine($"Error reading file: {ex.Message}");
            }
            catch (Exception ex)
            {
                Console.WriteLine($"An unexpected error occurred: {ex.Message}");
            }
        }
    }
}
