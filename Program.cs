// See https://aka.ms/new-console-template for more information

using generated;

namespace Compiladores
{
    using Antlr4.Runtime;
    using System;
    using System.IO;

    public class Compiler
    {
        public static void Main(string[] args)
        {
            string filePath = "test.mcs"; // Tu archivo de prueba MiniC#

            try
            {
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
                    // Aquí llamarías a tu MiniCSharpChecker con el 'tree'
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
