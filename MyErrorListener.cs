using Antlr4.Runtime;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace Compiladores
{
    // Hacemos que la clase implemente AMBAS interfaces.
    // BaseErrorListener ya nos da IAntlrErrorListener<IToken> implícitamente,
    // pero la añadimos para claridad y añadimos IAntlrErrorListener<int>.
    public class MyErrorListener : BaseErrorListener, IAntlrErrorListener<IToken>, IAntlrErrorListener<int>
    {
        public List<string> ErrorMessages { get; } = new List<string>();
        public bool HasErrors => ErrorMessages.Count > 0;

        // Método privado para centralizar el formato y añadido de errores.
        private void AddError(string errorType, int line, int charPositionInLine, string msg)
        {
            string errorMessage = $"{errorType} - ({line}:{charPositionInLine}) - {msg}";
            ErrorMessages.Add(errorMessage);
        }

        // --- Implementación para IAntlrErrorListener<IToken> (Parser) ---
        // Esta es la que ya teníamos, heredada y sobrescrita.
        public override void SyntaxError(TextWriter output, IRecognizer recognizer, IToken offendingSymbol, int line, int charPositionInLine, string msg, RecognitionException e)
        {
            AddError("PARSER ERROR", line, charPositionInLine, msg);
        }

        // --- Implementación EXPLICITA para IAntlrErrorListener<int> (Lexer) ---
        // Necesitamos implementar esta explícitamente porque BaseErrorListener
        // no tiene una versión para <int>, y necesitamos satisfacer la interfaz.
        public void SyntaxError(TextWriter output, IRecognizer recognizer, int offendingSymbol, int line, int charPositionInLine, string msg, RecognitionException e)
        {
            AddError("LEXER ERROR", line, charPositionInLine, msg);
        }

        // --- Resto de la clase ---
        public override string ToString()
        {
            if (!HasErrors)
            {
                return "Compilation successful (No Lexer/Parser errors)!";
            }

            StringBuilder builder = new StringBuilder();
            builder.AppendLine($"Found {ErrorMessages.Count} Lexer/Parser error(s):");
            foreach (string error in ErrorMessages)
            {
                builder.AppendLine($"- {error}");
            }
            return builder.ToString();
        }

        public void Clear()
        {
            ErrorMessages.Clear();
        }
    }
}