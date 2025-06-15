using Antlr4.Runtime;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace Compiladores
{
    
    public class MyErrorListener : BaseErrorListener, IAntlrErrorListener<IToken>, IAntlrErrorListener<int>
    {
        public List<string> ErrorMessages { get; } = new List<string>();
        public bool HasErrors => ErrorMessages.Count > 0;
        
        private void AddError(string errorType, int line, int charPositionInLine, string msg)
        {
            string errorMessage = $"{errorType} - ({line}:{charPositionInLine}) - {msg}";
            ErrorMessages.Add(errorMessage);
        }
        
        public override void SyntaxError(TextWriter output, IRecognizer recognizer, IToken offendingSymbol, int line, int charPositionInLine, string msg, RecognitionException e)
        {
            AddError("PARSER ERROR", line, charPositionInLine, msg);
        }
        
        public void SyntaxError(TextWriter output, IRecognizer recognizer, int offendingSymbol, int line, int charPositionInLine, string msg, RecognitionException e)
        {
            AddError("LEXER ERROR", line, charPositionInLine, msg);
        }
        
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