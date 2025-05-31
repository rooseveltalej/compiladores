using System;
using System.Net.Mime;
using System.Windows.Forms;

namespace Compiladores
{
    public static class Program
    {
        [STAThread]
        public static void Main()
        {
            System.Windows.Forms.Application.EnableVisualStyles();
            System.Windows.Forms.Application.SetCompatibleTextRenderingDefault(false);
            System.Windows.Forms.Application.Run(new MainForm()); // Inicia el formulario
        }
    }
}