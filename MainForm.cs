﻿using System;
using System.Drawing;
using System.IO;
using System.Threading.Tasks;
using System.Windows.Forms;
using System.Runtime.InteropServices;

namespace Compiladores
{
    public partial class MainForm : Form
    {
        private TabControl tabControlEditor;
        private Button btnNewTab;
        private Button btnOpenFile;
        private Button btnSaveFile;
        private Button btnCompile;
        private TextBox txtOutput;
        private StatusStrip statusStrip;
        private ToolStripStatusLabel lblLineInfoStatus;

        [DllImport("user32.dll")]
        private static extern IntPtr SendMessage(IntPtr hWnd, int msg, IntPtr wp, IntPtr lp);
        private const int EM_GETFIRSTVISIBLELINE = 0x00CE;
        private const int EM_LINEINDEX = 0x00BB;

        public MainForm()
        {
            Text = "MiniCSharp Editor Pro";
            WindowState = FormWindowState.Maximized;
            BackColor = Color.FromArgb(30, 30, 30);

            InitializeUiControls();

            string initialFilePath = @"C:\Users\Bayron\RiderProjects\compiladores\finalIntegrationTest.mcs";
            string initialContent = "class Program { void Main() { } }";
            if (File.Exists(initialFilePath))
            {
                try { initialContent = File.ReadAllText(initialFilePath); }
                catch { /* Ignorar */ }
            }
            AddNewTab(Path.GetFileName(initialFilePath), initialFilePath, initialContent);

            Resize += (_, __) => AdjustLayout();
            AdjustLayout();
        }

        private void InitializeUiControls()
        {
            tabControlEditor = new TabControl()
            {
                Location = new Point(10, 45),
                Anchor = AnchorStyles.Top | AnchorStyles.Bottom | AnchorStyles.Left | AnchorStyles.Right,
                DrawMode = TabDrawMode.OwnerDrawFixed,
                Padding = new Point(12, 4)
            };
            tabControlEditor.SelectedIndexChanged += TabControlEditor_SelectedIndexChanged;
            tabControlEditor.DrawItem += TabControlEditor_DrawItem;
            tabControlEditor.MouseClick += TabControlEditor_MouseClick;
            Controls.Add(tabControlEditor);

            btnNewTab = new Button()
            {
                Text = "📄 New", Location = new Point(10, 10), Width = 75, Height = 30, FlatStyle = FlatStyle.Flat,
                BackColor = Color.FromArgb(63, 63, 70), ForeColor = Color.White
            };
            btnNewTab.FlatAppearance.BorderSize = 0;
            btnNewTab.Click += BtnNewTab_Click;
            Controls.Add(btnNewTab);

            btnOpenFile = new Button()
            {
                Text = "📂 Open", Location = new Point(90, 10), Width = 75, Height = 30, FlatStyle = FlatStyle.Flat,
                BackColor = Color.FromArgb(63, 63, 70), ForeColor = Color.White
            };
            btnOpenFile.FlatAppearance.BorderSize = 0;
            btnOpenFile.Click += BtnOpenFile_Click;
            Controls.Add(btnOpenFile);

            btnSaveFile = new Button()
            {
                Text = "💾 Save", Location = new Point(170, 10), Width = 75, Height = 30, FlatStyle = FlatStyle.Flat,
                BackColor = Color.FromArgb(63, 63, 70), ForeColor = Color.White
            };
            btnSaveFile.FlatAppearance.BorderSize = 0;
            btnSaveFile.Click += BtnSaveFile_Click;
            Controls.Add(btnSaveFile);

            btnCompile = new Button()
            {
                Text = "▶️ Guardar y Compilar", Height = 35, FlatStyle = FlatStyle.Flat,
                BackColor = Color.FromArgb(57, 67, 159), ForeColor = Color.White,
                Font = new Font("Segoe UI", 10, FontStyle.Bold)
            };
            btnCompile.FlatAppearance.BorderSize = 0;
            btnCompile.Click += BtnCompile_Click;
            Controls.Add(btnCompile);

            txtOutput = new TextBox()
            {
                Multiline = true, ScrollBars = ScrollBars.Vertical, ReadOnly = true,
                Font = new Font("Consolas", 9), BackColor = Color.FromArgb(20, 20, 20),
                ForeColor = Color.LightGray, BorderStyle = BorderStyle.FixedSingle
            };
            Controls.Add(txtOutput);

            statusStrip = new StatusStrip() { BackColor = Color.FromArgb(45, 45, 45) };
            lblLineInfoStatus = new ToolStripStatusLabel()
            {
                Text = "Línea: 1, Col: 1", ForeColor = Color.White, Font = new Font("Segoe UI", 9)
            };
            statusStrip.Items.Add(lblLineInfoStatus);
            Controls.Add(statusStrip);
        }

        private async void BtnCompile_Click(object sender, EventArgs e)
        {
            if (tabControlEditor.SelectedTab == null) { txtOutput.Text = "No file to compile."; return; }
            var state = tabControlEditor.SelectedTab.Tag as EditorTabState;
            if (state == null || state.EditorControl == null) { txtOutput.Text = "Error accessing current editor."; return; }

            if (string.IsNullOrEmpty(state.FilePath) || state.HasUnsavedChanges)
            {
                if (!SaveCurrentTab()) { txtOutput.Text = "Compilation cancelled: File not saved."; return; }
            }
            if (string.IsNullOrEmpty(state.FilePath)) { txtOutput.Text = "Cannot compile without a valid file path."; return; }

            txtOutput.Text = "Compiling...";
            btnCompile.Enabled = false;

            try
            {
                await Task.Run(() =>
                {
                    var writer = new StringWriter();
                    Console.SetOut(writer);
                    
                    Compiler.Compile(new[] { state.FilePath });
                    
                    writer.Flush();
                    
                    this.Invoke((MethodInvoker)delegate {
                        txtOutput.Text = writer.ToString();
                    });
                });
            }
            catch (Exception ex) 
            { 
                txtOutput.Text = $"Error: {ex.Message}\n{ex.StackTrace}"; 
            }
            finally
            {
                 btnCompile.Enabled = true;
            }
        }

        private void AddNewTab(string tabTitle = "Nuevo", string filePath = null, string content = "")
        {
            TabPage tabPage = new TabPage(tabTitle);
            tabPage.BackColor = Color.FromArgb(30, 30, 30);
            tabPage.Padding = new Padding(3);

            var editorState = new EditorTabState { FilePath = filePath };
            tabPage.Tag = editorState;

            RichTextBox lineNumbersRtb = new RichTextBox()
            {
                Width = 45, Dock = DockStyle.Left, ReadOnly = true,
                ScrollBars = RichTextBoxScrollBars.None, Font = new Font("Consolas", 11),
                BackColor = Color.FromArgb(40, 40, 40), ForeColor = Color.DarkGray,
                BorderStyle = BorderStyle.None, WordWrap = false, RightToLeft = RightToLeft.Yes
            };
            editorState.LineNumbersControl = lineNumbersRtb;
            
            RichTextBox codeEditor = new RichTextBox()
            {
                Dock = DockStyle.Fill, 
                // --- CORRECCIÓN ---
                // Se cambia ForcedBoth por Both, que es universalmente compatible.
                ScrollBars = RichTextBoxScrollBars.Both, 
                WordWrap = false, Font = new Font("Consolas", 11),
                BackColor = Color.FromArgb(25, 25, 25), ForeColor = Color.White,
                BorderStyle = BorderStyle.None,
                AcceptsTab = true, Text = content,
                HideSelection = false
            };
            editorState.EditorControl = codeEditor;

            codeEditor.TextChanged += Editor_TextChanged;
            codeEditor.SelectionChanged += Editor_SelectionChanged;
            codeEditor.VScroll += Editor_VScroll;
            codeEditor.FontChanged += (s, e) => {
                lineNumbersRtb.Font = codeEditor.Font;
                UpdateLineNumbersForCurrentTab();
            };

            Panel editorContainerPanel = new Panel {
                Dock = DockStyle.Fill,
                BorderStyle = BorderStyle.FixedSingle,
                Padding = new Padding(0)
            };
            editorContainerPanel.Controls.Add(codeEditor);
            editorContainerPanel.Controls.Add(lineNumbersRtb);

            tabPage.Controls.Add(editorContainerPanel);
            tabControlEditor.TabPages.Add(tabPage);
            tabControlEditor.SelectedTab = tabPage;

            UpdateLineNumbers(codeEditor, lineNumbersRtb);
            UpdateEditorInfo(codeEditor);
            codeEditor.Focus();
        }

        private void UpdateLineNumbers(RichTextBox editor, RichTextBox lineNumbersRtb)
        {
            if (editor == null || lineNumbersRtb == null || editor.IsDisposed || lineNumbersRtb.IsDisposed) return;

            int totalLines = editor.Lines.Length;
            if (totalLines == 0) totalLines = 1;

            lineNumbersRtb.SuspendLayout();
            if (lineNumbersRtb.Lines.Length != totalLines)
            {
                lineNumbersRtb.Text = "";
                for (int i = 1; i <= totalLines; i++)
                {
                    lineNumbersRtb.AppendText(i + "\n");
                }
                lineNumbersRtb.SelectAll();
                lineNumbersRtb.SelectionAlignment = HorizontalAlignment.Right;
                lineNumbersRtb.DeselectAll();
            }
            lineNumbersRtb.ResumeLayout(false);

            ScrollLineNumbers(editor, lineNumbersRtb);
        }

        private void ScrollLineNumbers(RichTextBox editor, RichTextBox lineNumbers)
        {
             if (editor == null || lineNumbers == null || !editor.IsHandleCreated || !lineNumbers.IsHandleCreated || editor.IsDisposed || lineNumbers.IsDisposed) return;

            int firstVisibleLineEditor = (int)SendMessage(editor.Handle, EM_GETFIRSTVISIBLELINE, IntPtr.Zero, IntPtr.Zero);
            
            if (lineNumbers.Lines.Length > firstVisibleLineEditor && firstVisibleLineEditor >= 0)
            {
                if (lineNumbers.Tag is bool && (bool)lineNumbers.Tag) return;
                lineNumbers.Tag = true;

                int targetCharIndex = lineNumbers.GetFirstCharIndexFromLine(firstVisibleLineEditor);
                if (targetCharIndex >=0 && targetCharIndex < lineNumbers.TextLength) {
                    lineNumbers.Select(targetCharIndex, 0);
                } else if (firstVisibleLineEditor == 0) {
                     lineNumbers.Select(0,0);
                }
                lineNumbers.ScrollToCaret();
                
                lineNumbers.Tag = false;
            }
        }
        
        private void Editor_TextChanged(object sender, EventArgs e)
        {
            var currentEditor = sender as RichTextBox;
            var currentTab = tabControlEditor.SelectedTab;
            if (currentEditor != null && currentTab != null)
            {
                var state = currentTab.Tag as EditorTabState;
                if (state != null)
                {
                    UpdateLineNumbers(currentEditor, state.LineNumbersControl);
                    if (!currentTab.Text.EndsWith("*")) currentTab.Text += "*";
                    state.HasUnsavedChanges = true;
                }
            }
        }
        
        private void Editor_SelectionChanged(object sender, EventArgs e)
        {
            var currentEditor = sender as RichTextBox;
            if (currentEditor != null) UpdateEditorInfo(currentEditor);
        }

        private void UpdateEditorInfo(RichTextBox editor)
        {
            if (editor == null || !editor.IsHandleCreated || editor.IsDisposed)
            {
                 lblLineInfoStatus.Text = ""; return;
            }
            int index = editor.SelectionStart;
            int line = editor.GetLineFromCharIndex(index) + 1;
            int firstCharOfLine = editor.GetFirstCharIndexFromLine(line - 1);
            int col = index - firstCharOfLine + 1;
            lblLineInfoStatus.Text = $"Línea: {line}, Col: {col}";
        }
        
        private void Editor_VScroll(object sender, EventArgs e)
        {
            var currentEditor = sender as RichTextBox;
            var currentTab = tabControlEditor.SelectedTab;
            if (currentEditor != null && currentTab != null)
            {
                var state = currentTab.Tag as EditorTabState;
                if (state != null && state.LineNumbersControl != null)
                {
                    ScrollLineNumbers(currentEditor, state.LineNumbersControl);
                }
            }
        }

        private void AdjustLayout()
        {
            int topButtonPanelHeight = 45;
            int bottomPanelHeight = 180;
            int statusStripHeight = statusStrip.Height;

            tabControlEditor.Location = new Point(10, topButtonPanelHeight);
            tabControlEditor.Width = ClientSize.Width - 20;
            tabControlEditor.Height = ClientSize.Height - topButtonPanelHeight - bottomPanelHeight - statusStripHeight - 10;

            btnCompile.Location = new Point(10, ClientSize.Height - bottomPanelHeight - statusStripHeight + 10);
            btnCompile.Width = 200;

            txtOutput.Location = new Point(10, ClientSize.Height - bottomPanelHeight - statusStripHeight + 50);
            txtOutput.Width = ClientSize.Width - 20;
            txtOutput.Height = bottomPanelHeight - 60;
        }

        private void BtnNewTab_Click(object sender, EventArgs e) => AddNewTab();

        private void BtnOpenFile_Click(object sender, EventArgs e)
        {
            using (OpenFileDialog ofd = new OpenFileDialog())
            {
                ofd.Filter = "MiniCSharp Files (*.mcs)|*.mcs|Text Files (*.txt)|*.txt|All Files (*.*)|*.*";
                if (ofd.ShowDialog() == DialogResult.OK)
                {
                    try
                    {
                        string content = File.ReadAllText(ofd.FileName);
                        AddNewTab(Path.GetFileName(ofd.FileName), ofd.FileName, content);
                    }
                    catch (Exception ex)
                    { MessageBox.Show($"Error opening file: {ex.Message}", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error); }
                }
            }
        }

        private bool SaveCurrentTab(bool forceSaveAs = false)
        {
            if (tabControlEditor.SelectedTab == null) return false;
            var currentTab = tabControlEditor.SelectedTab;
            var state = currentTab.Tag as EditorTabState;
            if (state == null || state.EditorControl == null) return false;

            string currentFilePath = state.FilePath;
            if (forceSaveAs || string.IsNullOrEmpty(currentFilePath) || Path.GetFileName(currentFilePath) == "Nuevo")
            {
                using (SaveFileDialog sfd = new SaveFileDialog())
                {
                    sfd.Filter = "MiniCSharp Files (*.mcs)|*.mcs|Text Files (*.txt)|*.txt|All Files (*.*)|*.*";
                    sfd.FileName = string.IsNullOrEmpty(currentFilePath) || Path.GetFileName(currentFilePath) == "Nuevo" ? "" : Path.GetFileName(currentFilePath);
                    if (sfd.ShowDialog() == DialogResult.OK) currentFilePath = sfd.FileName; else return false;
                }
            }
            try
            {
                File.WriteAllText(currentFilePath, state.EditorControl.Text);
                state.FilePath = currentFilePath;
                currentTab.Text = Path.GetFileName(currentFilePath);
                state.HasUnsavedChanges = false; return true;
            }
            catch (Exception ex)
            { txtOutput.Text = $"Error saving file: {ex.Message}"; return false; }
        }

        private void BtnSaveFile_Click(object sender, EventArgs e) => SaveCurrentTab();
        
        private void TabControlEditor_SelectedIndexChanged(object sender, EventArgs e)
        {
            UpdateLineNumbersForCurrentTab();
            if (tabControlEditor.SelectedTab != null)
            {
                var state = tabControlEditor.SelectedTab.Tag as EditorTabState;
                if (state != null && state.EditorControl != null)
                {
                    UpdateEditorInfo(state.EditorControl);
                    state.EditorControl.Focus();
                } else {
                     lblLineInfoStatus.Text = "";
                }
            } else {
                lblLineInfoStatus.Text = "";
            }
        }
        
        private void UpdateLineNumbersForCurrentTab()
        {
            if (tabControlEditor.SelectedTab != null)
            {
                var state = tabControlEditor.SelectedTab.Tag as EditorTabState;
                if (state != null && state.EditorControl != null && state.LineNumbersControl != null)
                {
                    UpdateLineNumbers(state.EditorControl, state.LineNumbersControl);
                }
            }
        }

        private void TabControlEditor_DrawItem(object sender, DrawItemEventArgs e)
        {
            try
            {
                if (e.Index < 0 || e.Index >= tabControlEditor.TabCount) return;
                
                TabPage page = tabControlEditor.TabPages[e.Index];
                
                Color backgroundColor = (e.State == DrawItemState.Selected) 
                    ? Color.FromArgb(45, 45, 48)
                    : page.BackColor;
                
                using (SolidBrush backgroundBrush = new SolidBrush(backgroundColor))
                {
                    e.Graphics.FillRectangle(backgroundBrush, e.Bounds);
                }

                Rectangle paddedBounds = e.Bounds;
                int yOffset = (e.State == DrawItemState.Selected) ? -1 : 1;
                paddedBounds.Offset(2, yOffset);
                paddedBounds.Width -= 20;
                
                TextRenderer.DrawText(e.Graphics, page.Text, e.Font, paddedBounds, Color.White, TextFormatFlags.Left | TextFormatFlags.VerticalCenter);

                Rectangle closeButton = new Rectangle(e.Bounds.Right - 20, e.Bounds.Top + (e.Bounds.Height - 15) / 2, 15, 15);
                
                using (Font closeFont = new Font("Arial", 8, FontStyle.Bold))
                {
                    Point mousePos = tabControlEditor.PointToClient(Cursor.Position);
                    Brush closeBrush = closeButton.Contains(mousePos) ? Brushes.Red : Brushes.White;
                    e.Graphics.DrawString("x", closeFont, closeBrush, closeButton.Location);
                }
                
                e.DrawFocusRectangle();
            }
            catch { /* Ignorar errores de dibujado */ }
        }

        private void TabControlEditor_MouseClick(object sender, MouseEventArgs e)
        {
            for (int i = 0; i < tabControlEditor.TabCount; i++)
            {
                Rectangle tabBounds = tabControlEditor.GetTabRect(i);
                Rectangle closeButton = new Rectangle(tabBounds.Right - 20, tabBounds.Top + (tabBounds.Height - 15) / 2, 15, 15);
                if (closeButton.Contains(e.Location)) 
                { 
                    CloseTab(tabControlEditor.TabPages[i]);
                    return;
                }
            }
        }

        private void CloseTab(TabPage tabPage)
        {
            var state = tabPage.Tag as EditorTabState;
            if (state != null && state.HasUnsavedChanges)
            {
                var result = MessageBox.Show($"Save changes to {tabPage.Text.TrimEnd('*')}?", "Unsaved Changes", MessageBoxButtons.YesNoCancel, MessageBoxIcon.Warning);
                if (result == DialogResult.Yes) { if (!SaveCurrentTab()) return; }
                else if (result == DialogResult.Cancel) return;
            }
            tabControlEditor.TabPages.Remove(tabPage);
            tabPage.Dispose();
        }

        private class EditorTabState
        {
            public string FilePath { get; set; }
            public bool HasUnsavedChanges { get; set; }
            public RichTextBox EditorControl { get; set; }
            public RichTextBox LineNumbersControl { get; set; }
            public EditorTabState() { HasUnsavedChanges = false; }
        }
    }
}