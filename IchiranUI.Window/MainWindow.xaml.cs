using Avalonia;
using Avalonia.Controls;
using Avalonia.Markup.Xaml;

namespace IchiranUI.Window
{
    public class MainWindow : Avalonia.Controls.Window
    {
        public MainWindow()
        {
            InitializeComponent();
        }

        private void InitializeComponent()
        {
            AvaloniaXamlLoader.Load(this);
        }
    }
}