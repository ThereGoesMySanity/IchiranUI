using Avalonia;
using Avalonia.Controls;
using Avalonia.Markup.Xaml;

namespace IchiranUI.KanjiPlugin.Views
{
    public class SocketSettingsView : UserControl
    {
        public SocketSettingsView()
        {
            InitializeComponent();
        }

        private void InitializeComponent()
        {
            AvaloniaXamlLoader.Load(this);
        }
    }
}