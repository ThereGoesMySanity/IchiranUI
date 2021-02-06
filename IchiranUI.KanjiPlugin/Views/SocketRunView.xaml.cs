using Avalonia;
using Avalonia.Controls;
using Avalonia.Markup.Xaml;

namespace IchiranUI.KanjiPlugin.Views
{
    public class SocketRunView : UserControl
    {
        public SocketRunView()
        {
            InitializeComponent();
        }

        private void InitializeComponent()
        {
            AvaloniaXamlLoader.Load(this);
        }
    }
}