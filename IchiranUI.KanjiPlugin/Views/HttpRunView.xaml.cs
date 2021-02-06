using Avalonia;
using Avalonia.Controls;
using Avalonia.Markup.Xaml;

namespace IchiranUI.KanjiPlugin.Views
{
    public class HttpRunView : UserControl
    {
        public HttpRunView()
        {
            InitializeComponent();
        }

        private void InitializeComponent()
        {
            AvaloniaXamlLoader.Load(this);
        }
    }
}
