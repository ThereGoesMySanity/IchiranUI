using System;
using System.Threading.Tasks;
using Avalonia;
using Avalonia.Controls;

namespace IchiranUI.KanjiPlugin.Sources
{
    public class TextBoxSource : Source
    {
        public override Control SettingsPage => null;

        public override Control ControlsPage { get; } = new TextBox
        {
            TextWrapping = Avalonia.Media.TextWrapping.Wrap,
            Watermark = "Enter sentence here",
        };

        public override Task Start()
        {
            (ControlsPage as TextBox).GetObservable(TextBox.TextProperty).Subscribe(TextUpdated);
            return Task.CompletedTask;
        }

        private void TextUpdated(string str)
        {
            this.Sentences.Clear();
            if (str == null) return;
            AddSentences(str);
        }

        public override void End()
        {
            (ControlsPage as TextBox).Text = "";
        }

        public override string Name => "Text Box";
    }
}
