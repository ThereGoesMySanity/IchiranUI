using System.Linq;
using System;
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

        public override void Start()
        {
            (ControlsPage as TextBox).GetObservable(TextBox.TextProperty).Subscribe(TextUpdated);
        }

        private void TextUpdated(string str)
        {
            this.Sentences.Clear();
            if (str == null) return;
            foreach (string s in str.Split(new[]{'\n', '.', '。'}))
            {
                Sentences.Add(s);
            }
        }

        public override void End()
        {
            (ControlsPage as TextBox).Text = "";
        }

        public override string Name => "Text Box";
    }
}