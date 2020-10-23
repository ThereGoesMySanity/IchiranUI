
using System;
using System.Collections.ObjectModel;
using Avalonia.Controls;
using Kanji.Interface.Utilities;

namespace IchiranUI.KanjiPlugin.Sources
{
    public abstract class Source : NotifyPropertyChanged
    {
        private ObservableCollection<string> _sentences;
        public ObservableCollection<string> Sentences
        {
            get => _sentences;
            set
            {
                if (_sentences != value)
                {
                    _sentences = value;
                    RaisePropertyChanged();
                }
            }
        }
        public abstract void Start();
        public abstract void End();
        public abstract Control SettingsPage { get; }
        public abstract Control ControlsPage { get; }
        public abstract string Name { get; }

        public Source()
        {
            Sentences = new ObservableCollection<string>();
        }
    }
}