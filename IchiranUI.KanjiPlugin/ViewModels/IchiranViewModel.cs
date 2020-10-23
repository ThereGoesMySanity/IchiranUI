using IchiranUI.KanjiPlugin.Sources;
using Kanji.Interface.ViewModels;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using Kanji.Interface.Helpers;
using System;

namespace IchiranUI.KanjiPlugin.ViewModels
{
    public class IchiranViewModel : ImportModeViewModel
    {
        private string _ipAddress;
        private string port;
        private Source[] _sources;
        private Source _source;
        private string _selectedSentence;

        public Source[] Sources
        {
            get => _sources;
            set
            {
                if (value != _sources)
                {
                    _sources = value;
                    RaisePropertyChanged();
                }
            }
        }
        public Source Source
        {
            get => _source;
            set
            {
                if (value != _source)
                {
                    _source = value;
                    RaisePropertyChanged();
                }
            }
        }
        public string IpAddress
        {
            get => _ipAddress;
            set
            {
                if (value != _ipAddress)
                {
                    _ipAddress = value;
                    RaisePropertyChanged();
                }
            }
        }
        public string Port
        {
            get => port;
            set
            {
                if (value != port)
                {
                    port = value;
                    RaisePropertyChanged();
                }
            }
        }
        public string SelectedSentence
        {
            get => _selectedSentence;
            set
            {
                if (value != _selectedSentence)
                {
                    _selectedSentence = value;
                    RaisePropertyChanged();
                }
            }
        }
        public IchiranViewModel()
            : base()
        {
            var pluginsDir = Path.Combine(ConfigurationHelper.CommonDataDirectoryPath, "Plugins", "Sources");
            IEnumerable<Assembly> assemblies = new[] { Assembly.GetExecutingAssembly() };
            if (Directory.Exists(pluginsDir))
                assemblies = assemblies.Concat(Directory.GetFiles(pluginsDir, "*.dll").Select(Assembly.LoadFrom));
            
            Sources = assemblies.SelectMany(a => a.GetExportedTypes())
                .Where(t => !t.IsAbstract && t.IsSubclassOf(typeof(Source)))
                .Select(Activator.CreateInstance).Cast<Source>().ToArray();

            IpAddress = "localhost";
            Port = "13535";


            _steps = new List<ImportStepViewModel>(){
                    new IchiranImportViewModel(this),
                    new IchiranRunViewModel(this),
            };
            Initialize();
        }

    }
}