using Avalonia.Interactivity;
using IchiranUI.KanjiPlugin.Sources;
using Kanji.Interface.ViewModels;

namespace IchiranUI.KanjiPlugin.ViewModels
{
    public class HttpViewModel : ViewModel
    {
        private string _ipAddress;
        private string _status;
        private string _port;
        private bool _connectionFailed;

        private readonly HttpSource httpSource;

        public string Port
        {
            get => _port;
            set
            {
                if (value != _port)
                {
                    _port = value;
                    RaisePropertyChanged();
                }
            }
        }

        public string Status
        {
            get => _status;
            set
            {
                if (value != _status)
                {
                    _status = value;
                    RaisePropertyChanged();
                }
            }
        }
        
        public bool ConnectionFailed
        {
            get => _connectionFailed;
            set
            {
                if (value != _connectionFailed)
                {
                    _connectionFailed = value;
                    RaisePropertyChanged();
                }
            }
        }

        public HttpViewModel(HttpSource httpSource)
        {
            Port = "13536";
            this.httpSource = httpSource;
        }

        public void Clear(object sender, RoutedEventArgs e)
        {
            httpSource.Clear();
        }
    }
}
