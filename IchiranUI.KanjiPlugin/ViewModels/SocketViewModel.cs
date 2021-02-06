using Avalonia.Interactivity;
using IchiranUI.KanjiPlugin.Sources;
using Kanji.Interface.ViewModels;

namespace IchiranUI.KanjiPlugin.ViewModels
{
    public class SocketViewModel : ViewModel
    {
        private string _ipAddress;
        private string _status;
        private string _port;
        private bool _connectionFailed;

        private readonly SocketSource socketSource;
        
        public string  IpAddress
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

        public SocketViewModel(SocketSource socketSource)
        {
            IpAddress = "localhost";
            Port = "13536";
            this.socketSource = socketSource;
        }

        public void Clear(object sender, RoutedEventArgs e)
        {
            socketSource.Clear();
        }

        public async void Reconnect(object sender, RoutedEventArgs e)
        {
            await socketSource.Reconnect();
        }
    }
}
