using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.Linq;
using System.Net;
using System.Net.Sockets;
using System.Text;
using System.Text.RegularExpressions;
using Avalonia;
using Avalonia.Controls;
using Avalonia.Interactivity;
using Avalonia.Markup.Xaml;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;

namespace IchiranUI
{
    public class IchiranControl : UserControl
    {
        private IchiranControlViewModel ViewModel => (IchiranControlViewModel)DataContext;
        public event EventHandler<AddDefinitionsEventArgs> DefinitionsAdded;
        public IchiranControl()
        {
            DataContext = new IchiranControlViewModel();
            ViewModel.DefinitionsAdded += DefinitionsAdded;
            InitializeComponent();
        }
        internal async void SendRequest()
        {
            await ViewModel.SendRequest();
            this.FindControl<ItemsControl>("Responses").Items = ViewModel.Responses;
        }
        private void InitializeComponent()
        {
            AvaloniaXamlLoader.Load(this);
        }
    }
}