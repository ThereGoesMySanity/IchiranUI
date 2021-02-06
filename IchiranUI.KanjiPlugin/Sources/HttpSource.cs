using System.Threading.Tasks;
using IchiranUI.KanjiPlugin.ViewModels;
using IchiranUI.KanjiPlugin.Views;
using Avalonia.Controls;
using System.Net;
using System.IO;
using System;

namespace IchiranUI.KanjiPlugin.Sources
{
    public class HttpSource : Source
    {
        private HttpListener listener;
        private Task connectLoop;

        public HttpViewModel ViewModel { get; }

        public override Control SettingsPage {get;}

        public override Control ControlsPage {get;}

        public override string Name => "HTTP Server";

        public HttpSource()
        {
            ViewModel = new HttpViewModel(this);
            SettingsPage = new HttpSettingsView()
            {
                DataContext = ViewModel
            };
            ControlsPage = new HttpRunView()
            {
                DataContext = ViewModel
            };
        }

        public void Clear()
        {
            Sentences.Clear();
        }

        public override void End()
        {
            try
            {
                listener.Stop();
                connectLoop.Wait();
            }
            catch (Exception e)
            {
            }
        }

        public override Task Start()
        {
            listener = new HttpListener();
            listener.Prefixes.Add($"http://localhost:{ViewModel.Port}/");
            listener.Start();
            connectLoop = Task.Run(ConnectLoop);
            return Task.CompletedTask;
        }
        public async Task ConnectLoop()
        {
            while (listener.IsListening)
            {
                var context = await listener.GetContextAsync();
                var request = context.Request;
                using (Stream input = request.InputStream)
                using (StreamReader reader = new StreamReader(input, request.ContentEncoding))
                {
                    string text = reader.ReadToEnd();
                    AddSentences(text);
                }
            }
        }
    }
}
