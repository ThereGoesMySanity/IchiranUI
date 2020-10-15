using System;
using System.Linq;
using System.Net;
using System.Net.Sockets;
using System.Text;
using Avalonia;
using Avalonia.Controls;
using Avalonia.Interactivity;
using Avalonia.Markup.Xaml;
using Newtonsoft.Json.Linq;

namespace IchiranUI
{
    public class IchiranControl : UserControl
    {
        public IchiranControl()
        {
            DataContext = new IchiranResponse();
            InitializeComponent();
        }
        private async void OnClicked(object sender, RoutedEventArgs e)
        {
            string text = this.FindControl<TextBox>("Text").Text.Trim();
            var recvBuffer = new byte[1024];
            IPHostEntry ipHost = Dns.GetHostEntry(Dns.GetHostName());
            IPAddress ipAddr = IPAddress.Parse("127.0.0.1");
            IPEndPoint endpoint = new IPEndPoint(ipAddr, 13535);
            Socket client = new Socket(ipAddr.AddressFamily, SocketType.Stream, ProtocolType.Tcp);
            await client.ConnectAsync(endpoint);
            var buffer = Encoding.UTF8.GetBytes($"{text}\n");
            await client.SendAsync(new ArraySegment<byte>(buffer), SocketFlags.None);
            var sb = new StringBuilder();
            int num = await client.ReceiveAsync(new ArraySegment<byte>(recvBuffer), SocketFlags.None);
            sb.Append(Encoding.UTF8.GetString(recvBuffer, 0, num));
            while (client.Available > 0)
            {
                num = await client.ReceiveAsync(new ArraySegment<byte>(recvBuffer), SocketFlags.None);
                sb.Append(Encoding.UTF8.GetString(recvBuffer, 0, num));
            }
            IchiranResponse response = new IchiranResponse
            {
                Words = sb.ToString().Split('\n', StringSplitOptions.RemoveEmptyEntries)
                        .Select(JToken.Parse)
                        .Select(o => new IchiranWord
                        {
                            Alternatives = (o["alternative"] != null)? 
                                            o["alternative"].ToObject<IchiranMeaning[]>() 
                                            : new IchiranMeaning[] { o.ToObject<IchiranMeaning>() }
                        }).ToArray(),
                SubmittedText = text,
                RomanizedText = "",

            };
            DataContext = response;
        }

        private void InitializeComponent()
        {
            AvaloniaXamlLoader.Load(this);
        }
    }
}