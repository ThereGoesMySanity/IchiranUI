using System;
using System.Collections.Generic;
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
        private static readonly string japaneseTextRegex = "\u3041-\u309f\u30a0-\u30ff\u31f0-\u31ff\uFF66-\uFF9F\u4e00-\u9fff";
        // private static readonly string englishTextRegex = @"Ａ-Ｚａ-ｚA-Za-z'\-";
        private static readonly Regex separatorRegex = new Regex($"[{japaneseTextRegex}]+|[^{japaneseTextRegex}]+");
        private static readonly Regex textRegex = new Regex($"[{japaneseTextRegex}]");
        public IchiranControl()
        {
            DataContext = new IchiranResponse();
            InitializeComponent();
        }
        private async void OnClicked(object sender, RoutedEventArgs e)
        {
            byte[] recvBuffer = new byte[1024];
            string rawText = this.FindControl<TextBox>("Text").Text.Trim();
            (string value, bool text)[] splitText = separatorRegex.Matches(rawText).Select(m => (m.Value, textRegex.IsMatch(m.Value.Substring(0, 1)))).ToArray();
            string[] data = splitText.Where(t => t.text).Select(t => t.value).ToArray();
            var request = new IchiranRequest
            {
                RequestType = "romanize",
                Data = data,
            };
            IPHostEntry ipHost = Dns.GetHostEntry(Dns.GetHostName());
            IPAddress ipAddr = IPAddress.Parse("127.0.0.1");
            IPEndPoint endpoint = new IPEndPoint(ipAddr, 13535);
            Socket client = new Socket(ipAddr.AddressFamily, SocketType.Stream, ProtocolType.Tcp);
            await client.ConnectAsync(endpoint);
            var buffer = Encoding.UTF8.GetBytes($"{new JObject(request.Json).ToString(Formatting.None)}\n");
            await client.SendAsync(new ArraySegment<byte>(buffer), SocketFlags.None);
            var sb = new StringBuilder();
            int num = await client.ReceiveAsync(new ArraySegment<byte>(recvBuffer), SocketFlags.None);
            sb.Append(Encoding.UTF8.GetString(recvBuffer, 0, num));
            while (client.Available > 0 || sb.ToString().Count(c => c == '\n') != data.Length)
            {
                num = await client.ReceiveAsync(new ArraySegment<byte>(recvBuffer), SocketFlags.None);
                sb.Append(Encoding.UTF8.GetString(recvBuffer, 0, num));
            }
            var response = new IchiranResponses
            {
                Responses = sb.ToString().Split('\n').Select(JsonConvert.DeserializeObject<IchiranResponse>).ToArray(),
                Data = data,
                SubmittedText = rawText,
            };
            DataContext = response;
        }

        private void InitializeComponent()
        {
            AvaloniaXamlLoader.Load(this);
        }
    }
}