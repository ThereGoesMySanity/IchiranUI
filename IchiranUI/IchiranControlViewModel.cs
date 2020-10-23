using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Net;
using System.Net.Sockets;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using Avalonia.Interactivity;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;

namespace IchiranUI
{
    public class AddedDefinition
    {
        public IchiranMeaningBase Meaning;
        public IchiranGloss[] Definitions;
    }
    public class AddDefinitionsEventArgs : EventArgs
    {
        public AddedDefinition[] Definitions;
    }
    public class IchiranControlViewModel
    {
        private static readonly Dictionary<char, string> punctuationReplacements = new Dictionary<char, string>()
        {
            ['「'] = " \"",
            ['」'] = "\" ",
            ['『'] = " \"",
            ['』'] = "\" ",
            ['（'] = " (",
            ['）'] = ") ",
            ['。'] = ". ",
            ['、'] = ", ",
            ['；'] = ";",
            ['：'] = ":",
            ['　'] = " ",
            ['？'] = "? ",
            ['！'] = "! ",
        };
        private static readonly string japaneseTextRegex = "\u3041-\u309f\u30a0-\u30ff\u31f0-\u31ff\uFF66-\uFF9F\u4e00-\u9fff";
        // private static readonly string englishTextRegex = @"Ａ-Ｚａ-ｚA-Za-z'\-";
        private static readonly Regex separatorRegex = new Regex($"[{japaneseTextRegex}]+|[^{japaneseTextRegex}]+");
        private static readonly Regex textRegex = new Regex($"[{japaneseTextRegex}]");

        public string Text { get; set; }
        public IchiranResponse[] Responses { get; set; }
        public string SubmittedText { get; set; }
        public string RomanizedText => Responses != null? string.Concat(Responses.Zip(Data, (r, d) => (r, d))
                                                    .Aggregate(SubmittedText, (s, t) => s.Replace(t.d, t.r.Result.RomanizedText))
                                                    .Select(c => punctuationReplacements.GetValueOrDefault(c, c.ToString()))) : 
                                                    "";
        public string[] Data { get; set; }
        public ObservableCollection<IchiranGloss> SelectedDefinitions { get; set; } = new ObservableCollection<IchiranGloss>();

        public event EventHandler<AddDefinitionsEventArgs> DefinitionsAdded;

        internal void Clear()
        {
            SelectedDefinitions.Clear();
        }
        internal void AddSelectedDefinitions()
        {
            DefinitionsAdded?.Invoke(this, new AddDefinitionsEventArgs
            {
                Definitions = SelectedDefinitions
                    .Aggregate(new Dictionary<IchiranMeaningBase, List<IchiranGloss>>(), (dict, def) => 
                        {
                            if (!dict.ContainsKey(def.Parent)) dict[def.Parent] = new List<IchiranGloss>();
                            dict[def.Parent].Add(def);
                            return dict;
                        })
                    .Select(pair => new AddedDefinition
                        {
                            Meaning = pair.Key,
                            Definitions = pair.Value.ToArray(),
                        })
                    .ToArray()
            });
        }
        internal async Task SendRequest()
        {
            byte[] recvBuffer = new byte[1024];
            string rawText = Text.Trim();
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
            Responses = sb.ToString().Split('\n').Select(JsonConvert.DeserializeObject<IchiranResponse>).ToArray();
            Data = data;
            SubmittedText = rawText;
        }
    }
}