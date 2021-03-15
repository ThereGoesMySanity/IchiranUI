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
        public string Text { get; set; }
        public IchiranRomanizeResponse[] Responses { get; set; }
        public string SubmittedText { get; set; }
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
        public async Task SendRequest()
        {
            var responses = await IchiranApi.SendRequest<IchiranRomanizeResponse>("localhost", 13535, Text);
            Responses = responses.Responses;
            SubmittedText = responses.OriginalText;
            Data = responses.SplitText.Where(t => t.isText).Select(t => t.value).ToArray();
        }
    }
}
