
using System.Linq;
using Newtonsoft.Json;

namespace IchiranUI
{
    public class IchiranResponse
    {
        public IchiranWord[] Words { get; set; }
        public string SubmittedText { get; set; }
        public string RomanizedText { get; set; }
    }
    public class IchiranWord
    {
        public IchiranMeaning[] Alternatives { get; set; }
    }
    public class IchiranMeaning
    {
        [JsonProperty("reading")]
        public string Reading { get; set; }
        [JsonProperty("text")]
        public string Text { get; set; }
        [JsonProperty("kana")]
        public string Kana { get; set; }
        [JsonProperty("score")]
        public int Score { get; set; }
        [JsonProperty("seq")]
        public int Seq { get; set; }
        private IchiranGloss[] _glosses;
        [JsonProperty("gloss")]
        public IchiranGloss[] Glosses { get => _glosses; 
        set
        {
            _glosses = value.Select((g, i) => {g.Index = i + 1; return g;}).ToArray();
        } }
        [JsonProperty("conj")]
        public IchiranConjugation[] Conjugations { get; set; }
    }
    public class IchiranGloss
    {
        public int Index { get; set; }
        [JsonProperty("pos")]
        public string Position { get; set; }
        [JsonProperty("gloss")]
        public string Meanings { get; set; }
        [JsonProperty("info")]
        public string Info { get; set; }
    }
    public class IchiranConjugation
    {
        [JsonProperty("prop")]
        public IchiranConjProperty[] Properties { get; set; }
        [JsonProperty("reading")]
        public string Reading { get; set; }

        private IchiranGloss[] _glosses;
        [JsonProperty("gloss")]
        public IchiranGloss[] Glosses { get => _glosses; 
        set
        {
            _glosses = value.Select((g, i) => {g.Index = i + 1; return g;}).ToArray();
        } }
        [JsonProperty("readok")]
        public bool ReadOk { get; set; }
    }
    public class IchiranConjProperty
    {
        [JsonProperty("pos")]
        public string Position { get; set; }
        [JsonProperty("type")]
        public string Type { get; set; }
        [JsonProperty("fml")]
        public bool Formal { get; set; }
    }
}