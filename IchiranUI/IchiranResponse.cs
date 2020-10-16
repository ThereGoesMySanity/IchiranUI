
using System;
using System.Diagnostics.CodeAnalysis;
using System.Linq;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;

namespace IchiranUI
{
    public class IchiranResponse
    {
        [JsonProperty]
        public IchiranSentence[] Results { get; set; }
        public string SubmittedText { get; set; }
    }
    public class IchiranSentence
    {
        [JsonProperty("rank")]
        public int Rank { get; set; }
        public string Romanized => string.Join(' ', Words.Select(w => w.Romanized));
        [JsonProperty("words", ItemConverterType = typeof(WordConverter))]
        public IchiranWord[] Words { get; set; }
    }
    public class IchiranWord
    {
        [JsonProperty("romanized")]
        public string Romanized { get; set; }
        public IchiranMeaning[] Alternatives { get; set; }
    }
    public class WordConverter : JsonConverter<IchiranWord>
    {
        public override IchiranWord ReadJson(JsonReader reader, Type objectType, [AllowNull] IchiranWord existingValue, bool hasExistingValue, JsonSerializer serializer)
        {
            JToken value = JToken.ReadFrom(reader);
            IchiranWord obj = value.ToObject<IchiranWord>();
            if (value["data"] is JArray) obj.Alternatives = value["data"]["alternative"].ToObject<IchiranMeaning[]>();
            else obj.Alternatives = new[] {value["data"].ToObject<IchiranMeaning>()};
            return obj;
        }

        public override void WriteJson(JsonWriter writer, [AllowNull] IchiranWord value, JsonSerializer serializer)
        {
            serializer.Serialize(writer, value);
        }
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