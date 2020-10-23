
using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Diagnostics.CodeAnalysis;
using System.Linq;
using System.Text;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;

namespace IchiranUI
{
    public struct IchiranResponses
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
        public IchiranResponse[] Responses;
        public string OriginalText;
        public (string value, bool isText)[] SplitText;
        public string RomanizedText => Responses != null? string.Concat(Responses.Zip(SplitText.Where(t => t.isText), (r, d) => (r, d))
                                                    .Aggregate(OriginalText, (s, t) => s.Replace(t.d.value, t.r.Result.RomanizedText))
                                                    .Select(c => punctuationReplacements.GetValueOrDefault(c, c.ToString()))) : 
                                                    "";
    }
    public class IchiranResponse
    {
        public int CurrentResult { get; set; }
        public IchiranResult Result => Results?[CurrentResult];
        [JsonProperty("result")]
        public IchiranResult[] Results { get; set; }
    }
    public class IchiranResult
    {
        [JsonProperty("rank")]
        public int Rank { get; set; }
        public string RomanizedText => string.Join(' ', Words.Select(w => w.Romanized));
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
            if (value["data"]["alternative"] != null) obj.Alternatives = value["data"]["alternative"].ToObject<IchiranMeaning[]>();
            else obj.Alternatives = new[] {value["data"].ToObject<IchiranMeaning>()};
            return obj;
        }

        public override void WriteJson(JsonWriter writer, [AllowNull] IchiranWord value, JsonSerializer serializer)
        {
            serializer.Serialize(writer, value);
        }
    }
    public abstract class IchiranMeaningBase
    {
        [JsonProperty("reading")]
        public string Reading { get; set; }
        [JsonProperty("text")]
        public virtual string Text { get; set; }
        [JsonProperty("kana")]
        public virtual string Kana { get; set; }
        private IchiranGloss[] _glosses;
        [JsonProperty("gloss")]
        public IchiranGloss[] Glosses { get => _glosses; 
        set
        {
            _glosses = value.Select((g, i) => {g.Index = i + 1; g.Parent = this; return g;}).ToArray();
        } }
        public IEnumerable<string> GetVocab
          => ((this is IchiranMeaning a)? ((IchiranMeaningBase[])a.Components ?? a.Conjugations)?.SelectMany(w => w.GetVocab).DefaultIfEmpty(Text) : null) ?? new[]{Text};
    }
    public class IchiranMeaning : IchiranMeaningBase
    {
        [JsonProperty("score")]
        public int Score { get; set; }
        [JsonProperty("seq")]
        public int Seq { get; set; }
        [JsonProperty("conj")]
        public IchiranConjugation[] Conjugations { get; set; }
        [JsonProperty("compound")]
        public string[] Compound { get; set; }
        public string CompoundParts => string.Join('+', Compound);
        [JsonProperty("components")]
        public IchiranMeaning[] Components { get; set; }
        [JsonProperty("suffix")]
        public string Suffix { get; set; }
    }
    public class IchiranConjugation : IchiranMeaningBase
    {
        public override string Text => base.Text ?? (Reading.Contains('【') ? Reading.Substring(0, Reading.IndexOf(" 【")) : Reading);
        public override string Kana => base.Kana ?? (Reading.Contains('【') ? Reading.Substring(Reading.IndexOf(" 【")+2, Reading.IndexOf("】")) : Reading);
        [JsonProperty("prop")]
        public IchiranConjProperty[] Properties { get; set; }
        [JsonProperty("readok")]
        public bool ReadOk { get; set; }
    }
    public class IchiranGloss
    {
        public IchiranMeaningBase Parent { get; set; }
        public int Index { get; set; }
        [JsonProperty("pos")]
        public string Position { get; set; }
        [JsonProperty("gloss")]
        public string Meanings { get; set; }
        [JsonProperty("info")]
        public string Info { get; set; }
    }
    public class IchiranConjProperty
    {
        [JsonProperty("pos")]
        public string Position { get; set; }
        [JsonProperty("type")]
        public string Type { get; set; }
        [JsonProperty("fml")]
        public bool Formal { get; set; }
        [JsonProperty("neg")]
        public bool Negative { get; set; }
    }
}