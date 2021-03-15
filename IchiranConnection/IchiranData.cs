using System;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.Linq;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;

namespace IchiranUI
{
    public class IchiranResponses<T>
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
        public T[] Responses;
        public string OriginalText;
        public (string value, bool isText)[] SplitText;
        public string RomanizedText => (Responses != null && Responses is IchiranRomanizeResponse[] list)? string.Concat(list.Zip(SplitText.Where(t => t.isText), (r, d) => (r, d))
                                                    .Aggregate(OriginalText, (s, t) => s.Replace(t.d.value, t.r.Result.RomanizedText))
                                                    .Select(c => punctuationReplacements.GetValueOrDefault(c, c.ToString()))) : "";
    }
    public class IchiranSegmentRootResponse
    {
        [JsonProperty("result")]
        public IchiranSegmentRootResult[] Results { get; set; }
    }
    public class IchiranSegmentRootResult
    {
        [JsonProperty("seq")]
        public long Sequence { get; set; }
        [JsonProperty("kana")]
        public string Kana { get; set; }
        [JsonProperty("kanji")]
        public string? Kanji { get; set; }

        public override bool Equals(object obj)
        {
            if (obj == null) return false;
            return (obj is IchiranSegmentRootResult res) 
                && res.Sequence == this.Sequence
                && res.Kana == this.Kana
                && res.Kanji == this.Kanji;
        }

        public override int GetHashCode()
        {
            return HashCode.Combine(Sequence, Kana, Kanji);
        }
    }
    public class IchiranRomanizeResponse
    {
        public int CurrentResult { get; set; }
        public IchiranRomanizeResult Result => Results?[CurrentResult];
        [JsonProperty("result")]
        public IchiranRomanizeResult[] Results { get; set; }
    }
    public class IchiranSegmentResponse
    {
        [JsonProperty("result")]
        public IchiranAlternatives[] Results { get; set; }
    }
    public class IchiranRomanizeResult
    {
        [JsonProperty("rank")]
        public int Rank { get; set; }
        public string RomanizedText => string.Join(' ', Words.Select(w => w.Romanized));
        [JsonProperty("words", ItemConverterType = typeof(AlternativeConverter))]
        public IchiranWord[] Words { get; set; }
    }
    public class IchiranWord
    {
        [JsonProperty("romanized")]
        public string Romanized { get; set; }
        [JsonProperty("data")]
        public IchiranAlternatives Alternatives { get; set; }
    }
    [JsonConverter(typeof(AlternativeConverter))]
    public class IchiranAlternatives
    {
        public IchiranMeaning[] Alternatives { get; set; }
    }
    public class AlternativeConverter : JsonConverter<IchiranAlternatives>
    {
        public override IchiranAlternatives ReadJson(JsonReader reader, Type objectType, [AllowNull] IchiranAlternatives existingValue, bool hasExistingValue, JsonSerializer serializer)
        {
            JToken value = JToken.ReadFrom(reader);
            IchiranAlternatives obj = new IchiranAlternatives();
            if (value["alternative"] != null) obj.Alternatives = value["alternative"].ToObject<IchiranMeaning[]>();
            else obj.Alternatives = new[] {value.ToObject<IchiranMeaning>()};
            return obj;
        }

        public override void WriteJson(JsonWriter writer, [AllowNull] IchiranAlternatives value, JsonSerializer serializer)
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
        public override string Kana => base.Kana ?? (Reading.Contains('【') ? Reading.Substring(Reading.IndexOf(" 【")+2, Reading.IndexOf("】") - Reading.IndexOf(" 【") - 2) : Reading);
        [JsonProperty("via")]
        public IchiranConjugation[] Via { get; set; }
        [JsonProperty("prop")]
        public IchiranConjProperty[] Properties { get; set; }
        [JsonProperty("readok")]
        [JsonConverter(typeof(BoolConverter))]
        public bool ReadOk { get; set; }
    }
    public class BoolConverter : JsonConverter<bool>
    {
        public override bool ReadJson(JsonReader reader, Type objectType, [AllowNull] bool existingValue, bool hasExistingValue, JsonSerializer serializer)
        {
            JToken value = JToken.ReadFrom(reader);
            if (value.Type == JTokenType.Boolean) return (bool)value;
            else if (value is JArray arr && arr.Count == 0) return false;
            return false;
        }

        public override void WriteJson(JsonWriter writer, [AllowNull] bool value, JsonSerializer serializer)
        {
            serializer.Serialize(writer, value);
        }
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
