using Newtonsoft.Json.Linq;

namespace IchiranUI
{
    public class IchiranRequest
    {
        public string RequestType { get; set; }
        public string[] Data { get; set; }
        public JProperty Json => new JProperty(RequestType, Data);
    }
}