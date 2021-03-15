using System.ComponentModel;
using System.Linq;
using System.Threading.Tasks;
using Kanji.Interface.Actors;
using Kanji.Interface.Models;
using Kanji.Interface.ViewModels;

namespace IchiranUI.KanjiPlugin.ViewModels
{
    public class IchiranRunViewModel : ImportStepViewModel
    {
        private IchiranViewModel ParentMode => base.ParentMode as IchiranViewModel;

        private VocabListViewModel _vocabListVm;
        public VocabListViewModel VocabListVm
        {
            get => _vocabListVm;
            set
            {
                if (_vocabListVm != value)
                {
                    _vocabListVm = value;
                    RaisePropertyChanged();
                }
            }
        }

        private IchiranResponses<IchiranRomanizeResponse> _responses;
        
        public IchiranResponses<IchiranRomanizeResponse> Responses
        {
            get => _responses;
            set
            {
                if (value != _responses)
                {
                    _responses = value;
                    RaisePropertyChanged();
                }
            }
        }

        public IchiranRunViewModel(ImportModeViewModel parentMode) : base(parentMode)
        {
        }

        public override async Task OnEnterStep()
        {
            await ParentMode.Source.Start();
            ParentMode.PropertyChanged += OnPropertyChanged;
            if (ParentMode.SelectedSentence != null)
                await RequestApi();
        }
        
        private async void OnPropertyChanged(object sender, PropertyChangedEventArgs e)
        {
            if (e.PropertyName == "SelectedSentence")
            {
                await RequestApi();
            }
        }

        private async Task RequestApi()
        {
            Responses = await IchiranApi.SendRequest<IchiranRomanizeResponse>(ParentMode.IpAddress, int.Parse(ParentMode.Port), ParentMode.SelectedSentence);
            VocabFilter filter = new VocabFilter
            {
                Vocab = Responses.Responses.SelectMany(r => r.Result.Words.SelectMany(w =>
                                w.Alternatives.Alternatives.SelectMany(a => a.GetVocab))).ToArray(),
            };
            VocabListVm = new VocabListViewModel(filter);
            VocabListVm.KanjiNavigated += (obj, e) => NavigationActor.Instance.NavigateToKanji(e.Character);
        }

        public override bool OnNextStep()
        {
            ParentMode.Source.End();
            return true;
        }

        public override void OnPreviousStep()
        {
            ParentMode.Source.End();
        }
    }
}
