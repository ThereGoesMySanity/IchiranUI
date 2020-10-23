using System.Collections.Generic;
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

        public IchiranRunViewModel(ImportModeViewModel parentMode) : base(parentMode)
        {
        }

        public override void OnEnterStep()
        {
            ParentMode.Source.Start();
            ParentMode.PropertyChanged += OnPropertyChanged;
            if (ParentMode.SelectedSentence != null)
                RequestApi();

        }
        
        private void OnPropertyChanged(object sender, PropertyChangedEventArgs e)
        {
            if (e.PropertyName == "SelectedSentence")
            {
                RequestApi();
            }
        }

        private void RequestApi()
        {
            IchiranApi.SendRequest(ParentMode.SelectedSentence).ContinueWith(ApiResponse, null);
        }

        private async void ApiResponse(Task<IchiranResponses> task, object state)
        {
            var responses = await task;
            VocabFilter filter = new VocabFilter
            {
                Vocab = responses.Responses.SelectMany(r => r.Result.Words.SelectMany(w =>
                                w.Alternatives.SelectMany(a => a.GetVocab))).ToArray(),
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