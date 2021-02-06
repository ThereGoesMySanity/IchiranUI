using Kanji.Interface.ViewModels;

namespace IchiranUI.KanjiPlugin.ViewModels
{
    public class IchiranImportViewModel : ImportStepViewModel
    {
        public IchiranImportViewModel(ImportModeViewModel parentMode) : base(parentMode)
        {
        }

        public override bool OnNextStep()
        {
            return base.OnNextStep();
        }

        public override void OnPreviousStep()
        {
            base.OnPreviousStep();
        }
    }
}
