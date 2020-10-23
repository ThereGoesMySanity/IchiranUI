using System;
using IchiranUI.KanjiPlugin.ViewModels;
using IchiranUI.KanjiPlugin.Views;
using Kanji.Interface.Plugins;

namespace IchiranUI.KanjiPlugin
{
    public class IchiranPlugin : Plugin
    {
        public override string Image => "";

        public override string Description => "Ichiran connection";

        public override Type ViewModel => typeof(IchiranViewModel);

        public override (Type ViewModel, Type View)[] Steps { get; set; } = new (Type ViewModel, Type View)[]
        {
            (typeof(IchiranImportViewModel), typeof(IchiranImport)),
            (typeof(IchiranRunViewModel), typeof(IchiranRun)),
        };
    }
}